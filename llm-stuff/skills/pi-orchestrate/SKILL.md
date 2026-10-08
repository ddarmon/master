---
name: pi-orchestrate
description: >-
  Orchestrate a fleet of background `pi` CLI agents backed by Ollama-hosted
  models (GLM, Qwen, Kimi, DeepSeek, gemma, etc.) and monitor them live. Use
  when the user wants to delegate experiments, searches, or grunt work to
  local / non-Claude models running under `pi`, run several such workers in
  parallel as an orchestrator, watch their activity in real time, or reuse the
  pi-agent launch + live-monitor + collect harness. Triggers on: "orchestrate
  pi agents", "delegate to GLM/Ollama agents", "launch background pi workers",
  "have GLM run the experiment", "monitor pi sessions", "fan out pi agents".
---

# pi-orchestrate

Launch, monitor, and collect from background `pi` worker agents (backed by any
Ollama-hosted model) while you act as the orchestrator. Model-agnostic and
task-agnostic: you supply the task and any instrument; this skill supplies the
robust launch/monitor/collect mechanics.

## When to use vs. not

Use this to delegate to **non-Claude, local/cheap models** for parallelism,
model-diverse cross-checks, or offloading grunt work out of your own context.
For Claude-native subagents, prefer the Agent/Workflow tools instead — this
skill exists specifically for the `pi` + Ollama path.

## The one thing that makes monitoring work

`pi -p` **buffers stdout** when it is piped/redirected (Node/libuv, not libc —
`stdbuf` cannot fix it; BSD `script` fails headless). A mid-run kill then loses
everything. But `pi` **also appends its session transcript to disk
incrementally, per message, regardless of TTY**:

```
~/.pi/agent/sessions/<cwd-slug>/<timestamp>_<uuid>.jsonl
```

So **monitor the session log, not stdout.** Give each agent its own
`--session-dir` so its file is unambiguous, and let its stdout go to
`/dev/null`.

## Procedure

### 1. Preflight (fail fast)

```bash
which pi ollama || { echo "pi/ollama missing"; exit 1; }
curl -s http://$HOST/api/tags | python3 -c "import sys,json;print([m['name'] for m in json.load(sys.stdin)['models']])"
OLLAMA_HOST=$HOST pi -p --provider ollama --model $MODEL --thinking off --no-tools "Reply with the single word: PONG"
```
Confirm the target Ollama model(s) the *workers will probe* are also present.

### 2. Launch each worker (background, own session dir, stdin closed)

```bash
RUN=<scratchpad>/run_$$          # per-run root
mkdir -p "$RUN/agentA"
OLLAMA_HOST=$HOST pi -p \
  --session-dir "$RUN/agentA" \
  --provider ollama --model $MODEL --thinking low \
  --tools bash,read,write --name agentA \
  "<AGENT BRIEF>" </dev/null >/dev/null 2>&1
```

Launch with the Bash tool using **`run_in_background: true`** — that is the
*only* backgrounding mechanism. **Do NOT also append `&` to the command.**
`run_in_background: true` makes the harness track the `pi` process itself and
notify you when *it* exits; adding `&` forks `pi` loose from the tracked shell,
which returns instantly, so the harness fires a **false "completed"
notification within seconds** while `pi` is still running and no report exists.
Repeat per worker (they run concurrently). `pkill -f "pi -p"` first if
relaunching, to avoid orphan dupes.

### 3. Write good briefs (the worker is a scientist, not a shell)

Each brief should give the worker genuine agency and a durable output:
- **Role + autonomy**: "design conditions, run them, iterate, then report."
- **Target + instrument**: exact model/host and the path + usage of any
  instrument script you provide (workers can also write their own).
- **The question**, sharply framed, with any discriminating logic.
- **Deliverable contract (REQUIRED)**: end with *"use the write tool to save
  your full report to `report.md` in your session dir, AND print it as your
  final message."* Persisting to disk means results survive an interrupted run.
- **Incremental persistence**: for long sweeps, instruct the worker to run
  conditions one at a time and **append each result to `report.md` as it
  finishes** — not one giant parallel batch that writes nothing until the end.
  A slow or killed worker then still leaves usable partial data.

### 4. Monitor live

```bash
python3 <skilldir>/scripts/pi_watch.py "$RUN/agentA"          # newest .jsonl in dir
python3 <skilldir>/scripts/pi_watch.py "$RUN/agentA" --tail 15
```
It renders `USER ▷ … / 💬 narration / ▶ [bash] cmd / └ (ok|ERR) result` and a
footer with tool-call and error counts. Poll on demand; **do not** foreground-
wait (the Bash tool caps at ~2 min).

**Completion test** — the harness background-task notification is reliable
*only if you did not double-background* (see §2). Treat a worker as done only
when **its `report.md` exists** (and, to be safe, `pgrep -f "name <agent>"`
shows no live process). Never declare a result from the notification alone — a
premature "completed" with no `report.md` means the worker is still running.

### 5. Collect + verify

Read each `report.md`. As orchestrator, **independently spot-check the load-
bearing numbers** yourself (a couple of direct calls) before trusting the
synthesis — worker models can be confidently wrong. Then synthesize.

## Robustness rules (these all come from real failures)

- **Never depend on stdout.** Monitor the session log; enforce report-to-disk.
- **One `--session-dir` per worker** → unambiguous log; never guess filenames.
- **`</dev/null`** on every launch, or a headless worker may hang on stdin.
- **`pkill -f "pi -p"`** before relaunching a run to kill orphaned duplicates
  (they silently double-load the Ollama host).
- **Never append `&` when using `run_in_background: true`** — double-
  backgrounding fires a false "completed" notification while `pi` runs on.
- **Completion = `report.md` exists**, not the notification alone. Poll
  `pi_watch.py` only to inspect progress, not to decide done-ness.
- **Concurrency**: workers hitting one Ollama host contend; keep any sampling
  instrument threaded and keep the worker fleet modest (≈2–6).

## Knobs

- `HOST` — `OLLAMA_HOST`, e.g. `100.89.151.117:11434`.
- `MODEL` — any `ollama list` model, e.g. `glm-5.2:cloud`.
- `--thinking` — `off|low|medium|high` (low is a good default for workers).
- `--tools` — usually `bash,read,write`; drop to `--no-tools` for pure Q&A.
- `--model` inside the worker's instrument selects the model *under test*, which
  is independent of the worker's own `--model`.
