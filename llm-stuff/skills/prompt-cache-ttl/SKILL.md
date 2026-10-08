---
name: prompt-cache-ttl
description: >-
  Empirically measure, from ACTUAL BILLED token usage, whether the Anthropic prompt cache TTL in effect
  on the current billing route is 5 minutes or 1 hour — and whether that route honors an explicitly
  requested 1h TTL. Use this whenever the user asks how long cached prompts live, whether they're getting
  5-minute vs 1-hour caching, whether a cache hit will still be warm after N minutes, whether their direct
  API vs Claude Enterprise route (or a gateway/Bedrock/Vertex route) actually honors prompt-cache TTLs,
  how to confirm cache TTL against real charges, or wants to reproduce the "is my cache 5m or 1h"
  experiment. Trigger even if they don't say "TTL" — e.g. "does my cache survive 6 minutes",
  "why did my cached prompt get re-billed as a write", "confirm caching on the enterprise plan". This is
  about MEASURING the live TTL/route behavior, not about designing or optimizing a caching strategy.
---

# Prompt-cache TTL prober

Determine, from billed `usage`, whether the prompt cache on the current route uses a **5-minute** or
**1-hour** TTL. Two independent, charge-based signatures — they must agree:

1. **Write-price bucket:** each cache write reports `usage.cache_creation.ephemeral_5m_input_tokens` vs
   `ephemeral_1h_input_tokens`. That bucket *is* the billing multiplier (5m write = 1.25× input, 1h = 2×).
2. **Survival:** an entry is readable only within its TTL of the **last access**. After a gap of
   **7 minutes** (safely > 5 min, < 60 min), re-send the identical prefix — a cache **read** (~0.1×) means
   it survived → 1h; a **re-write** means it expired → 5m.

## Pick a mode

- **Mode A — self-introspection (default; native to Claude Code, no API key).** Reads this very session's
  transcript. Measures the TTL the current Claude Code route actually delivers for the config the client
  requests. Best for "is my Claude Code — on this direct/enterprise route — getting 1h caching?" It cannot
  test the 5-minute path (the client only requests one TTL).
- **Mode B — controlled raw-API battery (needs a credential).** Runs `scripts/cache_ttl_api.sh`, which
  tests *both* default-ephemeral (expect 5m) and explicit `ttl:"1h"` and reads their buckets + survival.
  Best for confirming both TTLs and their billing on a route, or when there's no live Claude Code session.

Ask which the user wants if unclear. When they say "run it here / in this session," use Mode A.

## Mode A — self-introspection procedure

Run these steps in order. The self-pause is the crux; follow the flush step exactly.

1. **Baseline.** Read recent cache usage and record the last request time:
   ```bash
   bash scripts/read_session_cache.sh 6
   BASELINE_TS=$(date -u +%Y-%m-%dT%H:%M:%SZ)   # note this; the gap is measured from ~now
   ```
   Confirm rows show `cache_creation` landing in the **1h** (or 5m) bucket — that's the TTL the client
   requests. Confirm caching is active (nonzero `cache_read` on recent turns).
2. **Self-pause ~7 min, then STOP.** Launch a background sleep that re-invokes you on completion, then
   **end your turn and do nothing** — any intervening tool call that hits the API resets the TTL clock:
   ```bash
   sleep 420   # run_in_background: true  → you are re-invoked when it exits
   ```
3. **On wake, flush then read.** The wake turn's own `usage` isn't in the transcript until the turn ends,
   so trigger one short background re-invoke to flush it, then read the post-gap rows:
   ```bash
   sleep 6     # run_in_background: true  → re-invokes; on that next turn, run:
   bash scripts/read_session_cache.sh 6 "$BASELINE_TS"
   ```
4. **Interpret** the first post-gap row: large `cache_read` with tiny `cache_creation` → **HIT** → the
   pre-gap entry survived ~7 min → **1-hour TTL**. Large `cache_creation` with `cache_read ≈ 0` →
   **MISS** → **5-minute TTL**. Corroborate with the write bucket from step 1.

> The empirically-confirmed reference result on a Claude Code session: writes land in `ephemeral_1h`, and a
> ~336k-token prefix read back as a HIT after a 6.6-min gap → **1-hour TTL**.

## Mode B — controlled raw-API battery

```bash
ROUTE_LABEL=direct MODEL=claude-sonnet-5 bash scripts/cache_ttl_api.sh
```
Credentials auto-detect (`ANTHROPIC_API_KEY` → `ANTHROPIC_OAUTH_TOKEN` → `ant auth print-credentials
--access-token`). It writes both prefixes, waits `GAP_SECONDS` (default 420), probes both, and prints a
verdict plus `cache-ttl-results-<route>/results.tsv` (per-call usage + `request-id`s). The script's
`sleep` is a single blocking call so the wait doesn't depend on the agent harness surviving idle.

## Interpretation truth table

| Test | Expected bucket | Expected probe @7 min | Verdict |
|---|---|---|---|
| default `ephemeral` | 5m | MISS | 5-minute TTL |
| explicit `ttl:"1h"` | 1h | HIT | 1-hour TTL |

Flag anomalies: default HIT (behaving > 5m), `ttl:"1h"` MISS (not honored), `cache_creation ≈ 0` on a
write (caching not engaging — prefix too small or `cache_control` stripped by a gateway).

## Cross-route (direct vs Claude Enterprise)

Run the chosen mode once per route and compare. Mode A: run in two Claude Code instances authenticated to
the two routes. Mode B: run with each route's credential (+ `ANTHROPIC_BASE_URL` for a gateway). See
`references/protocol.md` for Bedrock/Vertex and ZDR caveats.

## Reconcile against real charges

Note the run's UTC window and `request-id`s, then Console → **Usage/Cost** for that window: confirm the
itemized **cache write (5m)** / **cache write (1h)** / **cache read** lines match the token counts and the
1.25× / 2× / 0.1× multipliers. Details and a reporting template are in `references/protocol.md`.

## Guardrails

- Keep the probe prefix **byte-identical** to the write (no timestamps/UUIDs) — any drift silently forces a
  miss. Keep `thinking` constant across calls. Never send `temperature`/`top_p`/`top_k` on Sonnet 5 (400).
- During any survival wait, make **no** request touching the tested prefix — reads refresh the TTL.
- Full reasoning, edge cases, billing steps, and route caveats: **`references/protocol.md`**.
