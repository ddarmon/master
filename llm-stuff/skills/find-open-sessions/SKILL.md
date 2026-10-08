---
name: find-open-sessions
description: Identify Claude Code, Codex, and Pi coding-agent sessions that are currently open in terminal tabs, map their live processes to session IDs, and exclude the session running the check. Use whenever the user asks which agent sessions, windows, tabs, or coding-agent processes are open, live, running, or active now; wants live session IDs; or needs to choose another live session for sesh inspection or export. Do not infer liveness from recent transcript files.
---

# Find open sessions

Treat an open session as a terminal-attached live process, never as a recently
modified transcript. Use the bundled read-only script to correlate confirmed
Claude Code, Codex, and Pi processes with saved session metadata.

## Run the report

Resolve this skill's directory, then run:

```bash
python3 <skill-directory>/scripts/find_open_sessions.py
```

The default scope is the invoking agent's realpath-normalized working directory.
Use an explicit directory, `--all`, a provider filter, or JSON as needed:

```bash
python3 <skill-directory>/scripts/find_open_sessions.py /path/to/project --json
python3 <skill-directory>/scripts/find_open_sessions.py --all --json
python3 <skill-directory>/scripts/find_open_sessions.py --provider codex
```

Supported provider values are `all`, `claude`, `codex`, and `pi`. Do not combine
an explicit directory with `--all`.

## Report safely

- Keep the row marked `THIS SESSION — EXCLUDED` out of every suggested action.
- Treat `exact` as an argv-derived ID, `high` as a close unique birth-time
  assignment, `low` as distant or ambiguous, and `none` as unmapped. Surface
  `low`, `none`, warnings, and ambiguity without guessing.
- Use the result only to report candidates. Listing does not authorize reading,
  exporting, resuming, archiving, deleting, or messaging another session.
- Before a later `sesh export <ID>` or similar action, show candidate IDs and get
  explicit confirmation. Never act on the current session unless the user
  explicitly overrides its exclusion.
- Never replace process evidence with transcript recency or expose a complete
  process command line.

The script does not modify transcripts, terminals, sessions, or configuration.
It uses only Python 3's standard library plus macOS `ps` and `lsof`; other
platforms are unsupported. If Codex's sandbox denies process inspection, rerun
this read-only command with the required approval. A denied `ps` must be treated
as an inspection error, not as an empty session list.
