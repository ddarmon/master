#!/usr/bin/env python3
"""Distill a pi session log into a compact activity feed for live monitoring.

pi (`pi -p ...`) appends its session transcript to
  ~/.pi/agent/sessions/<cwd-slug>/<timestamp>_<uuid>.jsonl
INCREMENTALLY, per message, regardless of whether stdout is a TTY. That makes
the session file — not stdout — the reliable thing to tail. (stdout is buffered
when piped, and a killed run loses it; the session log is already on disk.)

Pass either the .jsonl file or the --session-dir you launched pi with (the
newest .jsonl in it is used). Parses the session `message` schema:
  role=user       -> the prompt
  role=assistant  -> content items: {type:text} narration, {type:toolCall,name,arguments}
  role=toolResult -> toolName + content[].text + isError

Usage: pi_watch.py <session.jsonl | session-dir> [--tail N] [--full]
"""
import json, os, sys, glob

def short(s, n=400):
    s = " ".join(s.split())
    return s if len(s) <= n else s[:n] + " …"

def resolve(path):
    if os.path.isdir(path):
        files = sorted(glob.glob(os.path.join(path, "*.jsonl")), key=os.path.getmtime)
        if not files:
            return None
        return files[-1]
    return path

def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    full = "--full" in sys.argv
    tail = None
    if "--tail" in sys.argv:
        tail = int(sys.argv[sys.argv.index("--tail") + 1])
    if not args:
        print("usage: pi_watch.py <session.jsonl|session-dir> [--tail N] [--full]")
        sys.exit(2)
    path = resolve(args[0])
    if not path or not os.path.exists(path):
        print("(no session log yet)")
        return

    lines = open(path, encoding="utf-8").read().splitlines()
    feed, n_calls, n_err, last_role = [], 0, 0, None
    for ln in lines:
        ln = ln.strip()
        if not ln:
            continue
        try:
            e = json.loads(ln)
        except Exception:
            continue  # tolerate a partial trailing line during live writes
        if e.get("type") != "message":
            continue
        m = e["message"]
        role = m.get("role")
        last_role = role
        if role == "user":
            txt = " ".join(c.get("text", "") for c in m.get("content", []) if c.get("type") == "text")
            feed.append(f"USER ▷ {short(txt, 220)}")
        elif role == "assistant":
            for c in m.get("content", []):
                if c.get("type") == "text" and c.get("text", "").strip():
                    feed.append(f"💬 {short(c['text'], 700 if full else 400)}")
                elif c.get("type") == "toolCall":
                    n_calls += 1
                    cmd = c.get("arguments", {})
                    cmd = cmd.get("command") if isinstance(cmd, dict) else cmd
                    feed.append(f"▶ [{c.get('name')}] {short(json.dumps(cmd) if not isinstance(cmd, str) else cmd, 600 if full else 400)}")
        elif role == "toolResult":
            txt = "".join(c.get("text", "") for c in m.get("content", []))
            if e.get("message", {}).get("isError") or m.get("isError"):
                n_err += 1
                feed.append(f"  └ (ERR) {short(txt, 600 if full else 400)}")
            else:
                feed.append(f"  └ (ok) {short(txt, 600 if full else 400)}")

    if tail:
        feed = feed[-tail:]
    print("\n".join(feed))
    print(f"\n--- {n_calls} tool calls | {n_err} errors | last message: {last_role or '(none)'} ---")

if __name__ == "__main__":
    main()
