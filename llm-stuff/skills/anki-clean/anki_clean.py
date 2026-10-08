#!/usr/bin/env python3
"""Clean existing Anki notes via AnkiConnect. Stdlib only — no pip install.

Two kinds of cleanup:

  * MECHANICAL (deterministic): by default strip `&nbsp;` / `&#160;` and
    trim leading/trailing whitespace per field. Internal double-space
    collapsing is opt-in (`--collapse-spaces`) and never touches <pre>
    blocks, where code/LaTeX whitespace is significant. `scan` reports
    them; `clean-mechanical` previews a diff and (with --apply) writes.

  * SUBJECT (judgment required, NOT automated here): detect notes whose
    `Subject` field doesn't conform to the hierarchical taxonomy
    (`general -- specific`). `scan` flags them; the human/LLM proposes a
    mapping file of {noteId, subject}; `apply-subjects` previews and (with
    --apply) writes it.

Nothing is written unless you pass --apply. Editing notes is reversible via
Anki's Undo / automatic backups, but back up before a large run.
"""

import argparse
import json
import re
import sys
import urllib.error
import urllib.request

DEFAULT_URL = "http://localhost:8765"

# Fields treated as free text for mechanical cleaning.
TEXT_FIELDS = ("Front", "Back", "Text", "Extra", "Subject")
# Anki cloze/basic field that carries the taxonomy path.
SUBJECT_FIELD = "Subject"


# ---------------------------------------------------------------------------
# AnkiConnect communication
# ---------------------------------------------------------------------------

def _invoke(url, action, **params):
    """Send a request to AnkiConnect and return the result."""
    payload = json.dumps({
        "action": action,
        "version": 6,
        "params": params,
    }).encode("utf-8")
    req = urllib.request.Request(
        url, data=payload, headers={"Content-Type": "application/json"}
    )
    with urllib.request.urlopen(req, timeout=15) as resp:
        body = json.loads(resp.read().decode("utf-8"))
    if body.get("error"):
        raise RuntimeError(f"AnkiConnect error: {body['error']}")
    return body["result"]


def _query(deck):
    """Build a findNotes query for a deck (or the whole collection)."""
    return f'deck:"{deck}"' if deck else "deck:*"


def _fetch_notes(url, deck):
    """Return notesInfo records for every note in the deck/collection."""
    ids = _invoke(url, "findNotes", query=_query(deck))
    if not ids:
        return []
    notes = []
    # Page to keep payloads sane on large collections.
    for i in range(0, len(ids), 200):
        notes.extend(_invoke(url, "notesInfo", notes=ids[i:i + 200]))
    return notes


# ---------------------------------------------------------------------------
# Mechanical cleaning (deterministic)
# ---------------------------------------------------------------------------

_NBSP_RE = re.compile(r"&nbsp;|&#160;|\xa0", re.IGNORECASE)
# Whitespace inside <pre> blocks is significant (code, LaTeX) — never touch it.
_PRE_RE = re.compile(r"(<pre\b[^>]*>.*?</pre>)", re.IGNORECASE | re.DOTALL)


def _collapse_outside_pre(value):
    """Collapse runs of spaces/tabs, but leave <pre>...</pre> regions intact."""
    parts = _PRE_RE.split(value)
    for i in range(0, len(parts), 2):  # even indices are non-<pre> segments
        parts[i] = re.sub(r"[ \t]{2,}", " ", parts[i])
    return "".join(parts)


def clean_text(value, collapse=False):
    """Deterministically clean one field's HTML string.

    By default this is conservative — it only:
    - turns non-breaking spaces (entity or literal) into normal spaces
    - trims leading/trailing whitespace on the whole field

    Collapsing internal runs of spaces is OFF by default because whitespace
    is significant inside <pre> code/LaTeX blocks. With collapse=True, runs
    are collapsed *outside* <pre> regions only. Real markup (<b>, <code>,
    <br>, cloze {{...}}) is always left untouched.
    """
    cleaned = _NBSP_RE.sub(" ", value)
    if collapse:
        cleaned = _collapse_outside_pre(cleaned)
    cleaned = cleaned.strip()
    return cleaned


def mechanical_diffs(notes, collapse=False):
    """Return [{noteId, model, field, before, after}] for fields that change."""
    diffs = []
    for n in notes:
        for fname, fobj in n.get("fields", {}).items():
            if fname not in TEXT_FIELDS:
                continue
            before = fobj.get("value", "")
            after = clean_text(before, collapse=collapse)
            if after != before:
                diffs.append({
                    "noteId": n["noteId"],
                    "model": n.get("modelName", ""),
                    "field": fname,
                    "before": before,
                    "after": after,
                })
    return diffs


# ---------------------------------------------------------------------------
# Subject conformance (detection only)
# ---------------------------------------------------------------------------

def subject_issue(value):
    """Return a short reason string if a Subject is non-conforming, else None."""
    if value is None:
        return None
    raw = value
    stripped = raw.strip()
    if stripped == "":
        return "empty subject"
    if _NBSP_RE.search(raw):
        return "contains &nbsp;"
    if raw != stripped:
        return "leading/trailing whitespace"
    if " -- " not in stripped:
        return "flat (no ' -- ' hierarchy)"
    # Top-level domain should be lowercase.
    top = stripped.split(" -- ", 1)[0]
    if top != top.lower():
        return "top-level domain not lowercase"
    return None


def subject_issues(notes):
    """Return [{noteId, model, subject, issue}] for non-conforming subjects."""
    out = []
    for n in notes:
        fobj = n.get("fields", {}).get(SUBJECT_FIELD)
        if fobj is None:
            continue
        value = fobj.get("value", "")
        reason = subject_issue(value)
        if reason:
            out.append({
                "noteId": n["noteId"],
                "model": n.get("modelName", ""),
                "subject": value,
                "issue": reason,
            })
    return out


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------

def _shorten(s, n=70):
    s = s.replace("\n", " ")
    return s if len(s) <= n else s[:n - 3] + "..."


def _print_mechanical(diffs):
    if not diffs:
        print("Mechanical: no issues found.")
        return
    print(f"Mechanical: {len(diffs)} field(s) to clean")
    for d in diffs:
        print(f"  note {d['noteId']} [{d['field']}]")
        print(f"    - {_shorten(d['before'])}")
        print(f"    + {_shorten(d['after'])}")


def _print_subjects(issues):
    if not issues:
        print("Subjects: all conform.")
        return
    print(f"Subjects: {len(issues)} non-conforming")
    for s in issues:
        print(f"  note {s['noteId']}: {s['issue']}")
        print(f"    Subject: {_shorten(s['subject'])}")


# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------

def cmd_scan(args):
    """Report mechanical issues and non-conforming subjects. Read-only."""
    notes = _fetch_notes(args.url, args.deck)
    diffs = mechanical_diffs(notes, collapse=args.collapse_spaces)
    issues = subject_issues(notes)
    if args.json:
        json.dump({
            "deck": args.deck or "(all)",
            "scanned": len(notes),
            "mechanical": diffs,
            "subjects": issues,
        }, sys.stdout, indent=2, ensure_ascii=False)
        print()
    else:
        print(f"Scanned {len(notes)} note(s) in "
              f"{args.deck or 'the whole collection'}.\n")
        _print_mechanical(diffs)
        print()
        _print_subjects(issues)


def cmd_clean_mechanical(args):
    """Preview (default) or --apply deterministic mechanical fixes."""
    notes = _fetch_notes(args.url, args.deck)
    diffs = mechanical_diffs(notes, collapse=args.collapse_spaces)

    if not diffs:
        print("Nothing to clean.")
        return

    if not args.apply:
        _print_mechanical(diffs)
        print(f"\nDRY RUN — {len(diffs)} field(s) would change. "
              f"Re-run with --apply to write.")
        return

    # Group changed fields by note so each note is one updateNoteFields call.
    by_note = {}
    for d in diffs:
        by_note.setdefault(d["noteId"], {})[d["field"]] = d["after"]

    succeeded = failed = 0
    for note_id, fields in by_note.items():
        try:
            _invoke(args.url, "updateNoteFields",
                    note={"id": note_id, "fields": fields})
            succeeded += 1
        except RuntimeError as e:
            failed += 1
            print(f"  FAILED note {note_id}: {e}", file=sys.stderr)
    print(f"Applied mechanical cleanup to {succeeded} note(s), "
          f"{failed} failed.")


def cmd_apply_subjects(args):
    """Apply a proposed Subject mapping. Preview by default; --apply writes.

    Mapping file: a JSON list of {"noteId": <int>, "subject": "<new path>"}.
    The new subjects are validated against the taxonomy rules; non-conforming
    proposals are reported and skipped so a bad mapping can't slip through.
    """
    with open(args.map, "r", encoding="utf-8") as f:
        mapping = json.load(f)
    if not isinstance(mapping, list):
        print("Error: mapping file must be a JSON list.", file=sys.stderr)
        sys.exit(1)

    valid, rejected = [], []
    for m in mapping:
        nid, subj = m.get("noteId"), m.get("subject")
        if not nid or not isinstance(subj, str):
            rejected.append((m, "missing noteId or subject"))
            continue
        bad = subject_issue(subj)
        if bad:
            rejected.append((m, f"proposed subject is non-conforming: {bad}"))
            continue
        valid.append((nid, subj))

    for m, why in rejected:
        print(f"  REJECTED note {m.get('noteId')}: {why}", file=sys.stderr)

    if not valid:
        print("No valid subject updates to apply.")
        sys.exit(1 if rejected else 0)

    if not args.apply:
        print(f"{len(valid)} subject update(s) would be applied "
              f"({len(rejected)} rejected):")
        for nid, subj in valid:
            print(f"  note {nid} -> {subj}")
        print("\nDRY RUN — re-run with --apply to write.")
        return

    succeeded = failed = 0
    for nid, subj in valid:
        try:
            _invoke(args.url, "updateNoteFields",
                    note={"id": nid, "fields": {SUBJECT_FIELD: subj}})
            succeeded += 1
        except RuntimeError as e:
            failed += 1
            print(f"  FAILED note {nid}: {e}", file=sys.stderr)
    print(f"Applied {succeeded} subject update(s), {failed} failed, "
          f"{len(rejected)} rejected.")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Clean existing Anki notes via AnkiConnect.")
    sub = parser.add_subparsers(dest="command")

    p_scan = sub.add_parser("scan", help="Report issues (read-only)")
    p_scan.add_argument("--deck", help="Deck name (default: whole collection)")
    p_scan.add_argument("--collapse-spaces", action="store_true",
                        dest="collapse_spaces",
                        help="Also flag internal double-spaces (outside <pre>)")
    p_scan.add_argument("--json", action="store_true", help="Output as JSON")
    p_scan.add_argument("--url", default=DEFAULT_URL, help="AnkiConnect URL")

    p_mech = sub.add_parser(
        "clean-mechanical",
        help="Preview/--apply deterministic &nbsp;/whitespace fixes")
    p_mech.add_argument("--deck", help="Deck name (default: whole collection)")
    p_mech.add_argument("--collapse-spaces", action="store_true",
                        dest="collapse_spaces",
                        help="Also collapse internal double-spaces "
                             "(outside <pre>); off by default as it is riskier")
    p_mech.add_argument("--apply", action="store_true",
                        help="Write changes (otherwise dry run)")
    p_mech.add_argument("--url", default=DEFAULT_URL, help="AnkiConnect URL")

    p_subj = sub.add_parser(
        "apply-subjects",
        help="Apply a proposed {noteId, subject} mapping file")
    p_subj.add_argument("--map", required=True,
                        help="Path to JSON mapping file")
    p_subj.add_argument("--apply", action="store_true",
                        help="Write changes (otherwise dry run)")
    p_subj.add_argument("--url", default=DEFAULT_URL, help="AnkiConnect URL")

    args = parser.parse_args()
    if not args.command:
        parser.print_help()
        sys.exit(1)

    try:
        {
            "scan": cmd_scan,
            "clean-mechanical": cmd_clean_mechanical,
            "apply-subjects": cmd_apply_subjects,
        }[args.command](args)
    except urllib.error.URLError:
        print(
            "Error: Cannot connect to Anki.\n"
            "Make sure Anki is running and AnkiConnect is installed "
            "(Tools > Add-ons > Get Add-ons > code 2055492159).",
            file=sys.stderr,
        )
        sys.exit(1)
    except RuntimeError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()

