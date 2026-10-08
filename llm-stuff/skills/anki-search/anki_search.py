#!/usr/bin/env python3
"""Search Anki flashcards via AnkiConnect. Stdlib only — no pip install needed."""

import argparse
import html
import json
import re
import sys
import urllib.error
import urllib.request
from html.parser import HTMLParser

DEFAULT_URL = "http://localhost:8765"


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
    with urllib.request.urlopen(req, timeout=10) as resp:
        body = json.loads(resp.read().decode("utf-8"))
    if body.get("error"):
        raise RuntimeError(f"AnkiConnect error: {body['error']}")
    return body["result"]


# ---------------------------------------------------------------------------
# HTML stripping (stdlib only)
# ---------------------------------------------------------------------------

class _HTMLStripper(HTMLParser):
    """Minimal HTML-to-text converter."""

    def __init__(self):
        super().__init__()
        self._parts: list[str] = []

    def handle_data(self, data):
        self._parts.append(data)

    def handle_starttag(self, tag, attrs):
        if tag in ("br", "p", "div", "li", "tr", "hr"):
            self._parts.append("\n")

    def handle_endtag(self, tag):
        if tag in ("p", "div", "ul", "ol", "table"):
            self._parts.append("\n")


def strip_html(html_str):
    """Strip HTML tags and unescape entities."""
    if not html_str:
        return ""
    stripper = _HTMLStripper()
    stripper.feed(html_str)
    text = "".join(stripper._parts)
    text = html.unescape(text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


# ---------------------------------------------------------------------------
# Note formatting
# ---------------------------------------------------------------------------

def _get_field_value(fields, name, strip):
    """Extract a field value, optionally stripping HTML."""
    raw = fields.get(name, {}).get("value", "")
    return strip_html(raw) if strip else raw


def format_note(note, index, total, strip=True, all_fields=False):
    """Format a single note for human-readable output."""
    fields = note.get("fields", {})
    model = note.get("modelName", "Unknown")
    tags = note.get("tags", [])
    note_id = note.get("noteId", "?")

    lines = [
        f"--- Note {index} of {total} ---",
        f"Note ID: {note_id}",
        f"Model:   {model}",
        f"Tags:    {', '.join(tags) if tags else '(none)'}",
    ]

    if all_fields:
        for name in fields:
            val = _get_field_value(fields, name, strip)
            lines.append(f"{name}: {val}")
    elif model == "Cloze":
        lines.append(f"Text:  {_get_field_value(fields, 'Text', strip)}")
        extra = _get_field_value(fields, "Extra", strip)
        if extra:
            lines.append(f"Extra: {extra}")
    elif "Front" in fields:
        lines.append(f"Front: {_get_field_value(fields, 'Front', strip)}")
        lines.append(f"Back:  {_get_field_value(fields, 'Back', strip)}")
    else:
        for name in fields:
            val = _get_field_value(fields, name, strip)
            lines.append(f"{name}: {val}")

    return "\n".join(lines)


def note_to_dict(note, strip=True):
    """Convert a note to a plain dict for JSON output."""
    fields = note.get("fields", {})
    out_fields = {}
    for name in fields:
        raw = fields[name].get("value", "")
        out_fields[name] = strip_html(raw) if strip else raw
    return {
        "noteId": note.get("noteId"),
        "modelName": note.get("modelName"),
        "tags": note.get("tags", []),
        "fields": out_fields,
    }


# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------

def cmd_decks(args):
    """List all deck names."""
    decks = _invoke(args.url, "deckNames")
    decks.sort()

    if args.json:
        if args.counts:
            result = []
            for d in decks:
                ids = _invoke(args.url, "findNotes", query=f'deck:"{d}"')
                result.append({"deck": d, "notes": len(ids)})
            json.dump(result, sys.stdout, indent=2)
        else:
            json.dump(decks, sys.stdout, indent=2)
        print()
        return

    print(f"Decks ({len(decks)}):")
    if args.counts:
        for d in decks:
            ids = _invoke(args.url, "findNotes", query=f'deck:"{d}"')
            print(f"  {d} ({len(ids)} notes)")
    else:
        for d in decks:
            print(f"  {d}")


def cmd_count(args):
    """Count notes matching a query."""
    ids = _invoke(args.url, "findNotes", query=args.query)
    if args.json:
        json.dump({"query": args.query, "count": len(ids)}, sys.stdout, indent=2)
        print()
    else:
        print(f"{len(ids)} notes match: {args.query}")


def cmd_search(args):
    """Search for cards and display results."""
    note_ids = _invoke(args.url, "findNotes", query=args.query)
    total = len(note_ids)

    page = note_ids[args.offset:args.offset + args.limit]
    if not page:
        if args.json:
            json.dump({
                "query": args.query,
                "total_matches": total,
                "returned": 0,
                "offset": args.offset,
                "notes": [],
            }, sys.stdout, indent=2)
            print()
        else:
            if total == 0:
                print(f"0 notes match: {args.query}")
            else:
                print(f"{total} notes match, but offset {args.offset} is past the end.")
        return

    notes = _invoke(args.url, "notesInfo", notes=page)
    strip = not args.no_strip

    if args.json:
        json.dump({
            "query": args.query,
            "total_matches": total,
            "returned": len(notes),
            "offset": args.offset,
            "notes": [note_to_dict(n, strip=strip) for n in notes],
        }, sys.stdout, indent=2)
        print()
        return

    print(f"Found {total} notes matching: {args.query}")
    if total > len(notes):
        showing_end = args.offset + len(notes)
        print(f"Showing {args.offset + 1}-{showing_end} of {total}")
    print()

    for i, note in enumerate(notes, start=args.offset + 1):
        print(format_note(note, i, total, strip=strip, all_fields=args.fields))
        print()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Search Anki flashcards via AnkiConnect."
    )
    sub = parser.add_subparsers(dest="command")

    # decks
    p_decks = sub.add_parser("decks", help="List deck names")
    p_decks.add_argument("--counts", action="store_true",
                         help="Include note count per deck")
    p_decks.add_argument("--json", action="store_true",
                         help="Output as JSON")
    p_decks.add_argument("--url", default=DEFAULT_URL,
                         help="AnkiConnect URL")

    # count
    p_count = sub.add_parser("count", help="Count matching notes")
    p_count.add_argument("query", help="Anki search query")
    p_count.add_argument("--json", action="store_true",
                         help="Output as JSON")
    p_count.add_argument("--url", default=DEFAULT_URL,
                         help="AnkiConnect URL")

    # search
    p_search = sub.add_parser("search", help="Search for cards")
    p_search.add_argument("query", help="Anki search query")
    p_search.add_argument("-n", "--limit", type=int, default=20,
                          help="Max notes to return (default: 20)")
    p_search.add_argument("--offset", type=int, default=0,
                          help="Skip first N results")
    p_search.add_argument("--fields", action="store_true",
                          help="Show all field names/values")
    p_search.add_argument("--json", action="store_true",
                          help="Output as JSON")
    p_search.add_argument("--no-strip", action="store_true",
                          help="Preserve raw HTML in fields")
    p_search.add_argument("--url", default=DEFAULT_URL,
                          help="AnkiConnect URL")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    try:
        {"decks": cmd_decks, "count": cmd_count, "search": cmd_search}[
            args.command
        ](args)
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
