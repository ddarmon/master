---
name: anki-clean
description: Audit and clean up existing Anki notes — strip &nbsp;/whitespace artifacts and normalize non-conforming Subject taxonomy lines. Use whenever the user wants to clean, tidy, fix, audit, normalize, or standardize cards already in their Anki decks (as opposed to creating new ones), or mentions messy subjects, stray &nbsp;, or inconsistent formatting across a deck.
---

# Anki Clean

Audit and fix notes that are **already in** the user's Anki decks. This is
the counterpart to `anki-create` (which makes new cards) and `anki-search`
(which reads them). It edits existing notes, so it is the one tool here
that changes data the user already has — treat it carefully.

Requires Anki running with the AnkiConnect plugin (Tools > Add-ons > code
2055492159). Stdlib only; no pip installs.

## The cardinal rule: nothing is written without explicit confirmation

Editing notes is reversible (Anki Undo / automatic backups), but a bad
bulk run is annoying to unwind. So:

- **Always scan/dry-run first** and show the user the diff.
- **Get explicit approval** before any `--apply`.
- **For a large run, tell the user to back up first** (Anki: File > Export
  the collection, or rely on its automatic backups).

## Two kinds of cleanup — keep them separate

The two problems have very different risk profiles. Do not bundle them
into one blind pass.

| Kind | What it fixes | Automatable? |
|------|---------------|--------------|
| **Mechanical** | `&nbsp;`/`&#160;` and leading/trailing whitespace by default; internal double-spaces only with `--collapse-spaces` | Yes — deterministic |
| **Subjects** | `Subject` lines that don't fit the `general -- specific` taxonomy | No — needs judgment + approval |

Mechanical fixes are safe to apply broadly after a dry-run. **Whitespace
collapsing is opt-in and deliberately conservative:** the default pass only
normalizes non-breaking spaces and trims field ends, because collapsing
runs of spaces inside `<pre>` code/LaTeX blocks would corrupt them.
`--collapse-spaces` enables internal collapsing but still skips `<pre>`
regions. Subject fixes require deciding the *correct* taxonomy path for
each note, which is a per-card judgment — never auto-rewrite a subject.

**Empty subjects are a different problem.** A note with no `Subject` at all
needs classification from scratch, not normalization — that is a separate,
deliberate tagging effort. Don't fold a large batch of empty subjects into
a routine cleanup; surface the count and let the user decide whether to
take it on as its own pass.

**Exclude `Course Decks` (and its subdecks) from subject normalization.**
That deck follows a separate course/exam/lecture taxonomy (e.g.
`MA 115 -- exam 1 -- lecture 1`), so its flat and non-lowercase subjects are
intentional — do not flag or rewrite them. Mechanical (whitespace/`&nbsp;`)
cleanup is still safe to run there; only the *subject* pass needs the
carve-out. The scanner audits the whole collection, so filter Course Decks
notes out of any proposed subject mapping yourself.

## Workflow

### Step 1: Scan (read-only)

Always start here. Pick the deck (ask if unspecified; default is the whole
collection).

```bash
python3 <skill-directory>/anki_clean.py scan --deck "DECK NAME"
```

This reports two lists: mechanical field issues (with before/after) and
non-conforming subjects (with the reason each was flagged). Summarize it
for the user before doing anything.

### Step 2: Mechanical cleanup

Preview the exact changes, then apply only after the user agrees:

```bash
python3 <skill-directory>/anki_clean.py clean-mechanical --deck "DECK NAME"          # dry run
python3 <skill-directory>/anki_clean.py clean-mechanical --deck "DECK NAME" --apply  # write
```

The script only touches fields that actually change, and groups changes by
note. By default it normalizes `&nbsp;` and trims field ends only. Add
`--collapse-spaces` to also collapse internal double-spaces (outside
`<pre>`); use it sparingly and always dry-run it first. Real markup
(`<b>`, `<code>`, `<br>`, cloze `{{...}}`) and `<pre>` contents are always
left intact.

### Step 3: Subject normalization (judgment + approval)

This is the part that needs you, the model, in the loop:

1. **Read the taxonomy** so proposals are consistent: load
   `<skills-root>/anki-create/references/taxonomy.md` (the cache of known
   `Subject` paths and conventions). If it's thin, also pull live examples
   for context:

   ```bash
   python3 <skills-root>/anki-search/anki_search.py search \
     'deck:"DECK NAME"' --fields -n 50
   ```

2. **Propose a mapping.** For each flagged note, decide the correct
   hierarchical path (`general -- specific`, lowercase top-level domain,
   ` -- ` delimiter), reusing an existing branch wherever one fits rather
   than inventing a parallel one. Write a mapping file:

   ```json
   [
     {"noteId": 1781361791022, "subject": "computer -- llms -- tools"},
     {"noteId": 1781361791034, "subject": "computer -- llms -- tools"}
   ]
   ```

3. **Show the proposed mapping to the user as a table and get approval.**
   These are content decisions — the user should sign off on the paths.

4. **Dry-run, then apply.** The script re-validates every proposed subject
   against the taxonomy rules and skips any that don't conform, so a
   malformed proposal can't slip through:

   ```bash
   python3 <skill-directory>/anki_clean.py apply-subjects --map /tmp/subj_map.json          # dry run
   python3 <skill-directory>/anki_clean.py apply-subjects --map /tmp/subj_map.json --apply  # write
   ```

5. **Update the cache.** Append any new branches you introduced to
   `<skills-root>/anki-create/references/taxonomy.md` so future card
   creation and cleaning stay consistent.

### Step 4: Report

Tell the user how many notes were changed in each pass, and note anything
that was skipped or rejected.

## What counts as a non-conforming subject

The scanner flags a `Subject` when it is: empty, contains `&nbsp;`, has
leading/trailing whitespace, is flat (no ` -- ` hierarchy), or has a
top-level domain that isn't lowercase. The conventions come from
`anki-create`'s **Subject Taxonomy** rules — keep the two skills in sync.

Two caveats when acting on these flags: (1) a **flat** subject is often fine
— it just means the note is unclassified, not wrong — so only add hierarchy
when a note clearly belongs under an existing branch; never bulk-rewrite flat
subjects. (2) Subjects in **`Course Decks`** are exempt (see above) — ignore
any flags raised for them.

## Script reference

| Command | Effect |
|---------|--------|
| `scan [--deck D] [--json]` | Read-only report of mechanical + subject issues |
| `clean-mechanical [--deck D] [--apply]` | Dry-run (default) or apply whitespace/`&nbsp;` fixes |
| `apply-subjects --map FILE [--apply]` | Dry-run (default) or apply a validated `{noteId, subject}` mapping |

All commands accept `--url` (default `http://localhost:8765`). Without
`--apply`, the write commands only preview.

## Error Handling

If the script reports a connection error, have the user:

1. Open Anki
2. Install AnkiConnect (Tools > Add-ons > Get Add-ons > code 2055492159)
3. Restart Anki

