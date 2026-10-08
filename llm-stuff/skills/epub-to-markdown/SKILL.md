---
name: epub-to-markdown
description: Convert an EPUB file into per-chapter Markdown files with extracted images. Use when asked to convert, split, or extract an EPUB book.
---

## Overview

Convert an EPUB file into clean, per-chapter Markdown files using pandoc and a Python chapter splitter. Extracts images and creates a symlinked `images/` directory so image references work from each chapter file.

## Workflow

Follow these steps in order:

### 1. Validate inputs

- Confirm the EPUB path supplied by the user exists. If not, ask for the correct path.
- Use the user-supplied output directory, or the current working directory when none was supplied.
- **Confirm the output directory with the user before proceeding.**

### 2. Run pandoc

First, check if the EPUB is actually a directory (Apple Books unpacks EPUBs as packages). If so, re-zip it into a proper EPUB file first:

```bash
if [ -d "$EPUB" ]; then
    TMPEPUB="/tmp/epub2md-$$.epub"
    rm -f "$TMPEPUB"
    (cd "$EPUB" && zip -X0 "$TMPEPUB" mimetype && zip -Xr9D "$TMPEPUB" -- * -x mimetype)
    EPUB="$TMPEPUB"
fi
```

Then convert the EPUB to a single Markdown file with extracted media:

```bash
pandoc "$EPUB" -t markdown --extract-media="$OUTDIR/media" -o "$OUTDIR/full.md"
```

Clean up the temp file if one was created:
```bash
[ -n "${TMPEPUB:-}" ] && rm -f "$TMPEPUB"
```

### 3. Inspect headings

Count headings at each level to decide where to split:

```bash
grep -c '^# ' "$OUTDIR/full.md"
grep -c '^## ' "$OUTDIR/full.md"
grep -c '^### ' "$OUTDIR/full.md"
```

Choose the heading level:

1. Start with H1. If H1 count is zero, try H2. If H2 count is zero, try H3.
2. **Check the count.** Most books have 5-30 chapters. If the count exceeds 40, note that paired-heading books (e.g., "Chapter 1" + "Title" as separate headings) can have ~2x the expected count — inspect the headings before concluding the level is wrong.
3. List the headings at the chosen level: `grep -n '^## ' "$OUTDIR/full.md"` (adjust pattern for chosen level).
4. **Show the heading list and count to the user and confirm before splitting.** If the structure looks unusual (too many headings, all sections rather than chapters, no clear chapter pattern), discuss with the user before proceeding.

### 4. Run the chapter splitter

```bash
python3 <skill-directory>/scripts/split_chapters.py "$OUTDIR/full.md" "$OUTDIR/chapters" "$OUTDIR/media"
```

Add `--heading-level 2` if H1 headings were absent and H2s are being used instead.

The splitter will:
- Split on headings at the specified level
- Merge paired "Chapter N" + subtitle headings into a single chapter
- Clean all Pandoc attributes (`{.class}`, `[]{#id}`, etc.)
- Normalize image paths to use `images/`
- Create an `images` symlink in the chapters directory pointing to the extracted media

### 5. Verify output

- List the chapter files in `$OUTDIR/chapters/`.
- Read the first 20 lines of 2-3 chapter files to spot-check content.
- Confirm the `images` symlink resolves: `ls -la "$OUTDIR/chapters/images"`.
- Confirm no `{...}` Pandoc attributes remain: `grep -r '{\\.' "$OUTDIR/chapters/"` should return nothing.

### 6. Format chapter files

Optionally run a Markdown formatter (e.g. `mdformat`) on each chapter file:

```bash
for f in "$OUTDIR/chapters"/*.md; do
    mdformat "$f"
done
```

### 7. Clean up

Remove the intermediate full Markdown file:

```bash
rm "$OUTDIR/full.md"
```

### 8. Report

Summarize what was done:
- Number of chapter files created
- Output location (`$OUTDIR/chapters/`)
- Any issues encountered (e.g., missing images, unusual heading structure)

## Important notes

- Always confirm the output directory before writing files.
- If pandoc is not installed, tell the user to install it (`brew install pandoc`).
- If the heading inspection shows an unexpected structure (e.g., no headings at any level, or a very high heading count), stop and discuss with the user. Report heading counts at H1, H2, and H3 levels so they can make an informed decision.
