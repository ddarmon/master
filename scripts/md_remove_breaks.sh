#!/bin/bash

# Remove line breaks within paragraphs and list items in Markdown,
# putting each paragraph (and each list item, including its indented
# continuation lines) onto a single line.
#
# - Preserves fenced code blocks (``` or ~~~) verbatim.
# - Preserves blank lines between blocks.
# - Keeps each list item, table row, ATX heading, and thematic break on
#   its own line (so itemization, enumeration, and pipe tables survive).
# - Works with files or clipboard (cross-platform).
#
# Usage: md_remove_breaks [file]   - rewrite file in-place
#        md_remove_breaks          - unwrap clipboard content
#
# David Darmon, 30 April 2026

set -euo pipefail

# Detect clipboard tools
if command -v pbcopy &> /dev/null && command -v pbpaste &> /dev/null; then
    CLIPBOARD_COPY="pbcopy"
    CLIPBOARD_PASTE="pbpaste"
elif command -v xclip &> /dev/null; then
    CLIPBOARD_COPY="xclip -selection clipboard"
    CLIPBOARD_PASTE="xclip -selection clipboard -o"
elif command -v wl-copy &> /dev/null && command -v wl-paste &> /dev/null; then
    CLIPBOARD_COPY="wl-copy"
    CLIPBOARD_PASTE="wl-paste"
else
    echo "Error: No supported clipboard tool found." >&2
    exit 1
fi

if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required." >&2
    exit 1
fi

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT
INPUT_FILE="$TEMP_DIR/input.md"
OUTPUT_FILE="$TEMP_DIR/output.md"

if [ $# -ge 1 ] && [ -n "$1" ]; then
    if [ ! -f "$1" ]; then
        echo "Error: File not found at '$1'" >&2
        exit 1
    fi
    cp "$1" "$INPUT_FILE"
    TARGET_FILE="$1"
else
    echo "No file provided, unwrapping clipboard content..."
    eval "$CLIPBOARD_PASTE" > "$INPUT_FILE"
    if [ ! -s "$INPUT_FILE" ]; then
        echo "Error: Clipboard is empty or contains no text." >&2
        exit 1
    fi
    TARGET_FILE=""
fi

INPUT_FILE="$INPUT_FILE" OUTPUT_FILE="$OUTPUT_FILE" python3 - <<'PYTHON_SCRIPT'
import os
import re

with open(os.environ['INPUT_FILE'], 'r') as f:
    text = f.read()

trailing_newline = text.endswith('\n')
lines = text.split('\n')
if trailing_newline and lines and lines[-1] == '':
    lines = lines[:-1]

fence_re = re.compile(r'^(\s*)(`{3,}|~{3,})')
# List item start: bullet (-, *, +) or ordered (1. / 1)) marker.
list_item_re = re.compile(r'^(\s*)(?:[-*+]|\d{1,9}[.)])\s+\S')
# Table row: starts (after optional indent) with a pipe.
table_row_re = re.compile(r'^\s{0,3}\|')
# ATX heading: 1-6 leading '#' followed by space or end of line.
heading_re = re.compile(r'^\s{0,3}#{1,6}(?:\s|$)')
# Thematic break: 3+ of -, *, or _ (optionally space-separated) on a line.
hr_re = re.compile(r'^\s{0,3}([-*_])(?:\s*\1){2,}\s*$')

out = []
buffer = []
buffer_is_list = False
buffer_indent = 0  # column at which a list-item continuation must begin
in_fence = False
fence_marker = None

def flush_buffer():
    global buffer, buffer_is_list, buffer_indent
    if not buffer:
        return
    joined = buffer[0].rstrip()
    for cont in buffer[1:]:
        joined += ' ' + cont.strip()
    out.append(joined)
    buffer = []
    buffer_is_list = False
    buffer_indent = 0

for line in lines:
    m = fence_re.match(line)
    if m and not in_fence:
        flush_buffer()
        out.append(line)
        in_fence = True
        fence_marker = m.group(2)[0]
        continue
    if in_fence:
        out.append(line)
        # Closing fence: same character, length >= opening, optional trailing whitespace only
        close_m = re.match(r'^\s*(' + re.escape(fence_marker) + r'{3,})\s*$', line)
        if close_m:
            in_fence = False
            fence_marker = None
        continue

    if line.strip() == '':
        flush_buffer()
        out.append('')
        continue

    # Block-level constructs that must stay on their own line.
    if table_row_re.match(line) or heading_re.match(line) or hr_re.match(line):
        flush_buffer()
        out.append(line)
        continue

    # Start of a new list item: end any prior block, begin a fresh buffer
    # whose indent column is where the marker's content begins, so that
    # subsequent indented lines merge into this item.
    li_m = list_item_re.match(line)
    if li_m:
        flush_buffer()
        buffer.append(line)
        buffer_is_list = True
        # Content column = end offset of the matched "<indent><marker><spaces>".
        buffer_indent = li_m.end() - 1
        continue

    # Continuation line of a list item: indented to at least the item's
    # content column. Otherwise it's a separate paragraph and we flush.
    if buffer_is_list:
        leading = len(line) - len(line.lstrip(' '))
        if leading >= buffer_indent and leading > 0:
            buffer.append(line)
            continue
        flush_buffer()
        buffer.append(line)
        continue

    buffer.append(line)

flush_buffer()

result = '\n'.join(out)
if trailing_newline and not result.endswith('\n'):
    result += '\n'

with open(os.environ['OUTPUT_FILE'], 'w') as f:
    f.write(result)
PYTHON_SCRIPT

if [ -n "$TARGET_FILE" ]; then
    cp "$OUTPUT_FILE" "$TARGET_FILE"
    echo "Unwrapped '$TARGET_FILE'."
else
    eval "$CLIPBOARD_COPY" < "$OUTPUT_FILE"
    echo "Unwrapped markdown copied to clipboard."
fi
