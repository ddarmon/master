# Rewritten from:
#
# pbpaste |
#   pandoc -f markdown -t html \
#   --wrap=preserve \
#   --lua-filter=/Users/daviddarmon/Dropbox/scripts/md2html.lua |
#   sed 's/CUSTOM_BREAK/<br><br>/g' |
#   perl -pe 'chomp if eof' |
#   pbcopy; echo 'Reformatted to clipboard.'
#
# by Gemini 2.5 Pro and then Claude 4 Sonnet on 21 June 2025

#!/usr/bin/env bash

# This script converts Markdown from the clipboard into self-contained HTML for Anki.

# --- Configuration ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LUA_FILTER="$SCRIPT_DIR/md2html.lua"
HIGHLIGHT_THEME="pygments"

# --- Script Logic ---
# Create a temporary file to avoid shell interpretation of escape sequences
temp_file=$(mktemp)
pbpaste > "$temp_file"

if grep -qE '```\s*[a-zA-Z]+' "$temp_file"; then
    echo "Language block detected. Generating HTML with syntax highlighting..."
    pandoc "$temp_file" -f markdown -t html --wrap=preserve \
      --standalone \
      --highlight-style="$HIGHLIGHT_THEME" \
      --lua-filter="$LUA_FILTER" | \
    awk '
    # Start a style block and capture syntax highlighting CSS
    # Also add the `.sourceCode { background: #f8f8f8; }`
    /CSS for syntax highlighting/ {in_syntax=1; print "<style>"; print ".highlight, .sourceCode { background: #f8f8f8; }"; next}
    in_syntax && /<\/style>/ {print; in_syntax=0}
    in_syntax {print}
    # Extract body content
    /<body/ {p=1; next}
    /<\/body>/ {p=0}
    p
    ' | \
    # Remove whitespace in `<style></style>` section
    perl -pe 's/^[[:space:]]+// if /<style>/ .. /<\/style>/' | \
    # Handle the conversion from `CUSTOM_BREAK` to `<br><br>`
    perl -pe 's/CUSTOM_BREAK/<br><br>/g' | \
    # Remove <br><br> before list elements and include a single newline
    perl -0pe 's/<br><br>\n(<[uo]l>)/$1/g' | \
    # Remove <br><br> before and after code blocks
    perl -0pe 's/<br><br>\n(<pre><code)/$1/g' | \
    perl -0pe 's/(<\/code><\/pre>)\n<br><br>/$1/g' | \
    # Remove final newline
    perl -pe 'chomp if eof' | \
    pbcopy
else
    echo "No language block detected. Generating plain HTML..."
    pandoc "$temp_file" -f markdown -t html --wrap=preserve \
      --lua-filter="$LUA_FILTER" | \
    # Handle the conversion from `CUSTOM_BREAK` to `<br><br>`
    perl -pe 's/CUSTOM_BREAK/<br><br>/g' | \
    # Remove <br><br> before list elements and fix double newlines
    perl -0pe 's/<br><br>\n(<[uo]l>)/$1/g' | \
    # Remove <br><br> before and after code blocks
    perl -0pe 's/<br><br>\n(<pre><code)/$1/g' | \
    perl -0pe 's/(<\/code><\/pre>)\n<br><br>/$1/g' | \
    perl -pe 'chomp if eof' | \
    pbcopy
fi

# Clean up temporary file
rm "$temp_file"

echo "Reformatted and copied to clipboard."