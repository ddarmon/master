#!/bin/bash

# A tool to format Markdown using Quarto's canonical formatting.
# Works with files or clipboard (cross-platform).
# Usage: md2md [file]    - format file in-place
#        md2md           - format clipboard content
#
# NOTE: Quarto has restrictions on output file paths - it cannot accept
# absolute or relative paths with the --output option. This script works
# around this limitation by changing to the temporary directory and using
# relative filenames when calling quarto render.
#
# Written with Claude Sonnet 4 in Claude Code
#
# David Darmon, 5 July 2025

# Configuration
WRAP_WIDTH=${MD2MD_WRAP_WIDTH:-80}
ENABLE_PREPROCESSING=${MD2MD_PREPROCESSING:-true}
DEBUG=${DEBUG:-false}

# Debug logging function
debug_log() {
    if [[ "$DEBUG" == "true" ]]; then
        echo "DEBUG: $*" >&2
    fi
}

# Check dependencies
check_dependencies() {
    debug_log "Checking dependencies..."

    # Check if Quarto is installed
    if ! command -v quarto &> /dev/null; then
        echo "Error: Quarto is not installed."
        echo "Install it from: https://quarto.org/docs/get-started/"
        exit 1
    fi

    # Check if Python3 is available
    if ! command -v python3 &> /dev/null; then
        echo "Error: Python3 is required but not installed."
        exit 1
    fi

    # Check for clipboard tools based on platform
    if command -v pbcopy &> /dev/null && command -v pbpaste &> /dev/null; then
        CLIPBOARD_COPY="pbcopy"
        CLIPBOARD_PASTE="pbpaste"
        debug_log "Using macOS clipboard tools"
    elif command -v xclip &> /dev/null; then
        CLIPBOARD_COPY="xclip -selection clipboard"
        CLIPBOARD_PASTE="xclip -selection clipboard -o"
        debug_log "Using xclip for clipboard"
    elif command -v wl-copy &> /dev/null && command -v wl-paste &> /dev/null; then
        CLIPBOARD_COPY="wl-copy"
        CLIPBOARD_PASTE="wl-paste"
        debug_log "Using Wayland clipboard tools"
    else
        echo "Error: No supported clipboard tool found."
        echo "Install one of: pbcopy/pbpaste (macOS), xclip (X11), or wl-copy/wl-paste (Wayland)"
        exit 1
    fi
}

# Input validation
validate_input_file() {
    local file="$1"
    # Check for directory traversal attempts
    if [[ "$file" == *".."* ]]; then
        echo "Error: Invalid file path."
        exit 1
    fi
}

# Check if file has YAML front matter
has_yaml_frontmatter() {
    local file="$1"
    head -1 "$file" | grep -q "^---$" && \
    tail -n +2 "$file" | grep -q "^---$"
}

# Extract YAML front matter
extract_yaml() {
    local file="$1"
    local yaml_end_line
    yaml_end_line=$(tail -n +2 "$file" | grep -n "^---$" | head -1 | cut -d: -f1)
    if [ -n "$yaml_end_line" ]; then
        yaml_end_line=$((yaml_end_line + 2))
        sed -n "1,${yaml_end_line}p" "$file"
    fi
}

# Extract content after YAML
extract_content_after_yaml() {
    local file="$1"
    local yaml_end_line
    yaml_end_line=$(tail -n +2 "$file" | grep -n "^---$" | head -1 | cut -d: -f1)
    if [ -n "$yaml_end_line" ]; then
        yaml_end_line=$((yaml_end_line + 2))
        tail -n +$((yaml_end_line + 1)) "$file"
    fi
}

# Setup temporary files
setup_temp_files() {
    debug_log "Setting up temporary files..."
    TEMP_DIR=$(mktemp -d)
    TEMP_INPUT_FILE="$TEMP_DIR/input.md"
    TEMP_QMD_FILE="$TEMP_DIR/temp.qmd"
    TEMP_OUTPUT_FILE="$TEMP_DIR/output.md"
    FINAL_TEMP="$TEMP_DIR/final.md"
    CLEANED_OUTPUT="$TEMP_DIR/cleaned.md"

    trap 'rm -rf "$TEMP_DIR"' EXIT
    debug_log "Temporary directory: $TEMP_DIR"
}

# Process input (clipboard or file)
process_input() {
    debug_log "Processing input..."

    # Determine input source
    USE_CLIPBOARD=false
    INPUT_FILE_PATH=""

    if [ -z "$1" ]; then
        # No argument provided - use clipboard
        USE_CLIPBOARD=true
        echo "No file provided, formatting clipboard content..."
    else
        # File argument provided
        INPUT_FILE_PATH="$1"
        validate_input_file "$INPUT_FILE_PATH"

        # Check if the specified file exists
        if [ ! -f "$INPUT_FILE_PATH" ]; then
            echo "Error: File not found at '$INPUT_FILE_PATH'"
            exit 1
        fi

        # Resolve the absolute path of the input file
        if [[ "$INPUT_FILE_PATH" != /* ]]; then
            INPUT_FILE_PATH="$(pwd)/$INPUT_FILE_PATH"
        fi
    fi

    # Get the input content
    if [ "$USE_CLIPBOARD" = true ]; then
        # Get content from clipboard
        $CLIPBOARD_PASTE > "$TEMP_INPUT_FILE"

        # Check if clipboard had any content
        if [ ! -s "$TEMP_INPUT_FILE" ]; then
            echo "Error: Clipboard is empty or contains no text."
            exit 1
        fi

        INPUT_CONTENT="$TEMP_INPUT_FILE"
    else
        # Use the provided file
        INPUT_CONTENT="$INPUT_FILE_PATH"
    fi

    debug_log "Input source: ${USE_CLIPBOARD:+clipboard}${INPUT_FILE_PATH:+$INPUT_FILE_PATH}"
}

# Format content with Quarto
format_with_quarto() {
    debug_log "Formatting content with Quarto..."

    # Process the content - check for existing YAML front matter
    if has_yaml_frontmatter "$INPUT_CONTENT"; then
        debug_log "Found existing YAML front matter"

        # Extract existing YAML (without the opening and closing ---)
        YAML_END_LINE=$(tail -n +2 "$INPUT_CONTENT" | grep -n "^---$" | head -1 | cut -d: -f1)
        if [ -n "$YAML_END_LINE" ]; then
            # Add 1 to account for skipping first line, then add 1 more for the closing ---
            YAML_END_LINE=$((YAML_END_LINE + 2))

            # Extract existing YAML (without the opening and closing ---)
            EXISTING_YAML=$(sed -n "2,$((YAML_END_LINE-1))p" "$INPUT_CONTENT")

            # Extract content after YAML
            CONTENT_AFTER_YAML=$(extract_content_after_yaml "$INPUT_CONTENT")

            # Create new file with merged YAML
            (cat <<EOF
---
editor:
  markdown:
    wrap: $WRAP_WIDTH
    canonical: true
$EXISTING_YAML
---
$CONTENT_AFTER_YAML
EOF
            ) > "$TEMP_QMD_FILE"
        else
            # Malformed YAML (opening --- but no closing ---), treat as no YAML
            (cat <<EOF
---
editor:
  markdown:
    wrap: $WRAP_WIDTH
    canonical: true
---
EOF
            cat "$INPUT_CONTENT"
            ) > "$TEMP_QMD_FILE"
        fi
    else
        debug_log "No existing YAML front matter found"
        # No existing YAML front matter
        (cat <<EOF
---
editor:
  markdown:
    wrap: $WRAP_WIDTH
    canonical: true
---
EOF
        # Preprocessing (if enabled)
        if [[ "$ENABLE_PREPROCESSING" == "true" ]]; then
            debug_log "Applying preprocessing..."
            # Very targeted preprocessing: only add blank lines before lists
            # when they would otherwise be merged into preceding text
            if ! python3 -c '
import sys
import re

lines = sys.stdin.readlines()
list_pattern = re.compile(r"^\s*(?:[-*+]|\d+\.)\s+")

# First pass: identify where we truly need blank lines
needs_blank = set()
for i in range(len(lines)):
    if i == 0:
        continue

    curr_is_list = bool(list_pattern.match(lines[i]))
    prev_is_list = bool(list_pattern.match(lines[i-1]))
    prev_is_blank = lines[i-1].strip() == ""

    # Only add blank if:
    # 1. Current line is a list item
    # 2. Previous line is not blank
    # 3. Previous line is not a list item
    # 4. We are not already in a list (to avoid making loose lists)
    if curr_is_list and not prev_is_blank and not prev_is_list:
        # Check if we are starting a new list or continuing one
        # Look backwards to see if we are already in a list context
        in_list_context = False
        for j in range(i-2, -1, -1):
            if lines[j].strip() == "":
                break
            if list_pattern.match(lines[j]):
                in_list_context = True
                break

        # Only add blank if we are truly starting a new list
        if not in_list_context:
            needs_blank.add(i)

# Second pass: output with blanks where needed
for i, line in enumerate(lines):
    if i in needs_blank:
        sys.stdout.write("\n")
    sys.stdout.write(line)
' < "$INPUT_CONTENT"; then
                echo "Error: Preprocessing failed."
                exit 1
            fi
        else
            debug_log "Preprocessing disabled, using content as-is"
            cat "$INPUT_CONTENT"
        fi
        ) > "$TEMP_QMD_FILE"
    fi

    # Render the temporary Quarto file
    debug_log "Rendering with Quarto..."
    # Change to temp directory to avoid absolute path issues with Quarto
    (cd "$TEMP_DIR" && quarto render "temp.qmd" --to markdown-raw_tex --output "output.md" --quiet) || {
        echo "Warning: Quarto render encountered issues, but continuing..."
        # If render failed, check if output file exists anyway
        if [ ! -f "$TEMP_OUTPUT_FILE" ]; then
            echo "Error: Formatting failed and no output file was created."
            exit 1
        fi
    }
}

# Handle output (restore YAML if needed)
handle_output() {
    debug_log "Handling output..."

    # Handle output - strip generated YAML if no original YAML existed
    FINAL_OUTPUT="$TEMP_OUTPUT_FILE"
    if has_yaml_frontmatter "$INPUT_CONTENT"; then
        debug_log "Restoring original YAML front matter"

        # Extract the original YAML front matter
        ORIGINAL_YAML=$(extract_yaml "$INPUT_CONTENT")

        if [ -n "$ORIGINAL_YAML" ]; then
            # Get the formatted content (skip the generated YAML header)
            # Find the end of the YAML front matter in the output
            YAML_END_LINE=$(tail -n +2 "$TEMP_OUTPUT_FILE" | grep -n "^---$" | head -1 | cut -d: -f1)
            if [ -n "$YAML_END_LINE" ]; then
                # Add 2 to account for skipping first line and the closing ---
                YAML_END_LINE=$((YAML_END_LINE + 2))
                FORMATTED_CONTENT=$(tail -n +$((YAML_END_LINE + 1)) "$TEMP_OUTPUT_FILE")
            else
                # Fallback: if no YAML found, take content after first few lines
                FORMATTED_CONTENT=$(tail -n +5 "$TEMP_OUTPUT_FILE")
            fi

            # Create final output with original YAML
            (echo "$ORIGINAL_YAML"; echo; echo "$FORMATTED_CONTENT") > "$FINAL_TEMP"
            FINAL_OUTPUT="$FINAL_TEMP"
        fi
    else
        debug_log "No original YAML, stripping generated YAML"
        # No original YAML - strip the generated YAML header
        YAML_END_LINE=$(tail -n +2 "$TEMP_OUTPUT_FILE" | grep -n "^---$" | head -1 | cut -d: -f1)
        if [ -n "$YAML_END_LINE" ]; then
            # Add 2 to account for skipping first line and the closing ---
            YAML_END_LINE=$((YAML_END_LINE + 2))
            tail -n +$((YAML_END_LINE + 1)) "$TEMP_OUTPUT_FILE" > "$FINAL_TEMP"
            FINAL_OUTPUT="$FINAL_TEMP"
        fi
    fi

    # Remove extra newlines at the beginning
    sed '/./,$!d' "$FINAL_OUTPUT" > "$CLEANED_OUTPUT"

    # Output the result
    if [ "$USE_CLIPBOARD" = true ]; then
        # Copy formatted content back to clipboard
        cat "$CLEANED_OUTPUT" | $CLIPBOARD_COPY
        echo "Formatted markdown copied to clipboard."
    else
        # Replace the original file with the formatted content
        cp "$CLEANED_OUTPUT" "$INPUT_FILE_PATH"
        echo "Successfully formatted '$INPUT_FILE_PATH'."
    fi
}

# Main execution
check_dependencies
setup_temp_files
process_input "$1"
format_with_quarto
handle_output