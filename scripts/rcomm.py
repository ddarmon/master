#!/usr/bin/env python3
#
# Name: rcomm.py
#
# Description:
# Reads R code from the system clipboard, identifies comment blocks, and
# formats them using an external, clipboard-based Markdown formatting script
# (like an alias for a tool that uses pbcopy/pbpaste).
#
# It strips the comment prefixes, copies the raw text to the clipboard, runs
# the external script, reads the formatted text back, and re-assembles the
# R code. The final, formatted code is copied back to the system clipboard.
#
# Usage:
# 1. Copy R code to your clipboard.
# 2. Run this script: ./rcomm.py
# 3. Paste the formatted code.
#
# Dependencies:
# - Python 3.6+
# - macOS with `pbcopy` and `pbpaste`.
# - An external command or alias `md2md` must be available to the shell.
#   If the alias is in .zshrc, this script will invoke it via Zsh.
#
# Author: Gemini
#

import sys
import re
import subprocess
from io import StringIO

# --- Configuration ---
# The command or alias to use for formatting the Markdown content.
MARKDOWN_FORMATTER_CMD = "md2md"

# Regular expression to identify and capture parts of an R comment line.
# Groups:
# 1: Leading whitespace (indentation)
# 2: The comment marker itself (e.g., '#', '##', "#'")
# 3: Whitespace immediately following the marker
# 4: The actual text content of the comment
COMMENT_REGEX = re.compile(r"^([ \t]*)(#+'?)([ \t]*)(.*)")


def process_and_flush_block(prefix, lines, output_buffer):
    """
    Takes a comment prefix and a list of text lines, formats them
    using the external clipboard-based script, and writes the result to an
    in-memory buffer.
    """
    if not lines:
        return

    raw_text = "\n".join(lines)
    # Guard against calling the formatter with empty/whitespace-only content.
    if not raw_text.strip():
        for line in lines:
            # For empty lines, just reconstruct them with the canonical prefix.
            output_buffer.write(f"{prefix}\n")
        return

    formatted_text = raw_text # Use original text as a fallback

    try:
        # Step 1: Copy the raw comment text to the clipboard.
        subprocess.run(['pbcopy'], input=raw_text, text=True, check=True)

        # Step 2: Run the external formatter via the shell.
        command_to_run = f"zsh -ic '{MARKDOWN_FORMATTER_CMD}'"
        subprocess.run(
            command_to_run,
            shell=True,
            check=True,
            capture_output=True # Hide "Formatting..." messages.
        )

        # Step 3: Read the formatted result back from the clipboard.
        paste_process = subprocess.run(['pbpaste'], capture_output=True, text=True, check=True)
        formatted_text = paste_process.stdout

    except subprocess.CalledProcessError as e:
        cmd_str = e.cmd
        if isinstance(e.cmd, list):
            cmd_str = ' '.join(e.cmd)

        print(f"Error: A subprocess command failed: `{cmd_str}`", file=sys.stderr)
        print(f"Stderr:\n{e.stderr.decode() if e.stderr else 'N/A'}", file=sys.stderr)
        print("Falling back to original comment block.", file=sys.stderr)

    # Write the newly formatted text to the output buffer.
    for line in formatted_text.rstrip("\n").splitlines():
        output_buffer.write(f"{prefix}{line}\n")


def main():
    """
    Main function to read from clipboard, process, and write back to clipboard.
    """
    try:
        # Read the initial code directly from the clipboard.
        paste_process = subprocess.run(['pbpaste'], capture_output=True, text=True, check=True)
        source_code = paste_process.stdout
    except (FileNotFoundError, subprocess.CalledProcessError) as e:
        print("Error: Could not read from `pbpaste`. Is `pbpaste` installed and working?", file=sys.stderr)
        if isinstance(e, subprocess.CalledProcessError):
            print(f"Stderr:\n{e.stderr}", file=sys.stderr)
        sys.exit(1)

    current_block_key = None # Will be a tuple (indent, marker)
    comment_buffer = []
    # Use an in-memory buffer to build the output.
    output_buffer = StringIO()

    for line in source_code.splitlines():
        match = COMMENT_REGEX.match(line)

        if match:
            indent, marker, _, text = match.groups()
            block_key = (indent, marker)

            # If the block key (indentation or marker type) changes, flush the previous block.
            if block_key != current_block_key and current_block_key is not None:
                old_indent, old_marker = current_block_key
                # Apply a canonical prefix with a single space.
                prefix_to_apply = f"{old_indent}{old_marker} "
                process_and_flush_block(prefix_to_apply, comment_buffer, output_buffer)
                comment_buffer = []

            current_block_key = block_key
            comment_buffer.append(text)
        else:
            # This line is code, not a comment. Flush any preceding comment block.
            if comment_buffer:
                old_indent, old_marker = current_block_key
                prefix_to_apply = f"{old_indent}{old_marker} "
                process_and_flush_block(prefix_to_apply, comment_buffer, output_buffer)
                comment_buffer = []
                current_block_key = None # Reset

            output_buffer.write(f"{line}\n")

    # After the loop, process any remaining comment block at the end of the file.
    if comment_buffer:
        old_indent, old_marker = current_block_key
        prefix_to_apply = f"{old_indent}{old_marker} "
        process_and_flush_block(prefix_to_apply, comment_buffer, output_buffer)

    # Finally, copy the complete, formatted content back to the clipboard.
    final_output = output_buffer.getvalue()
    subprocess.run(['pbcopy'], input=final_output, text=True)

    print("Formatted R code has been copied to the clipboard.", file=sys.stderr)


if __name__ == "__main__":
    main()
