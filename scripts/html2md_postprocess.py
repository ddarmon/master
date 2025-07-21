#!/usr/bin/env python3
"""
Markdown Postprocessor for html2md.sh

This script cleans up markdown output from pandoc to fix escaping issues and
formatting quirks that occur during HTML->Markdown conversion.

Key functions:
1. Converts math placeholder tokens back to proper $ and $$ delimiters
2. Fixes pandoc's over-escaping of LaTeX commands (\\alpha -> \alpha)
3. Unescapes special characters in math contexts (subscripts, superscripts)
4. Fixes code block and list formatting inconsistencies
5. Normalizes whitespace and paragraph spacing

This ensures the final markdown output matches the original as closely as possible.
"""

import re
import sys

def postprocess_markdown(input_file, output_file):
    # Read pandoc output
    with open(input_file, 'r') as f:
        content = f.read()

    # Convert CUSTOM_BREAK to proper paragraph breaks
    content = content.replace('CUSTOM_BREAK', '\n\n')

    # Convert math placeholders to actual math delimiters and fix escaping
    def unescape_math_content(match):
        math_content = match.group(1)
        # Fix pandoc's over-escaping in math contexts
        math_content = re.sub(r'\\\\([a-zA-Z]+)', r'\\\1', math_content)  # \\alpha -> \alpha
        math_content = math_content.replace('\\_', '_')  # subscripts
        math_content = math_content.replace('\\^', '^')  # superscripts  
        math_content = math_content.replace('\\{', '{')  # braces
        math_content = math_content.replace('\\}', '}')  # braces
        # Fix double-escaped line breaks in LaTeX environments
        math_content = math_content.replace('\\\\\\\\', '\\\\')
        return '$$' + math_content + '$$'
    
    def unescape_inline_math_content(match):
        math_content = match.group(1)
        # Fix pandoc's over-escaping in math contexts
        math_content = re.sub(r'\\\\([a-zA-Z]+)', r'\\\1', math_content)  # \\alpha -> \alpha  
        math_content = math_content.replace('\\_', '_')  # subscripts
        math_content = math_content.replace('\\^', '^')  # superscripts
        math_content = math_content.replace('\\{', '{')  # braces
        math_content = math_content.replace('\\}', '}')  # braces
        # Fix double-escaped line breaks in LaTeX environments
        math_content = math_content.replace('\\\\\\\\', '\\\\')
        return '$' + math_content + '$'
    
    # Process display math
    content = re.sub(r'DISPLAY_MATH_START(.*?)DISPLAY_MATH_END', unescape_math_content, content, flags=re.DOTALL)
    
    # Process inline math  
    content = re.sub(r'INLINE_MATH_START(.*?)INLINE_MATH_END', unescape_inline_math_content, content, flags=re.DOTALL)

    # Fix ordered list formatting (pandoc adds extra space)
    content = re.sub(r'^(\d+)\.  ', r'\1. ', content, flags=re.MULTILINE)

    # Fix code block formatting (remove space after backticks)
    content = re.sub(r'^``` ([a-zA-Z]+)$', r'```\1', content, flags=re.MULTILINE)

    # Remove excessive blank lines (3+ consecutive → 2)
    content = re.sub(r'\n{3,}', '\n\n', content)

    # Remove any pandoc div artifacts that might remain
    content = re.sub(r'^:::\s*\{[^}]*\}\s*$', '', content, flags=re.MULTILINE)
    content = re.sub(r'^:::\s*$', '', content, flags=re.MULTILINE)

    # Clean up spacing around code blocks
    content = re.sub(r'\n{2,}(`{3})', r'\n\n\1', content)
    content = re.sub(r'(`{3})\n{2,}', r'\1\n\n', content)

    # Remove trailing whitespace from lines
    content = re.sub(r'[ \t]+$', '', content, flags=re.MULTILINE)

    # Final cleanup of excessive newlines
    content = re.sub(r'\n{3,}', '\n\n', content)

    # Handle final newline to match original
    content = content.rstrip()
    if not content.endswith('\n'):
        content += '\n'

    # Write result
    with open(output_file, 'w') as f:
        f.write(content)

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print('Usage: html2md_postprocess.py input_file output_file')
        sys.exit(1)
    
    postprocess_markdown(sys.argv[1], sys.argv[2])