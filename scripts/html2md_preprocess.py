#!/usr/bin/env python3
"""
HTML Preprocessor for html2md.sh

This script prepares HTML (specifically output from md2html.sh) for optimal 
conversion by pandoc. It performs the following transformations:

1. Removes CSS style blocks added by md2html.sh
2. Converts LaTeX math delimiters to placeholder tokens (to prevent pandoc escaping)
3. Simplifies complex syntax-highlighted code blocks to basic pre/code format
4. Converts <br><br> patterns to CUSTOM_BREAK tokens

The goal is to create HTML that pandoc can convert cleanly to Markdown,
avoiding the escaping and formatting issues that occur with complex HTML.
"""

import re
import html
import sys

def simplify_code_block(match):
    """Convert complex div.sourceCode blocks to simple pre/code format."""
    lang = match.group(1) if match.group(1) else ''
    code_content = match.group(2)
    
    # Remove all span and anchor tags, keeping only text content
    # Handle the nested span/anchor structure from syntax highlighting
    code_content = re.sub(r'<span[^>]*?><a[^>]*?></a>([^<]*?)</span>', r'\1', code_content)
    code_content = re.sub(r'<span[^>]*?>([^<]*?)</span>', r'\1', code_content)
    code_content = re.sub(r'<a[^>]*?></a>', '', code_content)
    code_content = re.sub(r'<[^>]*?>', '', code_content)
    
    # Decode HTML entities
    code_content = html.unescape(code_content)
    
    # Convert to simple pre/code format that pandoc handles well
    if lang:
        return f'<pre><code class="{lang}">{code_content}</code></pre>'
    else:
        return f'<pre><code>{code_content}</code></pre>'

def preprocess_html(input_file, output_file):
    """Main preprocessing function."""
    # Read the HTML content
    with open(input_file, 'r') as f:
        content = f.read()

    # Step 1: Remove CSS styles block
    content = re.sub(r'<style>.*?</style>', '', content, flags=re.DOTALL)

    # Step 2: Convert LaTeX math delimiters to placeholders
    # Use placeholders to avoid encoding issues with $ signs during pandoc conversion
    content = content.replace('\[', 'DISPLAY_MATH_START').replace('\]', 'DISPLAY_MATH_END')
    content = content.replace('\(', 'INLINE_MATH_START').replace('\)', 'INLINE_MATH_END')

    # Step 3: Simplify code blocks for pandoc
    # Pattern to match the complex div.sourceCode structure
    code_pattern = r'<div class="sourceCode"[^>]*?><pre class="sourceCode ([^"]*?)"[^>]*?><code[^>]*?>(.*?)</code></pre></div>'
    content = re.sub(code_pattern, simplify_code_block, content, flags=re.DOTALL)

    # Step 4: Convert <br><br> to CUSTOM_BREAK for pandoc processing
    content = content.replace('<br><br>', 'CUSTOM_BREAK')

    # Write processed HTML
    with open(output_file, 'w') as f:
        f.write(content)

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print('Usage: html2md_preprocess.py input_file output_file')
        print('Preprocesses HTML from md2html.sh for optimal pandoc conversion')
        sys.exit(1)
    
    preprocess_html(sys.argv[1], sys.argv[2])