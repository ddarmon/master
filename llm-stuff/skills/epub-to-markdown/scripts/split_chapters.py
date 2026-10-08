#!/usr/bin/env python3
"""Split a Pandoc-generated Markdown file into per-chapter files.

Reads a Pandoc Markdown file, splits on headings at a specified level,
merges paired chapter headings (e.g. "Chapter 2" + subtitle), cleans
Pandoc markup, and writes numbered files into an output directory.
"""

import argparse
import os
import re
import sys
from pathlib import Path

KNOWN_LANGS = {
    'python', 'bash', 'sh', 'zsh', 'ruby', 'javascript', 'js', 'typescript',
    'ts', 'java', 'c', 'cpp', 'csharp', 'go', 'rust', 'r', 'sql', 'html',
    'css', 'json', 'yaml', 'yml', 'xml', 'perl', 'php', 'scala', 'kotlin',
    'swift', 'lua', 'haskell', 'clojure', 'elixir', 'erlang', 'dockerfile',
    'makefile', 'text', 'plain', 'console', 'shell',
}


def clean_heading(line):
    """Strip Pandoc attributes from a heading line.

    Removes {.class ...} suffixes and []{#id ...} empty anchor spans.
    Loops to handle nested spans like [[1]{.num-string}]{.chapter-title-numbering}.
    """
    # Strip trailing {.class-name ...} attributes
    line = re.sub(r'\s*\{[^}]*\}\s*$', '', line)
    # Strip []{#anchor ...} empty spans
    line = re.sub(r'\[\]\{[^}]*\}', '', line)
    # Strip {.class} from inline spans like [text]{.class}
    # Loop to handle nested spans
    prev = None
    while prev != line:
        prev = line
        line = re.sub(r'\[([^\]]*)\]\{[^}]*\}', r'\1', line)
    # Clean up extra whitespace
    line = re.sub(r'  +', ' ', line).strip()
    return line


def clean_body(text, media_basename):
    """Clean Pandoc markup from body text."""
    # Strip trailing {.class ...} or {#id .class ...} from heading lines
    text = re.sub(
        r'^(#{1,6}\s+.*?)\s*\{[^}]*\}\s*$', r'\1',
        text, flags=re.MULTILINE
    )
    # Strip []{#anchor ...} empty anchor spans
    text = re.sub(r'\[\]\{[^}]*\}', '', text)
    # Strip {=html} raw inline markers: remove comments, unwrap tags
    text = re.sub(r'`<!--.*?-->`\{=html\}', '', text, flags=re.DOTALL)
    text = re.sub(r'`(<[^`]*>)`\{=html\}', r'\1', text)
    # Strip {.class} from spans with nested brackets (e.g. [[A](#link)]{.class})
    # Loop to catch nested/revealed patterns
    prev = None
    while prev != text:
        prev = text
        text = re.sub(
            r'\[((?:\[[^\]]*\](?:\([^)]*\))?[^\]]*)*)\]\{[^}]*\}',
            r'\1',
            text
        )
        text = re.sub(r'\[([^\]]*)\]\{[^}]*\}', r'\1', text)
    # Fallback for spans with escaped brackets or multiline attributes
    text = re.sub(r'\]\{[^}]*\}', ']', text)
    # Strip {.class} attribute spans from inline text [text]{.class}
    # Fix image paths: .../media_dir/images/... -> images/...
    text = re.sub(
        r'!\[([^\]]*)\]\([^)]*' + re.escape(media_basename) + r'/images/',
        r'![\1](images/',
        text
    )
    # Fix image paths when images are directly in media dir (no images/ subdir)
    text = re.sub(
        r'!\[([^\]]*)\]\([^)]*' + re.escape(media_basename) + r'/',
        r'![\1](images/',
        text
    )
    # Strip {#id .class} attributes from images
    text = re.sub(r'(!\[[^\]]*\]\([^)]*\))\{[^}]*\}', r'\1', text)
    # Strip {.class} attributes from links: [text](url){.class} -> [text](url)
    text = re.sub(r'(\[[^\]]*\]\([^)]*\))\{[^}]*\}', r'\1', text)
    # Unwrap {.figure_inline} spans around images
    text = re.sub(r'\[(!\[[^\]]*\]\([^)]*\)),?\]\{[^}]*\}', r'\1', text)
    # Catch-all: strip any remaining {.class ...} attribute annotations
    text = re.sub(r'\{(?:\.[a-zA-Z_][\w. -]*|#[\w-]+[\w. #-]*)\}', '', text)
    # Remove Pandoc ordered-list restart comments
    text = re.sub(r'\n<!-- -->\n', '\n', text)
    # Remove raw HTML pagebreak spans (may be multiline)
    text = re.sub(
        r'<span[^>]*class="pagebreak"[^>]*>\s*</span>\s*', '',
        text, flags=re.DOTALL
    )
    # Convert raw HTML <img> tags to Markdown
    def img_to_md(m):
        src = m.group(1)
        src = re.sub(
            r'^\.?/?' + re.escape(media_basename) + r'/images/',
            'images/', src
        )
        src = re.sub(
            r'^\.?/?' + re.escape(media_basename) + r'/',
            'images/', src
        )
        return f'![]({src})'
    text = re.sub(r'<img\s+src="([^"]*)"[^/]*/>', img_to_md, text)
    # Normalize code fence attributes, preserving known language names when present
    def clean_code_fence(m):
        prefix = m.group(1)
        fence = m.group(2)
        attrs = m.group(3)
        lang = ''
        lang_match = re.search(r'code-language="(\w+)"', attrs)
        if lang_match and lang_match.group(1).lower() in KNOWN_LANGS:
            lang = lang_match.group(1).lower()
        else:
            for class_match in re.finditer(r'\.([a-zA-Z][\w+-]*)', attrs):
                candidate = class_match.group(1).lower()
                if candidate in KNOWN_LANGS:
                    lang = candidate
                    break
        return prefix + fence + lang
    text = re.sub(
        r'^([ \t]*(?::\s+)?)' + r'(`{3,})' + r' \{([^}]+)\}\s*$',
        clean_code_fence,
        text,
        flags=re.MULTILINE
    )
    # Remove ::: div fences and their class attributes
    text = re.sub(r'^::+.*$', '', text, flags=re.MULTILINE)
    # Collapse runs of 3+ blank lines into 2
    text = re.sub(r'\n{4,}', '\n\n\n', text)
    return text


def slugify(title):
    """Convert a title to a filename-safe slug."""
    s = title.lower()
    s = re.sub(r'[^a-z0-9\s-]', '', s)
    s = re.sub(r'[\s-]+', '-', s).strip('-')
    return s[:60]


def split_chapters(input_path, output_dir, media_dir, heading_level=1):
    """Split a Markdown file into per-chapter files.

    Returns the number of chapter files written.
    """
    text = Path(input_path).read_text(encoding='utf-8')
    lines = text.split('\n')

    heading_prefix = '#' * heading_level + ' '
    media_basename = os.path.basename(os.path.normpath(media_dir))

    # Find all heading positions at the target level, skipping code fences
    heading_positions = []
    in_code_fence = False
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith('```'):
            in_code_fence = not in_code_fence
            continue
        if in_code_fence:
            continue
        if line.startswith(heading_prefix) and (
            heading_level == 1 or not line.startswith('#' * (heading_level + 1) + ' ')
        ):
            heading_positions.append(i)

    # Filter out empty or non-title headings
    def is_valid_heading(line_idx):
        text = clean_heading(lines[line_idx])
        title = re.sub(r'^#{1,6}\s*', '', text).strip()
        return bool(title) and bool(re.search(r'[a-zA-Z0-9]', title))

    heading_positions = [p for p in heading_positions if is_valid_heading(p)]

    if not heading_positions:
        print(f"No H{heading_level} headings found.")
        return 0

    # Build sections: each is (heading_line, body_text)
    sections = []
    for idx, pos in enumerate(heading_positions):
        end = heading_positions[idx + 1] if idx + 1 < len(heading_positions) else len(lines)
        heading = clean_heading(lines[pos])
        body_lines = lines[pos + 1:end]
        body = '\n'.join(body_lines)
        sections.append((heading, body))

    # Merge paired chapter headings: "# Chapter N" followed by "# Subtitle"
    chapter_re = re.compile(
        r'^' + re.escape(heading_prefix)
        + r'(?:\*\*)?'
        + r'(?:Chapter|Part|Primer)\s+'
        + r'(?:\d+|[IVXLCDM]+)'
        + r'(?:\*\*)?'
        + r'\s*$',
        re.IGNORECASE
    )
    merged = []
    skip_next = False
    for i, (heading, body) in enumerate(sections):
        if skip_next:
            skip_next = False
            continue
        if chapter_re.match(heading):
            if i + 1 < len(sections):
                next_heading, next_body = sections[i + 1]
                subtitle = re.sub(r'^#{1,6} ', '', next_heading)
                combined_heading = f"{heading}: {subtitle}"
                combined_body = body.rstrip() + '\n\n' + next_body if body.strip() else next_body
                merged.append((combined_heading, combined_body))
                skip_next = True
                continue
        merged.append((heading, body))

    # Create output directory
    os.makedirs(output_dir, exist_ok=True)

    # Set up images symlink
    images_link = os.path.join(output_dir, 'images')
    if not os.path.exists(images_link):
        # Check if media_dir has an images/ subdirectory
        images_subdir = os.path.join(media_dir, 'images')
        if os.path.isdir(images_subdir):
            target = images_subdir
        else:
            target = media_dir
        rel_target = os.path.relpath(target, output_dir)
        os.symlink(rel_target, images_link)

    # Write chapter files
    for idx, (heading, body) in enumerate(merged):
        title = re.sub(r'^#{1,6} ', '', heading)
        slug = slugify(title)
        filename = f"{idx:02d}-{slug}.md"
        filepath = os.path.join(output_dir, filename)

        content = heading + '\n' + clean_body(body, media_basename)
        # Trim trailing whitespace on each line, ensure single newline at end
        content_lines = [line.rstrip() for line in content.split('\n')]
        content = '\n'.join(content_lines).strip() + '\n'

        Path(filepath).write_text(content, encoding='utf-8')
        print(f"  {filename}")

    print(f"\nWrote {len(merged)} chapter files to {output_dir}/")
    return len(merged)


def main():
    parser = argparse.ArgumentParser(
        description='Split a Pandoc-generated Markdown file into per-chapter files.'
    )
    parser.add_argument('input', help='Path to the Pandoc Markdown file')
    parser.add_argument('output_dir', help='Directory to write chapter files into')
    parser.add_argument('media_dir', help='Path to the extracted media directory')
    parser.add_argument(
        '--heading-level', type=int, default=1, choices=[1, 2, 3],
        help='Heading level to split on (default: 1)'
    )
    args = parser.parse_args()

    count = split_chapters(args.input, args.output_dir, args.media_dir, args.heading_level)
    if count == 0:
        sys.exit(1)


if __name__ == '__main__':
    main()
