---
name: excalidraw
description: Create, edit, inspect, validate, and render editable Excalidraw (.excalidraw) diagrams. Use when the user explicitly requests Excalidraw, wants an editable whiteboard or freeform box-and-arrow source, or asks to modify or review an existing .excalidraw file. For a generic diagram request, use diagram-generation to select the backend first; do not use Excalidraw for exact mathematical plots better suited to Matplotlib.
---

# Excalidraw Interactive Diagramming

Use `excalidraw-tools` to manipulate Excalidraw files deterministically.
Never emit raw Excalidraw JSON by hand. Library-backed generator scripts are
allowed because the library supplies required defaults, IDs, bindings, and
indexing.

## Preflight

Before authoring, check the render path:

```bash
<skill-directory>/scripts/render_diagrams.sh --help
curl -fsS http://localhost:3004/healthz
```

Do not force-reinstall a shared tool when a renderer or optional preview
dependency is missing. Use an already available isolated environment or report
the actionable missing dependency.

For a new Excalidraw diagram, establish three format-specific preferences when
they are not already known:

1. Sidecar spec: none, update on request, or keep synchronized after each edit.
2. Style: hand-drawn (Virgil, roughness 1) or clean (Helvetica, roughness 0).
3. Optional render-font substitution.

Ask these only after Excalidraw has been selected as the backend.

## Create or edit

Prefer this order:

1. Build a new diagram from a compact spec with `excalidraw-tools build`.
2. Use `excalidraw-tools edit` for deterministic moves, labels, colors,
   deletion, boxes, and connections.
3. Use a Python script importing `excalidraw_tools` when curves, custom lines,
   axes, or other elements exceed the spec/edit commands.

Use `connect(...)` for ordinary shape connections so arrows bind to shape
edges. For manually routed connectors, leave visible boundary gaps and confirm
that arrowheads do not cover outlines or labels.

When continuous spec synchronization is selected, pass `--sync-spec` on every
build/edit command or run `excalidraw-tools sync-spec` immediately afterward.

Read [references/tooling.md](references/tooling.md) when exact CLI syntax,
spec structure, or library APIs are needed. Read
[references/arrows.md](references/arrows.md) for manual routing and
[references/json-format.md](references/json-format.md) only when diagnosing
format-level behavior.

## Math labels

Use renderer-safe Unicode math-like text in Excalidraw labels, for example
`xₙ`, `x²`, `φ(α)`, `≤`, `−`, and `√π`.

- Do not use `$...$`, `\(...\)`, `\[...\]`, or LaTeX-like
  underscore/caret notation in final labels.
- Normalize math labels when editing an existing diagram.
- Keep symbol roles, sign conventions, and random-versus-realized notation
  consistent with the surrounding source.

## Validate, render, and inspect

After every edit:

1. Validate the Excalidraw structure.
2. Render the exact current file.
3. Inspect the exact render at full resolution and at the target display size.

```bash
<skill-directory>/scripts/render_diagrams.sh --input diagram.excalidraw
```

For a URL-encoded font map:

```bash
<skill-directory>/scripts/render_diagrams.sh \
  --input diagram.excalidraw \
  --font-map 'Helvetica:CMU+Sans+Serif'
```

The wrapper prefers the Chromium renderer and falls back to
`excalidraw-tools preview` only when no custom font was requested. The
fallback is approximate and cannot prove browser text parity. If a custom font
was requested but Chromium is unavailable, stop rather than silently claiming
the requested font was used.

Structural validation does not establish visual quality. Before delivery,
check:

- text-text, text-line, and arrow-box collisions;
- connector endpoints and arrowhead clearance;
- cropping, whitespace, visual hierarchy, and label legibility;
- readability at the destination size;
- semantic accuracy and explicit labeling of specialized examples.

Repeat `edit -> validate -> render -> inspect` after the final change. Do not
publish a render that has not been inspected since its last edit.

## Read an existing diagram

Render first, then describe its shapes, labels, connections, hierarchy, and
layout. Treat handwritten `freedraw` content as approximate. When reviewing
quality, separate format validity, mathematical/content validity, and visual
layout.

## Deliver

Use matching basenames for the Excalidraw source and rendered preview. Export
only the formats the user requested. Keep drafts in staging and promote only
the chosen source, synchronized spec when requested, and final render.

Report the final paths and disclose any renderer limitation that affects font
or layout fidelity.
