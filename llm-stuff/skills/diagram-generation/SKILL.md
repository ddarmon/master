---
name: diagram-generation
description: Route, create, review, and quality-check technical diagrams with the backend best suited to their structure and delivery medium. Use whenever the user asks for a diagram, mathematical plot, conceptual flow, architecture drawing, graph, or small-screen visual such as an Anki image, unless they explicitly require a particular lower-level format or tool.
---

# Diagram Generation

Choose the representation and backend before authoring, then treat the exact
final render as the artifact under review.

## Establish the output contract

Infer what is already clear and ask only for missing choices that materially
affect the result:

- target medium and approximate display size;
- destination and whether nearby existing assets should be considered;
- required source/editability and final formats;
- style or font requirements.

Do not create optional formats merely because a backend supports them. Work in
a staging directory until the user has selected the final assets, then promote
only the chosen source and outputs.

## Inspect before creating

Treat source citations, `Reference:` footers, and filesystem-like project names as pointers to possible visual precedents—not merely bibliographic text. Resolve them when possible and inventory the nearby figures, generators, styles, and destination folders before designing anything. Reuse a suitable visual when it already teaches the same idea at the target size. Check its mathematics and notation as well as its appearance.

For work inside a research directory organized by topic, inspect the relevant topic directory and any `anki-cards/` subdirectory first. Such topic directories are the durable home for generated card materials. Use a temporary directory only for rejected experiments; promote accepted source and final assets into the relevant topic's `anki-cards/` directory before downstream attachment.

Before extending an existing figure series, record a short style ledger from its actual source and renders:

- canvas dimensions and destination display size;
- font family, math font, and type scale;
- semantic palette and fill colors;
- line widths, markers, grids, and axis treatment;
- title, subtitle, caption, callout, and explanatory-panel patterns;
- margins, whitespace, and filename conventions.

Inherit that visual language unless the user asks for a redesign. For the preferred Anki-oriented Matplotlib pattern in this research tree, inspect `.../Research/2026/bg/6-geometry-of-proper-scoring-rules/anki-cards/proper_scoring_diagrams.py`: it demonstrates direct labeling, restrained semantic colors, explanatory side panels, deliberate margins, CMU math typography, reproducible source, and target-size review. Treat it as a quality exemplar, not as a universal subject-specific template.

## Choose the backend

Use the backend whose constraints match the information:

| Structure | Preferred backend |
|---|---|
| Axes, functions, curves, tangents, exact coordinates, aligned subplots | Python/Matplotlib |
| Editable box-and-arrow explanations, whiteboard layouts, freeform annotation | Excalidraw |
| Trees, dependency graphs, state diagrams, automatically laid-out networks | Graphviz or Mermaid |
| Precise browser-native layout or interactive explanatory visual | HTML/CSS/SVG |
| Illustrative raster art rather than exact technical geometry | Image generation |

Honor an explicitly requested backend. Otherwise explain a non-obvious choice
briefly. Prefer one backend across a series when visual consistency matters,
provided it remains fit for every member.

Use the `excalidraw` skill only after choosing Excalidraw. Its spec, style, and
render-font questions are format-specific rather than universal setup.

## Design for the destination

- Give each visual one main teaching purpose.
- Split dense composites for small screens such as Anki.
- Use a concrete example only when it clarifies the general result, and label
  it explicitly (for example, `Brier-loss example`).
- Keep notation, orientation, assumptions, and random-versus-realized symbols
  consistent with the surrounding text or cards.
- Prototype one representative visual before producing a series.

For flowcharts, connect shapes at their boundaries with visible padding. Do
not place arrowheads, connectors, or annotations over boxes or text.

## Validate in three gates

Validation is not complete until all three gates pass.

### 1. Structural

- The generator exits successfully.
- Expected files, dimensions, and formats exist.
- Format-specific schema or linkage validation passes.

### 2. Semantic

- Formulas, labels, directionality, and sign conventions are correct.
- Assumptions and specialized examples are visible.
- The visual does not imply that a schematic or one model is universal.
- Every symbol has the same type and meaning as in the source material.
- When plotted labels or classes can be derived from a mathematical
  definition—such as nondominated points, extrema, feasible points, or graph
  reachability—compute them from the underlying data rather than maintaining
  a separate hand-selected list. Add an assertion or small validation check
  where practical. Visual plausibility is not semantic validation.

### 3. Visual

- Render after the last edit and inspect that exact raster or browser output.
- Inspect at the approximate target display size first, then at native pixels only where fine detail is genuinely in question—crop the region of interest rather than reading a larger whole canvas. A native-pixel crop shows more real detail than an oversized full-frame read.
- Cap the long edge of a whole-canvas read at about 1568 px. Vision encoders downsample above roughly that width, so extra pixels buy no detail while costing payload and tokens; some endpoints also cap the entire request near 1 MB, where one oversized read fails the turn outright. For line art, quantize the downscale (`magick SRC -resize 1568x -colors 128 -strip PNG8:OUT`)—typically under a tenth the bytes of a plain resize at the same legibility.
- Perform an explicit collision audit: text–text, text–curve, text–axis, annotation–point, arrow–label, panel–content, and panel–canvas boundary.
- Do not accept a label sitting on a curve merely because it remains technically readable. Move it into whitespace; when proximity is pedagogically useful, use offset placement, leader lines, or an opaque background with visible padding.
- Check cropping, whitespace, hierarchy, legibility, connector endpoints, and whether explanatory panels remain readable at destination size.
- Verify that a requested font actually resolved; a declared fallback is not proof that the requested font rendered.
- If the visual belongs to a series, compare it side by side with the precedent at target size and correct conspicuous drift in typography, density, palette, margins, and annotation style.

Repeat `edit -> render -> inspect` after every visual change. Structural validation never substitutes for inspecting the final render, and a first successful render is a draft rather than evidence of visual completion.

## Handle feedback precisely

Translate feedback such as "smushed" or "needs work" into observable defects
by inspecting the artifact. If several interpretations remain plausible, ask
one focused question before redesigning unrelated parts.

## Deliver or publish

Export only the requested formats. Do not attach, upload, or embed an image in
a downstream system until the current render has passed semantic and visual
QA. After publishing, retrieve or read back the downstream artifact and compare
its filename or hash when the integration supports that check.

Preflight required renderers and dependencies before authoring. Never
force-reinstall a shared tool to obtain an optional renderer; use an isolated
environment or request approval for a documented installation.
