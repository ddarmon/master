# Excalidraw Tools Reference

## Contents

- CLI and environment
- Build from a spec
- Edit commands
- Python library
- Rendering and regression

## CLI and environment

`excalidraw-tools` provides:

- `build`: create a diagram from a compact JSON spec;
- `edit`: deterministic move, relabel, recolor, delete, add-box, and connect;
- `validate`: schema and linkage validation;
- `preview`: approximate Matplotlib rendering;
- `sync-spec`: derive or update a sidecar spec;
- `golden-check`: regression checks.

The package is installed as an isolated uv tool. Use:

| Task | Command |
|---|---|
| CLI | `excalidraw-tools <subcommand> ...` |
| Library script | `"$(uv tool dir)/excalidraw-tools/bin/python" script.py` |

Do not install or force-reinstall dependencies during a diagram task. If a
command is unavailable, report the missing prerequisite or request approval for
a separately scoped installation.

## Build from a spec

Minimal spec:

```json
{
  "seed": 42,
  "updated": 1700000000000,
  "style": {
    "fontFamily": 2,
    "roughness": 0
  },
  "nodes": [
    {
      "id": "api",
      "type": "rectangle",
      "label": "API",
      "x": 120,
      "y": 220,
      "width": 200,
      "height": 80,
      "stroke": "#7048e8",
      "background": "#d0bfff"
    }
  ],
  "edges": []
}
```

Build and keep a spec synchronized:

```bash
excalidraw-tools build \
  --spec diagram.spec.json \
  --output diagram.excalidraw \
  --sync-spec
```

If the spec lacks a global `style`, Excalidraw defaults to Virgil and
roughness 1. A node may override `fontFamily` or `roughness`.

## Edit commands

```bash
excalidraw-tools edit move \
  --input diagram.excalidraw --label "API" --dx 220 --dy 0

excalidraw-tools edit relabel \
  --input diagram.excalidraw --label "API" --text "Gateway API"

excalidraw-tools edit recolor \
  --input diagram.excalidraw --label "Gateway API" \
  --stroke "#1971c2" --background "#a5d8ff"

excalidraw-tools edit delete \
  --input diagram.excalidraw --label "Legacy Service"

excalidraw-tools edit add-box \
  --input diagram.excalidraw --label "Cache" \
  --x 520 --y 220 --width 180 --height 80 \
  --stroke "#fd7e14" --background "#ffe8cc"

excalidraw-tools edit connect \
  --input diagram.excalidraw \
  --from-label "Gateway API" --to-label "Cache" \
  --from-edge right --to-edge left --elbowed --label "Redis"
```

For clean additions, include `--font-family 2 --crisp`. Append
`--sync-spec` whenever continuous synchronization is active.

Create a spec from an existing diagram:

```bash
excalidraw-tools sync-spec \
  --diagram diagram.excalidraw \
  --spec diagram.spec.json
```

## Python library

Use the library when spec/edit commands cannot express the required elements.
Every constructor appends to `elements` and returns the new element. Let the
library assign IDs, indices, seeds, nonces, versions, timestamps, and bindings.

### Core imports

```python
from excalidraw_tools import (
    IdFactory,
    add_label,
    connect,
    load_diagram,
    make_arrow,
    make_shape,
    make_text,
    new_document,
    save_diagram,
)
```

### IDs

```python
ids = IdFactory(seed=42)
ids.random_id()
ids.random_id("arr")
ids.nonce()
ids.next_index()
ids.reserve_id("existing-id")
```

### Shapes and text

```python
shape = make_shape(
    elements, ids, "rectangle", 100, 100, 200, 80,
    stroke="#1971c2", background="#a5d8ff", roughness=0,
)
add_label(
    elements, ids, shape, "API Server",
    font_size=20, font_family=2, text_height=25,
)

make_text(
    elements, ids, "Clients", 160, 30, 100, 25,
    font_size=16, font_family=2,
)
```

`make_shape` supports `rectangle`, `ellipse`, and `diamond`. Never use
it with `line` or `arrow`.

### Lines, arrows, and connections

```python
arrow = make_arrow(
    elements, ids, 200, 180, [[0, 0], [0, 60]],
    stroke="#e03131", stroke_width=2,
)

line = make_arrow(elements, ids, 50, 200, [[0, 0], [300, 0]])
line["type"] = "line"
line["endArrowhead"] = None

connect(
    elements, ids, source, target,
    source_edge="right", target_edge="left",
    stroke="#1e1e1e", elbowed=True,
)
```

Use `make_arrow` for axes, curves, tick marks, polylines, and arrows. Point
coordinates are offsets from the element's `x, y`; the first sampled point
must therefore normalize to `[0, 0]`. Prefer `connect` for labeled shapes
because it calculates edge points and bindings.

### Save and load

```python
save_diagram("diagram.excalidraw", new_document(elements))
document = load_diagram("diagram.excalidraw")
```

Run the generator with the uv tool's Python:

```bash
"$(uv tool dir)/excalidraw-tools/bin/python" create_diagram.py
```

## Rendering and regression

Use the bundled wrapper from the skill directory:

```bash
scripts/render_diagrams.sh --input diagram.excalidraw
```

Validate the golden fixture after major skill or tool changes:

```bash
excalidraw-tools validate assets/golden/simple-flow.excalidraw
excalidraw-tools golden-check
```

The golden check covers schema, expected element counts, deterministic hashes,
spec round trips, and a render smoke test. It does not replace visual
inspection of the task's final render.
