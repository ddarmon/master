---
name: excalidraw
description: Create, edit, render, and iterate on Excalidraw diagrams interactively. Use when the user asks to create diagrams, visualize ideas, sketch flowcharts, edit .excalidraw files, or show what's in an existing drawing. Works like a shared whiteboard.
---

# Excalidraw Interactive Diagramming

Create, view, edit, and iterate on `.excalidraw` diagrams collaboratively.

---

## Trigger Phrases

```
"Create a diagram of..."
"Draw a flowchart for..."
"What's in this .excalidraw file?"
"Add a box labeled X to the diagram"
"Connect A to B"
"Show me the diagram"
"Change the colors to..."
"Move X to the right of Y"
```

---

## Core Workflow

Every interaction follows this loop:

1. **Create or Edit** the `.excalidraw` JSON (generate new or modify existing)
2. **Render** a preview image so the user can see the result
3. **Show** the preview and describe what changed
4. **Iterate** based on user feedback

**Always render after every change.** The user cannot see raw JSON — the rendered preview IS the shared whiteboard.

---

## Rendering Previews

After every create/edit, render with Python and show the image using the Read tool:

```python
python3 << 'PYEOF'
import json, sys
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyArrowPatch
import numpy as np

with open('FILEPATH') as f:
    data = json.load(f)

fig, ax = plt.subplots(1, 1, figsize=(14, 10))

for elem in data['elements']:
    if elem.get('isDeleted'): continue
    x, y = elem['x'], elem['y']
    w, h = elem.get('width', 0), elem.get('height', 0)
    bg = elem.get('backgroundColor', 'none')
    sc = elem.get('strokeColor', '#1e1e1e')
    ss = elem.get('strokeStyle', 'solid')
    lw = elem.get('strokeWidth', 2)
    linestyle = '--' if ss == 'dashed' else '-'
    fc = bg if bg != 'transparent' else 'none'

    if elem['type'] == 'rectangle':
        rect = patches.FancyBboxPatch((x, y), w, h,
            boxstyle='round,pad=0,rounding_size=6',
            linewidth=lw, edgecolor=sc, facecolor=fc, linestyle=linestyle)
        ax.add_patch(rect)

    elif elem['type'] == 'ellipse':
        ell = patches.Ellipse((x + w/2, y + h/2), w, h,
            linewidth=lw, edgecolor=sc, facecolor=fc, linestyle=linestyle)
        ax.add_patch(ell)

    elif elem['type'] == 'diamond':
        cx, cy = x + w/2, y + h/2
        dia = patches.Polygon([(cx, y), (x+w, cy), (cx, y+h), (x, cy)],
            closed=True, linewidth=lw, edgecolor=sc, facecolor=fc, linestyle=linestyle)
        ax.add_patch(dia)

    elif elem['type'] == 'arrow':
        pts = elem['points']
        xs = [x + p[0] for p in pts]
        ys = [y + p[1] for p in pts]
        for i in range(len(xs)-1):
            if i == len(xs) - 2:
                ax.annotate('', xy=(xs[i+1], ys[i+1]), xytext=(xs[i], ys[i]),
                    arrowprops=dict(arrowstyle='->', color=sc, lw=lw))
            else:
                ax.plot([xs[i], xs[i+1]], [ys[i], ys[i+1]], color=sc,
                    linewidth=lw, linestyle=linestyle)

    elif elem['type'] == 'line':
        pts = elem.get('points', [])
        xs = [x + p[0] for p in pts]
        ys = [y + p[1] for p in pts]
        ax.plot(xs, ys, color=sc, linewidth=lw, linestyle=linestyle)

    elif elem['type'] == 'freedraw':
        pts = elem['points']
        xs = [x + p[0] for p in pts]
        ys = [y + p[1] for p in pts]
        ax.plot(xs, ys, color=sc, linewidth=max(1, lw * 0.75))

    elif elem['type'] == 'text':
        fs = elem.get('fontSize', 16)
        ax.text(x + w/2, y + h/2, elem['text'],
            ha='center', va='center', fontsize=fs * 0.7,
            fontfamily='sans-serif', color=sc)

ax.set_aspect('equal')
ax.invert_yaxis()
ax.axis('off')
plt.tight_layout()
plt.savefig('/tmp/excalidraw_preview.png', dpi=150, bbox_inches='tight',
    facecolor='white')
print('Saved to /tmp/excalidraw_preview.png')
PYEOF
```

Then use the Read tool on `/tmp/excalidraw_preview.png` to show the user.

**Important rendering notes:**
- Always `invert_yaxis()` — Excalidraw uses screen coordinates (y increases downward)
- Use `facecolor='white'` for the save to match Excalidraw's white background
- Scale font size by ~0.7 to approximate Excalidraw's rendering
- The preview is approximate — tell the user to open in Excalidraw for the final result

---

## Creating New Diagrams

Use Python to generate the JSON programmatically. This avoids errors from hand-writing large JSON:

```python
python3 << 'PYEOF'
import json, random, time

random.seed(42)
ts = int(time.time() * 1000)
elements = []
idx_counter = [0]

def next_idx():
    i = idx_counter[0]; idx_counter[0] += 1
    chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    return f"a{chars[i]}" if i < len(chars) else f"b{chars[i-len(chars)]}"

def rid(prefix="el"):
    return f"{prefix}-{''.join(random.choice('abcdefghijklmnopqrstuvwxyz0123456789') for _ in range(12))}"

def make_shape(etype, x, y, w, h, stroke="#1e1e1e", bg="transparent",
               stroke_width=2, stroke_style="solid", roughness=1):
    roundness = None
    if etype == "rectangle": roundness = {"type": 3}
    elif etype in ("ellipse", "diamond"): roundness = {"type": 2}
    e = {
        "id": rid(), "type": etype, "x": x, "y": y, "width": w, "height": h,
        "angle": 0, "strokeColor": stroke, "backgroundColor": bg,
        "fillStyle": "solid", "strokeWidth": stroke_width,
        "strokeStyle": stroke_style, "roughness": roughness,
        "opacity": 100, "groupIds": [], "frameId": None,
        "index": next_idx(), "roundness": roundness,
        "seed": random.randint(1, 2**31), "version": 1,
        "versionNonce": random.randint(1, 2**31),
        "isDeleted": False, "boundElements": [],
        "updated": ts, "link": None, "locked": False
    }
    elements.append(e)
    return e

def make_text(content, x, y, w, h, container_id=None, font_size=20,
              stroke="#1e1e1e"):
    e = make_shape("text", x, y, w, h, stroke=stroke)
    e["roundness"] = None
    e.update({
        "text": content, "fontSize": font_size, "fontFamily": 1,
        "textAlign": "center",
        "verticalAlign": "middle" if container_id else "top",
        "containerId": container_id,
        "originalText": content, "autoResize": True
    })
    return e

def make_arrow(x, y, points, start_id=None, end_id=None, stroke="#1e1e1e",
               stroke_width=2, elbowed=False):
    e = make_shape("arrow", x, y, 0, 0, stroke=stroke, stroke_width=stroke_width)
    e["roundness"] = {"type": 2} if not elbowed else None
    e["roughness"] = 0 if elbowed else 1
    xs = [p[0] for p in points]; ys = [p[1] for p in points]
    e["width"] = max(abs(min(xs)), abs(max(xs)))
    e["height"] = max(abs(min(ys)), abs(max(ys)))
    e.update({
        "points": points,
        "startBinding": {"elementId": start_id, "focus": 0, "gap": 1,
                         "fixedPoint": None} if start_id else None,
        "endBinding": {"elementId": end_id, "focus": 0, "gap": 1,
                       "fixedPoint": None} if end_id else None,
        "startArrowhead": None, "endArrowhead": "arrow", "elbowed": elbowed
    })
    return e

def add_label(shape, text, font_size=20, text_height=25):
    """Add bound text label inside a shape."""
    tw = shape["width"] - 10
    tx = shape["x"] + 5
    ty = shape["y"] + (shape["height"] - text_height) / 2
    t = make_text(text, tx, ty, tw, text_height, container_id=shape["id"],
                  font_size=font_size)
    shape["boundElements"].append({"id": t["id"], "type": "text"})
    return t

def connect(source, target, source_edge="bottom", target_edge="top",
            stroke="#1e1e1e", elbowed=False):
    """Create an arrow between two shapes."""
    edges = {
        "top":    lambda s: (s["x"] + s["width"]/2, s["y"]),
        "bottom": lambda s: (s["x"] + s["width"]/2, s["y"] + s["height"]),
        "left":   lambda s: (s["x"], s["y"] + s["height"]/2),
        "right":  lambda s: (s["x"] + s["width"], s["y"] + s["height"]/2),
    }
    sx, sy = edges[source_edge](source)
    tx, ty = edges[target_edge](target)
    dx, dy = tx - sx, ty - sy

    if source_edge == "bottom" and target_edge == "top":
        points = [[0,0], [0, dy]] if abs(dx) < 10 else [[0,0], [dx,0], [dx,dy]]
    elif source_edge == "right" and target_edge == "left":
        points = [[0,0], [dx, 0]] if abs(dy) < 10 else [[0,0], [0,dy], [dx,dy]]
    elif source_edge == "right" and target_edge == "top":
        points = [[0,0], [dx, 0], [dx, dy]]
    else:
        points = [[0,0], [dx, dy]]  # direct line fallback

    a = make_arrow(sx, sy, points, source["id"], target["id"], stroke=stroke,
                   elbowed=elbowed)
    source["boundElements"].append({"id": a["id"], "type": "arrow"})
    target["boundElements"].append({"id": a["id"], "type": "arrow"})
    return a

# --- BUILD DIAGRAM HERE ---
# Example: box = make_shape("rectangle", 100, 100, 200, 80, bg="#a5d8ff", stroke="#1971c2")
#          add_label(box, "My Service")
#          connect(box1, box2)

# --- WRITE FILE ---
doc = {
    "type": "excalidraw", "version": 2,
    "source": "https://excalidraw.com",
    "elements": elements,
    "appState": {"gridSize": 20, "gridStep": 5, "gridModeEnabled": False,
                 "viewBackgroundColor": "#ffffff"},
    "files": {}
}

with open('OUTPUT_PATH', 'w') as f:
    json.dump(doc, f, indent=2)
print(f"Written {len(elements)} elements")
PYEOF
```

### Default Output Location

Save new diagrams to the current working directory unless the user specifies a path.
Use descriptive filenames: `system-architecture.excalidraw`, `user-flow.excalidraw`, etc.

---

## Editing Existing Diagrams

Use Python to load, modify, and rewrite:

```python
import json

with open('FILEPATH') as f:
    data = json.load(f)

# Find element by approximate text match
def find_by_label(data, label):
    for elem in data['elements']:
        if elem['type'] == 'text' and label.lower() in elem.get('text', '').lower():
            container_id = elem.get('containerId')
            if container_id:
                shape = next((e for e in data['elements'] if e['id'] == container_id), None)
                return shape, elem
            return None, elem
    return None, None

# Find element by ID
def find_by_id(data, eid):
    return next((e for e in data['elements'] if e['id'] == eid), None)

# Common edits:
# - Move: change x, y on shape AND its bound text
# - Resize: change width, height, reposition text
# - Recolor: change strokeColor, backgroundColor
# - Relabel: change text and originalText on text element
# - Delete: set isDeleted=True (preserves structure)
# - Add: append new elements to data['elements']

with open('FILEPATH', 'w') as f:
    json.dump(data, f, indent=2)
```

**When moving a shape**, also update:
- Its bound text element positions
- Arrow start/end points that connect to it

---

## Critical Format Rules

### 1. Labels Require TWO Elements

The `label` property does NOT work in raw JSON. Every labeled shape needs:

```json
// Shape with boundElements reference
{"id": "my-box", "type": "rectangle",
 "boundElements": [{"type": "text", "id": "my-box-text"}]}

// Separate text with containerId
{"id": "my-box-text", "type": "text",
 "containerId": "my-box", "text": "My Label",
 "textAlign": "center", "verticalAlign": "middle"}
```

### 2. Text Positioning Inside Shapes

```
text.x = shape.x + 5
text.y = shape.y + (shape.height - text.height) / 2
text.width = shape.width - 10
```

### 3. Arrow Edge Calculations

Arrows start/end at shape edges, not centers:

| Edge   | x                      | y                       |
|--------|------------------------|-------------------------|
| Top    | `shape.x + width/2`   | `shape.y`               |
| Bottom | `shape.x + width/2`   | `shape.y + height`      |
| Left   | `shape.x`             | `shape.y + height/2`    |
| Right  | `shape.x + width`     | `shape.y + height/2`    |

### 4. Elbow Arrows (90-degree corners)

For clean right-angle arrows, set all three:

```json
{"roughness": 0, "roundness": null, "elbowed": true}
```

### 5. Diamond Shapes

Diamonds work for simple diagrams but arrow connections to diamonds can be
unreliable in raw JSON. For complex diagrams with many arrows, prefer styled
rectangles (e.g., dashed border or distinct color) for decision points.

---

## Element Types

| Type        | Use For                                    |
|-------------|--------------------------------------------|
| `rectangle` | Boxes, services, processes, containers     |
| `ellipse`   | Start/end nodes, users, external systems   |
| `diamond`   | Decision points (simple diagrams)          |
| `arrow`     | Connections, data flow, dependencies       |
| `line`      | Separators, grouping boundaries            |
| `text`      | Labels (bound to shapes), annotations      |
| `freedraw`  | Freehand sketches (read-only, from user)   |

---

## Color Palette

| Component      | Background | Stroke    |
|----------------|------------|-----------|
| Frontend/UI    | `#a5d8ff`  | `#1971c2` |
| Backend/API    | `#d0bfff`  | `#7048e8` |
| Database       | `#b2f2bb`  | `#2f9e44` |
| Storage        | `#ffec99`  | `#f08c00` |
| AI/ML          | `#e599f7`  | `#9c36b5` |
| External       | `#ffc9c9`  | `#e03131` |
| Orchestration  | `#ffa8a8`  | `#c92a2a` |
| Cache          | `#ffe8cc`  | `#fd7e14` |
| Users          | `#e7f5ff`  | `#1971c2` |
| Neutral/Default| transparent| `#1e1e1e` |

Use colored fills for architecture/system diagrams. Use transparent backgrounds
with black strokes for simple flowcharts and sketches.

---

## Layout Quick Reference

### Vertical Flow (flowcharts, pipelines)

```
Rows: y = 50, 170, 290, 410, 530, ...  (gap: 120)
Center column: x = 300
Side branches: x = 580
Shape size: 200 x 70
```

### Horizontal Flow (timelines, processes)

```
Columns: x = 100, 350, 600, 850, ...  (gap: 250)
Center row: y = 200
Shape size: 200 x 80
```

### Grid (architecture diagrams)

```
Columns: x = 100, 320, 540, 760
Rows: y = 80, 230, 380, 530
Shape size: 180 x 80
```

---

## Reading/Interpreting Diagrams

When the user asks what's in an `.excalidraw` file:

1. **Render it** — always show the visual first
2. **Describe** the elements: shapes, labels, connections, layout
3. **Identify** handwritten text (freedraw) as best you can from the rendering
4. Freedraw paths are freehand pen strokes — they cannot be reliably interpreted
   from raw coordinates alone, only from the rendered image

---

## Detailed References

See `references/json-format.md` for complete element property reference.
See `references/arrows.md` for arrow routing patterns and worked examples.
