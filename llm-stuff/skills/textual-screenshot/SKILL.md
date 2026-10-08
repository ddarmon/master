---
name: textual-screenshot
description: Generate SVG and PNG screenshots of Textual TUI apps. Use when users ask to take screenshots, capture the TUI, generate documentation images, or preview how the app looks in different states.
---

# Textual Screenshot Skill

Generate headless screenshots of Textual apps using the built-in
`textual._doc.take_svg_screenshot()` function. This runs the app without
a terminal, simulates interactions, and exports an SVG.

## Core Workflow

1.  Write a Python script that sets up the app, injects data, simulates
    key presses, and calls `take_svg_screenshot()`
2.  Run the script with `uv run python3 script.py`
3.  Convert SVG to PNG via headless Chrome (not rsvg-convert or Inkscape
    -- they break Fira Code font metrics)
4.  Show the PNG to the user and iterate on feedback
5.  When done, tell the user the paths to both the `.svg` and `.png`

## Taking a Screenshot

Use `textual._doc.take_svg_screenshot()` which runs the app headlessly:

```python
from textual._doc import take_svg_screenshot
from textual.pilot import Pilot

app = YourApp()

# Disable real I/O (discovery, index loading, etc.)
app._load_from_index = lambda: False
app._discover_all = lambda: None

async def setup(pilot: Pilot):
    app = pilot.app
    # Inject data, populate widgets, etc.
    # ...
    await pilot.pause()
    # Optionally press keys to reach a specific state
    await pilot.press("question_mark")  # e.g. open help
    await pilot.pause()

svg = take_svg_screenshot(
    app=app,
    terminal_size=(120, 35),   # (columns, rows)
    title="App Title",
    run_before=setup,          # omit if no setup needed
)

with open("/tmp/screenshot.svg", "w") as f:
    f.write(svg)
```

### Key Parameters

| Parameter       | Type                    | Description                                                |
| --------------- | ----------------------- | ---------------------------------------------------------- |
| `app`           | `App`                   | A non-running Textual App instance                         |
| `terminal_size` | `(int, int)`            | Terminal dimensions as (columns, rows)                     |
| `title`         | `str`                   | Title shown in the SVG terminal chrome                     |
| `press`         | `Iterable[str]`         | Key names to press before capture. `"_"` = short pause     |
| `run_before`    | `async (Pilot) -> None` | Setup callback for data injection and complex interactions |

### Simulating Interactions in `run_before`

The `Pilot` object supports:

-   `await pilot.press("key1", "key2", ...)` -- simulate key presses
-   `await pilot.hover("#widget-id")` -- hover over a widget
-   `await pilot.pause()` -- wait for the app to settle
-   `await pilot.wait_for_scheduled_animations()` -- wait for animations
-   `pilot.app` -- access the running app instance

Common Textual key names: `"tab"`, `"enter"`, `"escape"`, `"up"`,
`"down"`, `"left"`, `"right"`, `"slash"`, `"question_mark"`, `"F"`,
`"T"`, `"t"`, `"b"`, `"f"`, `"s"`, `"o"`.

## SVG to PNG Conversion

**Always use headless Chrome.** Other converters (rsvg-convert,
Inkscape, ImageMagick) fail because they cannot load the Fira Code web
font referenced in Textual's SVG output, causing broken text alignment.

```bash
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --headless --disable-gpu \
    --screenshot=/tmp/screenshot.png \
    --window-size=1482,904 \
    "file:///tmp/screenshot.svg"
```

### Window Size

The `--window-size` should match or exceed the SVG's `viewBox`
dimensions. The viewBox is in the SVG's root element:

```xml
<svg viewBox="0 0 1482 904.0" ...>
```

Use those values as `--window-size=1482,904`. For a different terminal
size the viewBox will change -- read it from the generated SVG.

To calculate the expected viewBox from terminal_size:

-   Width: `columns * 12.2 + 18` (approx)
-   Height: `rows * 24.4 + 51` (approx)

### Using SVGs Directly

SVGs render perfectly in browsers and on GitHub. If the screenshots are
for a README or web documentation, you may not need PNG conversion at
all. GitHub renders inline SVGs natively:

```markdown
![sesh screenshot](docs/screenshots/main-view.svg)
```

## sesh-Specific Notes

For the sesh project:

### Imports

```python
from sesh.app import SeshApp, HelpScreen
from sesh.models import Project, Provider, SessionMeta, Message
from datetime import datetime, timezone
```

### Disabling I/O

```python
app = SeshApp()
app._load_from_index = lambda: False
app._discover_all = lambda: None
```

### Injecting Sessions

```python
async def setup(pilot: Pilot):
    app = pilot.app
    s1 = SessionMeta(
        id="s1",
        provider=Provider.CLAUDE,
        project_path="/home/user/project",
        summary="Session summary text",
        timestamp=datetime(2025, 6, 15, tzinfo=timezone.utc),
        message_count=42,
        model="opus-4",
    )
    app.projects = {
        "/home/user/project": Project(
            path="/home/user/project",
            display_name="project",
            providers={Provider.CLAUDE},
            session_count=1,
        ),
    }
    app.sessions = {"/home/user/project": [s1]}
    app._populate_tree()
    await pilot.pause()
```

### SessionMeta Fields

| Field           | Type       | Required | Description                                               |                                 |
| --------------- | ---------- | -------- | --------------------------------------------------------- | ------------------------------- |
| `id`            | `str`      | yes      | Session identifier                                        |                                 |
| `provider`      | `Provider` | yes      | `Provider.CLAUDE`, `Provider.CODEX`, or `Provider.CURSOR` |                                 |
| `project_path`  | `str`      | yes      | Path displayed in the tree                                |                                 |
| `summary`       | `str`      | yes      | Session summary shown in tree nodes                       |                                 |
| `timestamp`     | `datetime` | yes      | Session date (must be tz-aware)                           |                                 |
| `message_count` | `int`      | no       | Message count shown as `(N)`                              |                                 |
| `model`         | `str \     | None`    | no                                                        | Model name                      |
| `source_path`   | `str \     | None`    | no                                                        | File path for on-demand loading |

### Useful States to Screenshot

-   **Default view**: just inject sessions and `_populate_tree()`
-   **Help screen**: `await pilot.press("question_mark")`
-   **Fullscreen mode**: `await pilot.press("F")`
-   **Tool visibility on**: `await pilot.press("t")`
-   **With search focused**: `await pilot.press("slash")`
-   **Provider filtered**: `await pilot.press("f")` (cycles through
    filters)
