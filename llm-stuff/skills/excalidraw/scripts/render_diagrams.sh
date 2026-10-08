#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: render_diagrams.sh --input FILE.excalidraw [options]

Options:
  --output FILE.png      Output path; defaults to the input basename plus .png
  --font-map VALUE       URL-encoded Excalidraw fontMap value
                         (example: Helvetica:CMU+Sans+Serif)
  --renderer-url URL     Chromium renderer base URL
                         (default: EXCALIDRAW_RENDERER_URL or localhost:3004)
  -h, --help             Show this help
USAGE
}

input=""
output=""
font_map=""
renderer_url="${EXCALIDRAW_RENDERER_URL:-http://localhost:3004}"

while (($#)); do
  case "$1" in
    --input)
      [[ $# -ge 2 ]] || { echo "Error: --input requires a value" >&2; exit 2; }
      input="$2"
      shift 2
      ;;
    --output)
      [[ $# -ge 2 ]] || { echo "Error: --output requires a value" >&2; exit 2; }
      output="$2"
      shift 2
      ;;
    --font-map)
      [[ $# -ge 2 ]] || { echo "Error: --font-map requires a value" >&2; exit 2; }
      font_map="$2"
      shift 2
      ;;
    --renderer-url)
      [[ $# -ge 2 ]] || { echo "Error: --renderer-url requires a value" >&2; exit 2; }
      renderer_url="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Error: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

[[ -n "$input" ]] || { echo "Error: --input is required" >&2; usage >&2; exit 2; }
[[ -f "$input" ]] || { echo "Error: input does not exist: $input" >&2; exit 1; }

if [[ -z "$output" ]]; then
  if [[ "$input" == *.excalidraw ]]; then
    output="${input%.excalidraw}.png"
  else
    output="${input}.png"
  fi
fi

if [[ -n "$font_map" && ! "$font_map" =~ ^[A-Za-z0-9._:+,-]+$ ]]; then
  echo "Error: --font-map must already be URL encoded" >&2
  exit 2
fi

command -v excalidraw-tools >/dev/null || {
  echo "Error: excalidraw-tools is not installed or not on PATH" >&2
  exit 1
}
command -v curl >/dev/null || {
  echo "Error: curl is required for Chromium renderer health checks" >&2
  exit 1
}

excalidraw-tools validate "$input"

renderer_url="${renderer_url%/}"
if curl -fsS --max-time 2 "${renderer_url}/healthz" >/dev/null 2>&1; then
  render_url="${renderer_url}/render/png"
  if [[ -n "$font_map" ]]; then
    render_url="${render_url}?fontMap=${font_map}"
  fi
  curl -fsS -X POST "$render_url" \
    -H "Content-Type: text/plain" \
    --data-binary "@${input}" \
    -o "$output"
else
  if [[ -n "$font_map" ]]; then
    echo "Error: Chromium renderer is unavailable; fallback preview cannot apply --font-map" >&2
    exit 1
  fi
  if [[ -z "${MPLCONFIGDIR:-}" ]]; then
    export MPLCONFIGDIR="${TMPDIR:-/tmp}/excalidraw-tools-matplotlib"
    mkdir -p "$MPLCONFIGDIR"
  fi
  if ! excalidraw-tools preview "$input" --output "$output"; then
    echo "Error: fallback preview failed. Install its optional dependencies separately; this script will not modify shared tools." >&2
    exit 1
  fi
fi

[[ -s "$output" ]] || { echo "Error: renderer did not create a nonempty file: $output" >&2; exit 1; }
printf 'Rendered %s\n' "$output"
