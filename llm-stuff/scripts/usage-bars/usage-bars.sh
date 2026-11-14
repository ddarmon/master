#!/bin/bash
#
# usage-bars.sh - Render rate-limit usage windows as color bars with a
# sustainable-pace marker.
#
# This is only the rendering layer. It knows nothing about any provider:
# a separate fetcher turns a provider's usage response into rows on stdin,
# and this script draws them.
#
# Input (stdin): one tab-separated row per usage window,
#
#   label <TAB> used_pct <TAB> window_seconds <TAB> reset_epoch
#
#   label           display name, e.g. "Session (5h)"
#   used_pct        percent of the window's allowance consumed (0-100, may
#                   be fractional)
#   window_seconds  total length of the window; for calendar-based windows
#                   (e.g. a monthly quota) the fetcher computes it as
#                   reset_epoch - previous_reset_epoch
#   reset_epoch     Unix time at which the window resets
#
# Blank lines and lines starting with '#' are ignored. Rows with
# window_seconds <= 0 are skipped (use this for "unlimited" quotas).
#
# Pace: if usage accrued at a constant rate across the window, it would
# reach 100% exactly at reset. The magenta marker sits at the fraction of
# the window that has elapsed, (window - (reset - now)) / window, so a bar
# ending left of the marker means you are under the sustainable rate.
#
# Usage:
#   some-fetcher | usage-bars.sh [-t TITLE] [-s SUBTITLE]
#
# Environment:
#   NO_COLOR        if set (to anything), disables ANSI colors
#   USAGE_BARS_NOW  override "now" (Unix time); useful for demos and tests
#
# Requirements: bash 3.2+, awk, and either BSD date (macOS) or GNU date.

TITLE="Usage"
SUBTITLE=""
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--title)    TITLE=$2; shift 2 ;;
    -s|--subtitle) SUBTITLE=$2; shift 2 ;;
    -h|--help)     sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "usage-bars: unknown argument: $1" >&2; exit 2 ;;
  esac
done

if [[ -n "${NO_COLOR:-}" ]]; then
  R='' DIM='' BOLD='' GREEN='' YELLOW='' RED='' CYAN='' MAGENTA=''
else
  R=$'\033[0m' DIM=$'\033[2m' BOLD=$'\033[1m'
  GREEN=$'\033[32m' YELLOW=$'\033[33m' RED=$'\033[31m'
  CYAN=$'\033[36m' MAGENTA=$'\033[35m'
fi

BAR_WIDTH=40
NOW=${USAGE_BARS_NOW:-$(date +%s)}

# Format an epoch with a strftime pattern, on BSD or GNU date.
fmt_epoch() {
  date -r "$1" "+$2" 2>/dev/null || date -d "@$1" "+$2"
}

# Reset time rounded to the nearest 5 minutes (the APIs this was written
# for return sub-second jitter that makes the minute flicker between runs).
format_reset() {
  local ts=$1
  if [[ -z "$ts" || "$ts" -le 0 ]]; then echo "N/A"; return; fi
  ts=$(( ((ts + 150) / 300) * 300 ))
  local when
  if [[ "$(fmt_epoch "$ts" %Y-%m-%d)" == "$(fmt_epoch "$NOW" %Y-%m-%d)" ]]; then
    when=$(fmt_epoch "$ts" '%-I:%M%p')
  else
    when=$(fmt_epoch "$ts" '%b %-d at %-I:%M%p')
  fi
  when=${when/AM/am}; when=${when/PM/pm}
  echo "$when ($(fmt_epoch "$ts" %Z))"
}

# Print: pct_clamped pace_pct filled_cells pace_cell rounded_diff
compute() {
  awk -v pct="$1" -v win="$2" -v reset="$3" -v now="$NOW" -v w="$BAR_WIDTH" '
    function clamp(x, lo, hi) { return x < lo ? lo : (x > hi ? hi : x) }
    BEGIN {
      pct = clamp(pct + 0, 0, 100)
      remaining = clamp(reset - now, 0, win)
      pace = (win - remaining) * 100 / win
      filled = int(pct * w / 100)
      cell = int(pace * w / 100); if (cell >= w) cell = w - 1
      d = pct - pace
      printf "%s %.1f %d %d %d\n", pct, pace, filled, cell, (d < 0 ? -int(-d + 0.5) : int(d + 0.5))
    }'
}

color_for_pct() {
  awk -v p="$1" 'BEGIN { exit !(p < 50) }' && { printf '%s' "$GREEN"; return; }
  awk -v p="$1" 'BEGIN { exit !(p < 80) }' && { printf '%s' "$YELLOW"; return; }
  printf '%s' "$RED"
}

draw_bar() {
  local filled=$1 cell=$2 color=$3 out="" i
  for ((i = 0; i < BAR_WIDTH; i++)); do
    if ((i == cell)); then out+="${MAGENTA}│${R}"
    elif ((i < filled)); then out+="${color}━${R}"
    else out+="${DIM}─${R}"
    fi
  done
  printf '%s' "$out"
}

pace_status() {
  local diff=$1
  if ((diff > 5)); then printf '%s+%d%% ahead%s' "$RED" "$diff" "$R"
  elif ((diff < -5)); then printf '%s%d%% behind%s' "$GREEN" "$diff" "$R"
  else printf '%son pace%s' "$DIM" "$R"
  fi
}

RULE="─────────────────────────────────────────────────────────"
echo ""
printf '%s%s%s\n' "$DIM" "$RULE" "$R"
printf '  %s%s%s%s  %s│%s marks sustainable pace\n' "$CYAN" "$BOLD" "$TITLE" "$R" "$DIM" "$R"
[[ -n "$SUBTITLE" ]] && printf '  %s%s%s\n' "$DIM" "$SUBTITLE" "$R"
printf '%s%s%s\n' "$DIM" "$RULE" "$R"
echo ""

while IFS=$'\t' read -r label used win reset || [[ -n "$label" ]]; do
  [[ -z "$label" || "$label" == \#* ]] && continue
  if ! [[ "$win" =~ ^[0-9]+$ && "$reset" =~ ^[0-9]+$ ]]; then
    echo "usage-bars: skipping malformed row: $label" >&2
    continue
  fi
  ((win > 0)) || continue

  read -r pct pace filled cell diff < <(compute "$used" "$win" "$reset")
  color=$(color_for_pct "$pct")

  printf '%s%s%s\n' "$BOLD" "$label" "$R"
  printf '  %s %s%3.0f%%%s  ' "$(draw_bar "$filled" "$cell" "$color")" "$color" "$pct" "$R"
  pace_status "$diff"
  printf '  %sresets %s%s\n\n' "$DIM" "$(format_reset "$reset")" "$R"
done
