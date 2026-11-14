#!/bin/bash
#
# demo.sh - Render usage-bars.sh with made-up data, at a fixed "now".
#
# Shows the three kinds of window a fetcher typically produces: a short
# rolling window, a weekly window, and a calendar-month quota whose length
# the fetcher derives from the previous and next reset dates.

DIR=$(cd "$(dirname "$0")" && pwd)
NOW=$(date +%s)

# Fixed-length rolling windows: reset times relative to now.
FIVE_H=$((5 * 3600))
WEEK=$((7 * 86400))

# Calendar-month quota: window is previous reset -> next reset.
MONTH_START=$((NOW - 13 * 86400))
MONTH_END=$((NOW + 17 * 86400))

{
  printf '%s\t%s\t%s\t%s\n' "Session (5h)"      29 "$FIVE_H" $((NOW + 2 * 3600))
  printf '%s\t%s\t%s\t%s\n' "Week (all models)" 17 "$WEEK"   $((NOW + 3 * 86400))
  printf '%s\t%s\t%s\t%s\n' "Week (one model)"  71 "$WEEK"   $((NOW + 3 * 86400))
  printf '%s\t%s\t%s\t%s\n' "Monthly quota"     44 $((MONTH_END - MONTH_START)) "$MONTH_END"
  printf '%s\t%s\t%s\t%s\n' "Unlimited (hidden)" 0 0 0
} | USAGE_BARS_NOW=$NOW "$DIR/usage-bars.sh" -t "Example Usage" -s "Plan: demo"
