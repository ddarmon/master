# usage-bars

A terminal renderer for rate-limit and quota windows. It draws each window as a color bar with a marker showing where usage would be if you consumed the allowance at a constant rate.

```
─────────────────────────────────────────────────────────
  Claude Code Usage  │ marks sustainable pace
─────────────────────────────────────────────────────────

Session (5h)
  ━━━━━━━━━━━───────│─────────────────────  29%  -17% behind  resets 1:10pm (EST)

Week (all models)
  ━━━━━━───────────────│──────────────────  17%  -37% behind  resets Nov 17 at 3:00pm (EST)
```

The bar's color comes from the absolute level: green below 50%, yellow below 80%, and red otherwise. The status text comes from the gap between usage and pace. It says "ahead" in red when you are more than 5 points over pace, "behind" in green when you are more than 5 points under, and "on pace" otherwise.

## Design

The renderer is separate from data collection. `usage-bars.sh` knows nothing about any provider. Each provider gets its own small fetcher that authenticates, calls whatever reports usage, and prints normalized rows. The renderer reads those rows on stdin:

```
label <TAB> used_pct <TAB> window_seconds <TAB> reset_epoch
```

Every provider's usage report reduces to this tuple. A rolling window (five hours, seven days) has a fixed `window_seconds`. A calendar quota, such as one that resets on the first of each month, has a length that varies, so the fetcher computes it as `reset_epoch - previous_reset_epoch`. The renderer never needs to know which kind it has. Rows with `window_seconds` of 0 are skipped, which handles unlimited quotas.

Pace is the fraction of the window that has elapsed:

$$\text{pace} = \frac{W - (t_{\text{reset}} - t_{\text{now}})}{W}$$

A bar that ends left of the marker means you are under the sustainable rate.

Reset times are rounded to the nearest five minutes. Some usage APIs return reset timestamps with sub-second jitter, which otherwise makes the displayed minute change between runs.

## Usage

```bash
my-fetcher | ./usage-bars.sh -t "Provider Usage" -s "Plan: pro"
./demo.sh    # made-up data, including a calendar-month window
```

A fetcher is usually a few lines of `jq`. Given a response like `{"windows": [{"name": "5h", "used": 0.29, "length_s": 18000, "resets_at": "2026-11-14T18:10:00Z"}]}`:

```bash
curl -s ... | jq -r '.windows[] |
  [.name, (.used * 100), .length_s, (.resets_at | fromdate)] | @tsv' \
  | ./usage-bars.sh -t "Provider Usage"
```

Set `NO_COLOR` to disable ANSI colors. Set `USAGE_BARS_NOW` to a Unix time to fix "now" for tests.

Requirements: bash 3.2 or later (the macOS default works), awk, and BSD or GNU `date`.
