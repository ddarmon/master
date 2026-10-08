# prompt-cache-ttl — detailed protocol & interpretation

Read this when you need the full reasoning, the billing-reconciliation steps, the cross-route
comparison, or the edge-case handling. SKILL.md has the short version.

## The two signatures (both are billing-based)

1. **Write-price bucket.** Every cache write reports the TTL bucket it billed to:
   `usage.cache_creation.ephemeral_5m_input_tokens` vs `ephemeral_1h_input_tokens`. That bucket *is*
   the price multiplier — 5m writes bill **1.25×** base input, 1h writes **2×**. One write declares the TTL.
2. **Survival across a gap.** An entry is readable only within its TTL of the **last access**. After a gap
   `G` with `5 min < G < 60 min` (use 7 min), re-send the identical prefix: a **cache read (~0.1×)** means
   it survived → 1h; a **re-write** means it expired → 5m.

They should agree. If they don't, that disagreement is the finding — report it.

## Mode A — self-introspection (native to Claude Code, no key)

Measures the TTL that **this Claude Code session's route actually delivers** for the caching config the
Claude Code client requests. Because the running Claude Code instance is authenticated to whatever route
it's on, this directly answers "is my direct/enterprise Claude Code getting 5m or 1h caching."

Mechanics that matter:
- The current turn's `usage` is **not** written to the transcript until the turn finishes. So after the
  self-pause you cannot read the wake turn's usage within that same turn — trigger one short (~6 s)
  background re-invoke to flush it, then read.
- Reads refresh the TTL, so the pause must contain **no** API activity: launch the pause, end the turn,
  do nothing until re-invoked.
- Locate the transcript by `$CLAUDE_CODE_SESSION_ID` (see `scripts/read_session_cache.sh`).

Limitation: it only observes the TTL the Claude Code client requests (in practice `ttl:"1h"`). It cannot
exercise the 5-minute path. To test both 5m and 1h as route capabilities, use Mode B.

## Mode B — controlled raw-API battery (`scripts/cache_ttl_api.sh`)

Independently tests default-ephemeral (expect 5m) and explicit `ttl:"1h"` in one pass: writes both,
records buckets, waits 7 min, probes both. Needs a credential (auto-detected: `ANTHROPIC_API_KEY`, else
`ANTHROPIC_OAUTH_TOKEN`, else `ant auth print-credentials --access-token`).

## Interpretation (decision rules)

`W` = write's `cache_creation_input_tokens`; `R` = probe's `cache_read_input_tokens`. HIT if `R ≥ 0.5·W`.

| Test | Expect write bucket | Expect probe @7 min | Conclusion when as-expected |
|---|---|---|---|
| default ephemeral | 5m | MISS | 5-minute TTL |
| `ttl:"1h"` | 1h | HIT | 1-hour TTL |

Anomalies to flag: default HITs at 7 min (behaving longer than 5m); `ttl:"1h"` MISSes at 7 min (not
honored); any write with `cache_creation ≈ 0` (caching not engaging — prefix below the minimum cacheable
size, or a gateway stripped `cache_control`); bucket disagreeing with survival.

### Optional upper-bound bracket (adds ~65 min)
7-min HIT proves `ttl:"1h"` is > 5 min. To prove it's ~1 h and not longer, write a fresh `ttl:"1h"`
prefix, wait **65 min untouched**, probe: expect MISS. Together brackets TTL to (7 min, 65 min] ≈ 1 h.

## Cross-route comparison (direct vs Claude Enterprise)

Run the chosen mode **once per route** and diff.
- **Mode A:** run the same skill inside two Claude Code instances authenticated to the two routes
  (e.g. a direct API key vs an enterprise login), compare the buckets and HIT/MISS.
- **Mode B:** run `cache_ttl_api.sh` with each route's credential (and `ANTHROPIC_BASE_URL` if the
  enterprise route uses a gateway); compare `results.tsv`.

Route caveats:
- A gateway that strips/rewrites `cache_control` shows up as `cache_creation ≈ 0` on writes.
- **Amazon Bedrock / Google Vertex** enterprise routes: model IDs/clients differ (`anthropic.`-prefixed on
  Bedrock) and *automatic* prompt caching is unsupported, but *explicit* `cache_control` (5m/1h) is — keep
  the same request body and adapt auth via that provider's SDK; the raw-curl script targets first-party.
- Record whether the org is **ZDR** — if caching behaves differently under zero-retention, that's a finding.

## Reconcile against actual charges

1. Note the run's UTC window and the `request-id`s (Mode B writes them to `results.tsv`; Mode A: the
   transcript timestamps).
2. Console → **Usage** and **Cost** for that window, filtered to the model: confirm the itemized lines
   **cache write (5m)**, **cache write (1h)**, **cache read**, **input** match the token counts and reflect
   the 1.25× / 2× / 0.1× multipliers. On a HIT the probe's read tokens bill ~10–20× cheaper than a cold
   rewrite — that dollar gap is the empirical fingerprint of survival.
3. If the org exposes the Admin usage/cost report, pull the same window programmatically for the same split.

## Model-specific notes (Claude Sonnet 5)

- Keep every request-shaping field constant except the deliberately varied `ttl`; keep `thinking` constant
  (toggling it invalidates the system-level cache). The script uses `thinking:{type:"disabled"}`, `max_tokens:16`.
- Do **not** send `temperature`/`top_p`/`top_k` (Sonnet 5 → 400) or `budget_tokens` (removed).
- Sonnet 5's minimum cacheable prefix isn't published in the reference (Opus-tier 4096, Fable 5/Sonnet 4.6
  2048 tokens). The script sizes ~13k tokens and warns if caching didn't engage. Its tokenizer differs
  (~30% vs 4.6) — trust `usage` counts, not character estimates.

## Reporting template

```
model: ____   date(UTC): ____   route: direct|enterprise
base_url: ____  auth: apikey|oauth  org/workspace: ____  retention: ZDR|30d|____
default ephemeral: write 5m=__ 1h=__  probe@7min: HIT|MISS
ttl:"1h":          write 5m=__ 1h=__  probe@7min: HIT|MISS
caching engaged (cache_creation>0): yes/no    billing reconciled: yes/no
verdict: default => __-TTL ; ttl:"1h" => __-TTL
cross-route identical? yes/no  (differences: ____)
```
