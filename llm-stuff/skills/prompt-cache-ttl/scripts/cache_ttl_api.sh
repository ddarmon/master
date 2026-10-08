#!/usr/bin/env bash
# Controlled prompt-cache TTL battery against the Messages API (raw curl).
# Determines from BILLED usage whether cache_control settings yield 5m or 1h TTL,
# via two signatures: (1) write-price bucket, (2) survival across a >5min/<60min gap.
# Runs both a default-ephemeral (expect 5m) and an explicit ttl:1h test in one pass.
#
# Auth is auto-detected (override with AUTH_MODE): ANTHROPIC_API_KEY -> apikey;
# else ANTHROPIC_OAUTH_TOKEN -> oauth; else `ant auth print-credentials --access-token` -> oauth.
#
# Key knobs (env): MODEL (default claude-sonnet-5), ROUTE_LABEL, GAP_SECONDS (default 420),
#                  PREFIX_LINES (default 800 ~13k tokens), ANTHROPIC_BASE_URL, OUTDIR.
set -euo pipefail

BASE="${ANTHROPIC_BASE_URL:-https://api.anthropic.com}"
MODEL="${MODEL:-claude-sonnet-5}"
ROUTE_LABEL="${ROUTE_LABEL:-unlabeled}"
GAP_SECONDS="${GAP_SECONDS:-420}"
PREFIX_LINES="${PREFIX_LINES:-800}"
API_VERSION="${API_VERSION:-2023-06-01}"
OUTDIR="${OUTDIR:-./cache-ttl-results-$ROUTE_LABEL}"
mkdir -p "$OUTDIR"
command -v jq >/dev/null || { echo "need jq" >&2; exit 2; }
command -v curl >/dev/null || { echo "need curl" >&2; exit 2; }

# ---- Auth auto-detect ----
AUTH_MODE="${AUTH_MODE:-auto}"
auth_headers=()
if [ "$AUTH_MODE" = auto ]; then
  if [ -n "${ANTHROPIC_API_KEY:-}" ]; then AUTH_MODE=apikey
  elif [ -n "${ANTHROPIC_OAUTH_TOKEN:-}" ]; then AUTH_MODE=oauth
  elif command -v ant >/dev/null 2>&1; then
    ANTHROPIC_OAUTH_TOKEN="$(ant auth print-credentials --access-token 2>/dev/null || true)"
    [ -n "$ANTHROPIC_OAUTH_TOKEN" ] && AUTH_MODE=oauth
  fi
fi
case "$AUTH_MODE" in
  apikey) : "${ANTHROPIC_API_KEY:?no credential found}"
          auth_headers=(-H "x-api-key: $ANTHROPIC_API_KEY") ;;
  oauth)  : "${ANTHROPIC_OAUTH_TOKEN:?no credential found}"
          auth_headers=(-H "Authorization: Bearer $ANTHROPIC_OAUTH_TOKEN" -H "anthropic-beta: oauth-2025-04-20") ;;
  *) echo "No credential: set ANTHROPIC_API_KEY, or ANTHROPIC_OAUTH_TOKEN, or run 'ant auth login'." >&2; exit 2 ;;
esac

# ---- Two byte-distinct deterministic prefixes (distinct 1st line => independent cache keys) ----
mk_prefix () {
  printf 'EXPERIMENT VARIANT: %s (immutable; identical bytes on write and probe)\n' "$1"
  local i; for i in $(seq 1 "$PREFIX_LINES"); do
    printf 'Reference line %05d: prompt-cache TTL calibration corpus, stable deterministic filler token block.\n' "$i"
  done
}
mk_prefix "D-5M" > "$OUTDIR/prefix_D.txt"
mk_prefix "H-1H" > "$OUTDIR/prefix_H.txt"

# ---- One call -> one TSV row: tag ttl input cc_total cc_5m cc_1h read reqid ----
call () {
  local tag="$1" pfile="$2" ttl="$3" cc
  if [ "$ttl" = "1h" ]; then cc='{"type":"ephemeral","ttl":"1h"}'; else cc='{"type":"ephemeral"}'; fi
  local body; body="$(jq -n --rawfile p "$pfile" --arg m "$MODEL" --argjson cc "$cc" '
    {model:$m, max_tokens:16, thinking:{type:"disabled"},
     system:[{type:"text", text:$p, cache_control:$cc}],
     messages:[{role:"user", content:"Reply with the single character: x"}]}')"
  local hdr="$OUTDIR/${tag}.headers" resp="$OUTDIR/${tag}.json"
  curl -sS -D "$hdr" "$BASE/v1/messages" \
    -H "content-type: application/json" -H "anthropic-version: $API_VERSION" \
    "${auth_headers[@]}" -d "$body" > "$resp"
  if jq -e '.error' "$resp" >/dev/null 2>&1; then echo "API ERROR on $tag:" >&2; jq -c '.error' "$resp" >&2; exit 1; fi
  local rid; rid="$(grep -i '^request-id:' "$hdr" | tr -d '\r' | awk '{print $2}')"
  jq -r --arg t "$tag" --arg ttl "$ttl" --arg rid "${rid:-NA}" '.usage as $u |
    [$t,$ttl,($u.input_tokens//0),($u.cache_creation_input_tokens//0),
     ($u.cache_creation.ephemeral_5m_input_tokens//0),($u.cache_creation.ephemeral_1h_input_tokens//0),
     ($u.cache_read_input_tokens//0),$rid] | @tsv' "$resp"
}
f () { echo "$1" | cut -f"$2"; }
verdict () { awk -v p="$1" -v r="$2" 'BEGIN{print (r+0>=0.5*(p+0))?"HIT":"MISS"}'; }

T0="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "route=$ROUTE_LABEL base=$BASE model=$MODEL auth=$AUTH_MODE gap=${GAP_SECONDS}s start=$T0"
printf 'tag\tttl\tinput\tcc_total\tcc_5m\tcc_1h\tread\treqid\n' | tee "$OUTDIR/results.tsv"

wD="$(call write_D "$OUTDIR/prefix_D.txt" none)"; echo "$wD" | tee -a "$OUTDIR/results.tsv"
wH="$(call write_H "$OUTDIR/prefix_H.txt" 1h)";   echo "$wH" | tee -a "$OUTDIR/results.tsv"
if [ "$(f "$wD" 4)" -lt 1000 ] || [ "$(f "$wH" 4)" -lt 1000 ]; then
  echo "WARNING: cache_creation ~0 on a write -> prefix below Sonnet 5's min cacheable size (raise PREFIX_LINES) OR this route strips cache_control." >&2
fi

echo ">>> sleeping ${GAP_SECONDS}s; do not touch these prefixes (a read resets the TTL)."
sleep "$GAP_SECONDS"

pD="$(call probe_D "$OUTDIR/prefix_D.txt" none)"; echo "$pD" | tee -a "$OUTDIR/results.tsv"
pH="$(call probe_H "$OUTDIR/prefix_H.txt" 1h)";   echo "$pH" | tee -a "$OUTDIR/results.tsv"
T1="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

vD="$(verdict "$(f "$wD" 4)" "$(f "$pD" 7)")"; vH="$(verdict "$(f "$wH" 4)" "$(f "$pH" 7)")"
echo
echo "============ VERDICT  route=$ROUTE_LABEL  gap=${GAP_SECONDS}s ============"
echo "window (UTC): $T0 .. $T1   (reconcile against Console Usage & Cost)"
echo "default ephemeral: write 5m=$(f "$wD" 5) 1h=$(f "$wD" 6) | probe=$vD  => $([ "$vD" = MISS ] && echo '5-minute TTL' || echo 'NOT 5m (anomaly)')"
echo "explicit ttl=1h:   write 5m=$(f "$wH" 5) 1h=$(f "$wH" 6) | probe=$vH  => $([ "$vH" = HIT ]  && echo '1-hour TTL'  || echo 'ttl:1h NOT honored (anomaly)')"
echo "per-call usage + request-ids: $OUTDIR/results.tsv"
