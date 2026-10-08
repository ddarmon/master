#!/usr/bin/env bash
# Print recent prompt-cache usage rows from the CURRENT Claude Code session transcript.
# Used by the self-introspection mode of the prompt-cache-ttl skill.
#
# Output (TSV): timestamp  cache_creation_total  cache_read  ephemeral_5m  ephemeral_1h
#
# Args:
#   $1  N            number of most-recent assistant-usage rows to print (default 8)
#   $2  SINCE_ISO    optional; only rows with timestamp > this ISO-8601 string
set -euo pipefail
N="${1:-8}"
SINCE="${2:-}"

tx="$(find "$HOME/.claude/projects" -name "${CLAUDE_CODE_SESSION_ID:-__none__}.jsonl" 2>/dev/null | head -1)"
if [ -z "$tx" ]; then
  # Fallback: most recently modified transcript (best-effort if the env var is unset)
  tx="$(ls -t "$HOME"/.claude/projects/*/*.jsonl 2>/dev/null | head -1)"
fi
[ -n "$tx" ] || { echo "ERROR: could not locate a session transcript under ~/.claude/projects" >&2; exit 1; }
echo "# transcript: $tx" >&2

filter='select(.type=="assistant" and .message.usage!=null)'
[ -n "$SINCE" ] && filter="$filter | select(.timestamp > \"$SINCE\")"

jq -rc "$filter | [
    .timestamp,
    (.message.usage.cache_creation_input_tokens//0),
    (.message.usage.cache_read_input_tokens//0),
    (.message.usage.cache_creation.ephemeral_5m_input_tokens//0),
    (.message.usage.cache_creation.ephemeral_1h_input_tokens//0)
  ] | @tsv" "$tx" 2>/dev/null | tail -n "$N"
