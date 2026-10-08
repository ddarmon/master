---
name: gws-gmail
description: Read, search, send, and manage Gmail using the `gws gmail` CLI. Use this skill whenever the user asks about their email, inbox, unread messages, wants to send an email, search for messages, manage drafts, label or archive messages, or interact with Gmail in any way.
---

# gws gmail — Reading, Searching, and Sending Email

The `gws` CLI provides access to Gmail. This skill covers reading, searching, sending, and managing email.

## Quick Inbox Check with `+triage`

The fastest way to see what's in the inbox. Read-only and safe to run anytime.

```bash
gws gmail +triage                                  # unread inbox summary (sender, subject, date)
gws gmail +triage --max 5                           # limit to 5 messages
gws gmail +triage --query 'from:boss@example.com'   # filter with Gmail search syntax
gws gmail +triage --labels                          # include label names
gws gmail +triage --format json                     # json output (also: table, yaml, csv)
```

`+triage` is the right choice for quick questions like "what's in my inbox?" or "do I have unread email?"

## Sending Email with `+send`

```bash
gws gmail +send --to alice@example.com --subject 'Hello' --body 'Hi Alice!'
```

This handles RFC 2822 formatting and base64 encoding automatically. It only supports **plain text**. For HTML bodies, attachments, or CC/BCC, use the raw API:

```bash
gws gmail users messages send --json '{
  "raw": "<base64url-encoded RFC 2822 message>"
}'
```

## Reading a Full Message

`+triage` and `messages list` only return summaries or IDs. To read the full content of a message, use `messages get`:

```bash
gws gmail users messages get \
  --params '{"userId": "me", "id": "<MESSAGE_ID>", "format": "full"}'
```

The response includes:
- `payload.headers` — array of `{name, value}` objects (From, To, Subject, Date, etc.)
- `payload.body.data` — base64url-encoded message body
- `snippet` — short plain-text preview
- `labelIds` — labels on this message

### Decoding the message body

The body is base64url-encoded. Decode it with:

```bash
echo '<base64-data>' | python3 -c "import sys, base64; print(base64.urlsafe_b64decode(sys.stdin.read().strip()).decode())"
```

Or extract and decode in one step:

```bash
gws gmail users messages get \
  --params '{"userId": "me", "id": "<MESSAGE_ID>", "format": "full"}' \
  | python3 -c "
import json, sys, base64
msg = json.load(sys.stdin)
headers = {h['name']: h['value'] for h in msg['payload']['headers']}
body_data = msg['payload'].get('body', {}).get('data', '')
if not body_data:
    # multipart message — check parts
    for part in msg['payload'].get('parts', []):
        if part['mimeType'] == 'text/plain':
            body_data = part['body'].get('data', '')
            break
body = base64.urlsafe_b64decode(body_data).decode() if body_data else ''
print(f\"From: {headers.get('From', '')}\")
print(f\"Subject: {headers.get('Subject', '')}\")
print(f\"Date: {headers.get('Date', '')}\")
print()
print(body)
"
```

### Format options for `messages get`

| `format` | What you get |
|----------|-------------|
| `full` (default) | Headers + body (base64-encoded). Use this — it's the most reliable. |
| `metadata` | Headers only. The `metadataHeaders` filter is unreliable; prefer `full`. |
| `minimal` | IDs, labels, snippet only. |
| `raw` | Entire RFC 2822 message, base64url-encoded. |

## Searching Messages

Use `messages list` with the `q` parameter. It supports the same query syntax as the Gmail search box.

```bash
gws gmail users messages list \
  --params '{"userId": "me", "q": "from:alice@example.com newer_than:7d", "maxResults": 10}'
```

Common search operators:
- `from:`, `to:`, `cc:`, `bcc:` — sender/recipient
- `subject:` — subject line
- `newer_than:1d`, `older_than:2w` — relative dates (d=day, w=week, m=month, y=year)
- `after:2026/03/01`, `before:2026/03/14` — absolute dates
- `has:attachment` — messages with attachments
- `filename:pdf` — attachment file type
- `is:unread`, `is:read`, `is:starred` — message state
- `label:` — filter by label
- `in:inbox`, `in:sent`, `in:trash` — mailbox location
- `"exact phrase"` — exact match

This returns message IDs only. Loop over results with `messages get` to read content.

You can also filter by label without a query:

```bash
gws gmail users messages list \
  --params '{"userId": "me", "labelIds": "INBOX", "maxResults": 10}'
```

## Working with Threads

Threads group related messages in a conversation. Use threads when the user asks about a conversation or email chain.

```bash
# List threads
gws gmail users threads list \
  --params '{"userId": "me", "q": "subject:project update", "maxResults": 5}'

# Get all messages in a thread
gws gmail users threads get \
  --params '{"userId": "me", "id": "<THREAD_ID>", "format": "full"}'
```

`threads get` returns all messages in the thread, so you can show the full conversation.

## Managing Messages

### Label / unlabel

```bash
gws gmail users messages modify \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}' \
  --json '{"addLabelIds": ["STARRED"], "removeLabelIds": ["UNREAD"]}'
```

### Archive (remove from inbox)

```bash
gws gmail users messages modify \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}' \
  --json '{"removeLabelIds": ["INBOX"]}'
```

### Mark as read / unread

```bash
# Mark read
gws gmail users messages modify \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}' \
  --json '{"removeLabelIds": ["UNREAD"]}'

# Mark unread
gws gmail users messages modify \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}' \
  --json '{"addLabelIds": ["UNREAD"]}'
```

### Trash / untrash

```bash
gws gmail users messages trash \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}'

gws gmail users messages untrash \
  --params '{"userId": "me", "id": "<MESSAGE_ID>"}'
```

### Batch operations

Modify up to 1000 messages at once:

```bash
gws gmail users messages batchModify \
  --params '{"userId": "me"}' \
  --json '{"ids": ["<ID1>", "<ID2>"], "addLabelIds": ["STARRED"]}'
```

## Working with Drafts

```bash
# List drafts
gws gmail users drafts list --params '{"userId": "me", "maxResults": 10}'

# Create a draft
gws gmail users drafts create \
  --params '{"userId": "me"}' \
  --json '{"message": {"raw": "<base64url-encoded RFC 2822 message>"}}'

# Send a draft
gws gmail users drafts send \
  --params '{"userId": "me"}' \
  --json '{"id": "<DRAFT_ID>"}'

# Delete a draft (permanent)
gws gmail users drafts delete \
  --params '{"userId": "me", "id": "<DRAFT_ID>"}'
```

## Labels

```bash
# List all labels
gws gmail users labels list --params '{"userId": "me"}' --format table

# Get label details (includes unread count)
gws gmail users labels get --params '{"userId": "me", "id": "INBOX"}'

# Create a label
gws gmail users labels create \
  --params '{"userId": "me"}' \
  --json '{"name": "My Label", "labelListVisibility": "labelShow", "messageListVisibility": "show"}'
```

Common system labels: `INBOX`, `UNREAD`, `STARRED`, `SENT`, `DRAFT`, `TRASH`, `SPAM`, `IMPORTANT`, `CATEGORY_PERSONAL`, `CATEGORY_SOCIAL`, `CATEGORY_PROMOTIONS`, `CATEGORY_UPDATES`, `CATEGORY_FORUMS`.

## Presenting Results

When showing email to the user, format as a readable summary. Include:
- **From** and **Subject** prominently
- **Date/time** in 12-hour format
- **Snippet or body** as appropriate

The raw JSON from `gws` is hard to read — always reformat it. For inbox listings, a table format works well. For individual messages, show headers then body.

## Common Pitfalls

- **Message body is base64url-encoded**: The `payload.body.data` field needs decoding. See the decoding section above.
- **`messages list` returns IDs only**: You must call `messages get` for each message to get content, headers, or body.
- **`metadataHeaders` is unreliable**: When using `format: "metadata"`, the `metadataHeaders` filter often returns no headers. Use `format: "full"` instead and extract the headers you need.
- **Multipart messages**: HTML emails have the body in `payload.parts[]` rather than `payload.body`. Check for `parts` with `mimeType: "text/plain"` or `"text/html"`.
- **`+send` is plain text only**: For HTML, attachments, CC/BCC, construct a raw RFC 2822 message.
- **Pagination**: Use `--page-all` to auto-paginate, or pass `pageToken` from the response for manual pagination.
