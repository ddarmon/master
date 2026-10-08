---
name: anki-search
description: Search Anki flashcard decks via AnkiConnect. Use when the user asks about their flashcards, wants to find specific cards, check what they have in a deck, or look up card content. Requires Anki running with AnkiConnect plugin.
---

# Anki Search

Search Anki flashcard decks via the AnkiConnect API. Requires Anki to be
running with the AnkiConnect plugin installed.

## Prerequisites

- Anki desktop app running
- AnkiConnect plugin installed (Tools > Add-ons > code 2055492159)
- No additional Python packages needed (stdlib only)

## Commands

### List decks

```bash
python3 <skill-directory>/anki_search.py decks                # List all deck names
python3 <skill-directory>/anki_search.py decks --counts        # Include note counts
python3 <skill-directory>/anki_search.py decks --json          # JSON output
```

### Count matching notes

```bash
python3 <skill-directory>/anki_search.py count "deck:Spanish"        # Notes in deck
python3 <skill-directory>/anki_search.py count "*mitochondria*"       # Notes matching term
```

### Search for cards

```bash
python3 <skill-directory>/anki_search.py search "QUERY"               # Default limit 20
python3 <skill-directory>/anki_search.py search "QUERY" -n 50         # More results
python3 <skill-directory>/anki_search.py search "QUERY" --offset 20   # Skip first 20
python3 <skill-directory>/anki_search.py search "QUERY" --fields      # Show all fields
python3 <skill-directory>/anki_search.py search "QUERY" --json        # JSON output
python3 <skill-directory>/anki_search.py search "QUERY" --no-strip    # Keep raw HTML
```

### Options

| Flag | Default | Description |
|---|---|---|
| `-n`, `--limit` | 20 | Max notes to return |
| `--offset` | 0 | Skip first N results (pagination) |
| `--fields` | off | Show all field names/values |
| `--json` | off | Machine-readable JSON output |
| `--no-strip` | off | Preserve HTML tags in field values |
| `--counts` | off | Include card counts (decks only) |
| `--url URL` | `http://localhost:8765` | AnkiConnect endpoint |

## Anki Query Syntax

The query string is passed directly to Anki's built-in search engine.

### Filter by deck

```
deck:"Deck Name"                       # Exact deck
deck:"Parent::Child"                   # Nested deck
```

### Search card content

```
*keyword*                              # Any field (wildcard)
front:*keyword*                        # Front field only
back:*keyword*                         # Back field only
"exact phrase"                         # Exact phrase, any field
front:"exact phrase"                   # Exact phrase, front only
```

### Filter by tag

```
tag:mytag                              # Has tag
-tag:exclude                           # Does NOT have tag
tag:parent::child                      # Hierarchical tags
```

### Filter by note type

```
note:Basic                             # Basic notes only
note:Cloze                             # Cloze notes only
note:"Basic (and reversed card)"       # Custom note types
```

### Filter by time

```
added:N                                # Added in last N days
edited:N                               # Edited in last N days
rated:N                                # Reviewed in last N days
```

### Boolean logic

```
query1 query2                          # AND (default)
query1 OR query2                       # OR
-query                                 # NOT
(query1 OR query2) query3              # Grouping
```

### Combined examples

```
deck:"Spanish" *casa*                                  # "casa" in Spanish deck
deck:"Math" tag:calculus added:7                       # Recent calculus cards
(front:*derivative* OR front:*integral*) deck:"Math"   # Either term in Math
```

## Translating Plain-Language Requests

| User says | Query |
|---|---|
| "show me my Spanish cards about food" | `deck:"Spanish" *food*` |
| "what cards did I add this week?" | `added:7` |
| "find cards tagged 'important'" | `tag:important` |
| "search for mitochondria across all decks" | `*mitochondria*` |
| "how many cloze cards in Biology?" | count: `deck:"Biology" note:Cloze` |
| "cards about X or Y" | `(*X* OR *Y*)` |
| "cards NOT in the Default deck" | `-deck:Default` |

## Workflows

### Discovery

1. Run `decks --counts` to see what decks exist and their sizes.
2. Then search within a specific deck or across all.

### Large result sets

1. Use `count` to check how many notes match.
2. If > 20, either narrow the query or paginate with `--offset`.
3. Summarize results rather than dumping raw card content.

## Card Types

- **Basic**: fields `Front` and `Back`
- **Cloze**: fields `Text` (with `{{c1::answer}}` syntax) and `Extra`
- **Custom types**: use `--fields` to see all field names

## Error Handling

If the script prints a connection error, suggest the user:
1. Open Anki
2. Install AnkiConnect (Tools > Add-ons > Get Add-ons > code 2055492159)
3. Restart Anki
