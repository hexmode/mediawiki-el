# How the MediaWiki.org Support Desk Is Implemented

The [Project:Support desk](https://www.mediawiki.org/wiki/Project:Support_desk) is a question-and-answer forum for MediaWiki software support. Contrary to what was assumed in issue #58, it does **not** use StructuredDiscussions (Flow) or Phabricator. Since December 2024 it has been a **standard wikitext page** with sections — the same mechanism used by every MediaWiki talk page.

## Page Structure

The support desk page (`Project:Support_desk`) consists of:

1. **A header** — transcluded from `Project:Support_desk/Header` (or `Header/{{int:lang}}` for translated versions). This header page is itself translatable via the Translate extension, supporting ~25 languages. It contains the welcome message, the "Before you post" checklist, the "Post a new question" instructions, and a link to the archives.

2. **Auto-archiving configuration** — an `{{Auto archiving}}` template embedded in the page wikitext. The template itself is a no-op passthrough; the parameters are consumed by `User:ArchiverBot`.

3. **Questions as sections** — each question is a top-level `== Section Title ==`. Answers are indented with `:` (standard talk-page convention). Deeper nesting uses `::`, `:::`, etc.

4. **An "Add topic" link** — this is standard MediaWiki behaviour for any page with `__NEWSECTIONLINK__` or when the wiki has `$wgExtraNamespaces` configured. On the support desk, the `{{Auto archiving}}` template includes this via a `<meta property="mw:PageProp/newsectionlink"/>` tag.

```
                              +----------------------------------+
                              |     Project:Support_desk         |
                              +----------------------------------+
                              |                                  |
                              |  {{Project:Support desk/Header}} |  <-- transcluded,
                              |                                  |      translatable
                              |  {{Auto archiving                |      header + "Add
                              |   |archive = .../Archive %(counter)d |   topic" link
                              |   |algo = old(14d)                |
                              |   |counter = 25                   |
                              |   |...}}                          |
                              |                                  |
                              |  == Abuse filter on my bot ==    |  <-- question 1
                              |  : Reply text...                  |  <-- answer 1.1
                              |  :: Follow-up...                  |  <-- answer 1.2
                              |                                  |
                              |  == Can I find the MW version...  |  <-- question 2
                              |  : Reply text...                  |  <-- answer 2.1
                              |                                  |
                              +----------------------------------+
```

## The "Latest comment" Widget

Each question on the rendered page shows a summary line:

> **Latest comment: 12 days ago | 2 comments | 1 person in discussion**

This is **not** client-side JavaScript. It is server-side metadata exposed by the **DiscussionTools extension** (`mw:Extension:DiscussionTools`), which is installed on mediawiki.org. The extension provides an API module — `action=discussiontoolspageinfo` — that returns structured thread data:

```json
{
  "discussiontoolspageinfo": {
    "threaditemshtml": [
      {
        "id": "h-Slgrandson-20260624025200",
        "headingLevel": 2,
        "html": "Is there an extension...",
        "commentCount": 2,
        "authorCount": 2,
        "latestReplyTimestamp": "2026-06-26T15:53:00Z",
        "replies": [
          {
            "id": "c-Slgrandson-20260624025200-...",
            "timestamp": "2026-06-24T02:52:00Z",
            "author": "Slgrandson",
            "level": 1,
            "html": "<p>...that detects whether a certain...</p>",
            "replies": [
              {
                "id": "c-Ciencia_Al_Poder-20260626155300-...",
                "timestamp": "2026-06-26T15:53:00Z",
                "author": "Ciencia Al Poder",
                "level": 2,
                "html": "<p>Do you really need an extension?...</p>",
                "replies": []
              }
            ]
          }
        ]
      }
    ]
  }
}
```

Key fields per thread:
- `id` — unique thread identifier (derived from the section heading)
- `html` — rendered heading text
- `commentCount` — total comments (including the opening post)
- `authorCount` — number of distinct participants
- `latestReplyTimestamp` — ISO 8601 timestamp of the most recent reply
- `replies[]` — recursive tree of individual comments with IDs, timestamps, authors, levels, and rendered HTML

The DiscussionTools front-end JS consumes this API to render the "Latest comment" widget on the page, but the data is available to any API consumer.

## Auto-Archiving System

The `{{Auto archiving}}` template is a marker consumed by `User:ArchiverBot`, which runs Pywikibot's `archivebot.py` script approximately once per day. The bot reads the template parameters directly from the page wikitext (not from a transclusion), parses section headings and timestamps, and moves inactive threads to numbered archive pages.

Current configuration (as of June 2026):
| Parameter | Value | Meaning |
|---|---|---|
| `archive` | `Project:Support desk/Archive %(counter)d` | Archive pages follow the pattern Archive 21, Archive 22, … |
| `algo` | `old(14d)` | Archive threads with no activity for 14 days |
| `counter` | `25` | Next archive page to create is Archive 25 |
| `maxarchivesize` | `250K` | 250 KB per archive page |
| `minthreadsleft` | `2` | Keep at least 2 threads on the live page |
| `minthreadstoarchive` | `1` | Archive at least 1 thread per run |
| `archiveheader` | `{{talk archive}}` | Header placed on new archive pages |

When a thread is moved to an archive, the bot:
1. Copies the entire section (including all replies) to the archive page
2. Removes it from the live support desk page
3. Updates the `counter` parameter when the archive page exceeds `maxarchivesize`

## Thread Status Tracking

MediaWiki.org has a `{{Resolved}}` template (at [Template:Resolved](https://www.mediawiki.org/wiki/Template:Resolved)). Its wikitext source:

```wikitext
{{<includeonly>safesubst:</includeonly>tick|18}} '''{{<includeonly>safesubst:</includeonly>ucfirst:{{{1|Resolved}}}}}'''
```

This renders as: a checkmark icon (18px) followed by bold "Resolved" text. Anyone can add `{{Resolved}}` to a thread to mark it as complete. There is no formal state machine — threads are "resolved" by convention, not by software enforcement.

## Historical Context

### Ancient period (2006–2008)
The support desk started as a standard wikitext page. Archives were manually created by copying sections to numbered subpages (Archive 01 through Archive 20).

### By-topic period (March 2008–August 2010)
Threads were split into per-topic sections (Installation, System, Database, PHP, Uploading, Images, Formatting, Extensions, Miscellaneous), each with their own archive subpages.

### LiquidThreads/Flow period (August 2010–December 2024)
The page was converted to [LiquidThreads](https://www.mediawiki.org/wiki/LiquidThreads) and later migrated to [StructuredDiscussions (Flow)](https://www.mediawiki.org/wiki/Flow). Threads were stored in a separate database structure, not as wikitext.

### Wikitext migration (December 2024–present)
When Flow was [deprecated](https://www.mediawiki.org/wiki/Structured_Discussions/Deprecation), `User:Flow cleanup bot` exported all Flow boards to wikitext. Each month's topics were written to `Project:Support_desk/Flow/YYYY/MM` pages. The support desk was then converted back to a standard wikitext page with standard section-based threading and automatic archiving.

## API Observability

### Listing threads
```
action=discussiontoolspageinfo
&page=Project:Support_desk
&prop=threaditemshtml
&format=json&formatversion=2
```
Returns the full thread tree with metadata and rendered HTML.

### Fetching section content
```
action=parse
&page=Project:Support_desk
&prop=tocdata           ← note: "sections" is deprecated
&format=json&formatversion=2
```
Returns section TOC data (index, anchor, byte offset). Individual sections can be fetched with `&section=N`.

### Posting replies
```
action=edit
&title=Project:Support_desk
&section=N
&appendtext=: Reply text ~~~~
&token=...
```
Uses the standard section-append mechanism.

### Creating new threads
```
action=edit
&title=Project:Support_desk
&section=new
&sectiontitle=Question title
&text=Question body ~~~~
&token=...
```

### Searching
```
action=query
&list=search
&srsearch=prefix:Project:Support desk/
&srnamespace=4
```

Standard MediaWiki search restricted to the support desk namespace.

## Summary

| Aspect | Implementation |
|---|---|
| Page type | Standard wikitext page |
| Thread model | `== sections ==` with `:`-indented replies |
| Thread metadata | `action=discussiontoolspageinfo` (DiscussionTools extension) |
| "Latest comment" widget | Rendered from `discussiontoolspageinfo` response |
| Auto-archiving | `User:ArchiverBot` + `{{Auto archiving}}` template |
| Status tracking | `{{Resolved}}` template (convention-based) |
| Authentication required for edits | Yes (OAuth 2.0) |
| Search | `action=query&list=search` with prefix filter |
