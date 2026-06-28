# Support Desk Module: Implementation Plan

This document describes the plan for `mediawiki-support-desk.el`, a new module that provides an interactive workflow for managing the [MediaWiki Support Desk](https://www.mediawiki.org/wiki/Project:Support_desk).

See [SUPPORT-DESK-WIKI-IMPLEMENTATION.md](./SUPPORT-DESK-WIKI-IMPLEMENTATION.md) for how the support desk works on the wiki side.

## Architecture Overview

```
mediawiki-support-desk.el
  ├── requires mediawiki-api.el    (HTTP calls, token handling, JSON parsing)
  ├── requires mediawiki-auth.el   (OAuth 2.0 authentication)
  ├── requires mediawiki-site.el   (site URL construction)
  └── requires mediawiki-core.el   (fundamental utilities)
```

Two major modes:
- `mediawiki-support-desk-list-mode` — table of threads (derived from `tabulated-list-mode`)
- `mediawiki-support-desk-view-mode` — read-only view of a single thread with rendered HTML

## Data Model

Thread data is populated from the `discussiontoolspageinfo` API response.  The initial list fetch uses `threaditemsflags=activity|noreplies` for a lightweight payload; a full re-fetch (without `noreplies`) is done when the user needs thread content.

### Thread record (alist used as struct)

| Key | Source field | Example |
|---|---|---|
| `id` | `thread.id` | `"h-Slgrandson-20260624025200"` |
| `title` | `thread.html` (strip tags) | `"Is there an extension..."` |
| `author` | `thread.oldestReply.author` (from `activity` flag) | `"Slgrandson"` |
| `timestamp` | `thread.oldestReply.timestamp` | `"2026-06-24T02:52:00Z"` |
| `reply-count` | `thread.commentCount` | `2` |
| `author-count` | `thread.authorCount` | `2` |
| `last-reply` | `thread.latestReply` object (from `activity`) | `{timestamp, author, id}` |
| `status` | derived (see below) | `'active`, `'resolved`, `'stale` |
| `raw` | full thread object | (cached after Phase 2 fetch) |

### Status derivation

```
resolved?  ── scan thread HTML for "{{Resolved}}" or "{{resolved}}"
  ├── yes ── status = 'resolved
  └── no
       └── last-reply > 14 days ago?
            ├── yes ── status = 'stale
            └── no  ── status = 'active
```

### Priority sorting for list display

Threads are sorted by priority (highest first):

1. **Needs attention** — the logged-in user posted a reply and someone else replied after them. The user hasn't replied to that latest message. (Somebody answered your question — follow up.)
2. **Watching** — the logged-in user posted but nobody has replied yet.
3. **Active, OP replied** — another user's thread; the OP has replied to answers (active discussion).
4. **Unanswered** — no replies at all (just the opening post).
5. **Resolved** — contains `{{Resolved}}`.
6. **Stale** — > 14 days since last reply, will be archived soon.

## API Calls

The module uses these API endpoints through the existing `mediawiki-api.el` infrastructure:

### 1. List threads

```
action=discussiontoolspageinfo
&page=Project:Support_desk
&prop=threaditemshtml
&format=json&formatversion=2
```

Returns all threads with metadata and rendered HTML. Called once and cached for the session. The `replies` trees store individual comment IDs used for reply threading.

### 2. View a single thread

Uses the cached `raw` field from the list call. If needed, a targeted parse can be done:

```
action=parse
&page=Project:Support_desk
&section=N
&prop=text
```

However, the `discussiontoolspageinfo` response already includes per-comment HTML, so we can assemble the thread view from cached data without an additional request.

### 3. Reply to a thread

```
action=edit
&title=Project:Support_desk
&section=N
&appendtext=%0A: Reply text ~~~~
&summary=/* Section title */ Reply
&token=...
```

The section number `N` comes from `action=parse&prop=tocdata` (which maps `index` to section number). Alternatively, we can pass the thread ID and use `action=parse&section=N&prop=tocdata` to resolve it at reply time.

The reply text is prepended with `:` for standard talk-page indentation.

### 4. Create a new thread

```
action=edit
&title=Project:Support_desk
&section=new
&sectiontitle=Question title
&text=Question body ~~~~
&token=...
```

### 5. Mark as resolved

```
action=edit
&title=Project:Support_desk
&section=N
&appendtext=%0A: {{Resolved|~~~~}}
&summary=/* Section title */ Marking as resolved
&token=...
```

This inserts a new reply containing the `{{Resolved}}` template.  If the user provides a summary, it is included before the signature: `{{Resolved|summary text|~~~~}}`.  The summary can reference a specific reply comment by its DiscussionTools comment ID (e.g., `c-Ciencia_Al_Poder-20260626155300-...`) to link the resolution to the answer that solved it.  The signature is controlled by `mediawiki-support-desk-signature` (defaults to `~~~~`).  This uses the [existing template](https://www.mediawiki.org/wiki/Template:Resolved) on mediawiki.org.

### 6. Check if a thread was archived

```
action=query
&list=search
&srsearch=prefix:Project:Support desk/Archive
&srnamespace=4
&srlimit=50
```

Lists archive pages. Then for each archived thread ID, we can query the archive page's `discussiontoolspageinfo` to find it, or simply check that the thread ID no longer appears on the live page.

### 7. Search threads

```
action=query
&list=search
&srsearch=SEARCH_TERMS prefix:Project:Support desk
&srnamespace=4
```

Standard MediaWiki search scoped to the Project namespace and prefixed to the support desk.

### 8. API flags and optimisations

The `discussiontoolspageinfo` module accepts several flags on the `threaditemsflags` parameter, exposed as pipe-separated values:

| Flag | Effect |
|---|---|
| `activity` | Adds `oldestReply` and `latestReply` objects to each thread (timestamp, author, type, level, id) — lightweight references without nested reply trees or HTML |
| `noreplies` | Returns only top-level headings; omits the recursive `replies` tree entirely — fastest option for list views |
| `excludesignatures` | Strips user signatures from the rendered HTML — cleaner output for view mode, easier author detection |

**Recommended call pattern:**

```
;; Phase 1: Lightweight list view
action=discussiontoolspageinfo
&prop=threaditemshtml
&threaditemsflags=activity|noreplies

;; Phase 2: Full thread fetch when user views a thread
action=discussiontoolspageinfo
&prop=threaditemshtml
&threaditemsflags=activity|excludesignatures
```

Phase 1 fetches headings + counts + oldest/latest reply timestamps without any reply content — minimal bandwidth, perfect for the `tabulated-list` view. Phase 2 fetches the full reply tree with cleaned HTML for rendering the thread detail.

**Additional parameters:**

- `oldid=<revision>` — fetch thread structure as of a specific page revision; useful for detecting when a thread was archived (compare current vs. prior revision)
- `prop=transcludedfrom` — returns a map of thread/comment IDs to their transclusion source page (`false` = directly on the page). Currently all false on the support desk, but potentially useful if the page later uses transcluded discussion templates.

**Caution:** The API is marked as "internal or unstable" by MediaWiki. While it has been stable in practice for years, any code relying on it should handle graceful degradation if the response shape changes.

## Minor Modes

### `mediawiki-support-desk-list-mode`

Derived from `tabulated-list-mode`.

**Columns:**

| Width | Header | Content |
|---|---|---|
| 3 | `S` | Status icon: `◉` needs-attention, `○` watching, `·` active, `?` unanswered, `✓` resolved, `~` stale |
| 40 | Title | Thread title (truncated) |
| 16 | Author | Opening poster's username |
| 5 | `#` | Reply count |
| 20 | Last | Relative time since last reply (e.g., "2 days ago") |

**Default sort:** by priority (see priority sorting above).

**Keybindings:**

| Key | Command |
|---|---|
| `g` | `mediawiki-support-desk-refresh` — re-fetch thread list |
| `RET` | `mediawiki-support-desk-view-thread-at-point` — open view mode |
| `r` | `mediawiki-support-desk-reply` — reply to thread at point |
| `d` | `mediawiki-support-desk-resolve` — mark resolved (via `{{Resolved}}`) |
| `n` | `mediawiki-support-desk-new-thread` — create new thread |
| `s` | `mediawiki-support-desk-search` — search threads |
| `S` | Sort prompt (by title, author, date, replies) |
| `q` | Quit list mode |

### `mediawiki-support-desk-view-mode`

Read-only mode for viewing a single thread.

**Display:** Each comment rendered with:
- Author + timestamp header line
- Indented body text (stripped of most HTML but preserving links and code blocks)
- Replies nested with increasing indentation

**Keybindings:**

| Key | Command |
|---|---|
| `r` | Reply to this thread |
| `d` | Mark this thread as resolved |
| `g` | Refresh thread content |
| `n` / `p` | Next / previous thread (without returning to list) |
| `q` | Return to list mode |

## Core Functions

```elisp
;; Thread data management
(defun mediawiki-support-desk--fetch-threads (sitename &optional full)
  "Fetch thread data from the support desk for SITENAME.
If FULL is non-nil, fetch with full reply trees (no noreplies flag).
Otherwise uses threaditemsflags=activity|noreplies for lightweight list.
Returns an alist of thread records sorted by priority.")

(defun mediawiki-support-desk--thread-status (thread)
  "Determine the status of THREAD.
Returns one of: 'needs-attention, 'watching, 'active, 'unanswered,
'resolved, 'stale.")

(defun mediawiki-support-desk--thread-priority (thread)
  "Return a numeric priority for THREAD (lower = higher priority).")

(defun mediawiki-support-desk--thread-contains-username-p (thread username)
  "Return t if USERNAME appears in any reply in THREAD.")

;; List mode
(defun mediawiki-support-desk ()
  "Open the MediaWiki support desk thread list.
Prompts for a site if more than one is configured.")

(defun mediawiki-support-desk-refresh ()
  "Re-fetch the thread list for the current support desk.")

(defun mediawiki-support-desk-search (query)
  "Search support desk threads for QUERY.")

;; Thread actions
(defun mediawiki-support-desk-view-thread-at-point ()
  "View the full thread at point in a dedicated buffer.")

(defun mediawiki-support-desk-reply (text)
  "Post a reply to the thread at point with TEXT.")

(defun mediawiki-support-desk-new-thread (title text)
  "Create a new support desk thread with TITLE and TEXT.")

(defun mediawiki-support-desk-resolve (summary)
  "Mark the thread at point as resolved.
Prompts for an optional SUMMARY.  If provided, the resolution comment
includes the summary text followed by the signature.  The summary may
reference a specific reply comment (e.g. \"See reply above\") to link
the resolution to a particular answer.  If SUMMARY is empty, just
appends {{Resolved|~~~~}} with no additional text.")

(defun mediawiki-support-desk--check-archived (thread-id)
  "Return t if THREAD-ID has been archived, nil otherwise.")
```

## Implementation Phases

### Phase 1 — Thread listing

- Implement `mediawiki-support-desk--fetch-threads`
- Call `discussiontoolspageinfo&threaditemsflags=activity|noreplies`, parse headings + counts + timestamps into thread records (lightweight, no reply trees)
- Implement `mediawiki-support-desk-list-mode` with `tabulated-list`
- Implement status derivation — `active`/`stale` from `latestReplyTimestamp` (available in Phase 1 via `activity` flag). `resolved` detection requires full thread HTML (Phase 2/4).
- Implement priority sorting (without "needs attention" / "watching" — that requires the user's username in Phase 4)

### Phase 2 — Thread viewing

- Implement `mediawiki-support-desk-view-mode`
- Render thread from cached `discussiontoolspageinfo` data
- Navigate between threads with `n` / `p`

### Phase 3 — Posting

- Implement `mediawiki-support-desk-reply` (section append with `:`)
- Implement `mediawiki-support-desk-new-thread` (section=new)
- Handle edit token acquisition via `mediawiki-api.el`
- Handle OAuth authentication via `mediawiki-auth.el`

### Phase 4 — Status workflow

- Implement `mediawiki-support-desk-resolve` — prompts for optional summary, appends `{{Resolved|summary|~~~~}}` (or `{{Resolved|~~~~}}` if no summary). The summary may reference a reply comment ID.
- Implement "needs attention" and "watching" priority tiers
- Detect user's own comments in threads
- Detect when someone replied after the user

### Phase 5 — Archive awareness

- Implement `mediawiki-support-desk--check-archived`
- Mark archived threads as `closed` in the list
- Search archive pages for thread IDs that disappeared from live page

## Configuration

```elisp
;; Site configuration for the support desk
;; (mediawiki.org is already pre-configured in mediawiki-site-alist
;; if not, add it with OAuth credentials)

(defcustom mediawiki-support-desk-page "Project:Support_desk"
  "Page name for the support desk on the wiki."
  :type 'string
  :group 'mediawiki)

(defcustom mediawiki-support-desk-stale-days 14
  "Number of days before a thread is considered stale.
Should match the auto-archiving threshold on the wiki."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-support-desk-max-threads nil
  "Maximum number of threads to display. nil means no limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'mediawiki)

(defcustom mediawiki-support-desk-signature "~~~~"
  "Signature string appended to replies and resolution comments.
The default \"~~~~\" expands to the user's standard MediaWiki
signature (timestamp + username link) when saved.  Set to a custom
string like \"--~~~~\" or a raw wikitext sig if preferred."
  :type 'string
  :group 'mediawiki)
```

## Testing Protocol

Tests live in `tests/test-mediawiki-support-desk.el` and follow the ERT patterns used by the rest of the project (`cl-letf` mocks, macro fixtures, `should`/`should-not` assertions).  Run with `make test` (local) or `make ci-test` (CI).

### Unit Tests

Unit tests mock all network and I/O boundaries.  No API calls, no `auth-source`, no filesystem writes.

**API response parsing** (no network — mock the JSON response buffer)
- `discussiontoolspageinfo` response with `activity|noreplies` flags → correct thread records
- `discussiontoolspageinfo` response with full reply tree → correct comment nesting
- `action=parse&prop=tocdata` response → correct index-to-section-number mapping
- Empty page (no threads) → empty list, no error
- Malformed JSON response → graceful error, not a crash
- Missing fields (e.g., no `html` in heading) → default values, no crash

**Status derivation** (operates on thread records, no network)
- `latestReplyTimestamp` < 14 days old → `active`
- `latestReplyTimestamp` > 14 days old → `stale`
- Thread HTML contains checkmark entity (`&#x2713;`) → `resolved`
- Thread HTML contains `{{Resolved}}` template → `resolved`
- Thread HTML contains no markers → not `resolved`
- Thread with no `latestReplyTimestamp` (fallback) → `unanswered`

**Priority sorting** (operates on thread records, no network)
- Thread where user posted and got a reply → rank 1 (needs attention)
- Thread where user posted but no reply → rank 2 (watching)
- Thread with `resolved` status → rank 5
- Thread with `stale` status → rank 6
- Sort stability: same-rank threads keep API order

**Reply text construction**
- Empty signature config → falls back to `~~~~`
- Custom signature (`--~~~~`) → prepended correctly
- `: ` indentation prefix → always present for reply
- New-thread body → no auto-indentation, just signature appended

**Section-number mapping**
- `tocdata` with `See_also` (index `""`) → filtered out
- `tocdata` with `Before_you_post` (index `""`) → filtered out
- `tocdata` with `Post_a_new_question` (index `""`) → filtered out
- Thread at `index` 1 → maps to `number` 4 (after 3 header sections)

**Checkmark detection**
- `&#x2713;` in HTML → detected
- `{{Resolved}}` in HTML → detected
- `{{resolved}}` in HTML → detected
- `{{Resolved|summary|~~~~}}` → detected
- `✔` (bare character) → detected
- `&#x2714;` (heavy check) → detected

### Integration Tests

Integration tests use `cl-letf` to mock `url-retrieve-synchronously` at the HTTP level, exercising the full `mediawiki-api.el` → `mediawiki-support-desk.el` pipeline with realistic JSON payloads.

**Full list pipeline**
- Mock `discussiontoolspageinfo` → feed through `mediawiki-support-desk--fetch-threads` → verify `tabulated-list` entries have correct column values

**Full view pipeline**
- Mock `discussiontoolspageinfo` with reply tree → feed through thread-view buffer → verify indentation levels, author names, timestamps parsed

**Edit dispatch (mocked token + post)**
- Mock `action=query&meta=tokens` (CSRF token)
- Mock `action=edit&section=N&appendtext=...` (success response)
- Call `mediawiki-support-desk-reply` → verify correct URL, correct `appendtext` payload, correct summary

**Error handling**
- Mock HTTP 503 response → verify user-visible error message, no crash
- Mock `action=edit` with `result: Failure` (edit conflict) → verify conflict reported
- Mock `action=edit` with `abusefilter-disallowed` → verify abuse filter message reported
- Mock OAuth token expired mid-session → verify automatic token-refresh retry

**Resolve workflow (integration)**
- Mock `discussiontoolspageinfo` with unresolved thread
- Mock `action=edit&appendtext={{Resolved|...}}` success
- Verify thread re-fetched after resolve and status updated to `resolved`

**Archive detection**
- Mock `action=query&list=search` returning archive page list
- Mock archive page `discussiontoolspageinfo` containing a known thread ID
- Verify `mediawiki-support-desk--check-archived` returns `t`

### User Testing

These are manual tests performed against the live `mediawiki.org` site to verify real-world behaviour before release.

**Setup**
1. Configure a site entry for `mediawiki.org` with OAuth credentials in `mediawiki-site-alist`
2. Set `mediawiki-support-desk-signature` to `~~~~` (default)
3. Open `M-x mediawiki-support-desk` — verify thread list populates
4. Verify column sort order (needs-attention threads at top)

**Viewing**
5. Press `RET` on a multi-reply thread — verify all comments render with correct indentation
6. Press `RET` on a thread with zero replies — verify the opening post renders
7. Press `n` / `p` to navigate threads — verify boundary behaviour (first/last thread)

**Posting**
8. Press `n` to create a new thread — type a title and body — verify it appears on the live page
9. Press `r` on an existing thread — type a reply — verify it appears indented under the OP
10. Press `g` to refresh — verify the new reply appears with correct relative timestamp

**Resolving**
11. Press `d` on a thread — enter "See reply above" as summary — verify `{{Resolved|See reply above|~~~~}}` appears on the live page
12. Press `d` on a thread — leave summary empty — verify `{{Resolved|~~~~}}` appears
13. Refresh the thread — verify status icon changes to `✓`

**Edge cases**
14. Interrupted network (kill network mid-fetch) — verify graceful error, not a hang
15. Thread that was archived between list and view — verify "thread not found" message
16. Very long thread titles (>80 chars) — verify table column truncation with `…`
17. Unicode authors and titles — verify display without mojibake

## Resolved Design Decisions

1. **Recovering section numbers** — Call `action=parse&prop=tocdata` once and cache the `index → section-number` mapping. Filter out non-discussion sections (`See_also`, `Before_you_post`, `Post_a_new_question`). The `tocdata` response includes both `index` (matches the `discussiontoolspageinfo` thread order) and `number` (the integer used by `action=edit&section=`).

2. **Single-thread re-fetch** — After posting a reply or marking resolved, re-fetch the entire `discussiontoolspageinfo` response. This is simpler than updating the local cache and has the benefit of verifying that the edit posted correctly. The payload is small (~50KB for the current page).

3. **{{Resolved}} detection** — Scan for the checkmark icon in the rendered HTML rather than wikitext. The template renders as `&#x2713;` or the tick image; searching HTML is more robust against variant wikitext forms (`{{Resolved}}`, `{{resolved}}`, `{{Resolved|...}}`, manual checkmark insertions).
