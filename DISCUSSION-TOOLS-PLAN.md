# Discussion Tools Module: Implementation Plan

This document describes the plan for `mediawiki-discussion-tools.el`, a new generic module that provides an interactive discussion page browser backed by the MediaWiki DiscussionTools API.  `mediawiki-support-desk.el` is a thin wrapper that configures it for [Project:Support_desk](https://www.mediawiki.org/wiki/Project:Support_desk).

See [SUPPORT-DESK-WIKI-IMPLEMENTATION.md](./SUPPORT-DESK-WIKI-IMPLEMENTATION.md) for how the support desk page works on the wiki side (page structure, archiving, the `{{Resolved}}` template, and the DiscussionTools API).

## Architecture Overview

```
mediawiki-discussion-tools.el        ← generic discussion page browser
  ├── requires mediawiki-api.el      (HTTP calls, token handling, JSON parsing)
  ├── requires mediawiki-auth.el     (OAuth 2.0 authentication)
  ├── requires mediawiki-site.el     (site URL construction)
  └── requires mediawiki-core.el     (fundamental utilities)

mediawiki-support-desk.el            ← thin convenience wrapper
  └── requires mediawiki-discussion-tools.el
      (defun mediawiki-support-desk (&optional sitename)
        (mediawiki-discussion-tools "Project:Support_desk" sitename))
```

Two major modes:
- `mediawiki-discussion-tools-list-mode` — table of threads (derived from `tabulated-list-mode`)
- `mediawiki-discussion-tools-view-mode` — read-only view of a single thread with rendered HTML

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
       └── last-reply > stale-days ago?
            ├── yes ── status = 'stale
            └── no  ── status = 'active
```

### Priority sorting for list display

Threads are sorted by priority (highest first):

1. **Needs attention** — the logged-in user posted a reply and someone else replied after them. The user hasn't replied to that latest message.
2. **Watching** — the logged-in user posted but nobody has replied yet.
3. **Active, OP replied** — another user's thread; the OP has replied to answers (active discussion).
4. **Unanswered** — no replies at all (just the opening post).
5. **Resolved** — contains `{{Resolved}}`.
6. **Stale** — > stale-days since last reply.

## API Calls

The module uses these API endpoints through the existing `mediawiki-api.el` infrastructure:

### 1. List threads

```
action=discussiontoolspageinfo
&page=<wiki page>
&prop=threaditemshtml
&format=json&formatversion=2
```

Returns all threads with metadata and rendered HTML. Called once and cached for the session. The `replies` trees store individual comment IDs used for reply threading.

### 2. View a single thread

Uses the cached `raw` field from the list call. If needed, a targeted parse can be done:

```
action=parse
&page=<wiki page>
&section=N
&prop=text
```

However, the `discussiontoolspageinfo` response already includes per-comment HTML, so we can assemble the thread view from cached data without an additional request.

### 3. Reply to a thread

```
action=edit
&title=<wiki page>
&section=N
&appendtext=%0A: Reply text ~~~~
&summary=/* Section title */ Reply
&token=...
```

The section number `N` comes from `action=parse&prop=tocdata` (which maps `index` to section number).

The reply text is prepended with `:` for standard talk-page indentation.

### 4. Create a new thread

```
action=edit
&title=<wiki page>
&section=new
&sectiontitle=Topic title
&text=Topic body ~~~~
&token=...
```

### 5. Mark as resolved

```
action=edit
&title=<wiki page>
&section=N
&appendtext=%0A: {{Resolved|~~~~}}
&summary=/* Section title */ Marking as resolved
&token=...
```

This inserts a new reply containing the `{{Resolved}}` template.  If the user provides a summary, it is included before the signature: `{{Resolved|summary text|~~~~}}`.  The summary can reference a specific reply comment by its DiscussionTools comment ID (e.g., `c-Ciencia_Al_Poder-20260626155300-...`) to link the resolution to the answer that solved it.  The signature is controlled by `mediawiki-discussion-tools-signature` (defaults to `~~~~`).

### 6. Check if a thread was archived

```
action=query
&list=search
&srsearch=prefix:<wiki page>/Archive
&srnamespace=4
&srlimit=50
```

Lists archive pages. Then for each archived thread ID, we can query the archive page's `discussiontoolspageinfo` to find it, or simply check that the thread ID no longer appears on the live page.

### 7. Search threads

```
action=query
&list=search
&srsearch=SEARCH_TERMS prefix:<wiki page>
&srnamespace=4
```

Standard MediaWiki search scoped to the Project namespace and prefixed to the discussion page.

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
- `prop=transcludedfrom` — returns a map of thread/comment IDs to their transclusion source page (`false` = directly on the page).

**Caution:** The API is marked as "internal or unstable" by MediaWiki. While it has been stable in practice for years, any code relying on it should handle graceful degradation if the response shape changes.

## Minor Modes

### `mediawiki-discussion-tools-list-mode`

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
| `g` | `mediawiki-discussion-tools-refresh` — re-fetch thread list |
| `RET` | `mediawiki-discussion-tools-view-thread-at-point` — open view mode |
| `r` | `mediawiki-discussion-tools-reply` — reply to thread at point |
| `d` | `mediawiki-discussion-tools-resolve` — mark resolved |
| `n` | `mediawiki-discussion-tools-new-thread` — create new thread |
| `s` | `mediawiki-discussion-tools-search` — search threads |
| `S` | Sort prompt (by title, author, date, replies) |
| `q` | Quit list mode |

### `mediawiki-discussion-tools-view-mode`

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
(defun mediawiki-discussion-tools--fetch-threads (sitename page &optional full)
  "Fetch thread data from PAGE on SITENAME using the DiscussionTools API.
If FULL is non-nil, fetch with full reply trees (no noreplies flag).
Otherwise uses threaditemsflags=activity|noreplies for lightweight list.
Returns an alist of thread records sorted by priority.")

(defun mediawiki-discussion-tools--thread-status (thread)
  "Determine the status of THREAD.
Returns one of: 'needs-attention, 'watching, 'active, 'unanswered,
'resolved, 'stale.")

(defun mediawiki-discussion-tools--thread-priority (thread)
  "Return a numeric priority for THREAD (lower = higher priority).")

(defun mediawiki-discussion-tools--thread-contains-username-p (thread username)
  "Return t if USERNAME appears in any reply in THREAD.")

;; Entry point
(defun mediawiki-discussion-tools (page &optional sitename)
  "Open a discussion page thread list.
PAGE is the wiki page to browse.  SITENAME is the wiki site.
Displays threads sorted by priority.")

(defun mediawiki-discussion-tools-refresh ()
  "Re-fetch the thread list for the current discussion buffer.")

(defun mediawiki-discussion-tools-search (query)
  "Search threads on the current discussion page for QUERY.")

;; Thread actions
(defun mediawiki-discussion-tools-view-thread-at-point ()
  "View the full thread at point in a dedicated buffer.")

(defun mediawiki-discussion-tools-reply (text)
  "Post a reply to the thread at point with TEXT.")

(defun mediawiki-discussion-tools-new-thread (title text)
  "Create a new discussion thread with TITLE and TEXT.")

(defun mediawiki-discussion-tools-resolve (summary)
  "Mark the thread at point as resolved.
Prompts for an optional SUMMARY.  If provided, the resolution comment
includes the summary text followed by the signature.  The summary may
reference a specific reply comment (e.g. \"See reply above\") to link
the resolution to a particular answer.  If SUMMARY is empty, just
appends {{Resolved|~~~~}} with no additional text.")

(defun mediawiki-discussion-tools--check-archived (thread-id)
  "Return t if THREAD-ID has been archived, nil otherwise.")
```

### Support Desk Wrapper

```elisp
;;; mediawiki-support-desk.el
(require 'mediawiki-discussion-tools)

(defcustom mediawiki-support-desk-page "Project:Support_desk"
  "Page name for the support desk on the wiki."
  :type 'string
  :group 'mediawiki-support-desk)

;;;###autoload
(defun mediawiki-support-desk (&optional sitename)
  "Open the MediaWiki support desk thread list."
  (interactive)
  (mediawiki-discussion-tools mediawiki-support-desk-page sitename))
```

## Implementation Phases

### Phase 1 — Thread listing (done)

- [x] Implement `mediawiki-discussion-tools--fetch-threads`
- [x] Call `discussiontoolspageinfo&threaditemsflags=activity|noreplies`, parse headings + counts + timestamps into thread records (lightweight, no reply trees)
- [x] Implement `mediawiki-discussion-tools-list-mode` with `tabulated-list`
- [x] Implement status derivation — `active`/`stale` from `latestReplyTimestamp`. `resolved` detection requires full thread HTML (Phase 2/4).
- [x] Implement priority sorting (without "needs attention" / "watching" — that requires the user's username in Phase 4)
- [x] Implement `mediawiki-support-desk` wrapper

### Phase 2 — Thread viewing

- Implement `mediawiki-discussion-tools-view-mode`
- Render thread from cached `discussiontoolspageinfo` data
- Navigate between threads with `n` / `p`

### Phase 3 — Posting

- Implement `mediawiki-discussion-tools-reply` (section append with `:`)
- Implement `mediawiki-discussion-tools-new-thread` (section=new)
- Handle edit token acquisition via `mediawiki-api.el`
- Handle OAuth authentication via `mediawiki-auth.el`

### Phase 4 — Status workflow

- Implement `mediawiki-discussion-tools-resolve` — prompts for optional summary, appends `{{Resolved|summary|~~~~}}`. The summary may reference a reply comment ID.
- Implement "needs attention" and "watching" priority tiers
- Detect user's own comments in threads
- Detect when someone replied after the user

### Phase 5 — Archive awareness

- Implement `mediawiki-discussion-tools--check-archived`
- Mark archived threads as `closed` in the list
- Search archive pages for thread IDs that disappeared from live page

## Configuration

### Generic module (`mediawiki-discussion-tools.el`)

```elisp
(defcustom mediawiki-discussion-tools-stale-days 14
  "Number of days before a thread is considered stale.
Should match the auto-archiving threshold on the wiki."
  :type 'integer
  :group 'mediawiki-discussion-tools)

(defcustom mediawiki-discussion-tools-max-threads nil
  "Maximum number of threads to display.  nil means no limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'mediawiki-discussion-tools)

(defcustom mediawiki-discussion-tools-signature "~~~~"
  "Signature string appended to replies and resolution comments.
The default \"~~~~\" expands to the user's standard MediaWiki
signature (timestamp + username link) when saved."
  :type 'string
  :group 'mediawiki-discussion-tools)
```

### Support desk wrapper (`mediawiki-support-desk.el`)

```elisp
(defcustom mediawiki-support-desk-page "Project:Support_desk"
  "Page name for the support desk on the wiki."
  :type 'string
  :group 'mediawiki-support-desk)
```

## Testing Protocol

Tests follow the ERT patterns used by the rest of the project (`cl-letf` mocks, macro fixtures, `should`/`should-not` assertions).  Run with `make test` (local) or `make ci-test` (CI).

### File layout

| File | What it tests |
|---|---|
| `tests/test-mediawiki-discussion-tools.el` | Core module: parsing, status, sorting, formatting, fetch pipeline |
| `tests/test-mediawiki-support-desk.el` | Wrapper: config defaults, delegation to discussion-tools |

### Unit Tests

Unit tests mock all network and I/O boundaries.  No API calls, no `auth-source`, no filesystem writes.

**API response parsing** (mock the JSON response)
- `discussiontoolspageinfo` response with `activity|noreplies` flags → correct thread records
- `discussiontoolspageinfo` response with full reply tree → correct comment nesting
- `action=parse&prop=tocdata` response → correct index-to-section-number mapping
- Empty page (no threads) → empty list, no error
- Malformed JSON response → graceful error, not a crash
- Missing fields (e.g., no `html` in heading) → default values, no crash

**Status derivation** (operates on thread records, no network)
- `latestReplyTimestamp` < stale-days old → `active`
- `latestReplyTimestamp` > stale-days old → `stale`
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

Integration tests use `cl-letf` to mock `url-retrieve-synchronously` at the HTTP level, exercising the full `mediawiki-api.el` → `mediawiki-discussion-tools.el` pipeline with realistic JSON payloads.

**Full list pipeline**
- Mock `discussiontoolspageinfo` → feed through `mediawiki-discussion-tools--fetch-threads` → verify `tabulated-list` entries have correct column values

**Full view pipeline**
- Mock `discussiontoolspageinfo` with reply tree → feed through thread-view buffer → verify indentation levels, author names, timestamps parsed

**Edit dispatch (mocked token + post)**
- Mock `action=query&meta=tokens` (CSRF token)
- Mock `action=edit&section=N&appendtext=...` (success response)
- Call `mediawiki-discussion-tools-reply` → verify correct URL, correct `appendtext` payload, correct summary

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
- Verify `mediawiki-discussion-tools--check-archived` returns `t`

### User Testing

These are manual tests performed against live wikis to verify real-world behaviour before release.

#### Generic discussion tools

**Setup**
1. Configure a site entry with OAuth credentials in `mediawiki-site-alist`
2. Set `mediawiki-discussion-tools-signature` to `~~~~` (default)
3. Open `M-x mediawiki-discussion-tools` and enter a discussion page — verify thread list populates

**Viewing**
4. Press `RET` on a multi-reply thread — verify all comments render with correct indentation
5. Press `RET` on a thread with zero replies — verify the opening post renders
6. Press `n` / `p` to navigate threads — verify boundary behaviour

**Posting**
7. Press `n` to create a new thread — type a title and body — verify it appears on the page
8. Press `r` on an existing thread — type a reply — verify it appears with correct indentation
9. Press `g` to refresh — verify the new content appears

**Resolving**
10. Press `d` on a thread — enter a summary — verify `{{Resolved|summary|~~~~}}` appears
11. Press `d` on a thread — leave summary empty — verify `{{Resolved|~~~~}}` appears
12. Refresh — verify status icon changes to `✓`

**Edge cases**
13. Interrupted network mid-fetch — verify graceful error, not a hang
14. Thread archived between list and view — verify "thread not found"
15. Very long thread titles — verify table column truncation
16. Unicode authors and titles — verify display without mojibake

#### Support desk wrapper

1. `M-x mediawiki-support-desk` — verify delegates correctly to the support desk page
2. Verify `mediawiki-support-desk-page` customization works with alternate pages

## Resolved Design Decisions

1. **Recovering section numbers** — Call `action=parse&prop=tocdata` once and cache the `index → section-number` mapping. Filter out non-discussion sections. The `tocdata` response includes both `index` (matches the `discussiontoolspageinfo` thread order) and `number` (the integer used by `action=edit&section=`).

2. **Single-thread re-fetch** — After posting a reply or marking resolved, re-fetch the entire `discussiontoolspageinfo` response. This is simpler than updating the local cache and verifies the edit posted correctly. The payload is small (~50KB for a typical page).

3. **{{Resolved}} detection** — Scan for the checkmark icon in the rendered HTML rather than wikitext. The template renders as `&#x2713;` or the tick image; searching HTML is more robust against variant wikitext forms (`{{Resolved}}`, `{{resolved}}`, `{{Resolved|...}}`, manual checkmark insertions).

4. **Module split** — Core logic lives in `mediawiki-discussion-tools.el` with the page as a runtime parameter. `mediawiki-support-desk.el` is a thin wrapper that sets the page to `"Project:Support_desk"`. This allows the same browser to work with any discussion page (talk pages, village pumps, noticeboards) without duplicating code.
