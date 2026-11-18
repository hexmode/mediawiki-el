# Discussion Tools Module: Implementation Plan

This document describes the plan for `mediawiki-discussion-tools.el`, a new generic module that provides an interactive discussion page browser backed by the MediaWiki DiscussionTools API.  `mediawiki-support-desk.el` is a thin wrapper that configures it for [Project:Support_desk](https://www.mediawiki.org/wiki/Project:Support_desk).

See [SUPPORT-DESK-WIKI-IMPLEMENTATION.md](./SUPPORT-DESK-WIKI-IMPLEMENTATION.md) for how the support desk page works on the wiki side (page structure, archiving, the `{{Resolved}}` template, and the DiscussionTools API).

User-facing documentation lives in [DISCUSSION-TOOLS.mediawiki](../DISCUSSION-TOOLS.mediawiki): how to open a page, read a thread, reply, resolve and search.  This file stays the design record — API calls, data model, phases, and what was deliberately dropped — and does not repeat the user instructions.

## Status (as of 4.1.1)

|Phase                |State                                                                                                                                                       |
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
|1 — Thread listing   |Implemented                                                                                                                                                 |
|2 — Thread viewing   |Implemented                                                                                                                                                 |
|3 — Posting          |Implemented: `r` replies (compose buffer, `C-c C-c` to post, `C-c C-k` to cancel) and `m` / `C` start a new thread                                          |
|4 — Status workflow  |Implemented: `d` marks a thread resolved, the list detects `{{Resolved}}`, and the needs-attention / watching tiers work when the logged-in user is known   |
|5 — Archive awareness|Reduced: no per-archive scanning. Threads that vanish on refresh are reported, and `s` searches the page and its archives in one prefixed `list=search` call|

Everything below is the design this was built from; where it disagrees with
the code, the code wins.

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

Thread data is populated from the `discussiontoolspageinfo` API response.  The list fetch uses `threaditemsflags=activity|noreplies` for a lightweight payload, except when the logged-in user is known: the needs-attention and watching tiers then need the reply tree, so the list fetch uses `activity|excludesignatures`.  The thread view re-fetches the page with full trees when the record has none.

### Thread record (alist used as struct)

|Key           |Source field                                      |Example                                                                         |
|--------------|--------------------------------------------------|--------------------------------------------------------------------------------|
|`id`          |`thread.id`                                       |`"h-Slgrandson-20260624025200"`                                                 |
|`title`       |`thread.html` (strip tags)                        |`"Is there an extension..."`                                                    |
|`author`      |`thread.oldestReply.author` (from `activity` flag)|`"Slgrandson"`                                                                  |
|`timestamp`   |`thread.oldestReply.timestamp`                    |`"2026-06-24T02:52:00Z"`                                                        |
|`reply-count` |`thread.commentCount`                             |`2`                                                                             |
|`author-count`|`thread.authorCount`                              |`2`                                                                             |
|`last-reply`  |`thread.latestReply` object (from `activity`)     |`{timestamp, author, id}`                                                       |
|`replies`     |`thread.replies`                                  |nested reply tree, or `nil` with `noreplies`                                    |
|`status`      |derived (see below)                               |`'needs-attention`, `'watching`, `'active`, `'unanswered`, `'resolved`, `'stale`|

### Status derivation

```
resolved?  ── does the section wikitext match "{{Resolved"?
  ├── yes ── status = 'resolved
  └── no
       ├── user posted, someone replied after ── status = 'needs-attention
       ├── user posted, nobody replied after  ── status = 'watching
       ├── commentCount <= 1                 ── status = 'unanswered
       └── last-reply > stale-days ago?
            ├── yes ── status = 'stale
            └── no  ── status = 'active
```

The two attention tiers need the logged-in user name (site configuration,
or one `action=query&meta=userinfo` call) and the recursive reply tree.
When the user is known, the list fetch uses
`threaditemsflags=activity|excludesignatures` instead of `noreplies`, so
per-comment authors are available; anonymous browsing keeps the
lightweight payload.

Resolved detection reads the page once per refresh with
`action=parse`, slices each section out of the wikitext by `byteoffset`,
and matches `{{Resolved` (any case, optional spaces) followed by `|` or
`}}`.  Scanning wikitext rather than rendered HTML keeps the lightweight
list payload intact: one extra call per refresh, no per-thread fetches.
On MediaWiki 1.43 and later the call uses `prop=wikitext|tocdata`
(`sections` is deprecated there); older wikis use `prop=wikitext|sections`.
See `mediawiki-site-at-least`.

### Priority sorting for list display

Threads are sorted by priority (highest first), using the tiers in
`mediawiki-discussion-tools--priority-tiers`:

1. **Needs attention** — the logged-in user posted a reply and someone else replied after them. The user hasn't replied to that latest message.
2. **Watching** — the logged-in user posted and nobody has replied after their last comment.
3. **Active** — any other thread with replies.
4. **Unanswered** — no replies at all (just the opening post).
5. **Resolved** — the section contains `{{Resolved}}`.
6. **Stale** — > stale-days since last reply.

`sort` is stable, so threads in the same tier keep the order the API
returned them in.

## API Calls

The module uses these API endpoints through the existing `mediawiki-api.el` infrastructure:

### 1. List threads

```
action=discussiontoolspageinfo
&page=<wiki page>
&prop=threaditemshtml
&format=json&formatversion=2
```

Returns all threads with metadata and rendered HTML.  Called on every open and refresh; there is no session cache, because the page changes underneath it.  The `replies` trees store the comment IDs used for reply threading, and are the source for the attention tiers when they are present.

### 2. View a single thread

The view is rendered from the thread record.  When the record has no `replies` tree (the anonymous list fetch), the view re-fetches the page with full trees rather than parsing one section:

```
action=discussiontoolspageinfo
&page=<wiki page>
&prop=threaditemshtml
&threaditemsflags=activity|excludesignatures
```

The response already includes per-comment HTML, so no `action=parse&prop=text` call is needed.

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

The indentation is not hard-coded.  It is computed from the section's wikitext — the reply nests one level deeper than the last indented comment, so `::` answers a `:` comment and `*:` keeps a vote reply inside its list item — and falls back to `:` when the section has no indented comment.

### 4. Create a new thread

```
action=edit
&title=<wiki page>
&section=new
&sectiontitle=Topic title
&text=Topic body ~~~~
&token=...
```

The body is composed in `*MW New Thread*` (`C-c C-c` posts, `C-c C-k` cancels), so nothing is published from a single-line prompt.  The signature is appended, and unlike a reply the body gets no indentation prefix — a new thread starts at the section level.

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

The implementation does not hard-code the single `:`.  It reuses the reply-indent logic, so the marker is appended under the thread's last comment at the same indentation a human would use.

### 6. Notice archived threads

No archive API call.  The list buffer remembers the thread IDs it showed
at the last refresh (`mediawiki-discussion-tools--known-ids`); IDs that
disappear are reported in the echo area:

```
Discussion refreshed (12 threads); 3 disappeared (probably archived —
s to search archives)
```

That answers the actual question ("where did it go?") for the cost of a
`cl-remove-if`, instead of fetching every `Archive N` page's
`discussiontoolspageinfo` to find one thread.  The vanished IDs stay in
`mediawiki-discussion-tools--vanished` for callers that want them.

### 7. Search threads

```
action=query
&list=search
&srsearch=SEARCH_TERMS prefix:<wiki page>
&srnamespace=*
&srlimit=50
```

One prefixed search covers the live page *and* its archive subpages,
because `Project:Support desk/Archive 24` shares the
`Project:Support desk` prefix.  `srnamespace` is passed as `*` rather
than left out: it defaults to `0`, so without it the search would only
look in the main namespace and a Project- or Talk-namespace page would
match nothing however well its prefix matches.  The full-title prefix
keeps `*` from pulling in unrelated pages, and the same code works for
talk pages (namespace 1) and project pages (namespace 4) without
guessing a namespace number.

An empty search term lists every subpage, which is the quickest way to
find the archives.  Results render in `*MW Discussion Search*` with
tags stripped and entities decoded; `RET` opens the page through
`mediawiki-open`.

### 8. API flags and optimisations

The `discussiontoolspageinfo` module accepts several flags on the `threaditemsflags` parameter, exposed as pipe-separated values:

|Flag               |Effect                                                                                                                                                      |
|-------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
|`activity`         |Adds `oldestReply` and `latestReply` objects to each thread (timestamp, author, type, level, id) — lightweight references without nested reply trees or HTML|
|`noreplies`        |Returns only top-level headings; omits the recursive `replies` tree entirely — fastest option for list views                                                |
|`excludesignatures`|Strips user signatures from the rendered HTML — cleaner output for view mode, easier author detection                                                       |

**Recommended call pattern:**

```
;; Anonymous list view: lightweight
action=discussiontoolspageinfo
&prop=threaditemshtml
&threaditemsflags=activity|noreplies

;; List view when the logged-in user is known: the attention tiers
;; need per-comment authors
action=discussiontoolspageinfo
&prop=threaditemshtml
&threaditemsflags=activity|excludesignatures

;; Thread view (re-fetch when the record has no reply tree)
action=discussiontoolspageinfo
&prop=threaditemshtml
&threaditemsflags=activity|excludesignatures
```

The `noreplies` payload keeps headings, counts and the oldest/latest reply references without any reply content — minimal bandwidth, perfect for anonymous browsing of the `tabulated-list` view.  The full payload adds the reply tree with cleaned HTML, which the view renders and the attention tiers scan.  Resolved detection adds one `action=parse&prop=wikitext|sections` call per refresh, never a per-thread fetch.

**Additional parameters:**

- `oldid=<revision>` — fetch thread structure as of a specific page revision; useful for detecting when a thread was archived (compare current vs. prior revision)
- `prop=transcludedfrom` — returns a map of thread/comment IDs to their transclusion source page (`false` = directly on the page).

**Caution:** The API is marked as "internal or unstable" by MediaWiki. While it has been stable in practice for years, any code relying on it should handle graceful degradation if the response shape changes.

## Minor Modes

### `mediawiki-discussion-tools-list-mode`

Derived from `tabulated-list-mode`.

**Columns:**

|Width|Header |Content                                                                                            |
|-----|-------|---------------------------------------------------------------------------------------------------|
|3    |(blank)|Status icon: `!` needs attention, `*` watching, `·` active, `?` unanswered, `~` stale, `✓` resolved|
|40   |Title  |Thread title (truncated)                                                                           |
|16   |Author |Opening poster's username                                                                          |
|5    |`#`    |Reply count                                                                                        |
|20   |Last   |Relative time since last reply (e.g., "2d ago")                                                    |

**Default sort:** by priority (see priority sorting above); there is no header
sort, `tabulated-list-sort-key` is nil.

**Keybindings:** `g` refresh, `RET` view, `n` / `p` next / previous,
`m` / `C` new thread, `r` reply, `d` resolve, `s` search.  The user-facing
documents, `DISCUSSION-TOOLS.mediawiki`, owns the keybinding tables; this plan
does not duplicate them.

**Window layout:** `RET` splits the frame and shows the thread view buffer
in the lower portion.  The view buffer is named `*MW Thread View*` and is
reused across all thread navigation — `n` and `p` (from either the list
or view buffer) replace its content with the next or previous thread.

### `mediawiki-discussion-tools-view-mode`

Read-only mode for viewing a single thread.  Uses a single reusable
buffer so `n`/`p` navigation is instant and doesn't create new windows.

**Display:** Each comment rendered with:
- Author + timestamp header line
- Body rendered by `shr` from the DiscussionTools HTML, so links are clickable, images are shown and lists keep their shape; falls back to stripping tags when Emacs has no libxml support
- Replies nested with increasing indentation

**Keybindings:** `r` reply, `e` edit the post at point, `m` / `C` new thread,
`d` resolve, `g` re-render,
`n` / `p` next / previous, `s` search, `&` browse the comment at point,
`C-c C-o` open the link at point, `TAB` / `S-TAB` next / previous link,
`RET` / `q` close.  Again, the tables live in `DISCUSSION-TOOLS.mediawiki`.

## Core Functions

```elisp
;; Thread data management
(defun mediawiki-discussion-tools--fetch-threads (sitename page &optional full)
  "Fetch thread data from PAGE on SITENAME using the DiscussionTools API.
If FULL is non-nil, fetch with full reply trees (no noreplies flag).
Reply trees are also fetched when the logged-in user is known.
Otherwise uses threaditemsflags=activity|noreplies for lightweight list.
Returns an alist of thread records sorted by priority.")

(defun mediawiki-discussion-tools--thread-status (thread &optional username)
  "Determine the status of THREAD for USERNAME.
Returns one of: 'needs-attention, 'watching, 'active, 'unanswered,
'stale.  'resolved is applied later by --mark-resolved.")

(defun mediawiki-discussion-tools--thread-priority (thread)
  "Return a numeric priority for THREAD (lower = higher priority).")

(defun mediawiki-discussion-tools--thread-contains-username-p (thread username)
  "Return t if USERNAME appears in any reply in THREAD.")

(defun mediawiki-discussion-tools--replied-after-username-p (thread username)
  "Return t if someone other than USERNAME posted after USERNAME did.")

(defun mediawiki-discussion-tools--current-user (sitename)
  "Return the logged-in user name for SITENAME, or nil.")

(defun mediawiki-discussion-tools--flatten-replies (replies)
  "Return REPLIES and their nested replies as a flat, depth-first list.")

(defun mediawiki-discussion-tools--resolved-anchors (sitename page)
  "Return the anchors of PAGE sections that contain a resolved marker.")

(defun mediawiki-discussion-tools--mark-resolved (threads anchors)
  "Set THREADS whose anchor is in ANCHORS to 'resolved status.")

(defun mediawiki-discussion-tools--vanished-ids (old-ids threads)
  "Return the IDs in OLD-IDS that are absent from THREADS.")

;; Entry point
(defun mediawiki-discussion-tools (page &optional sitename)
  "Open a discussion page thread list.
PAGE is the wiki page to browse.  SITENAME is the wiki site.
Displays threads sorted by priority.")

(defun mediawiki-discussion-tools-refresh ()
  "Re-fetch the thread list for the current discussion buffer.")

(defun mediawiki-discussion-tools-search (query)
  "Search the current discussion page and its archives for QUERY.")

;; Thread actions
(defun mediawiki-discussion-tools-view-thread-at-point ()
  "View the full thread at point in a dedicated buffer.")

(defun mediawiki-discussion-tools-reply ()
  "Compose a reply to the thread at point in a reply buffer.")

(defun mediawiki-discussion-tools-new-thread ()
  "Compose a new discussion thread in a reply buffer.")

(defun mediawiki-discussion-tools-resolve (&optional summary)
  "Mark the thread at point as resolved.
Prompts for an optional SUMMARY.  If provided, the resolution comment
includes the summary text followed by the signature.  The summary may
reference a specific reply comment (e.g. \"See reply above\") to link
the resolution to a particular answer.  If SUMMARY is empty, just
appends {{Resolved|~~~~}} with no additional text.  The marker is
indented to answer the thread's last comment.")
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
- [x] Implement status derivation — `active`/`stale` from `latestReplyTimestamp`; `resolved` detection arrived in Phase 4.
- [x] Implement priority sorting (the attention tiers arrived in Phase 4)
- [x] Implement `mediawiki-support-desk` wrapper

### Phase 2 — Thread viewing (done)

- [x] Implement `mediawiki-discussion-tools-view-mode` with reusable `*MW Thread View*` buffer
- [x] Render thread from cached or re-fetched `discussiontoolspageinfo` data
- [x] Split-window layout: list on top, thread view below via `display-buffer-below-selected`
- [x] Navigate between threads with `n` / `p` from either list or view buffer
- [x] Close view with `RET` or `q` in view buffer

### Phase 3 — Posting (done)

- [x] Implement `mediawiki-discussion-tools-reply` (indented section append, composed in `*MW Reply*`)
- [x] Implement `mediawiki-discussion-tools-new-thread` (section=new, composed in `*MW New Thread*`)
- [x] Both posting commands use a compose buffer (`C-c C-c` posts, `C-c C-k` cancels); new thread is on `m` / `C`, as in Gnus
- [x] Handle edit token acquisition via `mediawiki-api.el`
- [x] Handle OAuth authentication via `mediawiki-auth.el`

### Phase 4 — Status workflow

- [x] Implement `mediawiki-discussion-tools-resolve` — prompts for optional summary, appends `{{Resolved|summary|~~~~}}` at the thread's indentation level. The summary may reference a reply comment ID.
- [x] Detect `{{Resolved}}` for the list (`--resolved-anchors`, one wikitext fetch per refresh) and mark those threads `resolved`
- [x] Detect user's own comments in threads (`--thread-contains-username-p`)
- [x] Detect when someone replied after the user (`--replied-after-username-p`)
- [x] Implement "needs attention" and "watching" priority tiers.  These need the user's name (site config or `meta=userinfo`) and the reply tree, so the list fetch switches to `activity|excludesignatures` when the user is known.

### Phase 5 — Archive awareness (reduced)

The original design scanned every `Archive N` page's `discussiontoolspageinfo` to find one thread — O(archives) API calls against an unstable API, to locate a thread that is no longer actionable.  Dropped in favour of:

- [x] Remember the thread IDs shown at the last refresh (`--known-ids`) and report vanished ones: `N disappeared (probably archived — s to search archives)`
- [x] `s` (`mediawiki-discussion-tools-search`) covers the page and its archives with one prefixed `list=search` call
- [x] Open a result with `RET` via `mediawiki-open`
- [ ] Not doing: `--check-archived`, `closed` status for archived threads, per-archive thread lookup

## Dropped from the plan

These were part of the original design and are deliberately not implemented:

|Dropped                                                                                |Replacement                                                                                                                                                                                                          |
|---------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|`mediawiki-discussion-tools--check-archived` and a `closed` status for archived threads|An archived thread is not actionable — replying to it is exactly what archiving prevents. The list reports vanished IDs instead, and `s` searches the page and its archives.                                         |
|Scanning every `Archive N` page with `discussiontoolspageinfo` to find one thread      |O(archives) calls against an API MediaWiki labels "internal or unstable", to locate something the user can no longer act on.                                                                                         |
|`srnamespace=4` on search                                                              |It hard-coded the Project namespace, so talk-page archives would never match. The search passes `srnamespace=*` instead: the API defaults to namespace 0, and the full-title `prefix:` is what restricts the results.|
|Checkmark detection in rendered HTML (`&#x2713;`, `&#x2714;`, a bare `✔`)              |Rendered HTML is not in the lightweight payload, so this only worked after a per-thread fetch. Matching `{{Resolved` in the section wikitext costs one call per refresh.                                             |
|`action=parse&prop=text` for viewing a single thread                                   |The full-tree `discussiontoolspageinfo` re-fetch already returns per-comment HTML.                                                                                                                                   |
|Caching the `tocdata` index → section-number map for the session                       |Pages change under the client, and a stale map posts a reply into the wrong section without any error. The map is fetched when it is needed.                                                                         |
|The `oldid=` and `prop=transcludedfrom` parameters                                     |Documented for completeness and never used; transcluded threads would need separate handling before the extra query is worth it.                                                                                     |
|A separate "OP replied" active tier                                                    |Distinguishing it requires the reply tree on every fetch, including anonymous ones. Every active thread ranks as tier 3; the two tiers that change the answer (needs attention, watching) are implemented.           |

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
signature (timestamp + username link) when saved.  Set to a custom
string like \"--~~~~\" or a raw wikitext sig if preferred."
  :type 'string
  :group 'mediawiki-discussion-tools)

(defcustom mediawiki-discussion-tools-reply-separator "--- Reply below this line ---"
  "Separator line inserted in the reply buffer.
Everything above and including this line is discarded when posting.
The user's reply text starts on the line after the separator."
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

|File                                      |What it tests                                                    |
|------------------------------------------|-----------------------------------------------------------------|
|`tests/test-mediawiki-discussion-tools.el`|Core module: parsing, status, sorting, formatting, fetch pipeline|
|`tests/test-mediawiki-support-desk.el`    |Wrapper: config defaults, delegation to discussion-tools         |

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
- Thread with no `latestReplyTimestamp` (fallback) → `unanswered`
- User posted and someone answered afterwards → `needs-attention`
- User's comment is still the last one → `watching`
- No user name known → the attention tiers are not produced
- Section wikitext contains `{{Resolved|~~~~}}` → `resolved` (see below)

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

**Resolved detection (section wikitext)**
- A section whose byte range contains `{{Resolved}}` → anchor returned
- `{{resolved}}` and `{{ Resolved }}` variants → detected
- `{{Resolved|summary|~~~~}}` → detected
- A section with no marker → not returned
- A failing `action=parse` call → no anchors, list still builds
- Only the marked section is returned when the page has several threads

### Integration Tests

Integration tests use `cl-letf` to mock `mediawiki-api-call` and `mediawiki-site-get-token`, so the whole module runs without touching the network, while the API arguments each command sends are asserted directly.

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

**Vanished threads and search**
- Refresh with one thread removed from the response → `--vanished` holds its ID and the message mentions "disappeared"
- Mock `action=query&list=search` → verify the request is one prefixed search (`"QUERY prefix:PAGE"`) and that hits render with tags stripped and entities decoded
- Empty result set → "No matches" rather than a blank buffer

### User Testing

These are manual tests performed against live wikis to verify real-world behaviour before release.

#### Generic discussion tools

**Setup**
1. Configure a site entry with OAuth credentials in `mediawiki-site-alist`
2. Set `mediawiki-discussion-tools-signature` to `~~~~` (default)
3. Open `M-x mediawiki-discussion-tools` and enter a discussion page — verify thread list populates

**Viewing**
4. Press `RET` on a multi-reply thread — verify thread renders in a split window below
5. Verify the viewed thread's row is highlighted (`hl-line-mode`) in the list buffer
6. Press `RET` on a thread with zero replies — verify the opening post renders and highlight moves
7. Press `n` from the list buffer — verify view buffer shows next thread, highlight moves, row scrolled into view
8. Press `p` from the view buffer — verify same behaviour, smooth navigation, highlight follows
9. Press `n` at the last thread — verify minibuffer shows "Last thread", no change
10. Press `q` or `RET` in view buffer — verify view window closes, focus returns to list

**Posting**
11. Press `m` (or `C`) to create a new thread — type a title, then a body in the compose buffer — verify `C-c C-k` cancels and `C-c C-c` posts
12. Press `r` on an existing thread — type a reply — verify it appears with correct indentation
13. Press `g` to refresh — verify the new content appears

**Resolving**
14. Press `d` on a thread — enter a summary — verify `{{Resolved|summary|~~~~}}` appears
15. Press `d` on a thread — leave summary empty — verify `{{Resolved|~~~~}}` appears
16. Refresh — verify status icon changes to `✓`

**Searching and archiving**
17. Press `s` with a term that only appears in an archived thread — verify it is found without opening the archive page
18. Press `s` with an empty term — verify the archive subpages are listed
19. Press `RET` on a result — verify the page opens through `mediawiki-open`

**Edge cases**
20. Interrupted network mid-fetch — verify graceful error, not a hang
21. Let a thread age out or archive it by hand, then press `g` — verify the "disappeared (probably archived)" notice and that `s` finds it
22. Very long thread titles — verify table column truncation
23. Unicode authors and titles — verify display without mojibake

#### Support desk wrapper

1. `M-x mediawiki-support-desk` — verify delegates correctly to the support desk page
2. Verify `mediawiki-support-desk-page` customization works with alternate pages

## Resolved Design Decisions

1. **Recovering section numbers** — Call `action=parse&prop=tocdata` and filter out non-discussion sections. The response includes both `index` (matches the `discussiontoolspageinfo` thread order) and `number` (the integer used by `action=edit&section=`), and the code deliberately uses `index`. The map is fetched when a reply or resolve needs it rather than cached for the session, because an edit elsewhere changes the section numbers and a stale map posts silently into the wrong section.

2. **Single-thread re-fetch** — After posting a reply or marking resolved, re-fetch the entire `discussiontoolspageinfo` response. This is simpler than updating the local cache and verifies the edit posted correctly. The payload is small (~50KB for a typical page).

3. **{{Resolved}} detection** — The original plan was to scan rendered HTML for the checkmark (`&#x2713;`, `&#x2714;`, a bare `✔`). That was reversed: the lightweight list payload has no HTML at all, so HTML scanning would have forced a per-thread fetch. The page wikitext is fetched once per refresh with `prop=wikitext|sections`, each section is sliced out by its `byteoffset`, and the slice is matched against `{{` + optional spaces + `Resolved` (any case) + optional spaces + `|` or `}}`. That catches `{{Resolved}}`, `{{resolved}}`, `{{Resolved|...}}` and spacing variants, at one request per refresh.  A regexp using `\_>` was tried first and rejected: it depends on the syntax table, and `Resolved|` did not match under the standard one, which failed silently.

4. **Module split** — Core logic lives in `mediawiki-discussion-tools.el` with the page as a runtime parameter. `mediawiki-support-desk.el` is a thin wrapper that sets the page to `"Project:Support_desk"`. This allows the same browser to work with any discussion page (talk pages, village pumps, noticeboards) without duplicating code.

5. **Attention tiers versus payload size** — Needs-attention and watching need per-comment authors, which `threaditemsflags=noreplies` omits. Rather than fetch the list twice, the list fetch upgrades itself to `activity|excludesignatures` when the logged-in user is known (site configuration, or one cached `meta=userinfo` call). Anonymous browsing keeps the lightweight payload and simply has no attention tiers.

6. **Keybindings follow Gnus** — `m` / `C` compose, `r` replies, `n` / `p` move, `RET` opens, `q` quits, so discussion pages feel like a newsgroup buffer. `s` is search, which Gnus does not bind the same way, but it does not collide with the keys above.  See `CONTRIBUTING.mediawiki` for the convention.
