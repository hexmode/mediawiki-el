# Recent pages: a persistent list of edited pages and discussion desks

Status: proposal, not implemented.

## 1. Problem

`mediawiki-open` only works when `mediawiki-site` is already set — that is, from
inside a mediawiki buffer or after `M-x mediawiki-site`.  Its suggestion list,
`mediawiki-page-history`, is per-site, in memory, and gone when Emacs exits.
`mediawiki-discussion-tools` has the same problem from the other end: the page
and the site both have to be retyped every time.

The pages a person works on are stable from day to day.  A small file outside
Customize, holding the pages and desks they have used together with the site
each belongs to, makes both commands usable from a plain `*scratch*` buffer.

## 2. Goals

1. A persistent, human-readable list of **pages you have edited** and of
   **discussion pages** you have opened, each recorded with its **site name**
   so the site's configuration (and therefore its authentication) is reused.
   A page counts only once a save succeeds; browsing does not fill the list.
2. `M-x mediawiki-open` outside a mediawiki buffer offers the remembered pages;
   choosing one opens it on that entry's site.
3. `M-x mediawiki-discussion-tools` outside a mediawiki buffer offers the
   remembered discussion pages, with the site filled in automatically.
4. Nothing secret is written: no passwords, tokens or client secrets, only site
   names and page titles.
5. The file is plain Lisp data, safe to read, grep and edit by hand.

## 3. Non-goals

* Not a bookmark manager: no names, tags, folders or synchronisation.
* Not a replacement for the in-memory `mediawiki-page-history` or the
  watchlist; it sits alongside them.
* Not a record of every page ever fetched (watchlist diffs, revision browsing).
  What counts as "used" is spelled out in §6.

## 4. Data model and file format

New module: **`mediawiki-recent.el`** (the name `mediawiki-history.el` is taken
by the revision-history browser).

File: `mediawiki-recent-file`, default `~/.emacs.d/mw-hist.el`.  It contains a
comment header and **one Lisp datum**, read with `read` from the buffer — not
`load`, so the file can never execute code.

```elisp
;;; mw-hist.el --- pages used by mediawiki.el  -*- mode: emacs-lisp; -*-
;; Written by mediawiki-recent.el; safe to edit by hand.

(:version 1
 :pages
 ((:site "Wikipedia" :title "Emacs" :used 1793558400.0 :count 3)
  (:site "mediawiki.org" :title "Project:Support desk" :used 1793551200.0 :count 7))
 :discussions
 ((:site "Wikipedia" :title "Wikipedia:Village pump (policy)"
   :used 1793558400.0 :count 12)
  (:site "mediawiki.org" :title "Project:Support desk"
   :used 1793472000.0 :count 5)))
```

Entry fields:

| Field | Meaning |
|---|---|
| `:site` | A key in `mediawiki-site-alist`; how authentication is found again |
| `:title` | The page title exactly as MediaWiki knows it (spaces, not underscores) |
| `:used` | Seconds since the epoch (`float-time`), for ordering and ageing |
| `:count` | How many times the page has been opened; kept for ordering experiments |

Rules:

* The identity of an entry is `(site . title)`; titles compare
  case-insensitively on the first character, as MediaWiki does, and exactly
  afterwards.
* Lists are kept newest-first by `:used`; `:count` is incremented on each use.
* Each list is capped at `mediawiki-recent-max-entries` (default 100), oldest
  first out.
* `:version` is checked on load.  Unknown versions are treated as a load error
  (§9) rather than guessed at.

## 5. Module API

```elisp
;;; Reading
(defun mediawiki-recent-entries (kind)
  "Return the entries for KIND, a symbol: `pages' or `discussions'.")

(defun mediawiki-recent-read (kind &optional sitename default extra)
  "Prompt for an entry of KIND, preferring SITENAME when non-nil.
DEFAULT is preselected when it names an entry; EXTRA is a list of
site-local titles to offer without storing them (`mediawiki-page-history').
Always returns (SITE . TITLE); SITE is nil when TITLE was typed rather
than chosen, so the caller can ask for a site.")

;;; Writing
(defun mediawiki-recent-record (kind sitename title)
  "Record that TITLE on SITENAME was used with KIND, then save.")

(defun mediawiki-recent-forget (kind sitename title)
  "Remove one entry of KIND.")
(defun mediawiki-recent-clear (&optional kind)
  "Remove all entries, or just KIND's.")
(defun mediawiki-recent-save ()
  "Write the file atomically.")
(defun mediawiki-recent-reload ()
  "Re-read the file, discarding in-memory changes.")
```

Internals: `mediawiki-recent--data` (the plist from the file),
`--loaded`, `--dirty`, `--load`, `--read-file`, `--quarantine`,
`--valid-p`, `--ordered`, `--label`, `--age` and `--completion-table`.

Loading is lazy: the first call to `mediawiki-recent-entries` (or any write)
loads the file.  `kill-emacs-hook` saves only if the data is dirty, so a
session that never uses the feature never touches the disk.

## 6. When entries are recorded

| Event | Kind | Where the call goes |
|---|---|---|
| A page is saved successfully | `pages` | `mediawiki-save-page`, after the `action=edit` call returns, next to its "Saved …" message |
| A desk is opened with `mediawiki-discussion-tools` | `discussions` | `mediawiki-discussion-tools`, after the thread list is built successfully |

Both hooks sit after success, so a failed save, an edit conflict that ends in an
error, or a desk whose fetch failed adds nothing.  `mediawiki-add-page-history`
is deliberately *not* the hook: it runs for every `mediawiki-edit`, which would
record pages merely looked at through the watchlist, a diff or the history
browser.  That keeps the two histories distinct — `mediawiki-page-history`
remembers everything opened in this session, the file remembers what you edited.

`mediawiki-save-page` is the single funnel: `mediawiki-save`,
`mediawiki-save-as` and `mediawiki-save-on` all end up there, so no save path is
missed.

## 7. Interactive behaviour

### 7.1 `mediawiki-open`

Signature grows an optional site: `(mediawiki-open name &optional sitename)`.

Interactive spec:

1. **Inside a mediawiki buffer** (`mediawiki-site` set and the buffer is in a
   mediawiki mode): this site's entries first, then the other sites' entries
   annotated with their site (`Wikipedia: Village pump`), most recent first
   within each group.  The page at point stays the default, and
   `mediawiki-page-history` still supplies the site-local in-memory names.
   Choosing an entry from another site switches `mediawiki-site` to that site
   and says so in the echo area; no confirmation is asked.
2. **Anywhere else**: `mediawiki-recent-read 'pages` over all sites, the most
   recent entry first, each line annotated with its site and age.
3. In both cases, typing a page that is not in the list falls through to
   `mediawiki-prompt-for-site`, defaulting to the site of the most recent
   entry, and the pair is recorded if the save succeeds.
4. `C-g` does nothing at all.

Choosing an entry sets `mediawiki-site` to that entry's site before
`mediawiki-edit` runs, so the right configuration and credentials are used.

### 7.2 `mediawiki-discussion-tools`

Signature is already `(page &optional sitename)`.

Interactive spec:

1. **Inside a discussion buffer**: this site's desks first, then the other
   sites', exactly as for `mediawiki-open`.
2. **Anywhere else**: `mediawiki-recent-read 'discussions`, most recent first.
   Choosing an entry passes its site as SITENAME, so authentication is reused.
   A new page falls through to the site prompt, defaulting to the site of the
   most recent discussion entry.

The list is recorded after the first successful fetch, so a typo or a page
without threads does not pollute it.

## 8. Configuration

All in the existing `mediawiki` group.

```elisp
(defcustom mediawiki-recent-file
  (expand-file-name "mw-hist.el" user-emacs-directory)
  "File where mediawiki.el keeps the pages you have used."
  :type 'file :group 'mediawiki)

(defcustom mediawiki-recent-max-entries 100
  "How many entries to keep in each list."
  :type 'integer :group 'mediawiki)

(defcustom mediawiki-recent-record-pages t
  "Whether to remember pages opened for editing or reading."
  :type 'boolean :group 'mediawiki)

(defcustom mediawiki-recent-record-discussions t
  "Whether to remember desks opened with the discussion browser."
  :type 'boolean :group 'mediawiki)
```

`mediawiki-recent-file` may be set to a path inside a dotfiles repository; the
file is written atomically (temp file plus rename) and with mode 0600, since
page titles can say something about the person reading them even though no
credentials are stored.

## 9. Failure modes

| Situation | Behaviour |
|---|---|
| File does not exist | Empty lists; the file is created on the first save |
| File unreadable or contains something other than the expected plist | `warn`, rename it to `mw-hist.el.bak` without parsing it, and continue with empty lists, writing a fresh file on the next record.  If `mw-hist.el.bak` already exists, it becomes `mw-hist.el.bak.1`, then `.bak.2`, and so on, so an earlier rescue is not lost |
| `:version` newer than supported | Treated as unreadable: the newer file is renamed to `.bak` rather than parsed or overwritten in place |
| Directory unwritable | `warn` once per session; the feature keeps working in memory |
| Entry's site is no longer in `mediawiki-site-alist` | The entry is still offered but choosing it signals `user-error "Site X is no longer configured; add it to mediawiki-site-alist"` |
| Two Emacsen writing at once | Last writer wins; the atomic rename guarantees the file is never half-written |

## 10. Testing

Unit tests, no network:

* **Round trip** — record two pages and a desk, save to a temporary
  `mediawiki-recent-file`, reload in a fresh state, entries come back in
  recency order with `:count` intact.
* **Dedupe** — recording the same `(site . title)` twice leaves one entry,
  moves it to the front and bumps `:count`; first-letter case differences
  collapse (`emacs` vs `Emacs`), the rest do not.
* **Cap** — recording `max + 5` entries keeps `max`, dropping the oldest.
* **Dirty flag** — no file is written when nothing was recorded, even with the
  kill hook installed.
* **Corrupt file** — invalid Lisp, a wrong version and an unexpected shape each
  warn, rename the file to `mw-hist.el.bak` (numbered when a backup already
  exists) and leave a working empty store behind.
* **Atomic write** — the temp file is in the same directory and gone after the
  save; the file is valid Lisp and mode 0600.
* **Prompting** — `mediawiki-recent-read` offers only the requested kind,
  annotates age, returns `(site . title)` for a choice and nil for new input
  (mock `completing-read`).
* **Local first** — inside a mediawiki buffer the current site's entries come
  before other sites', and entries from other sites are annotated with their
  site.
* **`mediawiki-open` outside a buffer** — choosing a history entry sets
  `mediawiki-site` to that entry's site before `mediawiki-edit` (mock
  `mediawiki-edit` and `completing-read`); a new page prompts for a site.
* **`mediawiki-discussion-tools`** — choosing an entry passes its site as
  SITENAME.
* **Unknown site** — choosing an entry whose site has been removed signals a
  `user-error` naming the site.
* **Recorded on save only** — opening a page (including through the watchlist,
  a diff or the history browser) records nothing; a successful
  `mediawiki-save-page` records it once.
* **No recording on failure** — a failed save, or a `mediawiki-discussion-tools`
  call that errors before the thread list is built, adds nothing.

## 11. Interaction with the existing history

`mediawiki-page-history` stays: it is the in-memory, per-site list that feeds
`read-string` defaults inside mediawiki buffers, and other code reads it.  It
records every page opened this session, while the file records only successful
saves, so the two overlap without being the same list.  Each keeps its own
purpose: session history for the prompt inside a wiki buffer, durable history
for opening a page from anywhere.

## 12. Decisions

Recorded answers to the questions this spec left open:

1. **Pages** — only pages actually edited.  Recorded from `mediawiki-save-page`
   on success, so watchlist, diff and history browsing add nothing.
2. **Inside a mediawiki buffer** — the current site's entries first, then other
   sites' entries annotated with their site name.
3. **Discussions** — anything opened with `mediawiki-discussion-tools`.
   Individual threads are not stored.
4. **Ordering** — plain recency; `:count` is still kept for a future frecency
   option.
5. **File mode** — 0600.
6. **Corrupt or newer file** — warn and rename to `mw-hist.el.bak` (numbered
   if a backup already exists), then carry on with a fresh list.
7. **Size** — one knob, `mediawiki-recent-max-entries` (default 100), applies
   to both lists.
