# Using mediawiki.el's discussion tools at the Wikipedia Village Pump

This is a practical guide to browsing and replying to
[Wikipedia:Village pump](https://en.wikipedia.org/wiki/Wikipedia:Village_pump)
with `mediawiki-discussion-tools`.  It records what was actually tested against
en.wikipedia.org on 2026-11-01 with mediawiki.el 4.1.1; nothing here is
speculative.

## TL;DR

Yes, it works — but open a **desk subpage**, not the Village pump index page.
`Wikipedia:Village pump` is a hub that transcludes the desks; it has two real
sections ("Archiving" and "See also") and is not a discussion page.

| Desk page | Threads at time of writing | Notes |
|---|---|---|
| `Wikipedia:Village pump (policy)` | 13 | |
| `Wikipedia:Village pump (proposals)` | 11 | |
| `Wikipedia:Village pump (technical)` | 25 | busiest desk; largest thread trees |
| `Wikipedia:Village pump (miscellaneous)` | 11 | |
| `Wikipedia:Village pump (idea lab)` | 15 | |
| `Wikipedia:Village pump (WMF)` | 12 | |
| `Wikipedia:Village pump (news)` | 0 | announcement board, not a discussion desk |

## One-time setup

1. Add en.wikipedia to `mediawiki-site-alist`.  Either
   `M-x mediawiki-oauth-setup-site` (preferred; register an owner-only consumer
   per [OAUTH-SETUP.org](OAUTH-SETUP.org)) or add the entry to your init file:

   ```elisp
   (add-to-list 'mediawiki-site-alist
                '("Wikipedia" "https://en.wikipedia.org/w/"
                  "YourUsername" ""))   ; password empty -> auth-source
   ```

2. `M-x mediawiki-site RET Wikipedia RET` once, so the CSRF token and OAuth
   token are in place.  Reads work anonymously; **posting requires the login**.

Village pump pages are only move-protected, so any logged-in account can post.
You do not need rollbacker or bot rights to reply.

## Browsing a desk

```
M-x mediawiki-discussion-tools RET Wikipedia:Village pump (technical) RET
```

That lists the desk's `== ... ==` sections as threads, one row each, sorted by
priority: unanswered first, then active, then stale (>14 days since the last
reply; change `mediawiki-discussion-tools-stale-days` to match the desk's
archiving age).

| Key | Command |
|---|---|
| `RET` | view the thread below the list |
| `n` / `p` | next / previous thread, updating the view |
| `r` | reply to the thread at point |
| `N` | start a new thread |
| `d` | mark resolved — **not implemented**, signals an error |
| `g` | refetch the list |

The list and view refresh from the API, so pressing `g` picks up other people's
edits.  Fetching a desk takes well under a second; the full reply tree for one
thread is fetched only when you view it.

## Replying

Press `r` on a thread (or in the view buffer).  A `*MW Reply*` buffer opens with
the thread title and a "Reply below this line" separator; type under it and
press `C-c C-c` (`C-c C-k` aborts).

The reply is appended to the thread's section with an indentation prefix taken
from the last comment in that section, plus one level — this is what makes the
desk usable rather than a pile of unrelated text:

| Last comment on the desk | Reply prefix |
|---|---|
| unindented prose | `:` |
| `: a reply` | `::` |
| `:: a reply to a reply` | `::: ` |
| `* '''Support'''` (RfC survey) | `*:` |

So replies to "Support"/"Oppose" votes stay inside the bulleted survey instead
of turning into stray indented paragraphs, and replies to nested comments land
at the right depth.  Your signature (`~~~~`, from
`mediawiki-discussion-tools-signature`) is appended on its own line, as
en.wikipedia requires.

The edit summary is `/* Thread title */ Reply`, which is the same convention the
WMF reply tool uses, so the section link works in watchlists and histories.

Replies need the section number, which is resolved from the page's table of
contents each time you press `r`.  If someone adds a section between your view
and your post, the tool still targets the section by its current number; when a
thread looks busy, press `g` first.

## Starting a new thread

`N` prompts for a title and body and posts with `action=edit&section=new`, which
gives you a normal `== Title ==` section.  It does not add the desk's
preloaded instructions or `{{subst:}}` templates some desks expect, so for a
formal RfC you may still prefer the desk's own "add topic" button.

## Known limitations

- **No resolving.**  `d` / `mediawiki-discussion-tools-resolve` is a stub
  (Phase 4 in [DISCUSSION-TOOLS-PLAN.md](DISCUSSION-TOOLS-PLAN.md)).  The Village
  pump does not use `{{resolved}}` much, so this rarely matters.
- **No watchlist / "someone replied to me" view.**
- **No per-comment reply.**  A reply is appended at the end of the section, not
  inline under the comment you are reading.  The indentation matches the last
  comment, so in a section with several parallel subthreads your reply joins
  the last one.  For long RfCs, quote the comment you answer.
- **Threads are `== ==` sections.**  Subsections (`=== Survey ===`) are not
  separate rows, and a desk page whose threads are not level-2 headings will
  show nothing.
- **Rate limits.**  Wikimedia throttles API traffic; `429` responses come back
  as plain text, not JSON.  mediawiki.el now quotes that text in the error
  instead of saying "there was an error parsing the result of the API call".
  Wait a minute (or authenticate, which raises the limit) and retry.
- **Large desks are large.**  `Wikipedia:Village pump (technical)` is around
  600 KB of JSON with reply trees; the view buffer is plain text, but expect a
  visible pause on slow connections.

## Suggested workflow

1. `M-x mediawiki-discussion-tools RET Wikipedia:Village pump (technical) RET`
2. `n` / `p` through the unanswered threads.
3. `r`, write, `C-c C-c`.
4. `g` to refresh and confirm it appears with your signature.
5. `M-x mediawiki-history` on the desk page if you want to see exactly what your
   edit added.
