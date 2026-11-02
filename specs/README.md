# Design records

Implementation plans and specifications for mediawiki.el.  These describe how a
feature is meant to work and why; the user-facing documentation lives in
`README.mediawiki`, `CONTRIBUTING.mediawiki` and `DISCUSSION-TOOLS.mediawiki`.

| Document | What it covers | State |
|---|---|---|
| [DISCUSSION-TOOLS-PLAN.md](DISCUSSION-TOOLS-PLAN.md) | The discussion page browser: data model, API calls, priority tiers, what was dropped, and every design decision behind it | Implemented in 4.1.1 |
| [SUPPORT-DESK-WIKI-IMPLEMENTATION.md](SUPPORT-DESK-WIKI-IMPLEMENTATION.md) | How the mediawiki.org support desk is built on the wiki side: header transclusion, ArchiverBot configuration, `{{Resolved}}`, and the DiscussionTools API | Reference |
| [RECENT-PAGES-SPEC.md](RECENT-PAGES-SPEC.md) | A persistent list of edited pages and opened discussion desks (`mw-hist.el`), and how `mediawiki-open` and `mediawiki-discussion-tools` prompt from it | Implemented |
