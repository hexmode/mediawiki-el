# Agent Instructions

Instructions for AI coding agents (Claude Code, OpenCode, Codex, Cursor, …)
working on mediawiki-el, an Emacs package for reading and editing MediaWiki
wikis.

This is the single source of truth for agent instructions.  There is no
`CLAUDE.md` any more — everything it used to say lives here.  Do not add
per-tool instruction files; update this one instead.

User-facing documentation lives in `README.mediawiki` (features and usage),
`CONTRIBUTING.mediawiki` (build, architecture, keybinding conventions) and
`DISCUSSION-TOOLS.mediawiki` (how to use the discussion browser).  When a change
alters user-visible behaviour, update those files too.

## Build & Test

Only Emacs (>= 28.1) is required.  Everything goes through the `Makefile`:

```bash
make ci-test                            # every suite; exits non-zero if any fails
make test                               # same suites, keeps going after failures
make autoloads                          # regenerate mediawiki-autoloads.el and byte-compile
make autoloads-only                     # regenerate mediawiki-autoloads.el only
make check-file FILE=mediawiki-api.el   # paren check a single file (misleading positions)
make check-parens                       # unbalanced-paren check that names the opening line
make check-parens FILE=mediawiki-api.el # ... for one file
make editorconfig                       # strip trailing whitespace, add final newlines
```

Rules:

- **Run `make check-parens` after editing any `.el` file, before `make ci-test`.**
  `check-parens' and the byte compiler report the point where they gave up, not
  the form that was left open, so a missing `)` at the end of a `defun` looks
  like a bogus error at the start of the next one.  `tools/check-parens.el`
  walks the buffer with the Lisp reader and prints the line of the innermost
  unclosed form; pre-commit runs it for you (`make setup-dev` installs the
  hook), and `make ci-test` depends on it.
- Run `make ci-test` before pushing.  Never push a red suite.
- A compiled `.elc` wins over a newer `.el` unless `load-prefer-newer` is set,
  so once a file has been byte-compiled the suites keep running the old code
  after you edit it.  Rerun `make autoloads` (or `make clean`) first; otherwise
  a fix looks like it had no effect.
- Tests must not touch the network: mock `mediawiki-api-call`, `url-http-post`
  or `url-retrieve`.
- Tests must not depend on today's date.  Build fixture timestamps relative to
  `(current-time)`; hardcoded dates silently expire (this bit us when fixtures
  pinned to June 2026 aged past `mediawiki-discussion-tools-stale-days`).
- CI (`.github/workflows/test.yml`) runs every suite on Emacs 28.1, 29.4 and
  30.1, built from source into `/opt/emacs-<version>` and cached under
  `emacs-<version>-nativecomp-opt-<os>`.  The first run under a new cache key
  rebuilds Emacs (roughly four minutes per leg); bump the key whenever the
  install prefix or the `configure` flags change.

## Architecture

- `mediawiki.el` — package entry point and metadata (`;; Version:` lives here).
- `mediawiki-core.el` — shared groups, `mediawiki-version`, compatibility shims.
- `mediawiki-site.el` — site configuration (`mediawiki-site-alist`, URLs, credentials).
- `mediawiki-http.el` — HTTP layer on `url.el` (`url-http-get` / `url-http-post`).
- `mediawiki-api.el` — API calls and JSON parsing.  This is the boundary where
  parameters are normalized (`mediawiki-api--stringify-args`), so callers may
  pass integers for revision IDs, limits, etc.
- `mediawiki-auth.el`, `mediawiki-oauth.el` — `action=clientlogin` login and OAuth 2.0.
- `mediawiki-mode.el`, `mediawiki-page.el`, `mediawiki-draft.el` — editing buffers.
- `mediawiki-history.el`, `mediawiki-diff.el` — revision history browser and diffs.
- `mediawiki-watchlist.el`, `mediawiki-user.el`, `mediawiki-completion.el`,
  `mediawiki-discussion-tools.el`, `mediawiki-support-desk.el` — feature modules.
- `mediawiki-autoloads.el` — generated but committed.
- `tests/test-<module>.el` — one ERT suite per module; run by `make ci-test`.

## Conventions

- Every file keeps `-*- lexical-binding: t; -*-`, the GPLv3+ header with the
  current copyright years, and ends with `(provide '…)` plus a `;;; … ends here`
  line.
- Docstrings for every public defun/defvar/defcustom; user options go in a
  `defgroup`.
- Keybindings follow Gnus where a command has a Gnus equivalent (`m`/`C`
  compose, `r` reply, `n`/`p` move, `RET` open, `q` quit).  Check the
  "Keybinding Conventions" section of `CONTRIBUTING.mediawiki` before adding a
  key, and keep that section current when you do.
- `ChangeLog` is maintained by hand: `YYYY-MM-DD  Name  <email>` followed by
  tab-indented `* file (function):` entries; mention `Fixes #N` for issues.
- Commits are sentence-case subject lines describing the bug, with a body that
  explains cause and fix.

## Non-Interactive Shell Commands

**ALWAYS use non-interactive flags** with file operations to avoid hanging on
confirmation prompts.  `cp`, `mv` and `rm` may be aliased to `-i` on some
systems, which blocks forever waiting for y/n.

```bash
cp -f source dest           # NOT: cp source dest
mv -f source dest           # NOT: mv source dest
rm -f file                  # NOT: rm file
rm -rf directory            # NOT: rm -r directory
cp -rf source dest          # NOT: cp -r source dest
```

Other commands that may prompt:

- `scp` — use `-o BatchMode=yes`
- `ssh` — use `-o BatchMode=yes` so it fails instead of prompting
- `apt-get` — use `-y`
- `brew` — set `HOMEBREW_NO_AUTO_UPDATE=1`

## Release Checklist

Follow these steps in order.  Do not tag anything until every fix is on
`master` and the suite is green — tags are what package archives consume, and a
tag cannot be fixed without deleting it from the remote.

1. **Land the work.** Pull requests merge into `master` with a merge commit
   (`gh pr merge <n> --merge --delete-branch`); the history in this repo uses
   merge commits, not squash or rebase.
2. **Verify green.** `git checkout master && git pull`, then `make ci-test`
   locally and confirm the `Tests` workflow for the tip of `master` is green.
3. **Pick the version** (semver): bug fixes bump the patch level
   (`4.1.0` → `4.1.1`), new user-visible features bump the minor level,
   incompatible changes bump the major level.
4. **Bump the version in both places** — they must match:
   - `mediawiki.el`: `;; Version: X.Y.Z`
   - `mediawiki-core.el`: `(defconst mediawiki-version "X.Y.Z" …)` — this one is
     sent in the HTTP `User-Agent`, so a stale value is visible to servers.
5. **Update `ChangeLog`** with a new dated entry at the top, in the existing
   format, listing each file and function that changed, with `Fixes #N` where
   an issue exists.  Include the `Bump version to X.Y.Z` line under
   `mediawiki.el`.
6. **Commit** on `master`: subject `Bump version to X.Y.Z`, body summarising
   what the release fixes.
7. **Tag** with an annotated, `v`-prefixed tag:
   ```bash
   git tag -a vX.Y.Z -m "mediawiki-el X.Y.Z

   <one-paragraph summary of the release>"
   ```
8. **Push both the branch and the tag**:
   ```bash
   git push origin master
   git push origin vX.Y.Z
   ```
9. **Verify the release state**:
   ```bash
   git ls-remote --tags origin | grep vX.Y.Z   # must show refs/tags/vX.Y.Z and refs/tags/vX.Y.Z^{}
   git status                                  # must be clean and up to date with origin
   ```
   and confirm the `Tests` workflow for the new `master` tip is green.
10. **Close the beads issues** for the released work and sync:
    ```bash
    bd close <id> --reason="Fixed in <sha>, released in vX.Y.Z"
    bd dolt push
    ```
11. **Know who consumes what.** MELPA reads `;; Version:` from `master`, so the
    bump is what triggers it; GNU ELPA and NonGNU ELPA consume the `vX.Y.Z` tag.
    Nothing is uploaded by hand, and repository release-notes files are not
    required.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:7510c1e2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->
