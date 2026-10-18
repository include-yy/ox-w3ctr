# AGENTS.md

Guidance for AI agents working in this repository.

## What this is

`ox-w3ctr` is an Emacs Lisp package: an Org export back-end that emits HTML
styled for W3C Technical Reports.  It is a "parasitic implementation" of
Org's `ox-html.el`, being progressively reimplemented (refactored) in its own
style.  Version 0.2.19; requires Emacs 31.

- `ox-w3ctr.el`       — the back-end (main source)
- `ox-w3ctr-tests.el` — ERT test suite
- `assets/`           — CSS / SVG / JS
- `jstools/`          — Node.js RPC helper: MathJax (math) and Shiki
                        (code highlighting)
- `doc/`              — `design-notes.md` (rationale and measured facts)
                        and `fontify.org` (fontify pipeline)
- `scratch/`          — throwaway files and local build outputs, never
                        the system temp (gitignored)
- `zhua.el`           — refactor proposals (gitignored)
- `.agents/`          — skills (committed) + env.md, next.md (gitignored)

Scratch and temporary files go in `scratch/` (gitignored), never
the system temp directory (`$TMPDIR` / `%TEMP%`).  Create it if absent.
Loose files are work-in-progress; once a scratch file outlives its
task, file it under the matching `_`-prefixed directory (the
underscore sorts them ahead of the loose files) or delete it:

- `_benchmark/` — bench/profile scripts and their output
- `_demos/`     — export-effect demos (Org input + rendered HTML)
- `_mut/`       — the mutation harness (`run-mutation.sh` and `mut*/`)
- `_plans/`     — design notes
- `_probes/`    — behaviour probes and debug scripts
- `_testquality/` — work files for the test-quality skill (seeded defects, samples)
- `_vendor/`    — third-party clones and reference sources

`scratch/build/` is the verify skill's build output, recreated by the
corpus harness on every run; do not file it under a `_` directory.

`.agents/next.md` is the handoff note from the previous session.  In a
new conversation, read it early, but treat it as possibly outdated:
check it against the current state before relying on it.

## Environment

Machine-specific values (Emacs, Python, Org sources, proxy, remotes,
Node) are auto-detected into `.agents/env.md` (gitignored).  On session
start, read it; when it is absent or older than 24 hours, re-run the
probes and rewrite it.  When a probe disagrees with a fresh file, report
the mismatch and let the user decide, rather than overwriting silently.

Only the env-detect skill writes `.agents/env.md`; a skill that changes
the environment (python-env) re-runs it instead of editing the file.
Project-level facts (a venv, its dependencies) live in project files
(`AGENTS.md`, plus `requirements.txt` when dependencies exist), never
in env.md.

Python commands run against the project venv, never the system
interpreter — the system Python is reserved for the env-detect probe.
The venv is a project fact: the python-env skill creates it.  There
are currently no Python dependencies; when one appears it is pinned in
`requirements.txt`, which the skill creates on demand.  Find the venv
python at `.venv/Scripts/python.exe` (Windows) or `.venv/bin/python`
(POSIX).

Invariants the detected values must satisfy:

- Emacs is a build of version >= 31 (the package's minimum).
- Python is 3.x and actually runs; `python3` can resolve to the
  Windows Store stub (exit code 49) and is broken.
- Org sources contain `ox-html.el`, `ox.el` and `org-element.el`
  (the reference for ports): the Org bundled with Emacs
  (`share/emacs/<version>/lisp/org`) by default, or a checkout via
  `ORG_SOURCES`.

Paths and commands below are bash-style.

pi's settings and sessions live in the Windows profile
(`C:\Users\<user>`); reach it with `"$USERPROFILE"` or
`cygpath -u "$USERPROFILE"`.

**Never search inside `node_modules`.**  No recursive `grep`/`find`/`rg`
over `node_modules` (including the pi install tree or any package's
`node_modules/`): it is enormous and the search hangs the shell.  Read
the specific documented file by path instead (e.g. under the pi
`docs/` directory) — never discover it with a recursive scan.

## Git

Remotes and how to reach them live in `.agents/env.md` (this checkout's
configuration); a GitHub-only clone has one remote and no proxy.

- Releases: bump `Package-Version` (header) and `t-version` together,
  commit, then tag `vX.Y.Z` (lightweight, matching `v0.2.5`) and push
  the branch and the tag to the remotes listed in `.agents/env.md`.
  The OINFO cache ships on: do **not** turn `org-w3ctr-oinfo-enabled`
  off for a release (`t-oinfo-enabled` says why).

## Running the tests

From the repo root (Emacs path from `.agents/env.md`):

```bash
"<emacs>" --batch -L . --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))" -l ox-w3ctr-tests.el -f ert-run-tests-batch-and-exit
```

Three gotchas:

- `load-prefer-newer t` — a stale `ox-w3ctr.elc` exists; without this,
  `load` picks the compiled file over the source.
- `system-time-locale` must be C/en; otherwise `%a` localizes day names and
  6 timestamp tests fail (e.g. `Fri` becomes a GBK-encoded Chinese string).
- `text-quoting-style` must be `grave` (the test file sets it): Org's error
  messages quote with backticks, and under `curve` (the interactive default)
  they become curved quotes, so the tests that assert them (e.g.
  `org-w3ctr--priority`) fail to match.

The acceptance criterion is **zero unexpected failures**, not a fixed
pass count — the suite keeps growing, so a pinned number only drifts.
In the shipped (cache) build the only skip is
`org-w3ctr--oinfo-plain-flavor`, which runs only when
`org-w3ctr-oinfo-enabled` is nil.  Run the cache build; a nil build is
for measuring, not a configuration to maintain (it skips the cache-path
tests and runs the plain-flavor test instead).

Four tests read `ox-w3ctr.el` next to the loaded file and skip without
it (`org-w3ctr--oinfo-props-are-looked-up`,
`org-w3ctr--oinfo-props-go-through-pget`, `org-w3ctr--load-file`,
`org-w3ctr-check-backend-transcoder-order`).
`org-w3ctr--jstools-tokens-live` runs the real Node helper and skips
without `node` or `jstools/node_modules`.

The Node helper has its own tests (they need `npm ci` first):

```bash
cd jstools && npm test
```

## Conventions

- Symbols in `ox-w3ctr.el` use the shorthand `t-` for `org-w3ctr-`
  (`read-symbol-shorthands`).  `zhua.el` must declare the same shorthand or
  its symbols will not shadow the package ones.
- **Docstrings and comments are string literals**, not read as Lisp, so
  `read-symbol-shorthands` does not apply.  Write every `t-*` / `t--*`
  symbol in them as its full `org-w3ctr-*` / `org-w3ctr--*` name.
- A refactored function has: a full docstring, `(declare (ftype ...))`,
  `(important-return-value t)` / `(pure t)` where applicable, uses the
  `t--*` helpers, reads and writes INFO through `t--pget` / `t--pput`
  (never `plist-get` / `plist-put`), and has ERT tests.
- Workflow: write proposals to `zhua.el`, review in Emacs, then merge into
  `ox-w3ctr.el`.  `zhua.el` is gitignored — do not commit it.
- **LF line endings.**  Any script or tool that rewrites a source file
  must write LF (`\n`), never CRLF.  A Windows Python `write_text`
  silently converts to CRLF and breaks multi-line string literals
  (navbar / footnote tests then fail).  After a rewrite, count CR
  bytes:

  ```bash
  tr -cd '\r' < ox-w3ctr.el | wc -c   # expect 0
  ```

  A fresh clone under Git for Windows inherits the system
  `core.autocrlf=true` and checks every file out as CRLF, which
  fails 9 tests before any change.  Set `git config core.autocrlf
  false` in the clone and convert the working tree back to LF.

- Do not commit changes unless explicitly asked.
- **Docstring**: every `defun`/`defsubst` gets a full docstring — a
  one-line summary first, then parameter / return-value notes where
  they are non-obvious.
- **Test docstrings**: a test named after a function opens with
  ``Tests for `org-w3ctr-\u2026'.`` and one line only -- extra explanation
  goes to body comments.  A test named otherwise (variant, property)
  is free-form.  The suite's own harness
  (`org-w3ctr-check-element-values` and its helpers) is exempt: its
  tests are documentation for the test infrastructure and may explain
  in the docstring.
- **Declarations**: refactored functions carry
  `(declare (ftype (function (ARGS) RET)))`; add
  `(important-return-value t)` where the caller must use the result;
  add `(pure t)` where the function is side-effect free and its result
  depends only on its arguments, and it signals no errors (constant
  folding would raise them at compile time).  Exemptions: `defsubst`, end-user
  commands (`t-export-*`, `t-publish-*`, `t-convert-*`), interactive
  commands whose return value is incidental.  `nil` is a subtype of
  both `list` and `symbol` (`(listp nil)` and `(symbolp nil)` are t),
  so a return type of `list` or `symbol` already admits `nil` — do not
  write `(or list null)` or `(or symbol null)`.
- **Naming**: internal helpers `t--*`, public API `t-*`.  No third
  scheme (`org-w3ctr-faces-*` is gone; keep it that way).  Formatter
  hooks are `<subject>-format-function`, their defaults
  `<subject>-default-format-function`; `:html-format-headline-function`
  is the one exception, kept verbatim from ox-html.
- **Headers**: `;;;` for major parts, `;;;;` for sections.  No
  `;;;;`-under-`;;;;` that pretends to be a third level.  A refactored
  element is either under a `;;;` part or a flat `;;;;` block — not a
  mix.
- **Ordering**: within a section, bottom-up (helper before its user)
  or top-down by call layer — pick one per section and keep it.
- **Test order**: `ox-w3ctr-tests.el` mirrors the source section by
  section, and inside a section the tests follow the source function
  order — a variant test (a name the function's name prefixes) sits next
  to its target.  A variable or constant without its own test is
  skipped.  Check with `elisp-test-quality`'s `scripts/order-check.el`
  (`OUT-OF-ORDER` / `SECTION-ORDER`).
- **Header hygiene**: correct spelling, no author names, no arithmetic
  comments that drift out of date.
- **No `cl-lib`.**  This package does not depend on `cl-lib`; use
  traditional Emacs Lisp constructs (`mapcar`, `let`, `dolist`) instead of
  `cl-loop`, `cl-destructuring-bind`, etc.  The one historic dependency was
  dropped.
- **Error signaling.**  Transcoders signal `org-w3ctr-error` (via
  `t-error`), never the generic `error`; in a `condition-case` handler,
  re-signal with `(signal err)` (the Emacs 31 one-argument form) rather
  than `(signal (car err) (cdr err))` — it is more concise and preserves
  `eq` equality of the error descriptor.
- **OINFO and purity.**  A function that reads or writes INFO through
  `t--pget` / `t--pput` is never marked `(pure t)`.
- **Docstring parameter references.**  Unused parameters carry a `_`
  prefix in the signature (e.g., `_info`); docstrings reference them
  without the prefix (write INFO, not _INFO).
- **Verification cadence.**  The ERT suite is the routine gate; also run
  the `ox-w3ctr-lint` skill's static checks on any change.  The
  `ox-w3ctr-verify` skill's corpus runs are for genuinely global changes
  (definition-time switches, option-list reorders), or when asked.

## Methodology

An AI author can afford to write everything down, so the refactor
favours "fix at generation time" over "infer at runtime": explicit
beats implicit.  The back-end trusts the author to have supplied every
fact — no guessing, no fallback — and checks that what was supplied is
well-formed.

- **Explicit over implicit.**  Prefer writing things down over
  computing them later: explicit `CUSTOM_ID`s and anchors instead of
  random reference ids, explicit configuration over conventions.
  Do not invent fallback machinery for input the author could have
  supplied explicitly.
- **Materialize what is stable, defer what varies.**  Identity (ids,
  crossrefs, anchors) is stable and cheap to maintain — make it
  explicit first.  Presentation (rendered HTML, highlighting, theme
  colours) varies with the environment — keep it deferred rather
  than baking it into the document.
- **The back-end degrades to a verifier.**  Its value shifts from
  "deriving the result" to checking the explicit input: emit the markup
  the author wrote instead of re-deriving it, and reject what is
  malformed instead of guessing.  Simpler transcoders that trust
  explicit markup beat large rule engines.
- **Fail loudly, with context.**  When explicit input is malformed,
  signal `org-w3ctr-error` with enough context — the offending value
  and, where the caller has one, the source line — to fix it.  Never
  degrade silently: dropping the input, or emitting something
  plausible, hides the mistake.
- **Design checkers that can disagree.**  Verification is part of the
  design: prefer invariants, source-scanning drift checks,
  property/fuzz tests and independent parsers over restating the
  implementation's own assumption.  A check that cannot fail is not a
  check.  For a property test, prefer a small exhaustive sweep over
  random sampling: it is deterministic and has no iteration count to
  tune.
- **The AI is the maintainer.**  Explicit things drift (a renamed
  heading, a stale id).  A human cannot afford to keep them in sync;
  an AI can — prefer explicitness wherever drift is catchable by a
  test or lint.

## Mainline: incremental refinement

Refine one function at a time — docstring, `declare`,
`important-return-value`/`pure`, helper use, tests — and file each finding
under the `Tasks` list of `README.org`'s Roadmap (small, same-session) or
its `TODO` list (larger).  Those two lists are the plan; there is no fixed
task list and no "underway" moment.  The current session state and next
task live in `.agents/next.md`.

These parts are stable — they carry unit, source-scanning and property
tests, verified end to end — touch them only for a concrete reason, with
a test, never as a drive-by while refining another section:

- `;;; Template and Inner Template` — finished
- `;;; Basic utilities` — frozen
- `;;; Greater elements` — frozen

Their one deliberate test gap, `org-w3ctr--table.el-table` (its markup
comes from `table-generate-source`), is annotated in the source.

Sections still to refine carry
`;; REFINE: this section is pending the mainline fine pass.` in the source
— `grep -n 'REFINE:' ox-w3ctr.el` lists them.  Take them in source order,
one section per pass, and remove the marker when the section is done.
`ox-w3ctr-tests.el` mirrors the source (`;;;` per major part, `;;;;` per
source section) and is worked through the same way, section by section.
