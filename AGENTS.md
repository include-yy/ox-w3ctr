# AGENTS.md

Guidance for AI agents working in this repository.

## What this is

`ox-w3ctr` is an Emacs Lisp package: an Org export back-end that emits HTML
styled for W3C Technical Reports.  It is a "parasitic implementation" of
Org's `ox-html.el`, being progressively reimplemented (refactored) in its own
style.  Version 0.2.18; requires Emacs 31.

- `ox-w3ctr.el`       — the back-end (main source)
- `ox-w3ctr-tests.el` — ERT test suite
- `assets/`           — CSS / SVG / JS
- `jstools/`          — Node.js RPC helper: MathJax (math) and Shiki
                        (code highlighting)
- `tools/`            — local build dirs and outputs (gitignored)
- `zhua.el`           — scratch file for refactor proposals (gitignored)
- `.agents/`          — local skills (untracked; see Mainline)

## Environment

Shell: MSYS2 bash (MINGW64); paths and commands below are bash-style.

- Emacs executable: `/d/emacs-build/bin/emacs.exe`
- Python: use `python` (3.14), not `python3` — `python3` resolves to
  the Windows Store stub (exit code 49) and is broken.
- Upstream Org sources (reference for ports):
  - `/d/org-mode/lisp/`  (the real Org source tree)
  - especially `ox-html.el`, `ox.el`, `org-element.el`
- **Never search inside `node_modules`.**  No recursive `grep`/`find`/`rg`
  over `node_modules` (including the pi install tree or any package's
  `node_modules/`): it is enormous and the search hangs the shell.  Read
  the specific documented file by path instead (e.g. under the pi
  `docs/` directory) — never discover it with a recursive scan.

## Git

- Remotes: `gh` = GitHub (`https://github.com/include-yy/ox-w3ctr`),
  `origin` = SourceHut (`git@git.sr.ht:~exkeq/ox-w3ctr`).
- GitHub is reached over HTTPS and needs the proxy; set it per command:
  `HTTPS_PROXY='http://127.0.0.1:7890' git push gh master v0.2.7`.
  SourceHut is over SSH and needs no proxy.
- Releases: bump `Package-Version` (header) and `t-version` together,
  commit, then tag `vX.Y.Z` (lightweight, matching `v0.2.5`) and push the
  branch and the tag to both remotes.  The OINFO cache ships on: do **not**
  turn `org-w3ctr-oinfo-enabled` off for a release (`t-oinfo-enabled` says
  why).

## Running the tests

From the repo root:

```bash
"/d/emacs-build/bin/emacs.exe" --batch -L . --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))" -l ox-w3ctr-tests.el -f ert-run-tests-batch-and-exit
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

Three tests read `ox-w3ctr.el` next to the loaded file and skip without
it (`org-w3ctr--oinfo-props-are-looked-up`,
`org-w3ctr--oinfo-props-go-through-pget`, `org-w3ctr--load-file`).
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

  Do not use `grep -c $'\r'` for this: in this MSYS2 environment it
  reports the line count for *any* file (a pure-LF `ox-w3ctr.el` reports
  as many hits as it has lines), so it always looks like a failure.
- Do not commit changes unless explicitly asked.
- **Docstring**: every `defun`/`defsubst` gets a full docstring — a
  one-line summary first, then parameter / return-value notes where
  they are non-obvious.
- **Test docstrings**: a test named after a function opens with
  `Tests for `org-w3ctr-\u2026'.` and one line only -- extra explanation
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
- **Temporary files.**  Use the project's `tools/tmp/` directory for
  scratch files, not the system temp directory (`$TMPDIR` / `%TEMP%`).
  Create `tools/tmp/` if it does not exist (it is gitignored).  This keeps
  project-related debris together and avoids polluting the system temp with
  Org export artifacts.

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

The per-element round is complete; from here the work is top-down.  Refine
one function at a time — docstring, `declare`, `important-return-value`/
`pure`, helper use, tests — and each problem found becomes an entry in
`## Tasks` (small, usually done in the same session) or the `Roadmap`
section of `README.org` (larger), rather than a plan of its own.  There
is no fixed task list and no "underway" moment: those two lists *are*
the plan.

The `;;; Template and Inner Template` part is finished; `;;; Basic
utilities` and `;;; Greater elements` are frozen.  Both are stable:
they carry unit, source-scanning and property tests and the JSON-RPC
rewrite, verified end to end — touch them only for a concrete reason,
with a test, never as a drive-by while refining another section.  Their
one deliberate test gap, `org-w3ctr--table.el-table` (its markup comes
from `table-generate-source`), is annotated in the source.  The
remaining refinement work is in the earlier parts (a section refile
scrambled their source order): the `REFINE` sections below.

The test-code tidy-up of the frozen parts is done: `;;; Basic
utilities` first — every section reviewed function by function, OINFO
included — then `;;; Greater elements`, whose ten sections were
reviewed for coverage, duplication, helper use, naming and layout (its
Table tests were rewritten to the suite's helper/table-driven style).
The one deliberate gap stays `org-w3ctr--table.el-table`.  Findings went
to `## Tasks` and the `README.org` Roadmap.

`ox-w3ctr-tests.el` mirrors the source: a `;;;` header per major part
and a `;;;;` header per source section (OINFO splits into helpers /
structural checks / reading and writing / cleanup and statistics).  Work
through it a section at a time.  Everything under `;;; Basic
utilities` and `;;; Greater elements` is reviewed; the other parts are
pending.

The sections still to refine carry
`;; REFINE: this section is pending the mainline fine pass.` in the
source.  One remains: Link (Engrave-faces subset and Source block
were done in the fontify refactor; see Notes).  Take them in source
order (`grep -n 'REFINE:' ox-w3ctr.el`), one section per pass — docstring,
`declare`, `important-return-value`/`pure`, helper use, tests — and
remove the marker when the section is done.  What a pass turns up goes to
`## Tasks` or `README.org` Roadmap.

Besides the passes: the options tidy-up in `README.org` Roadmap (the
`*-function` replacement and the ox-html compatibility chart).

## Notes

- **Compile-time switches and conditionals.**  Four measured facts,
  learned while building `org-w3ctr-oinfo-enabled`:
  - a top-level `defvar`/`defconst`/`defun` is *not* visible to compile-time
    evaluation later in the same file (it fails with a "void" error), so
    anything read at macro-expansion time has to be defined inside an
    `eval-and-compile`;
  - `eval-and-compile` evaluates its body *and* emits it, so a plain
    `defconst` init is re-evaluated when the `.elc` loads; freeze such a
    value with `eval-when-compile` when compiled call sites depend on it;
  - never put top-level definitions inside a conditional: the compiler
    hoists them and emits *both* branches, the later one winning (the
    symptom is a switch that reports one flavour while the other is built);
  - inside a `define-inline`, a flag test must sit *outside* the
    `inline-quote`: inside it is a runtime branch, and the whole machinery
    is expanded into every call site.
- **Custom elements (special-block).**  The design is explicit over
  implicit: every custom element must be registered in
  `org-w3ctr-special-block-custom-elements`, a `(NAME . PLIST)` alist.
  The back-end does not guess by name pattern.  Keys in the PLIST are
  read by two separate components: `:template` (Declarative Shadow DOM)
  is inserted by the transcoder (`org-w3ctr--special-block-custom`);
  `:src` and `:script` are read by the default head function
  (`org-w3ctr-special-block-head-default-function`) to build `<script>`
  tags.  Other keys are left to a custom head function.  The scan for
  used elements (`org-w3ctr--special-block-used-elements`) runs at
  template stage, takes INFO, and has no global state; `:noexport:`
  subtrees are excluded.  The exclusion is pruning's doing:
  `org-export--prune-tree' extracts `:noexport:' subtrees from the
  parse tree before the scan runs.  The INFO argument the scan passes
  to `org-element-map' is therefore unobservable (`:ignore-list' never
  holds special blocks): dropping it survives every test -- an
  equivalent mutant, kept annotated in the local mutation harness.
- **Table column groups.**  Org's `/`-row (`<`/`>`/`<>`) only marks group
  boundaries; it cannot carry attributes, because its cells must be exactly
  those markers or Org's own colgroup detection
  (`org-export-table-cell-borders`) breaks.  ox-w3ctr emits
  `<colgroup span="N">` and does **not** invent a
  per-group class syntax.  To style a group, put a class on the table
  (`#+attr__: [foo]` or `#+attr_html: :class foo`) and use a CSS positional
  selector, e.g. `.foo colgroup:nth-of-type(2) { ... }`.  Only
  `border`/`background`/`width`/`visibility` apply to `<colgroup>`;
  `text-align` does not (alignment is emitted inline on the cells).
  Per-column width would need `<col>`, which is deliberately dropped.
- **Object theming contract (planned).**  Style complex objects by
  injecting CSS custom properties, not by emitting bespoke classes.
  `#+attr__` can set a `style` attribute, so a table/block can carry
  `style="--w3c-accent: ..."`; the stylesheet consumes it with
  `var(--w3c-accent, fallback)`.  Each complex object should expose a
  small, documented set of custom properties.  Target children with
  structural/positional selectors (`colgroup:nth-of-type(n)`,
  `tr:nth-child(n)`, `td:first-child`), never with invented classes.
  Custom properties inherit down the subtree (and pierce Shadow DOM),
  so the same mechanism doubles as the theming API for DSD/Web
  Components.  (Rationale: several ox-html carry-over classes such as
  `org-left`/`org-center`/`org-right` and `t-above`/`t-bottom` are not
  defined by the W3C CSS, i.e. dead; do not add more of those.)
- **Math pipeline.**  LaTeX fragments reach the `jstools` Node helper as
  `\(...\)`/`\[...\]` (`t--normalize-latex`); the helper does all the
  MathJax work — extract the TeX, convert with the promise API, clean up
  the result — and returns ready-to-embed markup.  `mathml-by-mathjax` and
  `svg-by-mathjax` are therefore thin one-line RPC calls on the Emacs side.
  The helper loads `ui/safe`, without which the auto-loaded `html' TeX
  extension lets `\href{javascript:...}`, `\style` and `\class` through.
- **Fontify pipeline.**  `org-w3ctr-fontify-code` turns the method of a
  block (its `:fontify` header argument, else `#+OPTIONS: fontify:`,
  else `org-w3ctr-fontify-method`) into a chain of engines ending with
  `plain` (`t--fontify-chain-for`), and `t--fontify-dispatch` runs the
  engines of `org-w3ctr-fontify-engines` (`engrave`, `jstools`,
  `plain`) until one returns tokens.  The contract:
  - An engine returns `((CLASS . TEXT) ...)` or `:decline`, never
    HTML.  TEXT is raw code; CLASS is nil, a slug of the face tables,
    or `(style . CSS)` under the `inline` face fallback.
    `t--fontify-check-tokens` rejects tokens that do not add up to the
    code or carry an unknown class; `t--fontify-merge` joins
    neighbours of one class; `t--fontify-render` is the only place that
    escapes.
  - Fallback, in three layers: a face without a slug follows its
    `:inherit` chain (`t--engrave-face-slug`); a declining engine
    passes the block on, and a failing or unavailable one is handled
    by `:html-fontify-on-error` and broken for the rest of the export;
    `plain` takes what is left, and a language every engine declined
    goes through `:html-fontify-unknown-language`.  Everything is
    counted in INFO's `:html-fontify-state` and shown by
    `M-x org-w3ctr-show-fontify-report`.
  - Measured facts: Emacs 31 *asks* to install a missing tree-sitter
    grammar, which hangs a batch export, so `t--engrave-tokens` binds
    `treesit-auto-install-grammar` to `never` (declared with `defvar`,
    or the `let` would be lexical and do nothing) and declines a
    `*-ts-mode` left without a parser.  In batch, `color-values` rounds
    to the 8-colour terminal palette (`#1f5bff` comes back pure blue),
    so inline styles use `tty-color-standard-values`.  Mode hooks run
    delayed by default, and `font-lock-ensure` still fontifies
    everything under `delay-mode-hooks`.
  - The `jstools` engine calls `highlightLanguages` once per export
    and `highlight` per block; the helper answers `[TEXT, SLUG]` pairs.
    Slugs come from `jstools/lib/slugs.json` through a synthetic Shiki
    theme whose fake colours (`#000001`, ...) decode back to slugs, so
    Shiki's own scope matching picks them.  Error `-32010` means
    "unknown language", i.e. decline.
  - Colours: edit `org-w3ctr-fontify-palette`, then run `M-x
    org-w3ctr-fontify-update-stylesheet`, which rewrites the marked
    block of `assets/style.css`.  Tests tie the palette, that block,
    the face table and `slugs.json` together, and check WCAG contrast
    ≥ 4.5 on every code background in both themes.
  - The refactor changed one thing in the frozen `;;; Basic
    utilities`: `t--jstools-methods` gained `highlight` and
    `highlightLanguages` (guarded by `t--jstools-methods-drift`).
  - Local helpers under the gitignored `tools/`, when present:
    `tools/fontify-snapshot.el LABEL` exports `tools/fontify-corpus/`
    for byte-for-byte comparison (`diff -r` two labels), and
    `tools/static-check.el` prints the compiler and checkdoc warnings.
- **JSON-RPC transport (`jsonrpc.el`).**  The `jstools` connection is a
  callable `org-w3ctr--jrpc' oclosure over a `jsonrpc-process-connection';
  two measured facts to keep:
  - **No restart.**  `jsonrpc.el` sets the process once in
    `initialize-instance` (buffer, filter, sentinel, coding and stderr
    are all installed there), and `jsonrpc-shutdown` only tears down.  To
    restart, discard the connection and build a fresh one
    (`org-w3ctr--jrpc-ensure` / `-restart`).  Never `setf`
    `jsonrpc--process`.
  - **`:process` must be a function, and it must pass `:stderr`.**  The
    `:process` initarg is called from `initialize-instance` *after* a
    `*NAME stderr*` buffer is created, so the factory has to hand that
    buffer to `make-process` as `:stderr` (the "bad coupling" jsonrpc.el
    itself flags with a FIXME).  Passing a ready process silently loses
    stderr separation and merges it into stdout, corrupting the stream.
- **Error signaling: `t-error` over `error`.**  All transcoder error
  paths use `t-error` (the package's custom error type), not the generic
  `error`.  Inside a `condition-case` handler, re-signal with `(signal e)`
  (Emacs 31 syntax) instead of `(signal (car e) (cdr e))`.
- **OINFO.**  INFO access goes through `t--pget`/`t--pput`, never
  `plist-get`/`plist-put`; and a function reaching either is never
  marked `(pure t)`.  The cache has been verified end to end.
- **Docstring parameter references.**  Unused parameters carry a `_`
  prefix in the function signature (e.g., `_info`), but docstrings
  reference them without the prefix (write INFO, not _INFO).
- **`(signal err)` (Emacs 31+).**  The one-argument form `(signal err)`
  is equivalent to `(signal (car err) (cdr err))`, more concise, and
  preserves `eq` equality of the error descriptor.  Prefer it in
  `condition-case` handlers.
- **Verification cadence.**  The ERT suite is the routine gate.  For the
  cheap static checks to run on any change (strict compile, checkdoc, the
  `(pure t)` call-graph scan, ftype gaps, indentation), see the verify
  skill's `references/harness.md`.  Its corpus runs are not needed for
  routine changes; run them when asked, or for a genuinely global change
  (definition-time switches, option-list reorders), and report the numbers.

## Known issues

- **Link leftovers.**  The refactor is done, but a few spots are still weak
  or suspect.  Each of these carries a `FIXME` in the source: the
  cross-file ID fragment is built from `t--link-path`'s output instead of
  the raw path (`t--link-to-file`); `:html-link-home` /
  `:html-link-use-abs-url` are not implemented (`t--link-path`); LaTeX
  equation references only cover math environments under `mathjax`/`t`;
  and coderef support is kept only for ox-html compatibility.  One more
  divergence has no marker: `.org.gpg` files are not rewritten to
  `.html` (`t--link-org-files-as-html`), though ox-html does rewrite them.
- `t--link-broken` looks unreachable (Org handles broken links before the
  transcoder); it is `FIXME`-marked.  (`t--math-environment-p`, likewise
  FIXME-marked, was removed — its ordinal purpose is long gone.)
- `t--link-to-file`, `t--link-broken` and `t--link-coderef` still have no
  tests.
- **Unnamed elements get a fresh random id on every export.**  With no
  explicit label, `t--reference' falls back to `org-export-get-reference',
  which mints an `orgXXXXXXX` id from a randomly seeded counter.  Four of
  the 57 corpus documents hold such ids (`verify-corpus` counts them in
  `refs=`), and an anchor into one is not stable across exports.  Never diff
  raw export hashes — compare the normalized `norm=` (see the verification
  skill).  Either the author gives every referenced element an explicit
  `CUSTOM_ID`, or the back-end derives a stable id — see Non-goals.

## Tasks

Small items, found while refining a function and usually finished in the
same session.  Larger or planned work is in the =Roadmap= section of
=README.org=.

- **`org-w3ctr-check-element-values` expects its values in reverse.**
  The fixture `push`-accumulates `org-w3ctr-test-values`, so EXPECTED is
  in reverse call order (a test pins this).  Either document it or
  `nreverse` it in the fixture; the latter flips nearly every EXPECTED
  list in the suite.
- **The OINFO source-scanning tests hardcode `info`.**  Both
  `org-w3ctr--oinfo-props-are-looked-up` and
  `org-w3ctr--oinfo-props-go-through-pget` match the literal variable
  `info`; a differently named INFO plist would false-fail.  Loosen to
  `[^ \t\n()]+` if that ever changes.

- **checkdoc leftovers outside the REFINE passes.**  Emacs 31.1's
  checkdoc reports 17 warnings; 10 sit in the Link `REFINE:` section
  and its pass clears them.  Seven have no owner:
  `t-creator-string`'s docstring first line is not a complete sentence
  (it ends at the `%c` placeholder); the `t-export-as-html` /
  `t-export-to-html` docstrings never mention ASYNC (the argument is
  used — it goes to `org-export-to-buffer' / `org-export-to-file' — so
  the docstring should say so, not the name mangled); two docstrings
  name the bare word `org-publish` where checkdoc wants it quoted;
  and two say `jsonrpc-process-connection` without "class" or
  "symbol" in front of it.  `tools/static-check.el` lists them.

- **`org-w3ctr--read-attr`'s error message prints the property keyword
  as is.**  `Invalid attribute #+%s` interpolates the keyword, so the
  production message reads `Invalid attribute #+:attr__: ...` while the
  Org keyword is spelled `#+attr__:` (the colon sits differently).  A
  test pins the current message; if the formatting is ever fixed,
  flip that expectation.

- **`org-w3ctr--make-attr__id` recognizes an explicit id only as a
  list (`(id \u2026)`).**  A bare `id` atom in `#+attr__:` is not
  detected, so the reference id is prepended and the atom emitted as
  well: `#+name:1` with `#+attr__: id` gives ` id="1" id`, a malformed
  attribute string.  A test pins it; recognizing the atom form too
  would flip the expectation.  The mirror problem on the `:attr_html'
  side: `org-w3ctr--make-attr_html` keys its suppression off
  `plist-member', which sees the key regardless of value, so an
  explicit `:id nil` suppresses the auto id and emits no id at all.

- **Attribute input is not validated.**  In `org-w3ctr--make-attr`
  only the values go through `org-w3ctr--encode-plain-text*`; a name is
  just downcased, so `("<x>" v)` gives ` <x>="v"` and `(1)` gives ` 1`.
  `org-w3ctr--make-attribute-string` has the same asymmetry: keys go
  out as is (`(:<x> "v")` gives `<x>="v"`).
  A dotted element falls through to a primitive `wrong-type-argument'
  (reachable as `#+attr__: (a . b)`, which aborts the export) instead
  of `org-w3ctr-error' with context.  Duplicate keys are not merged
  either: `:class a :class b` gives ` class="a" class="b"`.  Tests pin
  the current behavior; a
  checker that accepts only HTML attribute names and proper lists (and
  signals `org-w3ctr-error' otherwise) would flip them.

- **Silent drops in S-exp rendering and attribute formatting.**
  `org-w3ctr--sexp2html` renders a non symbol/string/number child as
  nothing, `org-w3ctr--make-attr` returns nil for an attribute whose
  name is not convertible (`(nil 1)`), and `org-w3ctr--read-attr__`
  maps a nil element of a `[...]` vector to an empty contribution that
  keeps its separator (`[a nil b]` gives `("class" "a  b")`, via
  `mapconcat' over `org-w3ctr--2str' with `" "').  Tests pin
  all three.  They are the `;;; Basic utilities` exceptions to
  fail-loudly: either keep them documented as deliberate (the
  docstrings say so now) or signal `org-w3ctr-error' and flip the
  tests.

- **Docstring & layout leftovers (from the tidy pass).**
  - Add `(declare (ftype …))` to the one function that still lacks it
    (excluding `defsubst`, interactive, and end-user commands):
    `t-preamble-default-function`.

- **Shorthand symbol names in docstrings and comments.**  They are
  string literals and get no `read-symbol-shorthands`; write the full
  `org-w3ctr-*` name.  Fix per section in the passes.  grep
  `` `t- `` in `ox-w3ctr.el` finds, outside the
  `REFINE:` sections, only `t-style` / `t-style-file` (<head>).
  The rest sit inside the Link section, which its pass will fix:
  `t-inline-image-rules`, `t-inline-image-p`, `t-link`, `t--link-path`,
  `t--link-target`, `t--link-equation` (Link).

- **Three test sections sit out of source order.**  `order-check.el`
  flags Link, Headline and CC license badges.  Reorder them to the
  source function order when those sections are next touched.

- **Other transcoders take CONTENTS as `string' with no nil guard.**
  ox.el prunes math under `tex:nil', so a paragraph whose only content
  was pruned arrives with nil CONTENTS -- `org-w3ctr-paragraph' used to
  crash in `org-w3ctr--trim'; fixed in the LaTeX pass (nil becomes "",
  as `org-w3ctr-special-block' already did).  The remaining transcoders
  declare CONTENTS as `string'; guard one when a nil-contents case shows
  up.

- **order-check cannot see a test filed under the wrong section.**  It
  checks order within a section and the order of sections only: the
  `org-w3ctr--normalize-latex' and `org-w3ctr--format-latex' tests sat in
  the `Math config' section while the functions live in the source LaTeX
  section, and nothing flagged it.  Cross-section membership is checked
  by eye; done for the LaTeX pass.

- **The coderef link path is prefixed where the bare label is
  expected.**  `org-w3ctr-link' hands `org-w3ctr--link-coderef' the
  output of `org-w3ctr--link-path', which for a `[[(foo)]]' link is
  "coderef:foo" while the link's `:path' is "foo" (measured), so
  `org-export-resolve-coderef' receives the prefixed string and the
  fragment would be `#coderef-coderef:foo'.  Coderef support is the
  FIXME'd leftover in `org-w3ctr--link-coderef' (kept once for ox-html
  compatibility); the Link pass fixes or removes it.

- **`npm audit` flags `@xmldom/xmldom` in jstools.**  It comes with
  `mathjax` 4.0.0-beta.7, pinned in `package-lock.json` (MathJax 4.1.x
  is out); Shiki adds no finding.  Upgrade MathJax deliberately, re-run
  `npm test` and the math export tests, rather than `npm audit fix`.

- **Export blocks take no attributes or ids (deferred).**  Raw
  passthrough is the contract: a `#+name:' on an export block emits no
  anchor, so a link to it dangles (measured: `[[x]]' renders
  `<a href="#x">' with nothing to land on; a random id with
  `org-w3ctr-prefer-user-labels' off).  Decided 2026-10 to leave it;
  the Link pass should know before touching element references.
