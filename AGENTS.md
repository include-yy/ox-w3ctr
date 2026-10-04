# AGENTS.md

Guidance for AI agents working in this repository.

## What this is

`ox-w3ctr` is an Emacs Lisp package: an Org export back-end that emits HTML
styled for W3C Technical Reports.  It is a "parasitic implementation" of
Org's `ox-html.el`, being progressively reimplemented (refactored) in its own
style.  Version 0.2.14; requires Emacs 31.

- `ox-w3ctr.el`       — the back-end (main source)
- `ox-w3ctr-tests.el` — ERT test suite
- `assets/`           — CSS / SVG / JS
- `jstools/`          — Node.js MathJax RPC helper
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

Two gotchas:

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

Two tests read `ox-w3ctr.el` next to the loaded file and skip without it
(`org-w3ctr--oinfo-props-are-looked-up`,
`org-w3ctr--oinfo-props-go-through-pget`); `org-w3ctr--load-file` reads it
too but fails rather than skipping.

### Stricter checks

Beyond the suite, helpers under `tools/rpctest/` (gitignored) cover what
it cannot: `strict-compile.el` (byte-compile with `byte-compile-error-on-warn'
and `byte-compile-warnings' at `t'), `checkdoc.el`, `pure-scan.el` (a
call-graph scan for `(pure t)' functions reaching `t--pget'/`t--pput',
which this file forbids), `fuzz.el` (property checks: encoding leaves no
raw specials, `t--sexp2html' renders to a string or signals only
`org-w3ctr-error', the OINFO cache agrees with `plist-get'), and
`indent-tests.el` / `indent-w3ctr.el` (`indent-region' under
`emacs-lisp-mode').  Two gotchas they cost real time on:

- **Indentation: load the file first.**  Without it the helper macros'
  `(declare (indent ...))' specs are missing and `indent-region' reports
  hundreds of false deviations (523 vs 32 on the test file).
- **`byte-compile-file`'s second argument is `LOAD`, not an output path.**
  `(byte-compile-file "ox-w3ctr.el" "x")` still writes `ox-w3ctr.elc'
  beside the source and then tries to load `"x"'.

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
  (never `plist-get` / `plist-put` for a cached key), and has ERT tests.
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

  Do not use `grep -c $'\r'` for this: in this MSYS2 environment it
  reports the line count for *any* file (a pure-LF `ox-w3ctr.el` reports
  as many hits as it has lines), so it always looks like a failure.
- Do not commit changes unless explicitly asked.
- **Docstring**: every `defun`/`defsubst` gets a full docstring — a
  one-line summary first, then parameter / return-value notes where
  they are non-obvious.
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
- **Header hygiene**: correct spelling, no author names, no arithmetic
  comments that drift out of date.

## Methodology

An AI author can afford to write everything down, so the refactor
favours "fix at generation time" over "infer at runtime": explicit
beats implicit, and the back-end should trust explicit input.

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
  "deriving the result" to "checking the explicit input"; simpler
  transcoders that trust explicit markup beat large rule engines.
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

Sections below `;;;; OINFO oclosure` that are not yet refined carry
`;; REFINE: this section is pending the mainline fine pass.` in the
source.  The `;;; Template and Inner Template` part was finished on
2026-10-04; the work now is the earlier parts (the 2026-10 section
refile scrambled source order).  The seven `REFINE:` sections left, in
source order: Footnote, Special Block, Table, LaTeX, Engrave-faces
subset, Source block, Link.  Within a part, take each
section in source order (`grep -n 'REFINE:' ox-w3ctr.el`),
one section
per pass — docstring, `declare`, `important-return-value`/`pure`,
helper use, tests — and remove the marker when the section is done.
What a pass turns up goes to `## Tasks` or `README.org` Roadmap.

`;;; Basic utilities` has had its pass: every section is refined, and
the part carries unit, source-scanning and property tests (plus the
JSON-RPC rewrite, verified end to end).  Treat it as stable — touch it
only for a concrete reason, with a test, never as a drive-by while
refining another section.

A section deferred to a later round carries `;; REFINE2:` instead of
`;; REFINE:`, so the `grep -n 'REFINE:'` recipe skips it (the `2` breaks
the exact match).  Take the remaining `REFINE:` sections first; the
`REFINE2:` ones come afterwards.

Its one **precondition**: the two local skills
(`.agents/skills/ox-w3ctr-verify`, `.agents/skills/elisp-docstring`) get
refactored until they are actually usable — the owner reads their code
and takes part, so this is a dialogue, not a batch job.  They are
untracked (`.agents/` is), so what a round settles goes into this file
rather than into a commit.  "Usable" means:

- **The documented recipe covers a routine run.**  Needing a wrapper
  written somewhere else is a missing step in the recipe (2026-09: five
  such wrappers, three extra corpus passes, and a baseline that could not
  work).
- **Numbers, not adjectives** — hashes, counts, seconds, and the command
  that produced them — and **one** place for them (this file), referred
  to rather than copied, so the copies cannot drift.
- **A checker can disagree, and is itself checked**: a differential or a
  third-party parser over the corpus, a self-check (one build, twice), and
  its false alarms written down in `references/checker-design.md` instead
  of quietly fixed.
- **Costs are stated**: what each script exports, how long a pass takes,
  and which cross-checks a routine run skips (the nil flavour: OINFO
  changes only).

A skill round is done when the next run can follow it without asking a
question the files do not answer.

The first tasks, then: the options tidy-up in `README.org` Roadmap (the
`*-function` replacement and the ox-html compatibility chart), and the
special-block
Web Component after it.

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
- **Table column groups.**  Org's `/`-row (`<`/`>`/`<>`) only marks group
  boundaries; it cannot carry attributes, because its cells must be exactly
  those markers or Org's own colgroup detection (`org-export-table-cell-borders`)
  breaks.  ox-w3ctr emits `<colgroup span="N">` and does **not** invent a
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
- **`pure t` and the OINFO cache.**  Never mark a function `(pure t)` or
  side-effect-free when its call chain reaches `t--pget` or `t--pput`,
  even on a key outside `t--oinfo-cache-props`.  These are the cache
  interface: a cached key's oclosure mutates `cnt`/`pid`/`val` (observed
  by `t-collect-oinfo-statistics`), and a plain key may join the cache
  later, silently breaking the declaration.  Rule out any
  `t--pget`/`t--pput` use outright.
- **Docstring parameter references.**  Unused parameters carry a `_`
  prefix in the function signature (e.g., `_info`), but docstrings
  reference them without the prefix (write INFO, not _INFO).
- **`(signal err)` (Emacs 31+).**  The one-argument form `(signal err)`
  is equivalent to `(signal (car err) (cdr err))`, more concise, and
  preserves `eq` equality of the error descriptor.  Prefer it in
  `condition-case` handlers.
- **Verification cadence.**  The ERT suite is the routine gate.  The
  verify skill's corpus runs are not needed for routine changes; run
  them when asked, or for a genuinely global change (definition-time
  switches, option-list reorders), and report the numbers.

## Known issues

- **Link leftovers.**  The refactor is done, but a few spots are still weak
  or suspect (each carries a `FIXME` in the source): the cross-file ID
  fragment is built from `t--link-path`'s output instead of the raw path
  (`t--link-to-file`); `:html-link-home` / `:html-link-use-abs-url` are not
  implemented (`t--link-path`); `.org.gpg` files are not rewritten to
  `.html` (`t--link-org-files-as-html`); LaTeX equation references only
  cover math environments under `mathjax`/`t`; and coderef support is kept
  only for ox-html compatibility.
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

- **Docstring & layout leftovers (from the tidy pass).**
  - Add `(declare (ftype …))` to the 3 functions that still lack it
    (excluding `defsubst`, interactive, and end-user commands; count as
    of 2026-10-04): `t-special-block`, `t--textarea-block` and
    `t-preamble-default-function`.

- **Shorthand symbol names in docstrings and comments.**  They are
  string literals and get no `read-symbol-shorthands`; write the full
  `org-w3ctr-*` name.  Fix per section in the passes.  As of
  2026-10-04, grep `` `t- `` in `ox-w3ctr.el` finds, outside the
  `REFINE:` sections, only `t-style` / `t-style-file` (<head>).
  The rest sit inside sections the pass will fix:
  `t--engrave-face-transformer` (Engrave-faces subset);
  `t-fontify-method`, `t-fontify-code` (Source block);
  `t-inline-image-rules`, `t-inline-image-p`, `t-link`, `t--link-path`,
  `t--link-target`, `t--link-equation` (Link).
