# AGENTS.md

Guidance for AI agents working in this repository.

## What this is

`ox-w3ctr` is an Emacs Lisp package: an Org export back-end that emits HTML
styled for W3C Technical Reports.  It is a "parasitic implementation" of
Org's `ox-html.el`, being progressively reimplemented (refactored) in its own
style.  Version 0.2.7; requires Emacs 31.

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
  turn `org-w3ctr-oinfo-enabled' off for a release (`t-oinfo-enabled' says
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

Expected baseline: **172 tests, 170 pass, 2 skipped** (`org-w3ctr-headline`,
and `org-w3ctr--oinfo-plain-flavor`, which only runs in a build with
`org-w3ctr-oinfo-enabled' nil).  Run the cache build — the one that ships;
a nil build is for measuring, not a configuration to maintain.  (For
reference if you build one anyway: it skips the nine cache-path tests,
162 pass, 10 skipped.)

Two tests read `ox-w3ctr.el' next to the loaded file and skip without it
(`org-w3ctr--oinfo-props-are-looked-up',
`org-w3ctr--oinfo-props-go-through-pget'); `org-w3ctr--load-file' reads it
too but fails rather than skipping.

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
  reports the line count for *any* file (a pure-LF 4937-line
  `ox-w3ctr.el` gives 4937), so it always looks like a failure.
- Do not commit changes unless explicitly asked.

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
`## Tasks` (small, usually done in the same session) or `## TODO`
(larger), rather than a plan of its own.  There is no fixed task list and
no "underway" moment: those two lists *are* the plan.

Every section below `;;;; OINFO oclosure` is marked in the source with
`;; REFINE: this section is pending the mainline fine pass.`  Take them in
source order (`grep -n 'REFINE:' ox-w3ctr.el`), one section per pass —
docstring, `declare`, `important-return-value`/`pure`, helper use, tests —
and remove the marker when the section is done.  What a pass turns up goes
to `## Tasks` or `## TODO`.

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

The first tasks, then: the options tidy-up in `## TODO` (the `*-function`
replacement and the ox-html compatibility chart), and the special-block
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

## Code layout rules

What the docstring & code layout tidy settled; follow them for new code.

### Rules

- **Docstring**: every `defun`/`defsubst` gets a full docstring — a
  one-line summary first, then parameter / return-value notes where
  they are non-obvious.
- **Declarations**: refactored functions carry
  `(declare (ftype (function (ARGS) RET)))`; add
  `(important-return-value t)` where the caller must use the result;
  add `(pure t)` where the function is side-effect free and its result
  depends only on its arguments.  Exemptions: `defsubst`, end-user
  commands (`t-export-*`, `t-publish-*`, `t-convert-*`), interactive
  commands whose return value is incidental.
- **Naming**: internal helpers `t--*`, public API `t-*`.  No third
  scheme (`org-w3ctr-faces-*` is gone; keep it that way).
- **Headers**: `;;;` for major parts, `;;;;` for sections.  No
  `;;;;`-under-`;;;;` that pretends to be a third level.  A refactored
  element is either under a `;;;` part or a flat `;;;;` block — not a
  mix.
- **Ordering**: within a section, bottom-up (helper before its user)
  or top-down by call layer — pick one per section and keep it.
- **Header hygiene**: correct spelling, no author names, no arithmetic
  comments that drift out of date.

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
  which mints an `orgXXXXXXX' id from a randomly seeded counter.  Four of
  the 57 corpus documents hold such ids (`verify-corpus' counts them in
  `refs='), and an anchor into one is not stable across exports.  Never diff
  raw export hashes — compare the normalized `norm=' (see the verification
  skill).  Either the author gives every referenced element an explicit
  `CUSTOM_ID', or the back-end derives a stable id — see Non-goals.

## Tasks

Small items, found while refining a function and usually finished in the
same session; the mainline adds them here as it goes.  Larger or planned
work goes in `## TODO` below.

- **Docstring & layout leftovers (from the tidy pass).**
  - Add docstrings to the 13 jstools RPC functions
    (`t--rpc-make-json` … `t--jstools-call`).
  - Add `(declare (ftype …))` to the ~18 functions that still lack it
    (excluding `defsubst` and end-user commands).
  - Rename `;;;; Legacy home and up` (fold into Navbar or rename).

## TODO

- **Options tidy-up.**  The `:options-alist` is a grab-bag: a few
  entries are grouped (`;; Link`, `;; Footnote`), most are not, and a
  couple carry inline `;; Options:` comments.  Order every entry, add a
  `;; Options:` comment wherever the semantics are not obvious, and
  consider replacing the cumbersome string options with `*-function`
  ones (as `t-footnote-section-function` does).  Also chart
  compatibility with ox-html: for each option, whether ox-html has the
  same name and semantics, a different one, or none, and quantify the
  result (e.g. "N of M options shared").  Keep the `:html-*` keyword
  names compatible where cheap.
- **Src-block feature gaps vs ox-html.**  Its transcoders are the rough
  part of the refactor.  Dropped when forking and out of scope for now
  (judged low-value for W3C TR output).  Revisit later; each should slot in
  *between*
  `t-fontify-code` and the transcoders (a layout layer), not back into
  fontify.
  - Line numbers (`-n`/`+n` via `org-export-get-loc`), coderef
    (`(ref:label)` via `org-export-format-code`) and `retain-labels`:
    a bound trio in ox-html's `org-html-do-format-code`.
  - `:html-wrap-src-lines` (per-line `<code>`) and `:html-klipsify-src`.
  - Listing number in captions (`org-export-get-ordinal` +
    `org-html--translate "Listing %d:"`).
  - example-block fontification / line numbers: ox-html routes
    example-block through `org-html-format-code`; `t-example-block` is
    plain text.
  - Highlight engine is a *replacement*, not a gap: htmlize +
    `org-html-htmlize-output-type` / `-font-prefix` versus
    `t-fontify-method` + fixed `ef-` slugs.
- **Attr-reading machinery.**  The attribute helpers have grown several
  layers: `t--read-attr__`, `t--make-attr__`, `t--make-attr__id`,
  `t--make-attr__id*`, `t--make-attr_html`, `t--make-attribute-string`,
  plus the src-block-specific `t--src-block-attrs`.  Two syntaxes
  coexist (Lisp s-exprs for `#+attr__`, plists for `#+attr_html`), and
  the "add an id unless one is present" logic is duplicated across four
  functions.  Candidates: a single intermediate representation read
  from both syntaxes, or one canonical syntax with the other as a thin
  compatibility shim.
- **SVG global font cache.**  `svg-by-mathjax` currently uses MathJax's
  default `fontCache: 'local'`, which embeds every formula's glyph paths in
  every formula; a formula-heavy document reaches the megabyte range.
  MathJax's `fontCache: 'global'` shares one `<defs>` per document (measured
  ~1.9x smaller end-to-end on a 100-formula sample).  It needs a
  `svg-font-cache` RPC returning `output.fontCache.getCache()` wrapped in a
  hidden `<svg>` and then clearing it (the Node process outlives a single
  export), injected once in `t-inner-template` — not `<head>`, which may not
  contain `<svg>` and which is built after the body anyway.  Tried and
  reverted; revisit if SVG output is kept.
- **CSS cleanup in `assets/style.css`.**  Not urgent; note for later.
  - `assets/style.min.css` (stale, 24KB vs 49KB, June vs July) was
    deleted; do not regenerate a minified copy — the back-end reads
    `style.css` via `t-style-file`.
  - Dead rules: `#home-and-up` (superseded by `#navbar`, which
    `t-format-navbar-default-function` emits) and `.org-center` (never
    emitted).  Both violate the object-theming contract; remove them.
  - `.ef-*` highlight colours are global (outside the dark block) and
    lean dark-theme (`#b2b2b2` comments etc.); on a light background the
    low-contrast ones wash out.  Provide a light/dark pair (or vars).
  - `pre > code.src` uses `background: rgba(0,0,0,.03)`, invisible in the
    dark theme; add a dark-block override.
  - Inline src (`t-inline-src-block` emits `class="src-inline src-LANG"`)
    has no CSS: no `.src-inline`/`.src-*` rules.  Give it a style or drop
    the dead class from the back-end (src-block refactor decides).
- **Src-block highlight backends.**  Today `t-fontify-method` is
  `engrave` or nil (server-side).  Planned directions, either or both:
  add further server-side backends, or hand colouring to the client
  (e.g. highlight.js) by emitting bare
  `<code class="language-LANG">` and letting the front-end script
  highlight it.  The extension points are the dispatch in
  `t-fontify-code` and the class generation in `t--src-code-tag`
  (`src src-LANG` today, `language-LANG` for highlight.js).  When the
  client does the work, the `.ef-*` CSS in `style.css` becomes optional
  and the engrave engine is only needed for server-side output.

## Non-goals

Explicitly out of scope for now — do not start them: the mainline work
(the skills precondition, the options leftover, the special-block Web
Component) comes first.

- **special-block (Web Component).**  Deferred to the mainline, after the
  options tidy-up.
  The unfinished `t-special-block` lives in `zhua.el` and still needs
  `ox-w3ctr-component-registry` and `ox-w3ctr-collect-dependency`.
- **Dependency analysis.**  Charting how far ox-w3ctr leans on Org (which
  `org-*` symbols it calls, how many are private `org-*--*` API, and which
  Org file each comes from) and mapping the internal `t-*` call graph is a
  final global-optimization and cleanup task.  Do it by parsing the source
  with Emacs rather than grep, so comments and docstrings do not create
  false positives, and report the dynamic edges (`funcall`, `apply`,
  `:translate-alist` / `:options-alist` dispatch) separately instead of
  silently dropping them.
- **Cross-file link resolution (crossrefs).**  Resolving
  `[[file:other.org::*Heading]]` / `::#custom-id` to an anchor in the target
  document, with a project-scoped persistent cache so the anchor survives
  re-exports.  Currently using `org-export-get-reference' as fallback.
  Not now.
- **Drop the `ox-publish` dependency.**  ox-w3ctr requires `ox-publish` and
  calls `org-publish-file-relative-name` and
  `org-publish-resolve-external-link` from `t--link-path`, plus
  `org-publish-org-to` from `t-publish-to-html`.  Replace them with local
  implementations: the first two belong with the planned crossref backend
  (`t-xref-backend`), the last with yynt's publish flow.  Not now.
- **Distributed shortdoc.**  `define-short-documentation-group`
  overwrites a same-named group (it does `delq` then `push`), so shortdoc
  entries cannot be spread across modules by repeated calls to the same
  group.  If the shortdoc grows, use "data distributed, definition
  centralized": each module keeps a `t--shortdoc-*' list, and the single
  `define-short-documentation-group' at the end splices them with `,@'.
  Far-future; not now.
