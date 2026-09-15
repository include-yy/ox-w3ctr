# AGENTS.md

Guidance for AI agents working in this repository.

## What this is

`ox-w3ctr` is an Emacs Lisp package: an Org export back-end that emits HTML
styled for W3C Technical Reports.  It is a "parasitic implementation" of
Org's `ox-html.el`, being progressively reimplemented (refactored) in its own
style.  Version 0.2.6; requires Emacs 31.

- `ox-w3ctr.el`       — the back-end (main source)
- `ox-w3ctr-tests.el` — ERT test suite
- `assets/`           — CSS / SVG / JS
- `jstools/`          — Node.js MathJax RPC helper
- `zhua.el`           — scratch file for refactor proposals (gitignored)

## Environment

Shell: MSYS2 bash (MINGW64); paths and commands below are bash-style.

- Emacs executable: `/d/emacs-build/bin/emacs.exe`
- Upstream Org sources (reference for ports):
  - `/d/org-mode/lisp/`  (the real Org source tree)
  - especially `ox-html.el`, `ox.el`, `org-element.el`

## Git

- Remotes: `gh` = GitHub (`https://github.com/include-yy/ox-w3ctr`),
  `origin` = SourceHut (`git@git.sr.ht:~exkeq/ox-w3ctr`).
- GitHub is reached over HTTPS and needs the proxy; set it per command:
  `HTTPS_PROXY='http://127.0.0.1:7890' git push gh master v0.2.6`.
  SourceHut is over SSH and needs no proxy.
- Releases: bump `Package-Version` (header) and `t-version` together,
  commit, then tag `vX.Y.Z` (lightweight, matching `v0.2.5`) and push the
  branch and the tag to both remotes.

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

Expected baseline: **159 tests, 158 pass, 1 skipped** (`org-w3ctr-headline`).

## Conventions

- Symbols in `ox-w3ctr.el` use the shorthand `t-` for `org-w3ctr-`
  (`read-symbol-shorthands`).  `zhua.el` must declare the same shorthand or
  its symbols will not shadow the package ones.
- A refactored function has: a full docstring, `(declare (ftype ...))`,
  `(important-return-value t)` / `(pure t)` where applicable, uses the
  `t--*` helpers and the OINFO cache (`t--pget` / `t--pput`), and has ERT
  tests.
- Workflow: write proposals to `zhua.el`, review in Emacs, then merge into
  `ox-w3ctr.el`.  `zhua.el` is gitignored — do not commit it.
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

## Refactoring status

Done (refactored): center-block, drawer, dynamic-block, item, plain-list,
quote-block, example-block, export-block, fixed-width, horizontal-rule,
keyword, paragraph, verse-block, entity, export-snippet, line-break, target,
radio-target, statistics-cookie, subscript, superscript, bold, italic,
underline, verbatim, code, strike-through, plain-text, timestamp, section,
headline, inner-template, template, table (`table`/`table-row`/`table-cell`),
latex (`latex-fragment`/`latex-environment` + `t--format-latex`/
`t--normalize-latex`), link (the `t--link-*` helpers plus `t-inline-image-p`,
`t-standalone-image-p`, `t-image-link-filter`), footnote
(`footnote-reference` + `t-footnote-section`), and the whole `<head>` / CSS /
MathJax / navbar / license / preamble / TOC layer, src-block
(`inline-src-block` + the `t--engrave-*` subset, `t-fontify-code` and the
`t--src-*` transcoders; rough — see the `<code>` line-break note).

The first phase — per-element refactoring — is complete.  Its wrap-up
is reordering the whole document; see "Docstring & code layout tidy".

Not done (still ported from ox-html, no `(declare ...)`, no tests):

- `special-block` — deferred to phase 2 (see Non-goals)

## Refactoring order (dependency-based)

0. shared helpers — round 1 done (`t--void-element`, `t--has-caption-p`);
   further proposals live in `zhua.el`
1. table — done (tests in `ox-w3ctr-tests.el`)
2. latex — done
3. link & image — done (tests in `ox-w3ctr-tests.el`)
4. footnote — done (tests in `ox-w3ctr-tests.el`)
5. src-block (largest, includes the engrave-faces port) — done (rough)
6. options — tidy the whole `:options-alist` (see the Options note)
7. docstring & code layout tidy — first-phase wrap-up: reorder the
   whole document (headers, function order, docstrings).  Next.

## Notes

- **OINFO is instrumentation, not a speedup.**  `t--pget`/`t--pput` is best
  treated as a centralized, instrumented option-access layer.  Measured on a
  compiled build: a cache hit is ~2.5-4x faster than `plist-get`, but a real
  150-headline export makes only ~10k cached lookups against a ~356-entry
  INFO plist, saving on the order of 1-2 ms out of ~1 s (~0.1%).  A
  100-document build with ~500 lookups each saves single-digit milliseconds.
  `plist-get` is a C subr and is not the bottleneck; string building, tree
  walking, regexp replacement, and fontification are.  Keep-or-drop OINFO is
  deferred until the refactor is otherwise complete.
- Do **not** "optimize" by turning INFO into a hashtable: INFO is owned by
  Org's export engine and is a plist by contract (all of `ox.el`,
  `org-export-data`, and filters read it as a plist).

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

## Docstring & code layout tidy

The first-phase wrap-up (refactoring order item 7): reorder the whole
document — section headers, function order, docstrings.  Rules first,
then the current inventory.  Mechanical work; follow the rules, no
redesign.

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
- **Header hygiene**: correct spelling ("Fundamental"), no author
  names, no arithmetic comments that drift out of date.

### Inventory (as of the src-block round)

- 231 `defun`/`defsubst` total.
- **No docstring (13)** — all in the jstools RPC area:
  `t--rpc-make-json`, `t--rpc-send`, `t--rpc-call`, `t--rpc-filter`,
  `t--rpc-sentinel`, `t--rpc-start`, `t--rpc-request-sync`,
  `t--rpc-request!`, `t-toggle-jstools-debug`, `t--start-jstools`,
  `t--restart-jstools`, `t-launch-jstools`, `t--jstools-call`.
- **Docstring but no `declare` (23)** — `t-update-css-js`,
  `t--oinfo-cleanup`, `t-collect-oinfo-statistics`,
  `t-clear-oinfo-statistics`, `t--normalize-string`, `t--reference`,
  `t--get-headline-reference`, `t--format-checkbox`,
  `t--call-with-invalid-time-spec-handler`, `t--build-normal-headline`,
  `t-headline`, `t-clear-css`, `t-preamble-default-function`,
  `t--build-toc`, `t--textarea-block`, `t-special-block`,
  `t-final-function`, `t-export-as-html`, `t-convert-region-to-html`,
  `t-export-to-html`, `t-publish-to-html`, `t--trim`, `t--nw-trim`.
  (The last two are `defsubst`; the `t-export-*`/`t-publish-*`/
  `t-convert-*` ones are end-user commands — exempt per the rules.)
- **Section headers**:
  - `;;;; Fundmental utilities` → "Fundamental".
  - `;;;; Some options added by include-yy` → rename, drop the author.
  - `;;;; Legacy home and up` → fold into `;;;; Navbar` or rename.
  - The refactored elements `;;;; Table` / `;;;; LaTeX` / `;;;; Link` /
    `;;;; Footnote` / `;;;; Engrave-faces subset` / `;;;; Source block` /
    `;;;; Special Block` are flat `;;;;` blocks after
    `;;; Template and Inner Template`; either group them under one
    `;;;` part (e.g. `;;; Refactored elements`) or add a note that
    they are intentionally flat.
  - `;;; Greater elements (11 - 3 - 2 = 6).` and its Lesser/Objects/
    Smallest siblings: the arithmetic has drifted; drop the numbers or
    recompute.

### Execution plan (defcustom order is primary)

Three passes; the defcustom order below is the primary one, and the
other two follow it.

1. **defcustom reorder** — group the defcustoms by the element/object
   they serve, in this order:
   1. Headline & section — todo / priority / tags,
      `format-headline-function`, `toplevel-hlevel`,
      `honor-ox-headline-levels`, `container-element`,
      `self-link-headlines`, `headline-cnt`,
      `zeroth-section-tocname`.
   2. Markup texts — `text-markup-alist`.
   3. Item & plain lists — `checkbox-type`.
   4. Table — `table-use-header-tags-for-first-column`.
   5. LaTeX & math — `with-latex`, mathjax config, math-head
      function, `equation-reference-format`.
   6. Link & images — `link-org-files-as-html`, `inline-images`,
      image rules, `link-home`/`link-up`, navbar.
   7. Footnote — `footnotes-section`, `footnote-format`,
      `footnote-separator`, `footnote-section-function`.
   8. Src block — `fontify-method`, `example-default-class`.
   9. Timestamp — timezone, formats, option, wrapper,
      format-function.
   10. Head & template — head / style / viewport / meta-tags /
       coding-system / `toc-element` / `back-to-top` / fixup-js /
       `extension`.
   11. Preamble & license — license, cc-budget, preamble, postamble,
       creator, validation-link.
   12. Misc — `indent`, `use-babel`.
   Merge the "Some options added by include-yy" group into its proper
   places and drop the author name.
2. **`:options-alist` reorder** — order the option entries to follow
   the defcustom order above; keep the `;;` group comments in sync.
3. **Internal functions & helpers reorder** — move misplaced helpers
   (e.g. `t--textarea-block`) next to their element and sort the
   helper sections to follow the same element order.

## Known issues

- **Link leftovers.**  The refactor is done, but a few spots are still weak
  or suspect (each carries a `FIXME` in the source): the cross-file ID
  fragment is built from `t--link-path`'s output instead of the raw path
  (`t--link-to-file`); `:html-link-home` / `:html-link-use-abs-url` are not
  implemented (`t--link-path`); `.org.gpg` files are not rewritten to
  `.html` (`t--link-org-files-as-html`); LaTeX equation references only
  cover math environments under `mathjax`/`t`; and coderef support is kept
  only for ox-html compatibility.
- `t--math-environment-p` is now unused (the ordinal machinery it fed was
  removed) and `t--link-broken` looks unreachable (Org handles broken links
  before the transcoder); both are `FIXME`-marked.
- `t--link-to-file`, `t--link-broken` and `t--link-coderef` still have no
  tests.

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
- **Src-block feature gaps vs ox-html.**  Dropped when forking and out
  of scope for this refactor round (judged low-value for W3C TR
  output).  Revisit later; each should slot in *between*
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

Explicitly out of scope until the refactor is otherwise complete.  Do not
start these now.

- **special-block (Web Component).**  Deferred to the start of phase 2.
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
  re-exports (and can be pre-assigned when a link points at a file that has
  not been exported yet).  This is Org's `org-export-get-reference` +
  `:crossrefs` + `org-publish-resolve-external-link` machinery; ox-w3ctr's
  `t--reference` currently only returns explicit CUSTOM_ID / ID / NAME, and
  `t--link-path` delegates to `org-publish-resolve-external-link`.  Planned
  to ride on `yynt`'s project-local SQLite (an `XREF(path, cell, anchor)`
  table with resolve / record / forget), behind a pluggable `t-xref-backend`
  so ox-w3ctr still works without it.  Not now.
- **Drop the `ox-publish` dependency.**  ox-w3ctr currently requires
  `ox-publish` (`ox-w3ctr.el:48`) and calls `org-publish-file-relative-name`
  and `org-publish-resolve-external-link` from `t--link-path` (4283, 4296),
  plus `org-publish-org-to` from `t-publish-to-html` (4834).  Replace them
  with local implementations: the first two belong with the planned crossref
  backend (`t-xref-backend`), the last with yynt's publish flow.  Related to
  the crossref non-goal above.  Not now.
