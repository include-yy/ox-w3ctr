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

Expected baseline: **147 tests, 146 pass, 1 skipped** (`org-w3ctr-headline`).

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
MathJax / navbar / license / preamble / TOC layer.

Not done (still ported from ox-html, no `(declare ...)`, no tests):

- `src-block`, `inline-src-block` (+ `t-fontify-code`,
  `t-format-src-block-code`, the engrave-faces port)
- `special-block` (current WIP)

## Refactoring order (dependency-based)

0. shared helpers — round 1 done (`t--void-element`, `t--has-caption-p`);
   further proposals live in `zhua.el`
1. table — done (tests in `ox-w3ctr-tests.el`)
2. latex — done
3. link & image — done (tests in `ox-w3ctr-tests.el`)
4. footnote — done (tests in `ox-w3ctr-tests.el`)
5. src-block (largest, includes the engrave-faces port) — next
6. options — tidy the whole `:options-alist` (see the Options note)
7. special-block (Web Component design WIP)

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
- **Options.**  The `:options-alist` is a grab-bag: a few entries are
  grouped (`;; Link`, `;; Footnote`), most are not, and a couple carry
  inline `;; Options:` comments.  Planned tidy-up (after src-block,
  before special-block): order every entry, add a `;; Options:` comment
  wherever the semantics are not obvious, and consider replacing the
  cumbersome string options with `*-function` ones (as
  `t-footnote-section-function` does).  Also chart compatibility with
  ox-html: for each option, whether ox-html has the same name and
  semantics, a different one, or none, and quantify the result (e.g.
  "N of M options shared").  Keep the `:html-*` keyword names compatible
  where cheap.
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
- The unfinished Web Component `t-special-block` has been moved out of the
  back-end to `zhua.el`; it still needs `ox-w3ctr-component-registry` and
  `ox-w3ctr-collect-dependency`.

## TODO

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
  - `assets/style.min.css` is stale (24KB vs 49KB, June vs July): missing
    `#navbar`, `.ef-*` italic/weight, different colours.  Untracked and
    unused by the back-end (which reads `style.css` via `t-style-file`).
    Delete it or ignore it.
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

## Non-goals

Explicitly out of scope until the refactor is otherwise complete.  Do not
start these now.

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
