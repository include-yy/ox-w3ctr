# Design notes

Rationale and measured facts behind ox-w3ctr's design.  Read the relevant
section when working on that subsystem; not required reading at session
start.  The rules every session must follow live in `AGENTS.md`; the
backlog lives in the `Roadmap` section of `README.org`.

## Compile-time switches and conditionals

Four measured facts, learned while building `org-w3ctr-oinfo-enabled`:

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

## Custom elements (special-block)

The design is explicit over implicit: every custom element must be
registered in `org-w3ctr-special-block-custom-elements`, a
`(NAME . PLIST)` alist.  The back-end does not guess by name pattern.
Keys in the PLIST are read by two separate components: `:template`
(Declarative Shadow DOM) is inserted by the transcoder
(`org-w3ctr--special-block-custom`); `:src` and `:script` are read by the
default head function (`org-w3ctr-special-block-head-default-function`)
to build `<script>` tags.  Other keys are left to a custom head function.
The scan for used elements (`org-w3ctr--special-block-used-elements`) runs
at template stage, takes INFO, and has no global state; `:noexport:`
subtrees are excluded.  The exclusion is pruning's doing:
`org-export--prune-tree` extracts `:noexport:` subtrees from the parse
tree before the scan runs.  The INFO argument the scan passes to
`org-element-map` is therefore unobservable (`:ignore-list` never holds
special blocks): dropping it survives every test -- an equivalent mutant,
kept annotated in the local mutation harness.

## Table column groups

Org's `/`-row (`<`/`>`/`<>`) only marks group boundaries; it cannot carry
attributes, because its cells must be exactly those markers or Org's own
colgroup detection (`org-export-table-cell-borders`) breaks.  ox-w3ctr
emits `<colgroup span="N">` and does **not** invent a per-group class
syntax.  To style a group, put a class on the table (`#+attr__: [foo]` or
`#+attr_html: :class foo`) and use a CSS positional selector, e.g.
`.foo colgroup:nth-of-type(2) { ... }`.  Only
`border`/`background`/`width`/`visibility` apply to `<colgroup>`;
`text-align` does not (alignment is emitted inline on the cells).
Per-column width would need `<col>`, which is deliberately dropped.

## Object theming contract (planned)

Style complex objects by injecting CSS custom properties, not by emitting
bespoke classes.  `#+attr__` can set a `style` attribute, so a table/block
can carry `style="--w3c-accent: ..."`; the stylesheet consumes it with
`var(--w3c-accent, fallback)`.  Each complex object should expose a small,
documented set of custom properties.  Target children with
structural/positional selectors (`colgroup:nth-of-type(n)`,
`tr:nth-child(n)`, `td:first-child`), never with invented classes.
Custom properties inherit down the subtree (and pierce Shadow DOM), so the
same mechanism doubles as the theming API for DSD/Web Components.
(Rationale: several ox-html carry-over classes such as
`org-left`/`org-center`/`org-right` and `t-above`/`t-bottom` are not
defined by the W3C CSS, i.e. dead; do not add more of those.)

## Math pipeline

LaTeX fragments reach the `jstools` Node helper as `\(...\)`/`\[...\]`
(`t--normalize-latex`); the helper does all the MathJax work — extract the
TeX, convert with the promise API, clean up the result — and returns
ready-to-embed markup.  `mathml-by-mathjax` and `svg-by-mathjax` are
therefore thin one-line RPC calls on the Emacs side.  The helper loads
`ui/safe`, without which the auto-loaded `html` TeX extension lets
`\href{javascript:...}`, `\style` and `\class` through.

## Fontify pipeline

See also `doc/fontify.org` for the full details (face table, colours, the
Node engine, writing an engine of your own).

`org-w3ctr-fontify-code` turns the method of a block (its `:fontify`
header argument, else `#+OPTIONS: fontify:`, else
`org-w3ctr-fontify-method`) into a chain of engines ending with `plain`
(`t--fontify-chain-for`), and `t--fontify-dispatch` runs the engines of
`org-w3ctr-fontify-engines` (`engrave`, `jstools`, `plain`) until one
returns tokens.  The contract:

- An engine returns `((CLASS . TEXT) ...)` or `:decline`, never HTML.
  TEXT is raw code; CLASS is nil, a slug of the face tables, or
  `(style . CSS)` under the `inline` face fallback.
  `t--fontify-check-tokens` rejects tokens that do not add up to the code
  or carry an unknown class; `t--fontify-merge` joins neighbours of one
  class; `t--fontify-render` is the only place that escapes.
- Fallback, in three layers: a face without a slug follows its `:inherit`
  chain (`t--engrave-face-slug`); a declining engine passes the block on,
  and a failing or unavailable one is handled by `:html-fontify-on-error`
  and broken for the rest of the export; `plain` takes what is left, and a
  language every engine declined goes through
  `:html-fontify-unknown-language`.  Everything is counted in INFO's
  `:html-fontify-state` and shown by `M-x org-w3ctr-show-fontify-report`.
- Measured facts: Emacs 31 *asks* to install a missing tree-sitter
  grammar, which hangs a batch export, so `t--engrave-tokens` binds
  `treesit-auto-install-grammar` to `never` (declared with `defvar`, or
  the `let` would be lexical and do nothing) and declines a `*-ts-mode`
  left without a parser.  In batch, `color-values` rounds to the 8-colour
  terminal palette (`#1f5bff` comes back pure blue), so inline styles use
  `tty-color-standard-values`.  Mode hooks run delayed by default, and
  `font-lock-ensure` still fontifies everything under `delay-mode-hooks`.
- The `jstools` engine calls `highlightLanguages` once per export and
  `highlight` per block; the helper answers `[TEXT, SLUG]` pairs.  Slugs
  come from `jstools/lib/slugs.json` through a synthetic Shiki theme whose
  fake colours (`#000001`, ...) decode back to slugs, so Shiki's own scope
  matching picks them.  Error `-32010` means "unknown language", i.e.
  decline.
- Colours: edit `org-w3ctr-fontify-palette`, then run `M-x
  org-w3ctr-fontify-update-stylesheet`, which rewrites the marked block of
  `assets/style.css`.  Tests tie the palette, that block, the face table
  and `slugs.json` together, and check WCAG contrast ≥ 4.5 on every code
  background in both themes.
- The refactor changed one thing in the frozen `;;; Basic utilities`:
  `t--jstools-methods` gained `highlight` and `highlightLanguages` (guarded
  by `t--jstools-methods-drift`).

## JSON-RPC transport (`jsonrpc.el`)

The `jstools` connection is a callable `org-w3ctr--jrpc` oclosure over a
`jsonrpc-process-connection`; two measured facts to keep:

- **No restart.**  `jsonrpc.el` sets the process once in
  `initialize-instance` (buffer, filter, sentinel, coding and stderr are
  all installed there), and `jsonrpc-shutdown` only tears down.  To
  restart, discard the connection and build a fresh one
  (`org-w3ctr--jrpc-ensure` / `-restart`).  Never `setf` `jsonrpc--process`.
- **`:process` must be a function, and it must pass `:stderr`.**  The
  `:process` initarg is called from `initialize-instance` *after* a
  `*NAME stderr*` buffer is created, so the factory has to hand that buffer
  to `make-process` as `:stderr` (the "bad coupling" jsonrpc.el itself
  flags with a FIXME).  Passing a ready process silently loses stderr
  separation and merges it into stdout, corrupting the stream.
