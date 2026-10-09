# Classifying scan results

The scanner reports raw library names from `symbol-file`; it cannot tell
a real dependency from a local name or a preloaded built-in.  Use this
guide on the `USED BUT NOT REQUIRED` section.

## Decide by `feature=` first

- `feature=nil` — the library is **not loaded** after the target loads:
  the code relies on autoload alone.  This is the strongest "add a
  `require`" signal.  Verify the symbol is a genuine call/variable use
  (below), then add `(require 'lib)`.
- `feature=t` — the library is already loaded, either preloaded at dump
  time or pulled in by one of the existing requires.  Adding a require
  is an *explicitness* choice, not a correctness fix.  Do not add
  requires for preloaded libraries.

## Preloaded (no require needed)

These are dumped with Emacs; `(require 'lib)` on them is a harmless no-op.
Check the exact set with `emacs --batch -Q --eval "(featurep 'lib)"`.

`subr` `files` `simple` `gv` `pcase` `mule` `format` `rx` `regexp-opt`
`repeat` `byte-run` `custom` `faces` `indent` `version` `window`
`oclosure` `tabulated-list` `backquote` `loaddefs`

(`pcase` and `gv` are preloaded but this project requires them anyway;
that is a style choice, not a bug.)

## Transitive org libraries (do not require)

Pulled in by `ox` / `ox-html` / `org`.  They are dependencies of Org,
not of this package, and requiring them again would be noise:

`org` `org-element` `org-element-ast` `org-macs` `org-compat`
`org-src` `ob-exp` `ox-latex` `font-lock` `eieio`

Exception worth a FIXME, not a require: `ox-latex`'s
`org-latex--environment-type` is a *cross-backend internal* (double
dash) reached from the Link section.  It works because ox-html loads
ox-latex transitively, but it is the most fragile of the transitive
uses.

## Local-name false positives

The walker records every symbol in the code, including parameter and
local variable names.  A symbol that is also a global function or
variable shows up as if the code depended on it.  Known ones in this
project: `info` (parameter), `term`, `zone`, `align` (locals), `diary`
(data symbol).  Before adding a require, grep the file for the symbol
and confirm at least one use is a real call `(sym …)` or a bare
variable reference, not a binding in `let`/`lambda`/`defun`.

## Verify a candidate

1. `grep -n 'SYMBOL' FILE` and read each hit — call/reference vs
   binding vs docstring/comment.
2. For a function: `emacs --batch -Q --eval "(fboundp 'SYMBOL)"` — nil
   means genuinely undefined (a hard missing require, not just
   autoload).
3. For a variable: use `(boundp 'SYMBOL)`.

## Removing a require

A `UNUSED` require (used=0) is a remove candidate, but check first:

- the library may be needed at *compile* time for a macro, even if the
  macro's own name does not survive into the scanned code (rare);
- `git log -S"require 'lib"` to see why it was added;
- a require for a preloaded/transitive lib may be intentional
  documentation (this project requires `pcase`/`inline` explicitly).

After any change: byte-compile the file and run the test suite (see
AGENTS.md).  For a global change, run the verify skill's corpus.
