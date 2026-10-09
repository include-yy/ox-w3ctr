---
name: elisp-requires
description: Analyze and adjust the `require` forms of an Emacs Lisp file — find libraries used but not required (add them) and required but unused (remove them). Use when tidying dependencies, after moving code between files, or when asked whether a file's requires are complete.
---

# Adjusting Elisp `require` forms

Scan a file, then add the missing requires and drop the redundant ones.
The scanner reads the file's own `read-symbol-shorthands`, so package
shorthands resolve exactly as in the source.

## Workflow

1. **Scan.**  From the repository root:

   ```bash
   emacs --batch -L . -l .agents/skills/elisp-requires/scripts/scan-requires.el -- ox-w3ctr.el
   ```

2. **Read `=== REQUIRED ===`.**  Each line is `LIB line N used=K SYMS...`.
   `used=0 UNUSED` flags a redundant require.  Before removing, follow
   `references/classifying.md` (compile-time macro? why was it added?).

3. **Read `=== USED BUT NOT REQUIRED ===`.**  Classify every line with
   `references/classifying.md`:
   - `feature=nil` + a real call/variable use → add `(require 'lib)`.
   - `feature=t` → preloaded or transitive; only add for explicitness,
     never for preloaded libs.
   - the symbol is only a local name (`info`, `term`, `zone`, `align`,
     `diary`) → ignore.

4. **Edit the require block.**  Keep it alphabetical, one `(require 'x)`
   per line, and put runtime-optional requires with the rest.  Do not
   add requires for preloaded or transitive-org libraries (the list is
   in `references/classifying.md`).

5. **Verify.**  Byte-compile the file, then run the ERT suite:

   ```bash
   emacs --batch -L . --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))" -l ox-w3ctr-tests.el -f ert-run-tests-batch-and-exit
   ```

## Rules

- Only require what the code calls or reads.  A symbol in a docstring
  is not a dependency; the scanner only walks code (strings and
  comments are skipped).
- A `feature=nil` line means the code currently works by autoload.
  Adding the require makes the dependency explicit and version-proof;
  it is the one case where you should act without hesitation.
- `ox-latex` (`org-latex--environment-type`) is a cross-backend reach
  into an internal function.  Do not require it; record a FIXME in the
  Link section instead.
- After a global require reorder, run the ox-w3ctr-verify skill: a
  require change can shift load order, which a definition-time switch
  or a defconst could observe.
