---
name: elisp-docstring
description: Write GNU-compliant docstrings for Emacs Lisp code (defun, defmacro, defcustom, defvar). Use when writing, revising, or filling in missing docstrings in Emacs Lisp.
---

# Emacs Lisp docstring

Write docstrings following the GNU Emacs Lisp Reference Manual,
Appendix D "Documentation Tips".  The complete rule set is in
`references/gnu-docstring-rules.md`; the summary below covers most
cases.  For reviewing a whole section at once, run the two-role pass in
`references/two-role-pass.md` (a reader that sees only the prose, plus a
mechanical referee).

## Workflow

1. Infer the contract from the code: the name, arguments, body,
   comments, and any existing partial docstring.
2. Write the summary line first: a complete sentence, imperative mood,
   capitalised, ending in a period, mentioning the important arguments
   in call order.  Keep it at most 74 characters.
3. Blank line, then the body.  Reference arguments in UPPERCASE.
   Explain the return value — especially what nil means — and any
   error signals.
4. For a predicate use "Return t if ..." (or "Return non-nil if ..."
   when the value is not guaranteed to be t).  For a boolean variable
   use "Non-nil means ...".
5. Run `checkdoc` when available to catch mechanical mistakes.
6. For a whole region — a section, a family of helpers — review the
   docstrings together afterwards: extract their prose, ask a separate
   "reader" what the text fails to say, and classify the answers.  See
   `references/two-role-pass.md`.

## Quick rules

- Imperative, active voice, present tense.  No "cause", "iff", or
  Latin abbreviations (write "for example", not "e.g.").
- Quote Lisp symbols with a backquote and apostrophe, like this:

  ```elisp
  "See `forward-char' for the command and `buffer-file-name' for the variable."
  ```

  Leave t and nil bare.  A literal apostrophe is `\\='`; a literal
  backquote is `` \\=` ``.  Never change a symbol's case to fit
  sentence position — rewrite the sentence instead.
- Key bindings: `\\[forward-char]`; declare the local map with
  `\\<map>` once before the first `\\[...]`.
- Sentences separated by two spaces (traditional GNU convention).
- A docstring starts and ends with no whitespace; do not indent
  continuation lines to align with the source.
- ASCII only: docstrings and comments use plain ASCII.  Write "--"
  for an em-dash, "->" for an arrow, "..." for an ellipsis.  Never
  emit non-ASCII punctuation such as "—", "→", or "…".
