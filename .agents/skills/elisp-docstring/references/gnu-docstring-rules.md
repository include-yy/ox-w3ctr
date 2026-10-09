# GNU Emacs Lisp docstring rules

Reference for the `elisp-docstring` skill.  Condensed from the GNU
Emacs Lisp Reference Manual, Appendix D "Documentation Tips".

## I. Structure

1. **Summary line**: 1-2 complete, standalone sentences; capitalised;
   ends with a period; mentions the important arguments (required ones
   first) in call order.  Imperative mood ("Return ...", not
   "Returns ...").
2. The summary answers "what does this function do?" (or "what does
   this value mean?" for a variable), from the caller's point of view.
   Do not describe implementation details.
3. Blank line after the summary, then the body: full sentences
   explaining usage and details.  In this project the body may go
   beyond strict caller-contract style and mention implementation
   details — internal helpers, algorithm steps, internal use — because
   the reader is the AI maintainer (see AGENTS.md "The AI is the
   maintainer").  More detail helps, within reason; do not turn the
   body into a restatement of the implementation.
4. Emacs shows only the first paragraph (up to the first blank line)
   for disabled commands — put critical information there.

## II. Format

1. No leading or trailing whitespace in the string.  Do not indent
   continuation lines to align with the source.
2. Summary line ≤ 74 characters (for apropos); body lines ≤ 60
   characters recommended.
3. Separate logical sections with blank lines.  Prefer manually
   wrapped lines over auto-fill.
4. For pre-Emacs-27 compatibility, precede a line starting with `(`
   by a backslash: `\(a buffer position)`.  Skip this if the project
   does not support old Emacs.
5. Two spaces between sentences (traditional GNU convention).
6. ASCII only: write "--" for an em-dash, "->" for an arrow, "..."
   for an ellipsis.  Never emit non-ASCII punctuation such as "—",
   "→", or "…".

## III. Style

1. Mood: summary in imperative; later paragraphs may use declarative
   sentences with a clear subject.
2. Active voice, present tense.  No future tense ("A list will be
   returned" → "Return a list").
3. Avoid "cause" ("Cause Emacs to display X" → "Display X").
4. Avoid "iff"; write "if" or "if and only if".
5. Write out Latin abbreviations: "for example", "that is", "with
   respect to".

## IV. Quoting

1. Reference argument values in UPPERCASE.
2. Quote Lisp symbols with a backquote and apostrophe: `` `foo' ``.
   Leave `t` and `nil` bare.
3. Never change a symbol's case; rewrite the sentence if a lowercase
   symbol lands at the start.
4. Do not quote non-symbol expressions (a list shape): write
   `(NAME TYPE RANGE)`, not `` `(NAME TYPE RANGE)' ``.
5. A literal apostrophe is `\\='`; a literal backquote is `` \\=` ``.

## V. Links to Lisp symbols

1. `` `foo' `` auto-links when FOO has a definition.
2. Disambiguate with a keyword: variable / option / function /
   command, e.g. `` the variable `buffer-file-name' ``.
3. Prevent a link with the keyword `symbol` or `program`.
4. Faces need the keyword `face` before or after the name.

## VI. Links to external resources

- Info: `See Info node \`Font Lock'.`
- Man page: `See the man page \`chmod(1)' for details.`  (prefer Info
  when possible)
- Customization group: `See the customization group \`whitespace'.`
- URL: `(see URL \`https://www.gnu.org/')`

## VII. Key bindings

1. Never hard-code keys; use `\\[command]`.
2. Declare a major-mode map with `\\<map>` once, before the first
   `\\[...]`, ideally at a paragraph start.
3. Avoid repeating `\\[...]` for the same command in one docstring.

## VIII. Type-specific conventions

1. **Predicates**: start with "Return t if ..." (or "Return non-nil if
   ..." when the true value is not guaranteed to be `t`).
2. **Boolean variables**: start with "Non-nil means ..."; say what nil
   and non-nil each mean.
3. **Context-specific commands**: state the context, e.g. "In Dired,
   visit the file ...".
4. **User options**: use `defcustom` and describe each choice.

## IX. Return value and errors

1. State the return value, and explicitly what `nil` means ("Return
   nil if FILE does not exist").
2. Document error signals: which conditions signal `error` /
   `user-error`, and with what message.

## Verification

Run `checkdoc` on the definition.  It checks the summary sentence,
UPPERCASE arguments, quoting format, and common wording mistakes
("pathname" → "file name").
