# Two-role docstring pass

Review the *prose* of a whole region — a section, a family of helpers —
with a reader that never sees the code, and an editor that decides what
the text must say.  It finds missing **contract** facts (identity,
defaults, return values, hazards), not better wording.

Use it after the per-definition passes, when several docstrings and the
comments around them should read as one unit.

## Roles

| Role            | Does                                                                               | Must not                               |
|-----------------|------------------------------------------------------------------------------------|----------------------------------------|
| **Author**      | Writes or revises the docstrings                                                   | —                                      |
| **Dull reader** | Asks what the text fails to say, bluntly                                           | Suggest wording, rewrite, praise       |
| **Editor**      | Sorts the questions, rewrites what is in scope                                     | Invent facts the code does not support |
| **Referee**     | `checkdoc`, line widths, `declare` forms, the test suite, the code as ground truth | Be a model's opinion                   |

The reader asks *questions only*: the moment it proposes wording it is a
second author, and the roles collapse.  The editor never lets prose be
the evidence — every claim is checked against the code.

Two roles played by one model share one set of priors and converge on
fluent, plausible, *wrong* text; prose has no loss function, so the loop
needs a verifiable anchor.  Here the anchors are the code, the mechanical
rules, and the tests.

## Loop

1. **Extract** the prose of the region (see below).
2. **Ask**: run the dull reader on the extracted file.
3. **Classify** every question (the expensive half; see below).
4. **Apply** the real gaps, then run the referee.

The reader is a separate process, so its context is clean:

```sh
emacs --batch -l scripts/extract-docstrings.el FILE '^;;; SECTION' '^;;; NEXT' \
      prose.txt 't-'          # SHORTHAND, optional: prints real names

cd "$(dirname prose.txt)"     # the reader must not stumble on the source
pi -p "You are a dull, impatient Lisp programmer who has never seen this module's code.

Read the file prose.txt: the comments and docstrings of one section of a module, with each definition's kind, argument list, default value and declare form.

List every question you cannot answer from that text, numbered, one per line, questions only.
Rules:
- Ask only what a user of this API or a maintainer of this module must know — not what only the code can show (bodies, the full key list, internal mechanics).
- No suggestions, no rewrites, no praise, no summary. Blunt and short.
- Do not read any other file." --tools read --no-session
```

`pi -p` is one-shot text; any equivalent isolated call works.  A round
costs one model call (25–80 s in practice).

## The extraction

`extract-docstrings.el` prints comment lines verbatim (blank lines kept),
then for every definition: its **kind**, name, **argument list** or
**default value**, its **`declare` form**, and the docstring.  Nothing
else — no code, so the reader cannot answer from the body.

Include the metadata: without kind/args/default/declare, readers spend
their questions on things the source already states (`is it a defvar?`,
`what are the arguments?`).  Keep the region on whole top-level forms.
Let the reader know it may consult the code *to use* the API; ask it to
question only what a user or maintainer must know.

## Classifying the questions

Sort each question into exactly one bucket, and write the bucket down:

- **Already answered** — the text says it; the reader was lazy or did not
  cross-read.  No change (do not pad the docstring).
- **Extraction artifact** — the question is about metadata the extractor
  omitted (it belongs back in the extractor, not in the docstring).
- **Real gap** — a fact a user or maintainer needs and the text omits.
  Fix it, in the *shortest* form that states the contract.
- **Out of scope** — mechanisms, bodies, full data (the key list, exact
  sort order).  No change; the code is the reference.

Stop when every question is either answered by the text or explicitly
out of scope.  Without that condition the loop runs forever, polishing.

Repetition is signal: the same question from two independent readers
means the text really is ambiguous.  One reader's questions are cheap and
low-precision; the classification is what costs judgement.

## Calibration

Measured on `ox-w3ctr.el`'s OINFO section (13 docstrings, ~7 KB of prose):

- Round 1, 20 questions in 22 s — 7 already answered, 4 extraction
  artifacts, 3 out of scope, **5 real**.  Two were contract-level:
  the plist is compared with `eq` (identity, not `equal`), and a change
  *inside* a plist is invisible to the cache.
- Round 2, after those fixes, with the metadata in the extract and the
  "API user or maintainer" rule: 11 questions, 3 real (the precision of
  "later `pget` calls", what `CNT` counts, one cache entry per key).

Roughly a quarter of the questions survive classification — comparable to
a code review, at one round per region.

## Failure modes

- The reader suggests, praises or summarises: tighten the prompt, discard
  the round.  Questions only.
- The editor trusts the questions: a question can be answered, wrong, or
  out of scope.  Check each against the code.
- **Stale state**: a round run against a file that has since changed
  yields findings about a file that no longer exists.  Re-extract from
  the current source every round (the extractor always reads the file).
- Prose is not evidence.  A confident sentence can be false; only the
  code, the tests and the rules are ground truth.

## Referee (this repository)

```sh
emacs --batch -L . --eval "(require 'checkdoc)" ...          # checkdoc
emacs --batch -L . -l ox-w3ctr-tests.el -f ert-run-tests-batch-and-exit
tr -cd '\r' < ox-w3ctr.el | wc -c                            # expect 0
```

Summary line at most 74 characters, body lines around 72, arguments in
UPPERCASE, `(declare (ftype ...))` and return values documented where
non-obvious (see `references/gnu-docstring-rules.md`).  Baseline for the
test suite: 163 tests, 161 pass, 2 skipped.
