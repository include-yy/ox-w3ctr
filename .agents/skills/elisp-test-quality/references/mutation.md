# Mutation testing: the deferred phase

`test-mutate.el` was designed and deliberately not built.  This records
the design and why it is the last thing worth doing, so a future pass
does not rediscover the pitfalls.

## What it would add

The other four phases find tests that cannot fail (`test-scan`),
leaked state (`test-leaks`), order dependence (`test-isolation`),
direct-reference gaps (`test-map`), and unexecuted branches
(`test-cover`).  None of them finds a test that *executes* a branch but
does not assert its effect — for example a test that calls a transcoder
and checks only that it returned a string.  Mutation finds that: change
the branch's effect and the test should go red.

## Why it is deferred here

1. **The path coupling.** `org-w3ctr--dir` is computed from
   `load-file-name`, so a mutated *copy* of the source is loaded from the
   wrong directory and every test that reads `assets/` fails (the same
   trap `test-cover.el` fell into).  Mutation must happen in memory.
2. **Compile-time switches.** `org-w3ctr--oinfo-cache-p` is an
   `eval-when-compile` constant, and `static-when`/`static-if` resolve at
   macro-expansion time.  Editing the `.el` text and reloading does not
   reproduce the shipped (`.elc`) build, and a stale `.elc` makes it
   worse.
3. **Equivalent mutants.** Mutations in defensive `or` fallbacks,
   `t-error` messages and pure/1value forms survive for legitimate
   reasons.  The output is a triage list with a high false-positive rate,
   not a score.
4. **Cost.** `mutants × suite time`.  Even 60 mutants is minutes, and the
   suite must be kept isolated.

## The scoped design, if revisited

Mutate **in memory**, never on disk:

1. `load` the source as usual (through the test file, so `t--dir` is
   right).
2. Pick a function that `test-map.el --map` says is covered; read its
   `defun` form from the source (with the file's
   `read-symbol-shorthands`).
3. Apply **one** mutation, from a small operator set aimed at this
   back end's observable markup:
   - `&amp;` ↔ `&`; `&lt;`/`&gt;`/`&quot;`/`&apos;` likewise;
   - `<td` ↔ `<th`, `<colgroup` ↔ `<col`, `class=` ↔ `id=`;
   - a `t`/`nil` literal flip in a predicate's return;
   - `eq` ↔ `equal`; `=` ↔ `<`;
   - drop a `\n` from a format string.
4. `eval` the mutated form (redefine the function in place), run only
   the tests `test-map.el` mapped to it, and record red/survived.
5. `eval` the original form to restore.
6. Report every **survivor** for triage.  Do not score.

Keep it opt-in and sampled: a handful of functions, three or so mutants
each.  Validate with a seed whose survivor is known — e.g. a source
function that returns `(list 1)` and a test that calls it but asserts
only `(should (qa-check 'a))` on the other branch; a mutation changing
`(list 1)` to `(list 2)` must survive.

## When it is worth it

Only when the suite is already clean on P0–P3 and a specific,
high-value section (say Reference or Table) needs "executed but not
asserted" confidence.  Not as a routine gate.
