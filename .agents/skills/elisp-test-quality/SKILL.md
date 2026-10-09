---
name: elisp-test-quality
description: Triage the quality of an Emacs Lisp ERT test file — find tests that cannot fail, leak global state, depend on the environment, leave a function untested, or sit out of source order. Use when tidying or reviewing a test suite, or before freezing a refactored section.
---

# Emacs Lisp test quality

Judge a suite by whether it *can disagree* with the code: break an
assumption and it must go red, and the verdict must be the same every
run.  This skill produces a **triage list**, not a pass/fail gate — the
tags are heuristics with known false positives, and a clean scan is
evidence, not proof.

## Workflow

1. Static triage of the test file:

   ```bash
   emacs --batch -l scripts/test-scan.el -- TESTS.el [ASSERT-HELPER...]
   ```

   Trailing arguments name assertion *helpers* the suite calls instead
   of `should`; a test that only calls one of them is not reported as
   `NO-ASSERTION`.  Also read `references/quality.md` for the judgment
   pass the scripts cannot do.

2. Leak diff over the same file:

   ```bash
   emacs --batch -L . -l scripts/test-leaks.el -- TESTS.el
   ```

   It loads the file, then runs each test alone with a snapshot of the
   live buffers, processes and advised functions taken around it, so a
   leak is attributed to the test that caused it.

   Pass the suite's environment controls with `--eval` before `-l` when
   it has any (this repo:
   `--eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))"`).
   Without them the suite can fail for unrelated reasons and the diff
   is not comparable.

3. Order dependence over the same file:

   ```bash
   emacs --batch -L . -l scripts/test-isolation.el -- TESTS.el [--rounds N]
   ```

   It runs the suite in definition order, then in `--rounds` (default 3)
   shuffled orders, each in a separate Emacs process (so state cannot
   carry between orders), and reports every test whose result differs.
   The same `--eval` environment controls apply.

4. Function-to-test map over the source and the tests:

   ```bash
   emacs --batch -Q -l scripts/test-map.el -- SOURCE.el TESTS.el [--lines MIN MAX] [--map]
   ```

   It reports `UNCOVERED` functions (no test names or references them)
   and `ORPHAN-TEST` tests (naming or referencing no source definition);
   `--lines` restricts the source functions, and `--map` prints the
   `MAP FUNCTION TEST...` database the later phases need.

5. Test order over the source and the tests:

   ```bash
   emacs --batch -Q -l scripts/order-check.el -- SOURCE.el TESTS.el [--part NAME]
   ```

   It reports `OUT-OF-ORDER` tests (a test that maps to a function whose
   source definition comes after a function already tested in the same
   section) and `SECTION-ORDER` (a `;;;;` section out of source order).
   The convention it checks: a test section per source section, and
   inside a section the source function order, with a variant test next
   to its target.  `--part NAME` restricts it to one major part.

6. Branch coverage of the source under the suite:

   ```bash
   emacs --batch -L . -l scripts/test-cover.el -- TESTS.el SOURCE.el \
     [--lines MIN MAX] [--one-value] [--noreturn FN]...
   ```

   It instruments SOURCE with `testcover`, runs the suite against the
   instrumented definitions, and reports every form that never executed
   as `UNCOVERED`, with its function and source line.  Pass the same
   `--eval` environment controls.  `--noreturn FN` declares a wrapper
   around `signal`/`error` (this repo: `org-w3ctr-error`), which
   testcover would otherwise mark red forever.

7. Triage each candidate: fix, annotate, or dismiss.  Record the
   outcome, so a dismissed tag is not rediscovered next run.

## Script output

`test-scan.el` prints one line per finding:

```
<TAG>  FILE:LINE  TEST-NAME  [DETAIL]
```

| Tag | Candidate |
|---|---|
| `NO-ASSERTION` | test body has no assertion macro or helper |
| `GLOBAL-WRITE` | `setq`/`push`/`setf`/… on a symbol not bound in the test |
| `SETQ-LOCAL` | a buffer-local write; check whether it persists |
| `ADVICE-LEAK` | `advice-add` without a matching `advice-remove` |
| `FSET-LEAK` | `fset` without `fmakunbound` |
| `BUFFER-LEAK` | `get-buffer-create` without `kill-buffer` |
| `PROCESS-LEAK` | a process started without `delete-process` |
| `SKIP` | `skip-unless`/`skip-when`/`ert-skip`: a coverage hole |
| `ENV-SENSITIVE` | clock, randomness, network, filesystem or subprocess |

`test-leaks.el` prints `<TAG>  TEST-NAME  ITEM` for
`LEAK-BUFFER`/`LEAK-PROCESS`/`LEAK-ADVICE`, and a summary
(`;; N tests, M unexpected; leaks: K`).  One-time lazy initialization
(Org parsing, its advice) shows on whichever test triggers it first;
that cluster is infrastructure, not that test's fault — dismiss it and
read the rest.

`test-isolation.el` prints `ORDER-DEPENDENT NAME  suite=KIND
shuffled=KIND` and a summary (`;; N tests, M order-dependent`).  A test
reported as `suite=pass shuffled=fail` depends on an earlier test's
leftovers or on the suite context; `suite=fail shuffled=pass` means the
suite interferes with it.

`test-map.el` prints `UNCOVERED SOURCE:LINE  FUNCTION`,
`ORPHAN-TEST TESTS:LINE  TEST`, and (with `--map`)
`MAP FUNCTION TEST...`.  `UNCOVERED` is a *direct-reference* gap: a
function exercised only through another one (a helper called by a
covered transcoder) shows here too, so triage it before adding a test.
`ORPHAN-TEST` is the mirror, and a drift or source-scanning test — one
that inspects the test file rather than the source — is an expected
false positive.

`order-check.el` prints `OUT-OF-ORDER TESTS:LINE  TEST  SECTION: maps to
FUNCTION (source line N), after FUNCTION2 (source line M)` and
`SECTION-ORDER TESTS:LINE  SECTION  maps to source line N, after
SECTION2 (source line M)`, plus a summary (`;; N sections checked, M out
of order`).  A test naming nothing in its section is skipped
(`test-map.el` owns `ORPHAN-TEST`), and so are variables and constants
without a test of their own.  A clean run says the order mirrors the
source, not that the tests are good.

`test-cover.el` prints `UNCOVERED SOURCE:LINE  FUNCTION  TEXT` for the
branch-level gaps, a summary (`;; N instrumented, M uncovered forms in
K functions`), and the suite's own result.  It catches what
`test-map.el` cannot: a function whose *some* branches are tested and
others are not.  `define-inline'd helpers may not be instrumented, and a
form behind an indirection is missed.

## Keeping the checkers honest

A checker is only worth running if it *can* disagree with the code.
Before trusting a clean run, seed each defect and confirm the script
reports it:

- leak: `(get-buffer-create "*qa-leak*")` in a test → `LEAK-BUFFER`;
- global write: `(setq qa-x 1)` with no binding → `GLOBAL-WRITE`;
- dead test: `(ert-deftest qa-empty () "x" (ignore 1))` → `NO-ASSERTION`;
- environment: `(current-time)` → `ENV-SENSITIVE`;
- advice: `(advice-add #'car :before #'ignore)` → `ADVICE-LEAK`;
- test order: a test after a later function's test → `OUT-OF-ORDER`,
  and a `;;;;` section after a later one → `SECTION-ORDER`;
- order: one test sets a global, another reads it → `ORDER-DEPENDENT`
  when the shuffle runs the reader first.

`references/self-check.md` has the seed file and the exact output to
expect.

If a seeded defect is not reported, the checker is broken: fix it or
drop it.  Cross-check a surprising verdict with a second tool before
believing it (`ox-w3ctr-verify`'s `references/checker-design.md` has
two false alarms this rule would have caught).

## Mutation testing: designed, deferred

A `test-mutate.el` phase was designed but deliberately not built.  It is
the costliest phase and the noisiest, for reasons specific to a back end
whose behaviour depends on `load-file-name`, compile-time switches and
exact markup.  The four phases above already answer whether a test can
fail, whether it leaks, whether it is order-dependent, whether every
function is referenced, and whether every branch is reached; mutation
only adds "executed but not asserted", at the price of
equivalent-mutant triage and minutes of runtime.  See
`references/mutation.md` for the scoped design to use if it is ever
revisited.
