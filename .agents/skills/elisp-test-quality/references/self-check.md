# Seeding a defect, so the checker can disagree

A checker that has never reported anything is unproven.  Save this as a
throwaway file (this repo: under `scratch/`), run both scripts on it,
and confirm the expected lines appear.  A missing line means the
checker is broken; fix it or drop it.

## The seed

```elisp
;;; -*- lexical-binding:t; -*-
(require 'ert)

(defvar qa-x nil)

(ert-deftest qa-empty ()
  "No assertion at all."
  (ignore 1))

(ert-deftest qa-global ()
  "Writes a global."
  (setq qa-x 1)
  (should qa-x))

(ert-deftest qa-advice ()
  "Adds advice and never removes it."
  (advice-add #'car :filter-return #'identity)
  (should t))

(ert-deftest qa-env ()
  "Depends on the clock."
  (should (numberp (float-time))))

(ert-deftest qa-leak ()
  "Leaks a buffer."
  (get-buffer-create "*qa-leak*")
  (should t))
```

## Expected

`test-scan.el`:

```
NO-ASSERTION   FILE:6   qa-empty
GLOBAL-WRITE   FILE:10  qa-global  qa-x
ADVICE-LEAK    FILE:15  qa-advice
ENV-SENSITIVE  FILE:20  qa-env  float-time
BUFFER-LEAK    FILE:24  qa-leak
;; 5 tests, 5 findings
```

`test-leaks.el`:

```
LEAK-ADVICE   qa-advice  car
LEAK-BUFFER   qa-leak    *qa-leak*
;; 5 tests, 0 unexpected; leaks: 2
```

Each tag has exactly one seeded cause here, so a missing line is a real
checker regression, not a triage judgement.

## Order dependence

For `test-isolation.el`, a second seed file whose second test needs the
first test's global:

```elisp
;;; -*- lexical-binding:t; -*-
(require 'ert)
(defvar qa-order nil)
(ert-deftest qa-a-set () "Sets the global." (setq qa-order t) (should t))
(ert-deftest qa-b-use () "Needs qa-a-set first." (should qa-order))
```

Expected (a few rounds; the reader must come first in at least one):

```
;; test-isolation
ORDER-DEPENDENT qa-b-use  suite=pass shuffled=fail
;; 2 tests, 1 order-dependent
```

`qa-a-set` must *not* be reported: it passes in every order.

## Function-to-test map

For `test-map.el`, a small source and test file:

```elisp
;; qa-src.el
;;; -*- lexical-binding:t; -*-
(defun qa-used () "Used by a test." 1)
(defun qa-unused () "Never referenced." 2)
(defconst qa-const 3 "A constant, not a function.")
```

```elisp
;; qa-map-tests.el
;;; -*- lexical-binding:t; -*-
(require 'ert)
(ert-deftest qa-used-tests ()
  "Calls qa-used."
  (should (qa-used)))
(ert-deftest qa-orphan ()
  "References nothing defined in the source."
  (should (org-export-string-as "x" 'w3ctr)))
```

Expected:

```
;; test-map
UNCOVERED qa-src.el:3  qa-unused
ORPHAN-TEST qa-map-tests.el:6  qa-orphan
;; 2 functions, 1 uncovered; 2 tests, 1 orphan
```

Note what is *not* reported: `qa-used` is covered by `qa-used-tests`
through its body, and `qa-const` is not a function, so it is not
`UNCOVERED`.

## Branch coverage

For `test-cover.el`, a source with an untaken branch and a test that
takes only the other:

```elisp
;; qa-cov-src.el
;;; -*- lexical-binding:t; -*-
(defun qa-check (x)
  "Return 1 for `a', else a random number."
  (if (eq x 'a) 1 (random 100)))
```

```elisp
;; qa-cov-tests.el
;;; -*- lexical-binding:t; -*-
(require 'ert)
(ert-deftest qa-check-tests ()
  "Only the true branch."
  (should (qa-check 'a)))
```

Expected:

```
;; test-cover: qa-cov-src.el
UNCOVERED  qa-cov-src.el:4  qa-check  (if (eq x 'a) 1 (random 100)))
;; 1 instrumented, 1 uncovered forms in 1 functions; 0 one-value
;; suite: 1 expected, 0 unexpected
```

The untaken branch must be a form testcover does not regard as 1value:
a constant (`nil`) or a pure call (`(list 2)`) gets a tan `ONE-VALUE`
mark, not red, so pick something that can vary (`(random 100)`,
`(buffer-name)`).  The suite must also be clean (`0 unexpected`), or a
failing test leaves its remainder uncovered and the marks are noise.

## Test order

For `order-check.el`, a small source and two test files.  The source:

```elisp
;;; qa-order-src.el --- seed -*- lexical-binding:t; -*-
;;; A part
;;;; First
(defun qa-a () "A." 1)
(defun qa-b () "B." 2)
;;;; Second
(defun qa-c () "C." 3)
```

A test file with the two `First` tests swapped:

```elisp
;;; qa-order-fn-tests.el --- seed -*- lexical-binding:t; -*-
(require 'ert)
;;; A part
;;;; First
(ert-deftest qa-b-tests () "Seeded." (should (qa-b)))
(ert-deftest qa-a-tests () "Seeded." (should (qa-a)))
;;;; Second
(ert-deftest qa-c-tests () "Seeded." (should (qa-c)))
```

Expected:

```
;; order-check
OUT-OF-ORDER qa-order-fn-tests.el:6  qa-a-tests  First: maps to qa-a (source line 4), after qa-b (source line 5)
;; 2 sections checked, 1 out of order
```

A second test file with the sections swapped:

```elisp
;;; qa-order-sec-tests.el --- seed -*- lexical-binding:t; -*-
(require 'ert)
;;; A part
;;;; Second
(ert-deftest qa-c-tests () "Seeded." (should (qa-c)))
;;;; First
(ert-deftest qa-a-tests () "Seeded." (should (qa-a)))
(ert-deftest qa-b-tests () "Seeded." (should (qa-b)))
```

Expected:

```
;; order-check
SECTION-ORDER qa-order-sec-tests.el:6  First  maps to source line 3, after Second (source line 6)
;; 2 sections checked, 1 out of order
```

A clean run prints the header and the summary only: no
`OUT-OF-ORDER`/`SECTION-ORDER` line.
