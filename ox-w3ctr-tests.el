;;; -*- lexical-binding:t; no-byte-compile:t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ox-w3ctr)

;; Org's error messages quote with `...'; keep it as backticks so the
;; tests match whether they run under --batch or an interactive Emacs.
(setq text-quoting-style 'grave)

;;; Test helper functions
(defun $c (&rest args) "concat" (apply #'concat args))
(defun $s (a) "should" (should a))
(defun $n (a) "should-not" (should-not a))
(defun $q (a b) "should-eq" (should (eq a b)))
(defun $l (a b) "should-equal" (should (equal a b)))
(defun $nq (a b) "should-not-eq" (should-not (eq a b)))
(defun $nl (a b) "should-not-equal" (should-not (equal a b)))
(defmacro $e! (exp) "should-error" `(should-error ,exp))
(defmacro $e!l (exp val)
  "should-error-equal"
  `(should (equal (should-error ,exp) ,val)))
(defmacro $it (f &rest body)
  "Bind function to symbol `it'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'it) ',f))
     ,@body))

(defvar t-test-values nil
  "A list to store return values during testing.")
(defun t-advice-return-value (result)
  "Advice function to save and return RESULT.
Pushes RESULT onto `org-w3ctr-test-values' and returns RESULT.
Text properties are stripped from string results."
  (prog1 result
    (push (if (not (stringp result)) result
            (substring-no-properties result))
          t-test-values)))
(defun t-check-element-values (fn pairs &optional body-only plist)
  "Check that FN returns the expected values when exporting.

FN is a function to advice.  PAIRS is a list of the form
((INPUT . EXPECTED) ...).  INPUT is a string of Org markup to be
exported.  EXPECTED is a list of expected return values from FN.
BODY-ONLY and PLIST are optional arguments passed to
`org-export-string-as'.

Advice is installed on FN and removed in an `unwind-protect',
so a `signal' or `should-error' in any test case will not leak
the advice onto subsequent tests."
  (advice-add fn :filter-return #'t-advice-return-value)
  (unwind-protect
      (dolist (test pairs t)
        (let (t-test-values)
          (ignore (org-export-string-as
                   (car test) 'w3ctr body-only plist))
          (unless (equal t-test-values (cdr test))
            (ert-fail (list :input (car test)
                            :expected (cdr test)
                            :actual t-test-values)))))
    (advice-remove fn #'t-advice-return-value)))

(defun t-parse (type)
  "Return every element of TYPE in the current buffer."
  (org-element-map (org-element-parse-buffer) type #'identity))

(defun t-parse1 (type)
  "Return the first element of TYPE in the current buffer, or nil."
  (car (t-parse type)))

(defun t-get-parsed-elements (str type)
  "Parse STR as an Org buffer and return a list of elements of TYPE.

STR is a string containing Org content.  TYPE is an Org element type
symbol (such as \\='headline, \\='paragraph, etc)."
  (with-temp-buffer
    (save-excursion (insert str))
    (t-parse type)))

(defun t-get-element (str type)
  "Parse STR and return its first element of TYPE, or nil."
  (with-temp-buffer
    (save-excursion (insert str))
    (t-parse1 type)))

;;;; `t-check-element-values'

(ert-deftest t-check-element-values ()
  "Tests for `org-w3ctr-check-element-values'.
It catches FN's return values through the advice, compares them to
EXPECTED, signals `ert-test-failed' with the input/expected/actual on a
mismatch, and removes the advice in either case."
  (let ((in "#+begin_quote\n123\n#+end_quote")
        (out "<blockquote>\n<p>123</p>\n</blockquote>"))
    ;; A matching case passes and returns t.
    ($q (t-check-element-values #'t-quote-block (list (cons in (list out)))) t)
    ($n (advice-member-p #'t-advice-return-value 't-quote-block))
    ;; EXPECTED is in reverse call order: `org-w3ctr-test-values' is
    ;; push-accumulated, so two blocks come back as (SECOND FIRST).
    ($q (t-check-element-values
         #'t-quote-block
         (list (cons (concat "#+begin_quote\n1\n#+end_quote\n\n"
                             "#+begin_quote\n2\n#+end_quote")
                     (list "<blockquote>\n<p>2</p>\n</blockquote>"
                           "<blockquote>\n<p>1</p>\n</blockquote>"))))
        t)
    ($n (advice-member-p #'t-advice-return-value 't-quote-block))
    ;; A mismatch signals `ert-test-failed' with the diagnostic data.
    (let* ((err (should-error
                 (t-check-element-values #'t-quote-block
                                         (list (cons in (list "WRONG"))))))
           (data (cadr err)))
      ($q (car err) 'ert-test-failed)
      ($l (plist-get data :input) in)
      ($l (plist-get data :expected) '("WRONG"))
      ($l (plist-get data :actual) (list out)))
    ;; The advice must not leak after the failure either.
    ($n (advice-member-p #'t-advice-return-value 't-quote-block))))

;;; Fundamental utilities

(ert-deftest t-error ()
  "Tests for `org-w3ctr-error'."
  ($e!l (t-error "Hello world") '(org-w3ctr-error "Hello world"))
  ($e!l (signal '(t-error 1)) '(t-error 1)))

;;; Basic utilities

;;;; OINFO helpers

(defun t--oinfo-oget (prop)
  "Return the oclosure object for cached property PROP, or nil.
Return nil when PROP has no entry in `org-w3ctr--oinfo-cache-alist'."
  (when-let* ((f (alist-get prop t--oinfo-cache-alist)))
    (symbol-function f)))

(defun t-test-oinfo-oclosure (key)
  "Return the name of the test-only caching oclosure for property KEY.

It is deliberately distinct from `org-w3ctr--oinfo-oclosure', so that
the closures these tests install can never replace a real one."
  (intern (concat "org-w3ctr--oinfo-test" (symbol-name key))))

(defmacro t-test-oinfo-cache (keys &rest body)
  "Run BODY with a throwaway OINFO cache for the property keys KEYS.

Binds `org-w3ctr--oinfo-cache-props' and `org-w3ctr--oinfo-cache-alist'
to closures named by `org-w3ctr-test-oinfo-oclosure', and removes those
names again when BODY exits: `fset' is not undone by `dlet'.

Inside BODY call `org-w3ctr--pget'/`org-w3ctr--pput' through `eval':
both are `define-inline', expanded at call time against the current
`org-w3ctr--oinfo-cache-alist', so only `eval' re-expands them under the
throwaway cache this macro installs."
  (declare (indent 1))
  `(dlet ((org-w3ctr--oinfo-cache-props ,keys)
          (org-w3ctr--oinfo-cache-alist nil))
     (let (names)
       (unwind-protect
           (progn
             (dolist (a org-w3ctr--oinfo-cache-props)
               (let ((name (t-test-oinfo-oclosure a)))
                 (fset name (org-w3ctr--make-cache-oclosure a))
                 (push (cons a name) org-w3ctr--oinfo-cache-alist)
                 (push name names)))
             ,@body)
         (dolist (name names)
           (when (fboundp name) (fmakunbound name)))))))

;; Directly under its subject.

(ert-deftest t-test-oinfo-cache-macro ()
  "Smoke test for the `org-w3ctr-test-oinfo-cache' test helper macro.
Verifies setup, body evaluation, cleanup, and restore of throwaway
closures, and that both the read and the write go through them."
  (skip-unless t--oinfo-cache-p)
  (let ((sym (t-test-oinfo-oclosure :test-x))
        (alist t--oinfo-cache-alist))
    ;; Before: the symbol must not be a function.
    ($n (fboundp sym))
    (t-test-oinfo-cache '(:test-x)
      ;; Inside: cache alist is populated and callable.
      ($l (mapcar #'car t--oinfo-cache-alist) '(:test-x))
      ($q (cdr (assq :test-x t--oinfo-cache-alist)) sym)
      ($s (fboundp sym))
      ($l (eval '(t--pget (list :test-x 42) :test-x)) 42)
      ;; A write goes through the closure too, not the plist.
      (dlet ((info (list :test-x 1)))
        ($l (eval '(t--pput info :test-x 'NEW)) 'NEW)
        ($q (eval '(t--pget info :test-x)) 'NEW)))
    ;; After: the symbol is unbound and the real alist is restored.
    ($n (fboundp sym))
    ($q t--oinfo-cache-alist alist))
  ;; The `unwind-protect' cleanup also runs when BODY signals.
  (let ((sym (t-test-oinfo-oclosure :boom))
        (alist t--oinfo-cache-alist))
    ($e! (t-test-oinfo-cache '(:boom)
           ($s (fboundp sym))
           (error "boom")))
    ($n (fboundp sym))
    ($q t--oinfo-cache-alist alist)))

(ert-deftest t--make-cache-oclosure ()
  "Tests for `org-w3ctr--make-cache-oclosure'."
  (let ((info '(:a 1 :b 2 :c 3))
        (info2 '(:a 3 :b 2 :c 1))
        (info3 (list :x 99))
        (oa (t--make-cache-oclosure :a))
        (ob (t--make-cache-oclosure :b))
        (od (t--make-cache-oclosure :z)))
    ;; initial state
    ($l (t--oinfo--cnt oa) 0)
    ($l (t--oinfo--pid oa) nil)
    ($l (t--oinfo--val oa) nil)
    ;; first lookup: correct value, pid/val set, cnt=1
    ($l (funcall oa info) 1)
    ($q (t--oinfo--pid oa) info)
    ($l (t--oinfo--val oa) 1)
    ($l (t--oinfo--cnt oa) 1)
    ;; independent keys
    ($l (funcall ob info) 2)
    ($l (t--oinfo--cnt ob) 1)
    ;; cache hit: same plist, cnt increments
    ($l (funcall oa info) 1)
    ($l (t--oinfo--cnt oa) 2)
    ;; cache miss: different plist, value updates
    ($l (funcall oa info2) 3)
    ($q (t--oinfo--pid oa) info2)
    ($l (t--oinfo--val oa) 3)
    ;; key absent: val is nil, pid still set, cnt increments
    ($l (funcall od info3) nil)
    ($q (t--oinfo--pid od) info3)
    ($l (t--oinfo--val od) nil)
    ($l (t--oinfo--cnt od) 1)
    ;; same plist: a hit on the cached nil, not a re-read.  Mutating the
    ;; plist proves it -- a miss would return the new value.
    ($l (funcall od info3) nil)
    ($l (t--oinfo--cnt od) 2)
    (plist-put info3 :z 'present)
    ($l (funcall od info3) nil)
    ($l (t--oinfo--cnt od) 3)))

;;;; OINFO structural checks

(ert-deftest t--oinfo-switch-is-compile-time ()
  "Tests for `org-w3ctr-oinfo-enabled'.
The switch is resolved at macro-expansion time: the flag never reaches
the expanded call, and the oclosure is inlined when the cache is on.
`macroexpand-all' runs `org-w3ctr--pget''s compiler macro, the same
mechanism the byte compiler uses."
  (let ((symbols (flatten-tree (macroexpand-all '(t--pget info :title)))))
    ($n (memq 'org-w3ctr-oinfo-enabled symbols))
    ($l (and (memq 'org-w3ctr--oinfo:title symbols) t)
        t--oinfo-cache-p)))

(ert-deftest t--oinfo-props-are-looked-up ()
  "Static check: every `org-w3ctr--oinfo-cache-props' key appears as
a literal second argument to `org-w3ctr--pget' in the source file."
  (let* ((build (symbol-file 'org-w3ctr--pget 'defun))
         (source (and build (concat (file-name-sans-extension build) ".el"))))
    (skip-unless (and source (file-readable-p source)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (key t--oinfo-cache-props)
        (goto-char (point-min))
        (should (re-search-forward
                 (format "[(]t--pget[ \t\n]+info[ \t\n]+%s[ \t\n]*[)]"
                         (regexp-quote (symbol-name key)))
                 nil t))))))

(ert-deftest t--oinfo-props-go-through-pget ()
  "Static check: no `org-w3ctr--oinfo-cache-props' key is read or
written with a literal `plist-get', `plist-put' or
`setf (plist-get ...)' in the source file.
Only literal keys are checked: a computed key cannot be seen here."
  (let* ((build (symbol-file 'org-w3ctr--pget 'defun))
         (source (and build (concat (file-name-sans-extension build) ".el"))))
    (skip-unless (and source (file-readable-p source)))
    (with-temp-buffer
      (insert-file-contents source)
      (let ((offenders nil)
            (patterns
             '("[(]plist-get[ \t\n]+info[ \t\n]+%s[ \t\n]*[)]"
               "[(]plist-put[ \t\n]+info[ \t\n]+%s"
               "[(]setf[ \t\n]+[(]plist-get[ \t\n]+info[ \t\n]+%s[ \t\n]*[)]"))
            (samples '("(plist-get info :k)"
                       "(plist-put info :k v)"
                       "(setf (plist-get info :k) v)")))
        ;; Self-check: a pattern that misses its own sample is dead.
        (dolist (pair (cl-mapcar #'cons patterns samples))
          ($s (string-match-p (format (car pair) ":k") (cdr pair))))
        (dolist (key t--oinfo-cache-props)
          (dolist (pat patterns)
            (goto-char (point-min))
            (when (re-search-forward
                   (format pat (regexp-quote (symbol-name key))) nil t)
              (push (format "%s at line %d" key (line-number-at-pos))
                    offenders))))
        ($l offenders nil)))))

(ert-deftest t--oinfo-test-namespace ()
  "The names the tests generate can never replace a production closure."
  (dolist (key t--oinfo-cache-props)
    ($n (eq (t-test-oinfo-oclosure key) (t--oinfo-oclosure key)))))

(ert-deftest t-test-oinfo-cache-guarded ()
  "Static check: every throwaway-cache test is guarded by `skip-unless'.
`org-w3ctr-test-oinfo-cache' only makes sense when the cache is on, so
a test that uses it must also `skip-unless' `org-w3ctr-oinfo-cache-p';
otherwise its cached assertions fail in the nil build."
  (let* ((build (symbol-file 'org-w3ctr-test-oinfo-cache 'defun))
         (source (and build (concat (file-name-sans-extension build) ".el")))
         ;; Built so this file does not itself match the pattern below.
         (use (concat "(t-test-oinfo" "-cache\\_>"))
         (guard "skip-unless t--oinfo-cache-p"))
    (skip-unless (and source (file-readable-p source)))
    (with-temp-buffer
      (insert-file-contents source)
      (let (starts)
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \t]*(ert-deftest[ \t]+\\([^ \t\n()]+\\)" nil t)
          (push (cons (match-string 1) (match-beginning 0)) starts))
        (let ((offenders nil)
              (last (point-max)))
          ;; `starts' is collected back-to-front, so walking it as is
          ;; goes from the end of the file: each test's body then spans
          ;; its start to the next test's start, and the last test's
          ;; body extends to the end of the file.
          (dolist (cell starts)
            (let ((name (car cell))
                  (beg (cdr cell)))
              (let ((body (buffer-substring-no-properties beg last)))
                (when (and (string-match-p use body)
                           (not (string-match-p guard body)))
                  (push name offenders)))
              (setq last beg)))
          ($l (nreverse offenders) nil))))))

(ert-deftest t--oinfo-oclosure-names ()
  "The closure symbol is the struct name followed by the keyword."
  (dolist (key t--oinfo-cache-props)
    ($l (t--oinfo-oclosure key)
        (intern (concat "org-w3ctr--oinfo" (symbol-name key))))))

(ert-deftest t--oinfo-cache-alist-matches-props ()
  "Structural check: `org-w3ctr--oinfo-cache-alist' keys match
`org-w3ctr--oinfo-cache-props', and each entry points to a live
oclosure with the correct key."
  (unless t--oinfo-cache-p
    ($l t--oinfo-cache-alist nil))
  (when t--oinfo-cache-p
    (let ((keys (mapcar #'car t--oinfo-cache-alist)))
      ($l (cl-set-difference keys t--oinfo-cache-props) nil)
      ($l (cl-set-difference t--oinfo-cache-props keys) nil))
    (pcase-dolist (`(,key . ,name) t--oinfo-cache-alist)
      ($l name (t--oinfo-oclosure key))
      ($l (functionp (symbol-function name)) t)
      ($l (t--oinfo--key (symbol-function name)) key))))

(ert-deftest t--oinfo-cache-props-invariants ()
  "Every cached property is a keyword, with no duplicates.
A non-keyword or a duplicate would break `org-w3ctr--oinfo-oclosure'\='s
naming and the alist built from it."
  ($s (cl-every #'keywordp t--oinfo-cache-props))
  ($l (length t--oinfo-cache-props)
      (length (delete-dups (copy-sequence t--oinfo-cache-props)))))

;;;; OINFO reading and writing

(ert-deftest t--oinfo-pget ()
  "Tests for `org-w3ctr--pget'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a :b)
    (dlet ((info '(:a 1 :b 2 :c 3)))
      ($l (eval '(t--pget info :a)) 1)
      ($l (t--oinfo--cnt (t--oinfo-oget :a)) 1)
      ($l (eval '(t--pget info :b)) 2)
      ($l (t--oinfo--cnt (t--oinfo-oget :b)) 1)
      ($l (eval '(t--pget info :c)) 3)
      ($n (alist-get :c t--oinfo-cache-alist))
      ;; cached key absent from plist: nil, cnt increments
      ($l (eval '(t--pget (list :x 1) :a)) nil)
      ($l (t--oinfo--cnt (t--oinfo-oget :a)) 2)
      ;; non-cached key absent from plist: nil
      ($l (eval '(t--pget (list :x 1) :d)) nil))))

(ert-deftest t--oinfo-pget-nil-info ()
  "`org-w3ctr--pget'/`org-w3ctr--pput' tolerate a nil INFO plist.
A cached key written for nil INFO is read back from the cache, since
nil is `eq' to itself, so the write is invisible to `plist-get'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    ;; a non-cached key behaves like `plist-get' on nil, and is not cached
    ($l (eval '(t--pget nil :none)) nil)
    ($n (alist-get :none t--oinfo-cache-alist))
    ;; a cached key is held by the oclosure, not the plist
    ($l (eval '(t--pput nil :a 5)) 5)
    ($l (eval '(t--pget nil :a)) 5)
    (t--oinfo-cleanup)
    ($l (eval '(t--pget nil :a)) nil)))

(ert-deftest t--oinfo-pput ()
  "Tests for `org-w3ctr--pput'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info '(:a 1 :c 3))
           (val 1))
      ;; cached key: write to oclosure, plist untouched
      ($l (eval '(t--pput info :a 2)) 2)
      ($l (eval '(t--pget info :a)) 2)
      ($l (t--oinfo--val (t--oinfo-oget :a)) 2)
      ($l (plist-get info :a) 1)
      ;; cached key with value expression
      ($l (eval '(t--pput info :a (incf val))) 2)
      ($l (eval '(t--pget info :a)) 2)
      ($l (plist-get info :a) 1)
      ;; non-cached key: plist-put, returns VALUE
      ($l (eval '(t--pput info :c 4)) 4)
      ($l (eval '(t--pget info :c)) 4)
      ($l (plist-get info :c) 4)
      ;; non-cached key with value expression
      ($l (eval '(t--pput info :c (incf val))) 3)
      ($l (plist-get info :c) 3))))

(ert-deftest t--oinfo-pput-value-evaluation-order ()
  "`org-w3ctr--pput' evaluates VALUE before updating the oclosure.
VALUE can safely read from INFO (including the same PROP) without
seeing a stale cached value, and a signal during VALUE evaluation
leaves the cache unchanged."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info-a (list :a "A"))
           (info-b (list :a "B")))
      ;; Prime the cache with info-a.
      ($l (eval '(t--pget info-a :a)) "A")
      ;; VALUE reads the same key of a different INFO.
      ($l (eval '(t--pput info-b :a
                          (format "seen=%s" (t--pget info-b :a))))
          "seen=B")
      ($l (eval '(t--pget info-b :a)) "seen=B")
      ;; Cleanup before the error test.
      (t--oinfo-cleanup)
      ($l (eval '(t--pget info-a :a)) "A")
      ;; VALUE signals: the cache must not be half-updated.
      (should-error (eval '(t--pput info-b :a (error "boom"))))
      ;; The oclosure should still hold info-a, unchanged: the signal
      ;; fires during VALUE evaluation, before the pid slot is written.
      ($q (t--oinfo--pid (t--oinfo-oget :a)) info-a)
      ;; Reading info-b must return "B" from the plist, not a stale "A".
      ($l (eval '(t--pget info-b :a)) "B"))))

(ert-deftest t--oinfo-non-inlined-call ()
  "A variable KEY falls back to the non-inlined cache lookup.
`org-w3ctr--pget'/`org-w3ctr--pput' cannot inline a non-literal key, so
they run their function definitions and look KEY up in
`org-w3ctr--oinfo-cache-alist' at run time -- the path
`org-w3ctr--build-pre/postamble' uses.  The written value must read
back while the plist keeps the old one; that divergence is what shows
the cache, not `plist-get', answered."
  (skip-unless t--oinfo-cache-p)
  (let* ((key (car t--oinfo-cache-props))
         (info (list key 1)))
    ;; a cached read matches `plist-get' before any write
    ($l (t--pget info key) (plist-get info key))
    ;; a cached write reads back, but leaves the plist untouched
    (t--pput info key 'NEW)
    ($q (t--pget info key) 'NEW)
    ($l (plist-get info key) 1)
    ;; a non-cached key goes through the plist
    (t--pput info :plain 2)
    ($l (t--pget info :plain) 2))
  ;; do not leave the real oclosure holding this test's plist
  (t--oinfo-cleanup))

(ert-deftest t--oinfo-cache-is-per-plist ()
  "A copy of the plist is a cache miss: oclosures compare with `eq'.
`plist-get' works on any plist with matching keys, but the cache
uses object identity, so an equal but distinct plist is a miss."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info (list :a 1))
           (twin nil))
      ($l (eval '(t--pget info :a)) 1)
      (setq twin (copy-sequence info))
      ($nq twin info)
      ($l twin info)
      ($l (eval '(t--pget twin :a)) 1)
      ($q (t--oinfo--pid (t--oinfo-oget :a)) twin))))

(ert-deftest t--oinfo-mutation-is-invisible ()
  "Changing the plist object in place does not reach the cache."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info (list :a 1)))
      ($l (eval '(t--pget info :a)) 1)
      (plist-put info :a 99)
      ($l (plist-get info :a) 99)
      ($l (eval '(t--pget info :a)) 1))))

(ert-deftest t--oinfo-pput-is-single-slot ()
  "A written value is evicted when another plist is read.
`org-w3ctr--pput' fills the oclosure's single (PID . VAL) slot, so a
read of a different plist replaces it; the original plist then misses
too."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info (list :a 1))
           (twin nil))
      (setq twin (copy-sequence info))
      ($l (eval '(t--pput info :a 2)) 2)
      ($l (eval '(t--pget info :a)) 2)
      ($l (eval '(t--pget twin :a)) 1)
      ($q (t--oinfo--pid (t--oinfo-oget :a)) twin)
      ($l (eval '(t--pget info :a)) 1))))

(ert-deftest t--oinfo-plain-flavor ()
  "Tests for `org-w3ctr--pget' and `org-w3ctr--pput' when
the OINFO cache is off."
  (skip-when t--oinfo-cache-p)
  (dlet ((info (list :a 1)))
    ($l (eval '(t--pget info :a)) 1)
    ($l (eval '(t--pput info :a 2)) 2)
    ($l (plist-get info :a) 2)
    ($l (eval '(t--pget info :a)) 2)))

;;;; OINFO cleanup and statistics

(ert-deftest t--oinfo-cleanup ()
  "Tests for `org-w3ctr--oinfo-cleanup'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a :b)
    (dlet ((info '(:a 1 :b 2)))
      ($l (eval '(t--pget info :a)) 1)
      ($l (eval '(t--pget info :b)) 2)
      ($q (t--oinfo--pid (t--oinfo-oget :a)) info)
      ($q (t--oinfo--pid (t--oinfo-oget :b)) info)
      ($q (t--oinfo--val (t--oinfo-oget :a)) 1)
      ($q (t--oinfo--val (t--oinfo-oget :b)) 2)
      ($q (t--oinfo--cnt (t--oinfo-oget :a)) 1)
      ($q (t--oinfo--cnt (t--oinfo-oget :b)) 1)
      (t--oinfo-cleanup)
      ($l (t--oinfo--pid (t--oinfo-oget :a)) nil)
      ($l (t--oinfo--pid (t--oinfo-oget :b)) nil)
      ($l (t--oinfo--val (t--oinfo-oget :a)) nil)
      ($l (t--oinfo--val (t--oinfo-oget :b)) nil)
      ($q (t--oinfo--cnt (t--oinfo-oget :a)) 1)
      ($q (t--oinfo--cnt (t--oinfo-oget :b)) 1))))

(ert-deftest t-oinfo-cleanup-before-export ()
  "Tests for `org-w3ctr-oinfo-cleanup-before-export'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a)
    (dlet ((info (list :a 1)))
      ($l (eval '(t--pget info :a)) 1)
      ($q (t--oinfo--pid (t--oinfo-oget :a)) info)
      (t-oinfo-cleanup-before-export 'w3ctr)
      ($l (t--oinfo--pid (t--oinfo-oget :a)) nil)
      ($l (t--oinfo--val (t--oinfo-oget :a)) nil))))

(ert-deftest t-clear-oinfo-statistics ()
  "Tests for `org-w3ctr-clear-oinfo-statistics'."
  (skip-unless t--oinfo-cache-p)
  (t-test-oinfo-cache '(:a :b)
    (dlet ((info '(:a 1 :b 2)))
      ($l (eval '(t--pget info :a)) 1)
      ($l (eval '(t--pget info :a)) 1)
      ($l (eval '(t--pget info :b)) 2)
      ($l (t--oinfo--cnt (t--oinfo-oget :a)) 2)
      (t-clear-oinfo-statistics)
      ($l (t--oinfo--cnt (t--oinfo-oget :a)) 0)
      ($l (t--oinfo--cnt (t--oinfo-oget :b)) 0)
      ($l (t--oinfo--pid (t--oinfo-oget :a)) nil)
      ($l (t--oinfo--val (t--oinfo-oget :a)) nil)
      ($l (eval '(t--pget info :a)) 1)
      ($l (t--oinfo--cnt (t--oinfo-oget :a)) 1))))

(ert-deftest t-collect-oinfo-statistics ()
  "Tests for `org-w3ctr-collect-oinfo-statistics'."
  (skip-unless t--oinfo-cache-p)
  (unwind-protect
      (t-test-oinfo-cache '(:a :b)
        (dlet ((info '(:a 1 :b 2)))
          ($l (eval '(t--pget info :b)) 2)
          ($l (eval '(t--pget info :b)) 2)
          ($l (eval '(t--pget info :a)) 1)
          (t-collect-oinfo-statistics)
          (with-current-buffer "*ox-w3ctr-oinfo*"
            ;; Check that buffer uses tabulated-list-mode
            ($l major-mode 'tabulated-list-mode)
            ;; Check entries: should be sorted by count descending
            (let ((entries tabulated-list-entries))
              ($l (length entries) 2)
              ;; First entry: :b with count 2
              ($l (nth 0 (car entries)) :b)
              ($l (aref (nth 1 (car entries)) 0) ":b")
              (let ((count-str (aref (nth 1 (car entries)) 1)))
                ($l (get-text-property 0 'count count-str) 2))
              ;; Second entry: :a with count 1
              ($l (nth 0 (cadr entries)) :a)
              ($l (aref (nth 1 (cadr entries)) 0) ":a")
              (let ((count-str (aref (nth 1 (cadr entries)) 1)))
                ($l (get-text-property 0 'count count-str) 1))))))
    (when (get-buffer "*ox-w3ctr-oinfo*")
      (kill-buffer "*ox-w3ctr-oinfo*"))))

(ert-deftest t-collect-oinfo-statistics-revert ()
  "Tests for the revert handler of `org-w3ctr-collect-oinfo-statistics'."
  ;; The buffer-local `revert-buffer-function' rebuilds the listing
  ;; from the live counters; a plain re-print would keep stale numbers.
  (skip-unless t--oinfo-cache-p)
  (unwind-protect
      (t-test-oinfo-cache '(:a :b)
        (dlet ((info '(:a 1 :b 2)))
          ($l (eval '(t--pget info :a)) 1)
          (t-collect-oinfo-statistics)
          ;; more traffic after the listing was built
          ($l (eval '(t--pget info :a)) 1)
          ($l (eval '(t--pget info :a)) 1)
          (with-current-buffer "*ox-w3ctr-oinfo*"
            (funcall revert-buffer-function)
            ;; the counts are fresh: :a is 3 now and comes first
            (let* ((entries tabulated-list-entries)
                   (count-str (aref (nth 1 (car entries)) 1)))
              ($l (length entries) 2)
              ($l (nth 0 (car entries)) :a)
              ($l (get-text-property 0 'count count-str) 3)))))
    (when (get-buffer "*ox-w3ctr-oinfo*")
      (kill-buffer "*ox-w3ctr-oinfo*"))))

(ert-deftest t--oinfo-compare-count ()
  "Tests for `org-w3ctr--oinfo-compare-count'."
  (let ((entry (lambda (n)
                 (list n (vector (format "%d" n)
                                 (propertize (format "%d" n) 'count n))))))
    ($s (t--oinfo-compare-count (funcall entry 1) (funcall entry 2)))
    ($n (t--oinfo-compare-count (funcall entry 2) (funcall entry 1)))
    ($n (t--oinfo-compare-count (funcall entry 2) (funcall entry 2)))))

;;;; String helpers

(ert-deftest t--nw-p ()
  "Tests for `org-w3ctr--nw-p'."
  ($l (t--nw-p "123") "123")
  ($l (t--nw-p " 1") " 1")
  ($l (t--nw-p "\t\r\n2") "\t\r\n2")
  ($n (t--nw-p ""))
  ($n (t--nw-p "\t\s\r\n"))
  ($n (t--nw-p nil))
  ($n (t--nw-p 0))
  ;; \f and NBSP are content, not whitespace
  ($l (t--nw-p "\f") "\f")
  ($l (t--nw-p "\u00a0") "\u00a0"))

(ert-deftest t--2str ()
  "Tests for `org-w3ctr--2str'."
  ($q (t--2str nil) nil)
  ($l (t--2str 1) "1")
  ($l (t--2str 114.514) "114.514")
  ($l (t--2str ?a) "97")
  ($l (t--2str 'hello) "hello")
  ($l (t--2str :foo) ":foo")
  ($l (t--2str 'has\ space) "has space")
  ($l (t--2str 'has\#) "has#")
  ($l (t--2str "string") "string")
  ($n (t--2str [1]))
  ($n (t--2str (make-char-table 'sub)))
  ($n (t--2str (make-bool-vector 3 t)))
  ($n (t--2str (make-hash-table)))
  ($n (t--2str (lambda (x) x))))

(ert-deftest t--trim ()
  "Tests for `org-w3ctr--trim'."
  ($l (t--trim "123") "123")
  ($l (t--trim " 123") "123")
  ($l (t--trim " 123 ") "123")
  ($l (t--trim "  123  ") "123")
  ;; \r is whitespace as well, not content
  ($l (t--trim " \r\n123 \r\n") "123")
  ($l (t--trim "  123\n 456\n") "123\n 456")
  ($l (t--trim "\n 123" t) " 123")
  ($l (t--trim "\n\n  123\n" t) "  123")
  ;; KEEP-LEAD with no leading blank line: the indentation stays
  ($l (t--trim "  123" t) "  123")
  ;; all blank: nothing is content
  ($l (t--trim "\n\n" t) "")
  ;; A CRLF blank line is not removed in KEEP-LEAD mode: unlike the
  ;; plain head class [ \t\n\r]+, the KEEP-LEAD one is \`\([ \t]*\n\)+
  ;; and has no \r.  Current behavior, pinned as is -- flip this
  ;; expectation if the head class ever gains \r.
  ($l (t--trim "\r\n  123" t) "\r\n  123"))

(ert-deftest t--nw-trim ()
  "Tests for `org-w3ctr--nw-trim'."
  ($n (t--nw-trim ""))
  ($l (t--nw-trim " ") nil)
  ($l (t--nw-trim " 1 ") "1")
  ($l (t--nw-trim "234\n") "234")
  ;; composed whitespace classes: space, \t, \r, \n all go
  ($l (t--nw-trim "\t\r\n x \r\n\t") "x")
  ($l (t--nw-trim 1) nil)
  ($l (t--nw-trim 'hello) nil)
  ($l (t--nw-trim nil) nil))

(ert-deftest t--prepend-newline ()
  "Tests for `org-w3ctr--prepend-newline'."
  ($it t--prepend-newline
    ($l (it nil) "")
    ($l (it "") "\n")
    ($l (it "abc") "\nabc")
    ($l (it 123) "")
    ($l (it '(1 2)) "")))

(ert-deftest t--make-string ()
  "Tests for `org-w3ctr--make-string'."
  ($l (t--make-string 1 "a") "a")
  ($l (t--make-string 2 "a") "aa")
  ($l (t--make-string 3 "a") "aaa")
  ($l (t--make-string 2 "ab") "abab")
  ($l (t--make-string 0 "a") "")
  ($l (t--make-string -1 "a") "")
  ($l (t--make-string 100 "") "")
  ($e! (t--make-string 3 [?a ?b]))
  ($e! (t--make-string "a" "a")))

;;;; HTML escaping

(ert-deftest t--encode-plain-text ()
  "Tests for `org-w3ctr--encode-plain-text'."
  ($l (t--encode-plain-text "") "")
  ($l (t--encode-plain-text "123") "123")
  ($l (t--encode-plain-text "hello world") "hello world")
  ($l (t--encode-plain-text "&") "&amp;")
  ($l (t--encode-plain-text "<") "&lt;")
  ($l (t--encode-plain-text ">") "&gt;")
  ($l (t--encode-plain-text "<&>") "&lt;&amp;&gt;")
  ;; existing entities are escaped again -- blind, not smart, escaping
  ($l (t--encode-plain-text "&amp;") "&amp;amp;")
  ;; quotes pass through: only the * variant escapes them
  ($l (t--encode-plain-text "\"'") "\"'")
  (dolist (a '(("a&b&c" . "a&amp;b&amp;c")
               ("<div>" . "&lt;div&gt;")
               ("<span>" . "&lt;span&gt;")))
    ($l (t--encode-plain-text (car a)) (cdr a))))

(ert-deftest t--encode-plain-text* ()
  "Tests for `org-w3ctr--encode-plain-text*'."
  ;; non-special text is kept as is
  ($l (t--encode-plain-text* "123") "123")
  ($l (t--encode-plain-text* "&") "&amp;")
  ($l (t--encode-plain-text* "<") "&lt;")
  ($l (t--encode-plain-text* ">") "&gt;")
  ($l (t--encode-plain-text* "<&>") "&lt;&amp;&gt;")
  ;; existing entities are escaped again -- blind, not smart, escaping
  ($l (t--encode-plain-text* "&amp;") "&amp;amp;")
  ($l (t--encode-plain-text* "'") "&apos;")
  ($l (t--encode-plain-text* "\"") "&quot;")
  ($l (t--encode-plain-text* "\"'&\"")
      "&quot;&apos;&amp;&quot;"))

(ert-deftest t--attribute-escaping-roundtrip ()
  "Attribute escaping is safe and reversible on every short string.
Exhaustive over a, space, &, <, >, \" and ' up to length 2 (57 strings)
plus pre-formed entities: no sampling and no iteration count to tune --
every failure of a per-character substitution shows up in a short
string.  Per string: no raw specials, every `&' starts an entity, and
decoding returns the input."
  (let* ((entity-re "&\\(?:amp\\|lt\\|gt\\|apos\\|quot\\);")
         (entity-map '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                       ("&apos;" . "'") ("&quot;" . "\"")))
         (chars (append "a &<>\"'" nil))
         (decode (lambda (s)
                   (replace-regexp-in-string
                    entity-re (lambda (m) (cdr (assoc m entity-map))) s t t)))
         (check (lambda (s)
                  (let ((b (t--encode-plain-text* s)))
                    ;; attribute-safe
                    ($n (string-match-p "[<>'\"]" b))
                    ;; every & starts an entity: strip them, none may remain
                    ($n (string-match-p
                         "&" (replace-regexp-in-string entity-re "" b t t)))
                    ($l (funcall decode b) s)))))
    ;; the decoder is a fixture: prove it on a known pair first
    ($l (funcall decode "&amp;lt;") "&lt;")
    (funcall check "")
    (funcall check "&amp;")
    (funcall check "&amp;lt;")
    (dolist (c1 chars)
      (funcall check (string c1))
      (dolist (c2 chars)
        (funcall check (string c1 c2))))))

;;;; HTML attributes

(ert-deftest t--read-attr ()
  "Tests for `org-w3ctr--read-attr'."
  ;; `org-element-property' use `org-element--property'
  ;; and defined using `define-inline'.
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n _deft _force) n)))
    ($l (org-element-property :attr__ 123) 123)
    ($l (org-element-property nil 1) 1)
    ($l (t--read-attr nil '("123")) '(123))
    ($l (t--read-attr nil '("1 2 3" "4 5 6")) '(1 2 3 4 5 6))
    ;; a form split across lines reassembles: the join is a space
    ($l (t--read-attr nil '("(a" "b)")) '((a b)))
    ($l (t--read-attr nil '("(class data) [hello] (id ui)"))
        '((class data) [hello] (id ui)))
    ($l (t--read-attr nil '("\"123\"")) '("123"))
    ;; whitespace-only: the docstring's third nil case
    ($n (t--read-attr nil '("   ")))
    ($e!l (t--read-attr nil '("(invalid"))
          '(org-w3ctr-error "Invalid attribute #+nil: (invalid"))
    ;; the same error with the attribute name callers actually pass
    ($e!l (t--read-attr :attr__ '("(invalid"))
          '(org-w3ctr-error "Invalid attribute #+:attr__: (invalid")))
  (t-check-element-values
   #'t--read-attr
   '(("#+attr__: 1 2 3\n#+attr__: 4 5 6\nhello world"
      (1 2 3 4 5 6))
     ("#+attr__: [hello world] (id no1)\nhello"
      ([hello world] (id no1)))
     ("nothing but text" . nil)
     ("#+attr__: \"str\"\nstring" ("str"))
     ("#+attr__:\nempty" nil))))

(ert-deftest t--read-attr__ ()
  "Tests for `org-w3ctr--read-attr__'."
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n _deft _force) n)))
    ($l (t--read-attr__ '("1 2 3")) '(1 2 3))
    ($l (t--read-attr__ '("(class data) open"))
        '((class data) open))
    ($l (t--read-attr__ '("(class hello world)" "foo"))
        '((class hello world) foo))
    ($l (t--read-attr__ '("[nim zig]")) '(("class" "nim zig")))
    ;; numbers in a vector are stringified too
    ($l (t--read-attr__ '("[1 2]")) '(("class" "1 2")))
    ;; a nil in a vector contributes nothing but its separator:
    ;; `mapconcat' over `org-w3ctr--2str' with " " turns [a nil b] into
    ;; "a  b", not "a b" -- current behavior, pinned
    ($l (t--read-attr__ '("[a nil b]")) '(("class" "a  b")))
    ($l (t--read-attr__ '("[]")) '(nil))
    ($l (t--read-attr__ '("[][][]")) '(nil nil nil)))
  (t-check-element-values
   #'t--read-attr__
   '(("#+attr__: 1 2 3\n#+attr__: 4\ntest" (1 2 3 4))
     ("#+attr__: [hello world] (id no1)\ntest"
      (("class" "hello world") (id no1)))
     ("test" . nil)
     ("#+attr__:\n#+attr__:\ntest" nil)
     ("#+attr__: []\ntest" (nil))
     ("#+attr__: [][][]\ntest" (nil nil nil)))))

(ert-deftest t--make-attr ()
  "Tests for `org-w3ctr--make-attr'."
  ($n (t--make-attr nil))
  ($n (t--make-attr '(nil 1)))
  ($n (t--make-attr '([x])))
  ($l (t--make-attr '(open)) " open")
  ($l (t--make-attr '("disabled")) " disabled")
  ($l (t--make-attr '(FOO)) " foo")
  ($l (t--make-attr '(a b)) " a=\"b\"")
  ;; values concatenate without separator: the docstring's VAL1VAL2
  ($l (t--make-attr '(id yy 123)) " id=\"yy123\"")
  ;; (open) is the boolean form, (open nil) the empty-valued one
  ($l (t--make-attr '(open nil)) " open=\"\"")
  ($l (t--make-attr '(class "example two")) " class=\"example two\"")
  ($l (t--make-attr '(foo [bar] baz)) " foo=\"baz\"")
  ($l (t--make-attr '(data-A "base64...")) " data-a=\"base64...\"")
  ;; names are downcased but neither escaped nor validated
  ($l (t--make-attr '("<x>" v)) " <x>=\"v\"")
  ($l (t--make-attr '(data-tt "a < b && c"))
      " data-tt=\"a &lt; b &amp;&amp; c\"")
  ($l (t--make-attr '(data-he "\"hello world\""))
      " data-he=\"&quot;hello world&quot;\"")
  ($l (t--make-attr '(sig "''")) " sig=\"&apos;&apos;\"")
  ($l (t--make-attr '(test ">'\""))
      " test=\"&gt;&apos;&quot;\""))

(ert-deftest t--make-attr__ ()
  "Tests for `org-w3ctr--make-attr__'."
  ($l (t--make-attr__ nil) "")
  ($l (t--make-attr__ '(nil)) "")
  ($l (t--make-attr__ '(nil nil [])) "")
  ;; a nil element vanishes cleanly: no separator is left behind,
  ;; unlike the value-level mapconcat in `org-w3ctr--read-attr__'
  ($l (t--make-attr__ '((a) nil (b c))) " a b=\"c\"")
  ($l (t--make-attr__ '(a)) " a")
  ;; a string element takes the atom path too
  ($l (t--make-attr__ '("disabled")) " disabled")
  ($l (t--make-attr__ '((id yy 123) (class a\ b) test))
      " id=\"yy123\" class=\"a b\" test")
  ($l (t--make-attr__ '((test this th&t <=>)))
      " test=\"thisth&amp;t&lt;=&gt;\"")
  ;; a dotted element signals a primitive error, not
  ;; `org-w3ctr-error' -- reachable as #+attr__: (a . b), which then
  ;; aborts the export
  ($e! (t--make-attr__ '((a . b)))))

(ert-deftest t--make-attribute-string ()
  "Tests for `org-w3ctr--make-attribute-string'."
  ($l (t--make-attribute-string '(:a "1" :b "2"))
      "a=\"1\" b=\"2\"")
  ($l (t--make-attribute-string nil) "")
  ($l (t--make-attribute-string '(:a nil)) "")
  ;; a dangling key stays bare: only a nil *value* pops it
  ($l (t--make-attribute-string '(:a "1" :b)) "a=\"1\" b")
  ($l (t--make-attribute-string '(:a "\"a\""))
      "a=\"&quot;a&quot;\"")
  ($l (t--make-attribute-string '(:open "open"))
      "open=\"open\"")
  ($l (t--make-attribute-string '(:test "'\"'"))
      "test=\"&apos;&quot;&apos;\"")
  ;; A plain symbol key is accepted too (there is no colon to strip).
  ($l (t--make-attribute-string '(open "open"))
      "open=\"open\"")
  (t-check-element-values
   #'t--make-attribute-string
   '(("#+attr_html: :open open :class a\ntest"
      "open=\"open\" class=\"a\"")
     ("#+attr_html: :id wo-1 :two\ntest" "id=\"wo-1\"")
     ("#+attr_html: :id :idd hhh\ntest" "idd=\"hhh\"")
     ("#+attr_html: :null nil :this test\ntest" "this=\"test\""))))

(ert-deftest t--make-attr__id ()
  "Tests for `org-w3ctr--make-attr__id'."
  (t-check-element-values
   #'t--make-attr__id
   '(("#+attr__:\ntest" "")
     ;; no reference (unnamed element): attributes come out as is
     ("#+attr__: hello\ntest" " hello")
     ("#+name:test\n#+attr__: hello\ntest" " id=\"test\" hello")
     ("#+name:1\n#+attr__:[data] (style {a:b})\ntest"
      " id=\"1\" class=\"data\" style=\"{a:b}\"")
     ("#+name:1\n#+attr__:[hello world]\ntest"
      " id=\"1\" class=\"hello world\"")
     ("#+name:1\n#+attr__:[]\ntest" " id=\"1\"")
     ("#+name:1\n#+attr__:(data-test \"test double quote\")\nh"
      " id=\"1\" data-test=\"test double quote\"")
     ("#+name:1\n#+attr__:(something <=>)\nt"
      " id=\"1\" something=\"&lt;=&gt;\"")
     ;; explicit id in attr__ overrides auto-generated reference
     ("#+name:auto\n#+attr__:(id \"custom\")\ntest" " id=\"custom\"")
     ;; an id as a bare atom is NOT recognized as explicit: the
     ;; reference id is prepended and the atom emitted too -- double id
     ("#+name:1\n#+attr__: id\ntest" " id=\"1\" id"))
   nil '(:html-prefer-user-labels t)))

(ert-deftest t--make-attr_html ()
  "Tests for `org-w3ctr--make-attr_html'."
  (t-check-element-values
   #'t--make-attr_html
   '(("#+attr_html:\ntest" "")
     ("#+attr_html: :hello hello\ntest" " hello=\"hello\"")
     ("#+name: 1\n#+attr_html: :class data\ntest"
      " class=\"data\" id=\"1\"")
     ("#+attr_html: :id 1 :class data\ntest"
      " id=\"1\" class=\"data\"")
     ("#+name: 1\n#+attr_html: :id 2 :class data two\ntest"
      " id=\"2\" class=\"data two\"")
     ;; an explicit :id entry suppresses the auto id even when its
     ;; value is nil -- the id is then simply gone
     ("#+name: 1\n#+attr_html: :id nil :class data\ntest"
      " class=\"data\"")
     ("#+attr_html: :data-id < > ? 2 =\ntest"
      " data-id=\"&lt; &gt; ? 2 =\"")
     ;; duplicate keys are both emitted
     ("#+attr_html: :class a :class b\ntest"
      " class=\"a\" class=\"b\""))
   nil '(:html-prefer-user-labels t)))

(ert-deftest t--make-attr__id* ()
  "Tests for `org-w3ctr--make-attr__id*'."
  (t-check-element-values
   #'t--make-attr__id*
   '(("#+attr__:\n#+attr_html: :class a\ntest" "")
     ;; a whitespace-only #+attr__: wins just the same
     ("#+attr__:  \n#+attr_html: :class a\ntest" "")
     ("#+attr_html: :class a\ntest" " class=\"a\"")
     ;; auto id on the attr__ branch with no attributes at all
     ("#+name: 1\n#+attr__:\ntest" " id=\"1\"")
     ("#+name: 1\n#+attr__: (id 2)\n#+attr_html: :id 3\ntest"
      " id=\"2\"")
     ("#+name: 1\n#+attr_html: :id 3\ntest" " id=\"3\""))
   nil '(:html-prefer-user-labels t)))

;;;; File and regexp

(ert-deftest t--load-file ()
  "Tests for `org-w3ctr--load-file'."
  (let* ((build (symbol-file 't--load-file 'defun))
         (file (and build (concat (file-name-sans-extension build) ".el"))))
    (skip-unless (and file (file-readable-p file)))
    (let ((ox (let ((coding-system-for-read 'utf-8))
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-substring-no-properties
                   (point-min) (point-max))))))
      ($l ox (t--load-file file))
      ;; decoded as UTF-8 regardless of the ambient coding: drop the
      ;; internal utf-8 binding and this line goes red
      (let ((coding-system-for-read 'iso-latin-1))
        ($l ox (t--load-file file))))))

(ert-deftest t--load-file-missing ()
  "`org-w3ctr--load-file' rejects a missing file or a directory.
Both cases signal `org-w3ctr-error'; the error value is pinned."
  ($e!l (t--load-file "no-such-dir/no-such-file")
        '(org-w3ctr-error "Invalid file: no-such-dir/no-such-file"))
  ($e!l (t--load-file ".")
        '(org-w3ctr-error "Invalid file: .")))

(ert-deftest t--find-all ()
  "Tests for `org-w3ctr--find-all'."
  ($l (t--find-all "[0-9]" "114514") '("1" "1" "4" "5" "1" "4"))
  ($l (t--find-all "[0-9]\\{2\\}" "191981") '("19" "19" "81"))
  ;; the whole match is returned, not a capture group
  ($l (t--find-all "\\([a-z]+\\)[0-9]" "ab1 cd2") '("ab1" "cd2"))
  ($l (t--find-all "" "123") nil)
  ($l (t--find-all "1" "") nil)
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]") '("[2000-01-02]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-02]")
      '("[2000-01-02]" "[2000-01-02]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-03]" 1)
      '("[2000-01-03]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-03]" -1)
      '("[2000-01-02]" "[2000-01-03]"))
  ;; Zero-width regexps terminate: empty matches are skipped, not pushed.
  ($l (t--find-all "a*" "bc") nil)
  ($l (t--find-all "b*" "ab") '("b"))
  ($l (t--find-all "a*" "a") '("a"))
  ;; START beyond the string length signals nothing and returns nil.
  ($l (t--find-all "z" "abc" 5) nil)
  ;; START at the end of the string is still a valid search position.
  ($l (t--find-all "[0-9]" "abc1" 3) '("1")))

;;;; S-exp rendering

(ert-deftest t--void-element ()
  "Tests for `org-w3ctr--void-element'."
  ($l (t--void-element "br" nil) "<br>")
  ($l (t--void-element "br" "") "<br>")
  ;; surrounding whitespace in ATTRS is trimmed
  ($l (t--void-element "br" "   ") "<br>")
  ($l (t--void-element "img" "src=\"x\"") "<img src=\"x\">")
  ($l (t--void-element "img" "  src=\"x\"  ") "<img src=\"x\">")
  ;; trimming stops at the surrounding whitespace: inner runs stay
  ($l (t--void-element "img" "a=\"1\"  b=\"2\"") "<img a=\"1\"  b=\"2\">")
  ;; TAG is used as is -- downcasing is the caller's job
  ($l (t--void-element "IMG" nil) "<IMG>"))

(ert-deftest t--sexp2html-tag ()
  "Tests for `org-w3ctr--sexp2html-tag'."
  ($l (t--sexp2html-tag 'div) "div")
  ($l (t--sexp2html-tag 'DIV) "div")
  ;; anything but a non-nil symbol signals, message pinned on the first
  ($e!l (t--sexp2html-tag nil)
        '(org-w3ctr-error "Invalid S-expression tag: nil"))
  ($e! (t--sexp2html-tag 1.5))
  ($e! (t--sexp2html-tag "div")))

(ert-deftest t--sexp2html-attrs ()
  "Tests for `org-w3ctr--sexp2html-attrs'."
  ($l (t--sexp2html-attrs nil) "")
  ($l (t--sexp2html-attrs t) "")
  ($l (t--sexp2html-attrs '((id x))) " id=\"x\"")
  ;; anything but nil, t, or a proper list signals, message pinned on
  ;; the first
  ($e!l (t--sexp2html-attrs "text")
        '(org-w3ctr-error "Invalid S-expression attribute list: \"text\""))
  ($e! (t--sexp2html-attrs 5))
  ($e! (t--sexp2html-attrs [1 2]))
  ($e! (t--sexp2html-attrs '(a . b))))

(ert-deftest t--sexp2html ()
  "Tests for `org-w3ctr--sexp2html'."
  ($l (t--sexp2html nil) "")
  ;; Basic tag with no attributes
  ($l (t--sexp2html '(p () "123")) "<p>123</p>")
  ($l (t--sexp2html '(p t "123")) "<p>123</p>")
  ;; Tag with attributes
  ($l (t--sexp2html '(a ((href "https://example.com")) "link"))
      "<a href=\"https://example.com\">link</a>")
  ($l (t--sexp2html '(img ((src "../" "1.jpg") (alt "../1.jpg"))))
      "<img src=\"../1.jpg\" alt=\"../1.jpg\">")
  ;; Nested tags
  ($l (t--sexp2html '(div () (p () "Hello") (p () "World")))
      "<div><p>Hello</p><p>World</p></div>")
  ;; Symbol as tag name
  ($l (t--sexp2html '(my-tag () "content")) "<my-tag>content</my-tag>")
  ;; Empty tag
  ($l (t--sexp2html '(area ())) "<area>")
  ($l (t--sexp2html '(base ())) "<base>")
  ($l (t--sexp2html '(br ())) "<br>")
  ($l (t--sexp2html '(col ())) "<col>")
  ($l (t--sexp2html '(embed ())) "<embed>")
  ($l (t--sexp2html '(hr ())) "<hr>")
  ($l (t--sexp2html '(img ())) "<img>")
  ($l (t--sexp2html '(input ())) "<input>")
  ($l (t--sexp2html '(link ())) "<link>")
  ($l (t--sexp2html '(meta ())) "<meta>")
  ($l (t--sexp2html '(param ())) "<param>")
  ($l (t--sexp2html '(source ())) "<source>")
  ($l (t--sexp2html '(track ())) "<track>")
  ($l (t--sexp2html '(wbr ())) "<wbr>")
  ;; Number as content
  ($l (t--sexp2html '(span () 42)) "<span>42</span>")
  ;; Mixed content (text and elements)
  ($l (t--sexp2html '(div () "Text " (span () "inside") " more text"))
      "<div>Text <span>inside</span> more text</div>")
  ;; Ignore unsupported types (e.g., vectors)
  ($l (t--sexp2html '(div () [1 2 3])) "<div></div>")
  ;; Always downcase
  ($l (t--sexp2html '(DIV () "123")) "<div>123</div>")
  ;; Allow bare tags
  ($l (t--sexp2html '(p)) "<p></p>")
  ($l (t--sexp2html '(hr)) "<hr>")
  ;; Escape
  ($l (t--sexp2html '(p () "123<456>")) "<p>123&lt;456&gt;</p>")
  ($l (t--sexp2html '(p () (b () "a&b"))) "<p><b>a&amp;b</b></p>")
  ;; A list's first element must be a non-nil symbol; the message is
  ;; pinned on the first case.
  ($e!l (t--sexp2html '(nil))
        '(org-w3ctr-error "Invalid S-expression tag: nil"))
  ($e! (t--sexp2html '(1.5 () "x")))
  ($e! (t--sexp2html '("div" () "x")))
  ;; The attribute list must be nil, t, or a proper list.
  ($e!l (t--sexp2html '(p "text"))
        '(org-w3ctr-error "Invalid S-expression attribute list: \"text\""))
  ($e! (t--sexp2html '(p 5 "x")))
  ($e! (t--sexp2html '(p [1 2] "x")))
  ($e! (t--sexp2html '(p (a . b) "x"))))

(ert-deftest t--sexp2html-contract ()
  "A form renders to a string or signals only `org-w3ctr-error'.
Exhaustive over a small shape grammar: valid and invalid tags, every
attribute-list shape, children of every accepted kind plus dropped and
malformed ones.  Anything else -- a leaked `wrong-type-argument', say
-- fails the test."
  (let ((tags '(t0 |My Tag| nil 1.5 "s"))
        (attrs '(nil t () ((id a)) ((id "a b") (class c))
                     (open) [1] (a . b) "s"))
        (children '("" "a&b<c" 42 foo
                    (nil ()) (t0 () (t0 ())) [1])))
    (dolist (tag tags)
      (dolist (a attrs)
        (dolist (c children)
          ($s (condition-case nil
                  (stringp (t--sexp2html (list tag a c)))
                (org-w3ctr-error t))))))))

;;;; References

(ert-deftest t--new-reference ()
  "Tests for `org-w3ctr--new-reference'."
  (let ((n (t--new-reference '((a . 7) (b . 8)))))
    ($s (integerp n))
    ($s (<= 0 n))
    ($s (< n #x10000000))
    ($nl n 7)
    ($nl n 8))
  (let ((n (t--new-reference nil)))
    ($s (integerp n))
    ($s (< n #x10000000))))

(ert-deftest t--new-reference-collision ()
  "A reference number already in use is drawn again.
`org-w3ctr--new-reference' loops while the draw is taken; the test
stubs `random' to hand back a taken number first and counts the draws."
  (let ((draws 0)
        (limits nil))
    (cl-letf (((symbol-function 'random)
               (lambda (&optional limit)
                 (setq draws (1+ draws)
                       limits (cons limit limits))
                 (if (= draws 1) 7 9))))
      ($l (t--new-reference '((a . 7) (b . 8))) 9)
      ($l draws 2)
      ;; the draw bound is #x10000000 exactly -- the range check on
      ;; real draws only catches a wrong one probabilistically
      ($l limits '(#x10000000 #x10000000)))))

(ert-deftest t--format-reference ()
  "Tests for `org-w3ctr--format-reference'."
  ($l (t--format-reference 0) "org0000000")
  ($l (t--format-reference 1) "org0000001")
  ($l (t--format-reference #x1234567) "org1234567")
  ($l (t--format-reference #xabcdef) "org0abcdef")
  ;; the largest legal draw, filling the width exactly
  ($l (t--format-reference #xfffffff) "orgfffffff"))

(ert-deftest t--get-reference ()
  "Tests for `org-w3ctr--get-reference'."
  ;; Stub `random' with an increasing counter: every draw is distinct,
  ;; so two references cannot coincide by chance and the per-INFO
  ;; assertion below cannot flake on a collision.
  (let* ((draw -1)
         (para (t-get-element "hello" 'paragraph))
         (other (t-get-element "world" 'paragraph))
         (info (list :foo 1)))
    (cl-letf (((symbol-function 'random)
               (lambda (&optional _limit) (setq draw (1+ draw)))))
      (let ((ref (t--get-reference para info)))
        ($s (string-match-p "org[0-9a-f]+" ref))
        ;; Same DATUM + same INFO -> same reference (cached).
        ($l (t--get-reference para info) ref)
        ;; The cache records the reference string for DATUM.
        ($s (assoc ref (t--pget info :internal-references))))
      ;; A different datum gets a different reference.
      ($nl (t--get-reference para info) (t--get-reference other info))
      ;; The cache is per-INFO: the same datum draws afresh in a fresh
      ;; INFO; the stubbed draws guarantee the two references differ.
      ($nl (t--get-reference para info) (t--get-reference para (list :foo 1)))
      ;; The search cells are cached as (CELL . NUMBER), the shape
      ;; `org-export-get-reference' reads; the number formats back to the
      ;; datum's reference.  Only named elements have cells, and each
      ;; call builds fresh ones, so look the cell up with `assoc'.
      (let* ((named (t-get-element "#+name: x\nhello" 'paragraph))
             (ref2 (t--get-reference named info)))
        (dolist (cell (org-export-search-cells named))
          (let ((entry (assoc cell (t--pget info :internal-references))))
            ($s entry)
            ($l (t--format-reference (cdr entry)) ref2)))))))

(ert-deftest t--target-reference ()
  "Tests for `org-w3ctr--target-reference'."
  (let ((get-target (lambda (val)
                      (t-get-element (format "<<%s>>" val) 'target))))
    ;; valid target
    ($l (t--target-reference (funcall get-target "foo")) "foo")
    ;; valid radio-target
    ($l (t--target-reference (t-get-element "<<<bar>>>" 'radio-target)) "bar")
    ;; hyphens and underscores allowed
    ($l (t--target-reference (funcall get-target "my-tag_1")) "my-tag_1")
    ;; uppercase letters and a lone letter are fine too
    ($l (t--target-reference (funcall get-target "Foo-1")) "Foo-1")
    ($l (t--target-reference (funcall get-target "x")) "x")
    ;; non-target element -> nil
    ($n (t--target-reference (t-get-element "hello" 'paragraph)))
    ;; space in value -> nil
    ($n (t--target-reference (funcall get-target "my target")))
    ;; starts with digit -> nil
    ($n (t--target-reference (funcall get-target "123")))
    ;; dot in value -> nil
    ($n (t--target-reference (funcall get-target "foo.bar")))
    ;; empty value -> nil
    ($n (t--target-reference (funcall get-target "")))))

(ert-deftest t--reference ()
  "Tests for `org-w3ctr--reference'."
  (let ((no-labels nil)
        (with-labels '(:html-prefer-user-labels t)))
    ;; CUSTOM_ID always wins.
    (let ((h (t-get-element "* H\n:PROPERTIES:\n:CUSTOM_ID: my-id\n:END:"
                            'headline)))
      ($l (t--reference h no-labels) "my-id")
      ($l (t--reference h with-labels) "my-id"))
    ;; a target's value is the reference (the cond routes targets there)
    ($l (t--reference (t-get-element "<<foo>>" 'target) no-labels) "foo")
    ;; NAME with prefer-user-labels=t (paragraph, since #+name: does not
    ;; set :name on headlines).
    (let ((para (t-get-element "#+name: my-name\nhello" 'paragraph)))
      ($l (t--reference para with-labels) "my-name")
      ;; NAME with prefer-user-labels=nil -> falls through to random.
      ($s (string-match-p "org[0-9a-f]+" (t--reference para no-labels))))
    ;; ID property with prefer-user-labels=t -> the "ID-" prefix.
    (let ((h (t-get-element "* H\n:PROPERTIES:\n:ID: my-uid\n:END:" 'headline)))
      ($l (t--reference h with-labels) "ID-my-uid")
      ;; ID with prefer-user-labels=nil -> falls through to random.
      ($s (string-match-p "org[0-9a-f]+" (t--reference h no-labels))))
    ;; named-only + no name + not headline -> nil.
    (let ((para (t-get-element "hello" 'paragraph)))
      ($n (t--reference para (list :html-prefer-user-labels nil) t)))
    ;; headlines, radio targets and targets are exempt from named-only
    ($s (string-match-p "org[0-9a-f]+"
                        (t--reference (t-get-element "* H" 'headline)
                                      no-labels t)))
    ($l (t--reference (t-get-element "<<foo>>" 'target) no-labels t) "foo")
    ($l (t--reference (t-get-element "<<<bar>>>" 'radio-target) no-labels t)
        "bar")))

;;;; Filter Functions

(ert-deftest t-image-link-filter ()
  "Tests for `org-w3ctr-image-link-filter'."
  (let (seen)
    (cl-letf (((symbol-function 'org-export-insert-image-links)
               (lambda (data info rules)
                 (setq seen (list data info rules))
                 'TREE)))
      ($q (t-image-link-filter 'DATA 'backend 'INFO) 'TREE)
      ($l seen (list 'DATA 'INFO t-inline-image-rules)))))

(ert-deftest t-final-function ()
  "Tests for `org-w3ctr-final-function'."
  ;; indent off: CONTENTS comes back unchanged.
  ($l (t-final-function "<ul>\n   <li>a</li>\n</ul>" nil '(:html-indent nil))
      "<ul>\n   <li>a</li>\n</ul>")
  ;; indent on: the major mode is set and the region indented.
  ($l (t-final-function "<ul>\n   <li>a</li>\n</ul>" nil '(:html-indent t))
      "<ul>\n<li>a</li>\n</ul>")
  ;; a single line has nothing to reindent.
  ($l (t-final-function "<p>x</p>" nil '(:html-indent t)) "<p>x</p>")
  ;; the major mode is set only when indenting
  (let (mode-sets)
    (cl-letf (((symbol-function 'set-auto-mode)
               (lambda (&rest args) (setq mode-sets (cons args mode-sets)))))
      (t-final-function "<p>x</p>" nil '(:html-indent nil))
      ($n mode-sets)
      (t-final-function "<p>x</p>" nil '(:html-indent t))
      ($l (length mode-sets) 1))))

(ert-deftest t-filter-registration ()
  "The backend routes both filter slots to both functions."
  (let ((filters (org-export-backend-filters (org-export-get-backend 'w3ctr))))
    ($l (cdr (assq :filter-parse-tree filters)) 't-image-link-filter)
    ($l (cdr (assq :filter-final-output filters)) 't-final-function)))

;;;; JSON-RPC

(ert-deftest t--jrpc-make ()
  "Tests for `org-w3ctr--jrpc-make'."
  (let ((client (t--jrpc-make "test" '("true") nil '(tex2mml))))
    ($s (functionp client))
    ($l (t--jrpc--name client) "test")
    ($q (t--jrpc--conn client) nil)
    ($l (t--jrpc--timeout client) 10.0)
    ($l (t--jrpc--command client) '("true"))
    ($l (t--jrpc--methods client) '(tex2mml))
    ;; callable directly as documented: (METHOD PARAMS &optional TIMEOUT)
    (let (sent)
      (cl-letf (((symbol-function 'jsonrpc-request)
                 (lambda (&rest args) (setq sent args) "RESULT")))
        ($l (funcall client 'tex2mml '(:fragment "x")) "RESULT")
        ;; the conn slot is forwarded as is
        ($l sent '(nil tex2mml (:fragment "x") :timeout 10.0))
        ($l (funcall client 'tex2mml '(:fragment "x") 3) "RESULT")
        ($l sent '(nil tex2mml (:fragment "x") :timeout 3)))))
  ;; the TIMEOUT argument seeds the slot
  ($l (t--jrpc--timeout (t--jrpc-make "x" '("true") 3.5)) 3.5))

(ert-deftest t--jrpc-connect ()
  "Tests for `org-w3ctr--jrpc-connect'."
  ;; The `:process' factory's wiring is checked on a real connection,
  ;; with `make-process' replaced by a pipe process, so no child is
  ;; spawned: the factory must hand jsonrpc's `*NAME stderr*' buffer
  ;; to `make-process' as :stderr, under the exact name the coupling
  ;; needs.  End-to-end stderr separation on a live server is the RPC
  ;; transport harness's job (see the ox-w3ctr-verify skill's
  ;; scripts/stderr-our.el).
  (let* ((name "ox-w3ctr-test-jrpc")
         (command (list "some-server" "--arg"))
         (args nil)
         (stderr-name nil)
         (pipe-buffer nil)
         (conn nil)
         (proc nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest a)
                       (setq args a
                             stderr-name
                             (let ((b (plist-get a :stderr)))
                               (and (bufferp b) (buffer-name b))))
                       (let ((p (make-pipe-process :name name :noquery t)))
                         ;; the pipe's own buffer, before jsonrpc swaps in
                         ;; its output buffer -- it must not be left over
                         (setq pipe-buffer (process-buffer p))
                         p))))
            (setq conn (t--jrpc-connect name command)))
          (setq proc (jsonrpc--process conn))
          ($s (cl-typep conn 'jsonrpc-process-connection))
          ($s (process-live-p proc))
          ;; The factory passes the argv on ...
          ($l (plist-get args :command) command)
          ($q (plist-get args :name) name)
          ($q (plist-get args :noquery) t)
          ($l (plist-get args :coding) 'binary)
          ;; ... and hands over jsonrpc's stderr buffer as :stderr,
          ;; under the exact name the coupling needs -- the "bad
          ;; coupling" jsonrpc.el flags with a FIXME.  jsonrpc renames
          ;; the buffer right after the factory returns, so the name is
          ;; recorded inside the stub.
          ($l stderr-name (format "*%s stderr*" name))
          ($q (plist-get args :stderr) (jsonrpc-stderr-buffer conn)))
      (when (process-live-p proc) (delete-process proc))
      (dolist (b (list (and conn (jsonrpc-stderr-buffer conn))
                       (and proc (process-buffer proc))
                       pipe-buffer
                       (get-buffer (format "*%s events*" name))
                       (get-buffer (format " *%s stderr*" name))
                       (get-buffer (format " *%s output*" name))))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest t--jrpc-shutdown ()
  "Tests for `org-w3ctr--jrpc-shutdown'."
  ;; a live connection is shut down with cleanup, and the slot cleared
  (let ((client (t--jrpc-make "test" '("true"))) shut)
    (setf (t--jrpc--conn client) 'conn)
    (cl-letf (((symbol-function 'jsonrpc-shutdown)
               (lambda (&rest args) (setq shut args))))
      ($n (t--jrpc-shutdown client))
      ($l shut '(conn t))
      ($q (t--jrpc--conn client) nil)))
  ;; no connection: nothing is shut down
  (let ((client (t--jrpc-make "test" '("true"))))
    (cl-letf (((symbol-function 'jsonrpc-shutdown)
               (lambda (&rest _) (error "should not run"))))
      (t--jrpc-shutdown client)
      ($q (t--jrpc--conn client) nil)))
  ;; a failing shutdown is swallowed and the slot is still cleared
  (let ((client (t--jrpc-make "test" '("true"))))
    (setf (t--jrpc--conn client) 'conn)
    (cl-letf (((symbol-function 'jsonrpc-shutdown)
               (lambda (&rest _) (error "boom"))))
      (t--jrpc-shutdown client)
      ($q (t--jrpc--conn client) nil))))

(ert-deftest t--jrpc-ensure ()
  "Tests for `org-w3ctr--jrpc-ensure'."
  ;; a live connection is reused, nothing is started
  (let ((client (t--jrpc-make "test" '("true"))) connected)
    (setf (t--jrpc--conn client) 'live)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_conn) t))
              ((symbol-function 't--jrpc-connect)
               (lambda (&rest _) (setq connected t) 'fresh)))
      ($q (t--jrpc-ensure client) 'live)
      ($n connected)))
  ;; a dead one is shut down and replaced
  (let ((client (t--jrpc-make "test" '("true"))) shut)
    (setf (t--jrpc--conn client) 'dead)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_conn) nil))
              ((symbol-function 'jsonrpc-shutdown)
               (lambda (_conn &rest _) (setq shut t)))
              ((symbol-function 't--jrpc-connect)
               (lambda (name command) (list name command))))
      ($l (t--jrpc-ensure client) '("test" ("true")))
      ($s shut)
      ($l (t--jrpc--conn client) '("test" ("true")))))
  ;; an absent connection is built, without consulting its liveness
  (let ((client (t--jrpc-make "test" '("true"))) shut)
    (cl-letf (((symbol-function 'jsonrpc-running-p)
               (lambda (&rest _) (error "should not run")))
              ((symbol-function 't--jrpc-shutdown)
               (lambda (_client) (setq shut t)))
              ((symbol-function 't--jrpc-connect)
               (lambda (name command) (list name command))))
      ($l (t--jrpc-ensure client) '("test" ("true")))
      ($s shut)
      ($l (t--jrpc--conn client) '("test" ("true"))))))

(ert-deftest t--jrpc-restart ()
  "Tests for `org-w3ctr--jrpc-restart'."
  (let ((client (t--jrpc-make "test" '("true"))) shut)
    (setf (t--jrpc--conn client) 'old)
    (cl-letf (((symbol-function 'jsonrpc-shutdown)
               (lambda (_conn &rest _) (setq shut t)))
              ((symbol-function 'jsonrpc-running-p) (lambda (_conn) nil))
              ((symbol-function 't--jrpc-connect) (lambda (_n _c) 'new)))
      ($q (t--jrpc-restart client) 'new)
      ($s shut)
      ($q (t--jrpc--conn client) 'new)))
  ;; a live connection is replaced all the same -- that is the restart
  (let ((client (t--jrpc-make "test" '("true"))) shut)
    (setf (t--jrpc--conn client) 'old)
    (cl-letf (((symbol-function 'jsonrpc-shutdown)
               (lambda (_conn &rest _) (setq shut t)))
              ((symbol-function 'jsonrpc-running-p) (lambda (_conn) t))
              ((symbol-function 't--jrpc-connect) (lambda (_n _c) 'new)))
      ($q (t--jrpc-restart client) 'new)
      ($s shut)
      ($q (t--jrpc--conn client) 'new))))

(ert-deftest t--jcall ()
  "Tests for `org-w3ctr--jcall'."
  (let ((client (t--jrpc-make "test" '("true") nil '(tex2mml))) sent)
    (setf (t--jrpc--conn client) 'conn)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_conn) t))
              ((symbol-function 'jsonrpc-request)
               (lambda (conn method params &rest args)
                 (setq sent (list conn method params args))
                 "RESULT")))
      ;; a method in the table is forwarded with the default timeout
      ($l (t--jcall client 'tex2mml '(:fragment "x")) "RESULT")
      ($l sent '(conn tex2mml (:fragment "x") (:timeout 10.0)))
      ;; the TIMEOUT argument overrides the slot
      (setq sent nil)
      (t--jcall client 'tex2mml '(:fragment "x") 3)
      ($l sent '(conn tex2mml (:fragment "x") (:timeout 3)))
      ;; a method outside the table signals and sends nothing
      (setq sent nil)
      ($e!l (t--jcall client 'tex2svg '(:fragment "x"))
            '(org-w3ctr-error "Unknown jstools method: tex2svg"))
      ($n sent)))
  ;; a nil METHODS accepts any method
  (let ((client (t--jrpc-make "test" '("true"))) sent)
    (setf (t--jrpc--conn client) 'conn)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_conn) t))
              ((symbol-function 'jsonrpc-request)
               (lambda (&rest args) (setq sent args) "RESULT")))
      ($l (t--jcall client 'anything '(:x 1)) "RESULT")
      ($l sent '(conn anything (:x 1) :timeout 10.0))))
  ;; the connection is built before the call when it is not there
  (let ((client (t--jrpc-make "test" '("true") nil '(tex2mml))) sent)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_conn) nil))
              ((symbol-function 'jsonrpc-shutdown) (lambda (&rest _) nil))
              ((symbol-function 't--jrpc-connect) (lambda (_n _c) 'fresh))
              ((symbol-function 'jsonrpc-request)
               (lambda (conn method params &rest args)
                 (setq sent (list conn method params args))
                 "RESULT")))
      ($l (t--jcall client 'tex2mml '(:fragment "x")) "RESULT")
      ($l sent '(fresh tex2mml (:fragment "x") (:timeout 10.0))))))

(ert-deftest t--jstools-methods-drift ()
  "Static check: every method `org-w3ctr--jstools-methods' exposes is
implemented by an `addMethod' call in jstools/index.js."
  (let ((file (file-name-concat t--dir "jstools/index.js"))
        (re "addMethod([ \t]*['\"]\\([^'\"]+\\)['\"]"))
    (skip-unless (file-readable-p file))
    (let* ((source (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
           (implemented
            (let ((i 0) names)
              (while (string-match re source i)
                (push (intern (match-string 1 source)) names)
                (setq i (match-end 0)))
              (nreverse names))))
      ($s implemented)
      (dolist (method t--jstools-methods)
        ($s (memq method implemented))))))

(ert-deftest t--jstools ()
  "Tests for `org-w3ctr--jstools'."
  ($l (t--jrpc--name t--jstools) "ox-w3ctr-jstools")
  ($l (t--jrpc--methods t--jstools) t--jstools-methods)
  ($l (t--jrpc--timeout t--jstools) 10.0)
  ($l (car (t--jrpc--command t--jstools)) "node")
  ($l (cadr (t--jrpc--command t--jstools))
      (file-name-concat t--dir "jstools/index.js"))
  ($l (cddr (t--jrpc--command t--jstools)) '("--timeout" "30000")))

(ert-deftest t-show-jstools-events ()
  "Tests for `org-w3ctr-show-jstools-events'."
  ;; Only the wiring is checked (a smoke test): the command shows the
  ;; jstools client's events buffer, and an interactive command's
  ;; return value is incidental.
  (let ((conn (make-instance 'jsonrpc-connection :name "ox-w3ctr-test-events"))
        (buf-name "*ox-w3ctr-test-events events*"))
    (unwind-protect
        (cl-letf (((symbol-function 't--jrpc-ensure)
                   (lambda (client) ($q client t--jstools) conn)))
          (t-show-jstools-events)
          ($l (buffer-name (current-buffer)) buf-name))
      (when (get-buffer buf-name) (kill-buffer buf-name)))))

(ert-deftest t-launch-jstools ()
  "Tests for `org-w3ctr-launch-jstools'."
  ;; Only the wiring is checked (a smoke test): the command restarts
  ;; the jstools client.
  (let (restarted)
    (cl-letf (((symbol-function 't--jrpc-restart)
               (lambda (client) (setq restarted client) 'new)))
      (t-launch-jstools)
      ($q restarted t--jstools))))

;;; Greater elements

;;;; Center Block

(ert-deftest t-center-block ()
  "Tests for `org-w3ctr-center-block'."
  (t-check-element-values
   #'t-center-block
   '(;; default: centering style
     ("#+begin_center\n#+end_center"
      "<div style=\"text-align:center;\"></div>")
     ("#+begin_center\n123\n#+end_center"
      "<div style=\"text-align:center;\">\n<p>123</p>\n</div>")
     ("#+BEGIN_CENTER\n\n\n#+END_CENTER"
      "<div style=\"text-align:center;\">\n\n</div>")
     ("#+BEGIN_CENTER\n\n\n\n\n\n#+END_CENTER"
      "<div style=\"text-align:center;\">\n\n</div>")
     ;; with attr__: generic div, no centering
     ("#+attr__: [my-class]\n#+begin_center\nhello\n#+end_center"
      "<div class=\"my-class\">\n<p>hello</p>\n</div>")
     ;; with attr__: explicit style overrides centering
     ("#+attr__:(style \"text-align:right\")\n#+begin_center\nhello\n#+end_center"
      "<div style=\"text-align:right\">\n<p>hello</p>\n</div>")
     ;; explicit id: how a center block gets an anchor
     ("#+attr__: (id foo)\n#+begin_center\nhello\n#+end_center"
      "<div id=\"foo\">\n<p>hello</p>\n</div>")
     ;; with attr_html: the standard Org attribute syntax drops the
     ;; centering too
     ("#+attr_html: :class foo\n#+begin_center\nhello\n#+end_center"
      "<div class=\"foo\">\n<p>hello</p>\n</div>")
     ;; presence alone drops the centering: an empty attribute keyword
     ;; still selects a generic div
     ("#+attr__:\n#+begin_center\nhello\n#+end_center"
      "<div>\n<p>hello</p>\n</div>")
     ("#+attr_html:\n#+begin_center\nhello\n#+end_center"
      "<div>\n<p>hello</p>\n</div>")
     ;; #+name: without attr__: keeps centering (name does not add id by
     ;; #default)
     ("#+name: my-block\n#+begin_center\nhello\n#+end_center"
      "<div style=\"text-align:center;\">\n<p>hello</p>\n</div>"))))

;;;; Drawer

(ert-deftest t-drawer-default-format-function ()
  "Tests for `org-w3ctr-drawer-default-format-function'."
  ($l (t-drawer-default-format-function "name" "sum" "" nil nil)
      "<details><summary>sum</summary></details>")
  ($l (t-drawer-default-format-function "name" "sum" "" "body" nil)
      "<details><summary>sum</summary>\nbody</details>")
  ($l (t-drawer-default-format-function "name" "sum" " id=\"d\"" "body" nil)
      "<details id=\"d\"><summary>sum</summary>\nbody</details>"))

(ert-deftest t-drawer-format-function ()
  "The drawer goes through `org-w3ctr-drawer-format-function'."
  (let ((org-w3ctr-drawer-format-function
         (lambda (name summary attrs _contents info)
           (format "<DRAWER name=%s summary=%s attrs=%s labels=%s/>"
                   name summary attrs
                   (if (t--pget info :html-prefer-user-labels) "on" "off")))))
    (t-check-element-values
     #'t-drawer
     '((":hello:\n:end:"
        "<DRAWER name=hello summary=hello attrs= labels=on/>")
       ("#+caption: what can i say\n:test:\n:end:"
        "<DRAWER name=test summary=what can i say attrs= labels=on/>")
       ("#+name: id\n#+attr__: [example]\n:h:\n:end:"
        "<DRAWER name=h summary=h attrs= id=\"id\" class=\"example\" labels=on/>"))
     nil '(:html-prefer-user-labels t))))

(ert-deftest t-drawer ()
  "Tests for `org-w3ctr-drawer'."
  (t-check-element-values
   #'t-drawer
   '((":hello:\n:end:"
      "<details><summary>hello</summary></details>")
     ("#+caption: what can i say\n:test:\n:end:"
      "<details><summary>what can i say</summary></details>")
     ;; caption markup is exported, not escaped
     ("#+caption: *bold* text\n:test:\n:end:"
      "<details><summary><b>bold</b> text</summary></details>")
     ("#+name: id\n#+attr__: [example]\n:h:\n:end:"
      "<details id=\"id\" class=\"example\"><summary>h</summary></details>")
     ("#+attr__: (open)\n:h:\n:end:"
      "<details open><summary>h</summary></details>")
     ;; attr_html is honoured like attr__
     ("#+attr_html: :class foo\n:test:\n:end:"
      "<details class=\"foo\"><summary>test</summary></details>")
     (":try-this:\n=int a = 1;=\n:end:"
      "<details><summary>try-this</summary>\n<p><code>\
int a = 1;</code></p>\n</details>")
     ("#+CAPTION:\n:test:\n:end:"
      "<details><summary>test</summary></details>")
     ("#+caption: \n:test:\n:end:"
      "<details><summary>test</summary></details>")
     ("#+caption:         \t\n:test:\n:end:"
      "<details><summary>test</summary></details>"))
   nil '(:html-prefer-user-labels t))
  ;; a nil format function falls back to the default
  (t-check-element-values
   #'t-drawer
   '((":hello:\n:end:" "<details><summary>hello</summary></details>"))
   nil '(:html-format-drawer-function nil)))

;;;; Dynamic Block

(ert-deftest t-dynamic-block ()
  "Tests for `org-w3ctr-dynamic-block'."
  (t-check-element-values
   #'t-dynamic-block
   '(("#+begin: hello\n123\n#+end:" "<p>123</p>\n")
     ("#+begin: nothing\n#+end:" ""))))

;;;; Footnote

(ert-deftest t--footnote-key ()
  "Tests for `org-w3ctr--footnote-key'."
  ($l (t--footnote-key "name" 3) "name")
  ($l (t--footnote-key "1" 3) 3)
  ($l (t--footnote-key nil 3) 3))

(ert-deftest t--footnote-id ()
  "Tests for `org-w3ctr--footnote-id'."
  ($l (t--footnote-id "name" 3) "fn-name")
  ($l (t--footnote-id "1" 3) "fn-3")
  ($l (t--footnote-id nil 3) "fn-3"))

(ert-deftest t-footnote-reference ()
  "Tests for `org-w3ctr-footnote-reference'."
  (t-check-element-values
   #'t-footnote-reference
   '(("A[fn:1].\n\n[fn:1] The definition." "[<a href=\"#fn-1\">1</a>]")
     ("A[fn:name].\n\n[fn:name] The definition."
      "[<a href=\"#fn-name\">name</a>]")
     ("A[fn::text]." "[<a href=\"#fn-1\">1</a>]")
     ;; Two footnotes in a row are separated (values are in reverse
     ;; call order, as in the other `org-w3ctr-check-element-values' tests).
     ("A[fn:1][fn:2].\n\n[fn:1] one.\n\n[fn:2] two."
      ", [<a href=\"#fn-2\">2</a>]" "[<a href=\"#fn-1\">1</a>]"))
   t '(:with-latex verbatim :html-prefer-user-labels t))
  ;; a custom reference format is read from INFO
  (t-check-element-values
   #'t-footnote-reference
   '(("A[fn:1].\n\n[fn:1] one."
      "<sup><a href=\"#fn-1\">1</a></sup>"))
   t '(:with-latex verbatim :html-footnote-format "<sup>%s</sup>"))
  ;; a custom separator is read from INFO
  (t-check-element-values
   #'t-footnote-reference
   '(("A[fn:1][fn:2].\n\n[fn:1] one.\n\n[fn:2] two."
      " | [<a href=\"#fn-2\">2</a>]" "[<a href=\"#fn-1\">1</a>]"))
   t '(:with-latex verbatim :html-footnote-separator " | ")))

(ert-deftest t--footnote-definition ()
  "Tests for `org-w3ctr--footnote-definition'."
  (cl-letf (((symbol-function 'org-export-data)
             (lambda (data _info)
               (if (stringp data) data
                 (let ((text (string-trim-right
                              (org-element-interpret-data data))))
                   (format "<p>%s</p>" text))))))
    (cl-flet ((p (s) (t-get-element s 'paragraph)))
      (let ((info '(:html-footnote-format "[%s]")))
        ;; Numbered footnote with paragraph
        ($l (t--footnote-definition (list 1 nil (p "The definition.")) info)
            ($c "<dt id=\"fn-1\">[1]</dt>\n"
                "<dd>\n<p>The definition.</p>\n</dd>"))
        ;; Named footnote with paragraph
        ($l (t--footnote-definition (list 1 "name" (p "The definition.")) info)
            ($c "<dt id=\"fn-name\">[name]</dt>\n"
                "<dd>\n<p>The definition.</p>\n</dd>"))
        ;; Purely numeric label uses number
        ($l (t--footnote-definition (list 3 "1" (p "Text.")) info)
            ($c "<dt id=\"fn-3\">[3]</dt>\n"
                "<dd>\n<p>Text.</p>\n</dd>"))
        ;; Inline definition (no paragraph wrapper, just string)
        ($l (t--footnote-definition (list 1 "name" "text") info)
            ($c "<dt id=\"fn-name\">[name]</dt>\n"
                "<dd>\ntext\n</dd>"))
        ;; Whitespace trimming on string
        ($l (t--footnote-definition (list 1 nil "\n  text  \n") info)
            ($c "<dt id=\"fn-1\">[1]</dt>\n"
                "<dd>\ntext\n</dd>"))))))

(ert-deftest t-footnote-section-default-function ()
  "Tests for `org-w3ctr-footnote-section-default-function'."
  (cl-letf (((symbol-function 'org-export-data)
             (lambda (data _info)
               (if (stringp data)
                   data
                 (let ((text (string-trim-right
                              (org-element-interpret-data data))))
                   (format "<p>%s</p>" text))))))
    (cl-flet ((p (s) (t-get-element s 'paragraph)))
      (let ((info (list :html-footnotes-section
                        ($c "<div id=\"references\">\n<h2>%s</h2>\n"
                            "<dl>%s</dl>\n</div>\n")
                        :html-footnote-format "[%s]")))
        ;; Single footnote
        ($l (t-footnote-section-default-function
             (list (list 1 nil (p "Text."))) info)
            ($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
                "<dt id=\"fn-1\">[1]</dt>\n<dd>\n<p>Text.</p>\n</dd>\n"
                "</dl>\n</div>\n"))
        ;; Multiple footnotes
        ($l (t-footnote-section-default-function
             (list (list 1 nil (p "One."))
                   (list 2 "name" (p "Two."))) info)
            ($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
                "<dt id=\"fn-1\">[1]</dt>\n<dd>\n<p>One.</p>\n</dd>\n"
                "<dt id=\"fn-name\">[name]</dt>\n<dd>\n<p>Two.</p>\n</dd>\n"
                "</dl>\n</div>\n"))
        ;; Custom footnote format
        (let ((info2 (plist-put (copy-sequence info)
                                :html-footnote-format "<sup>%s</sup>")))
          ($l (t-footnote-section-default-function
               (list (list 1 nil "text")) info2)
              ($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
                  "<dt id=\"fn-1\"><sup>1</sup></dt>\n<dd>\ntext\n</dd>\n"
                  "</dl>\n</div>\n")))
        ;; Custom section wrapper
        (let ((info3 (plist-put (copy-sequence info)
                                :html-footnotes-section
                                "<section><h3>%s</h3>%s</section>")))
          ($l (t-footnote-section-default-function
               (list (list 1 nil "text")) info3)
              ($c "<section><h3>References</h3>\n"
                  "<dt id=\"fn-1\">[1]</dt>\n<dd>\ntext\n</dd>\n"
                  "</section>")))))))

(ert-deftest t-footnote-section-function ()
  "The footnotes section goes through `org-w3ctr-footnote-section-function'."
  (let ((org-w3ctr-footnote-section-function
         (lambda (definitions _info)
           (format "<FOOTNOTES n=%d/>" (length definitions)))))
    (t-check-element-values
     #'t-footnote-section
     '(("A[fn:1] B[fn:2].\n\n[fn:1] one.\n\n[fn:2] two."
        "<FOOTNOTES n=2/>"))
     nil '(:with-latex verbatim))))

(ert-deftest t-footnote-section ()
  "Tests for `org-w3ctr-footnote-section'."
  (t-check-element-values
   #'t-footnote-section
   `(("A[fn:1].\n\n[fn:1] The definition."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-1\">[1]</dt>\n"
           "<dd>\n<p>The definition.</p>\n</dd>\n"
           "</dl>\n</div>\n"))
     ("A[fn:name].\n\n[fn:name] The definition."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-name\">[name]</dt>\n"
           "<dd>\n<p>The definition.</p>\n</dd>\n"
           "</dl>\n</div>\n"))
     ("A[fn:name:text]."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-name\">[name]</dt>\n<dd>\ntext\n</dd>\n"
           "</dl>\n</div>\n"))
     ("A[fn::text]."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-1\">[1]</dt>\n<dd>\ntext\n</dd>\n"
           "</dl>\n</div>\n"))
     ("A[fn:1] B[fn:2].\n\n[fn:1] one.\n\n[fn:2] two."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-1\">[1]</dt>\n"
           "<dd>\n<p>one.</p>\n</dd>\n"
           "<dt id=\"fn-2\">[2]</dt>\n"
           "<dd>\n<p>two.</p>\n</dd>\n"
           "</dl>\n</div>\n"))
     ;; no footnotes: the section is nil, not an empty one
     ("Just text." nil))
   nil '(:with-latex verbatim))
  ;; a nil section function falls back to the default
  (t-check-element-values
   #'t-footnote-section
   `(("A[fn:1].\n\n[fn:1] The definition."
      ,($c "<div id=\"references\">\n<h2>References</h2>\n<dl>\n"
           "<dt id=\"fn-1\">[1]</dt>\n"
           "<dd>\n<p>The definition.</p>\n</dd>\n"
           "</dl>\n</div>\n")))
   nil '(:with-latex verbatim :html-footnote-section-function nil)))

;;;; Item and Plain Lists helper functions

(ert-deftest t-checkbox-types ()
  "Tests for `org-w3ctr-checkbox-types'."
  ;; The registry has exactly the three documented types, each with
  ;; exactly the three states and non-empty string markup.
  ($l (mapcar #'car org-w3ctr-checkbox-types) '(unicode ascii html))
  (dolist (entry org-w3ctr-checkbox-types)
    ($l (mapcar #'car (cdr entry)) '(on off trans))
    (dolist (state (cdr entry))
      ($s (stringp (cdr state)))
      ($s (> (length (cdr state)) 0)))))

(ert-deftest t--checkbox ()
  "Tests for `org-w3ctr-checkbox'."
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--checkbox 'on info) "&#x2611;")
    ($l (t--checkbox 'off info) "&#x2610;")
    ($l (t--checkbox 'trans info) "&#x2612;")
    ($l (t--checkbox nil info) nil)
    ($l (t--checkbox [1] info) nil)
    ($l (t--checkbox "hello" info) nil)
    ($l (t--checkbox "on" info) nil)
    ($l (t--checkbox "off" info) nil)
    ($l (t--checkbox "trans" info) nil))
  (let ((info '(:html-checkbox-type ascii)))
    ($l (t--checkbox 'off info) "<code>[&#xa0;]</code>")
    ($l (t--checkbox 'on info) "<code>[X]</code>")
    ($l (t--checkbox 'trans info) "<code>[-]</code>"))
  (let ((info '(:html-checkbox-type html)))
    ($l (t--checkbox 'off info) "<input type=\"checkbox\">")
    ($l (t--checkbox 'on info) "<input type=\"checkbox\" checked>")
    ($l (t--checkbox 'trans info) "<input type=\"checkbox\">"))
  ;; An unknown checkbox type is an error, not a dropped checkbox;
  ;; with no checkbox there is nothing to format, so no error.
  (let ((info '(:html-checkbox-type unicod)))
    ($e!l (t--checkbox 'on info)
          '(org-w3ctr-error "Unknown checkbox type: unicod"))
    ($l (t--checkbox nil info) nil)))

(ert-deftest t--format-checkbox ()
  "Tests for `org-w3ctr--format-checkbox'."
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-checkbox 'off info) "&#x2610; ")
    ($l (t--format-checkbox 'on info) "&#x2611; ")
    ($l (t--format-checkbox 'trans info) "&#x2612; ")
    ($l (t--format-checkbox nil info) "")
    ($l (t--format-checkbox 'test info) "")
    ($l (t--format-checkbox [1] info) "")
    ($l (t--format-checkbox '(1 . 2) info) "")
    ($l (t--format-checkbox #s(hello wtf) info) "")))

(ert-deftest t--format-ordered-item ()
  "Tests for `org-w3ctr--format-ordered-item'."
  ($l (t--format-ordered-item "" nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "\n  \n" nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "\t\r\n " nil nil nil) "<li></li>")
  ;; nil contents: a bare list bullet exports as an empty element
  ($l (t--format-ordered-item nil nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "123" nil nil nil) "<li>123</li>")
  ($l (t--format-ordered-item " 123 " nil nil nil) "<li>123</li>")
  ($l (t--format-ordered-item "123" nil nil 10) "<li value=\"10\">123</li>")
  ($l (t--format-ordered-item "123" nil nil 114514)
      "<li value=\"114514\">123</li>")
  ($l (t--format-ordered-item "123" nil nil 191981)
      "<li value=\"191981\">123</li>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-ordered-item "123" 'off info nil)
        "<li>&#x2610; 123</li>")
    ($l (t--format-ordered-item "123" 'on info nil)
        "<li>&#x2611; 123</li>")
    ($l (t--format-ordered-item "123" 'trans info nil)
        "<li>&#x2612; 123</li>")
    ($l (t--format-ordered-item "123" 'on info 114)
        "<li value=\"114\">&#x2611; 123</li>")))

(ert-deftest t--format-unordered-item ()
  "Tests for `org-w3ctr--format-unordered-item'."
  ($l (t--format-unordered-item "" nil nil) "<li></li>")
  ($l (t--format-unordered-item "\n  \n" nil nil) "<li></li>")
  ($l (t--format-unordered-item "\t\r\n " nil nil) "<li></li>")
  ;; nil contents: a bare list bullet exports as an empty element
  ($l (t--format-unordered-item nil nil nil) "<li></li>")
  ($l (t--format-unordered-item "123" nil nil) "<li>123</li>")
  ($l (t--format-unordered-item " 123 " nil nil) "<li>123</li>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-unordered-item "123" 'off info)
        "<li>&#x2610; 123</li>")
    ($l (t--format-unordered-item "123" 'on info)
        "<li>&#x2611; 123</li>")
    ($l (t--format-unordered-item "123" 'trans info)
        "<li>&#x2612; 123</li>")))

(ert-deftest t--format-descriptive-item ()
  "Tests for `org-w3ctr--format-descriptive-item'."
  ($l (t--format-descriptive-item "" nil nil nil)
      "<dt></dt><dd></dd>")
  ($l (t--format-descriptive-item " " nil nil nil)
      "<dt></dt><dd></dd>")
  ($l (t--format-descriptive-item "\r\n\t " nil nil nil)
      "<dt></dt><dd></dd>")
  ;; nil contents: a bare list bullet exports as an empty element
  ($l (t--format-descriptive-item nil nil nil nil)
      "<dt></dt><dd></dd>")
  ($l (t--format-descriptive-item "123" nil nil nil)
      "<dt></dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil nil)
      "<dt></dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "ONE")
      "<dt>ONE</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "TWO")
      "<dt>TWO</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "THREE ")
      "<dt>THREE </dt><dd>123</dd>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-descriptive-item "123" 'off info nil)
        "<dt>&#x2610; </dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'on info nil)
        "<dt>&#x2611; </dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'trans info nil)
        "<dt>&#x2612; </dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'trans info " test ")
        "<dt>&#x2612;  test </dt><dd>123</dd>")))

;;;; Item

(ert-deftest t-item-unordered ()
  "Tests for `org-w3ctr-item' unordered clause."

  (t-check-element-values
   #'t-item
   '(("- 123" "<li>123</li>")
     ("- hello \n 123" "<li>hello \n123</li>")
     ("- hello \n\n123" "<li>hello</li>")
     ("- hello \n\n 123" "<li>hello\n\n<p>123</p></li>")
     ("- hello \n\n     \t123" "<li>hello\n\n<p>123</p></li>")
     ("- [ ] 123" "<li>&#x2610; 123</li>")
     ("- [X] 123" "<li>&#x2611; 123</li>")
     ("- [ ] 123   \n 234" "<li>&#x2610; 123   \n234</li>")
     ("- [ ] 123 \n\n234" "<li>&#x2610; 123</li>")
     ("- [ ] 123 \n\n 234" "<li>&#x2610; 123\n\n<p>234</p></li>")
     ("- [ ] [@1] 123" "<li>&#x2610; [@1] 123</li>")
     ("- [ ] [@1]123" "<li>&#x2610; [@1]123</li>")
     ("- [@2] 123" "<li>123</li>")
     ("- [@1]123"  "<li>123</li>")
     ("- [@a] 123" "<li>123</li>")
     ("- [@1] [ ] 123" "<li>&#x2610; 123</li>")
     ("- [@a] [ ] 123" "<li>&#x2610; 123</li>")
     ("- [@pp] [ ] 123" "<li>[@pp] [ ] 123</li>")
     ;; zero width space
     ("- [​@1] [ ] 123" "<li>[​@1] [ ] 123</li>"))
   nil '(:html-checkbox-type unicode)))

(ert-deftest t-item-ordered ()
  "Tests for `org-w3ctr-item' ordered clause."

  (t-check-element-values
   #'t-item
   '(("1. 123" "<li>123</li>")
     ("1. hello \n 123" "<li>hello \n123</li>")
     ("1. hello \n\n123" "<li>hello</li>")
     ("1. hello \n\n 123" "<li>hello\n\n<p>123</p></li>")
     ("1. hello \n\n     \t123" "<li>hello\n\n<p>123</p></li>")
     ("1. [ ] 123" "<li>&#x2610; 123</li>")
     ("1. [X] 123" "<li>&#x2611; 123</li>")
     ("1. [ ] 123   \n 234" "<li>&#x2610; 123   \n234</li>")
     ("1. [ ] 123 \n\n234" "<li>&#x2610; 123</li>")
     ("1. [ ] 123 \n\n 234" "<li>&#x2610; 123\n\n<p>234</p></li>")
     ("1. [ ] [@1] 123" "<li>&#x2610; [@1] 123</li>")
     ("1. [ ] [@1]123" "<li>&#x2610; [@1]123</li>")
     ("1. [@2] 123" "<li value=\"2\">123</li>")
     ("1. [@1]123"  "<li value=\"1\">123</li>")
     ("1. [@a] 123" "<li value=\"1\">123</li>")
     ("1. [@1] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
     ("1. [@a] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
     ("1. [@z] [ ] 123" "<li value=\"26\">&#x2610; 123</li>")
     ("1. [@pp] [ ] 123" "<li>[@pp] [ ] 123</li>")
     ;; zero width space
     ("1. [​@1] [ ] 123" "<li>[​@1] [ ] 123</li>"))
   nil '(:html-checkbox-type unicode)))

(ert-deftest t-item-descriptive ()
  "Tests for `org-w3ctr-item' descriptive clause."

  (t-check-element-values
   #'t-item
   '(("- 123 :: tag" "<dt>123</dt><dd>tag</dd>")
     ("- *bold* tag :: x" "<dt><b>bold</b> tag</dt><dd>x</dd>")
     ("- hello :: test \n 123" "<dt>hello</dt><dd>test \n123</dd>")
     ("- hello :: \n\n123" "<dt>hello</dt><dd></dd>")
     ("- hello :: \n\n 123" "<dt>hello</dt><dd>123</dd>")
     ("- hello :: \n\n  123" "<dt>hello</dt><dd>123</dd>")
     ("- hello :: \n\n\n  123" "<dt>hello</dt><dd></dd>")
     ("- hello :: \n\n     \t123" "<dt>hello</dt><dd>123</dd>")
     ("- hello :: world 123" "<dt>hello</dt><dd>world 123</dd>")
     ("- h :: w \n123" "<dt>h</dt><dd>w</dd>")
     ("- h :: w \n 123" "<dt>h</dt><dd>w \n123</dd>")
     ("- h :: w \n\n123" "<dt>h</dt><dd>w</dd>")
     ("- h :: w \n\n 123" "<dt>h</dt><dd>w\n\n<p>123</p></dd>")
     ("- h :: w \n\n 123\n 456" "<dt>h</dt><dd>w\n\n<p>123\n456</p></dd>")
     ("- [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
     ("- [X] 123 ::" "<dt>&#x2611; 123</dt><dd></dd>")
     ("- [ ] 123 ::  \n 234" "<dt>&#x2610; 123</dt><dd>234</dd>")
     ("- [ ] 123 :: \n\n234" "<dt>&#x2610; 123</dt><dd></dd>")
     ("- [ ] 123 :: \n\n 234" "<dt>&#x2610; 123</dt><dd>234</dd>")
     ("- [ ] [@1] 123 ::" "<dt>&#x2610; [@1] 123</dt><dd></dd>")
     ("- [ ] [@1]123 ::" "<dt>&#x2610; [@1]123</dt><dd></dd>")
     ("- [@2] 123 ::" "<dt>123</dt><dd></dd>")
     ("- [@1]123 ::"  "<dt>123</dt><dd></dd>")
     ("- [@1]123::" "<li>123::</li>")
     ("- [@a] 123 ::" "<dt>123</dt><dd></dd>")
     ("- [@1] [ ] 123 ::" "<dt>&#x2610; 123</dt><dd></dd>")
     ("- [@a] [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
     ("- [@pp] [ ] :: 123" "<dt>[@pp] [ ]</dt><dd>123</dd>")
     ;; zero width space
     ("- [​@1] [ ] 123 :: " "<dt>[​@1] [ ] 123</dt><dd></dd>")
     ("- a ::" "<dt>a</dt><dd></dd>")
     ("- :: 3" "<li>:: 3</li>")
     ("- a :: b\n- c" "<dt></dt><dd>c</dd>"
      "<dt>a</dt><dd>b</dd>"))
   nil '(:html-checkbox-type unicode)))

(ert-deftest t-item ()
  "Tests for `org-w3ctr-item'."

  (t-check-element-values
   #'t-item
   '(("- [@a] [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
     ("1. [@1] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
     ("- [ ] 123 \n\n 234" "<li>&#x2610; 123\n\n<p>234</p></li>"))
   nil '(:html-checkbox-type unicode))
  ($e!l (t-item nil "123" nil)
        '(org-w3ctr-error "Unknown list item type: nil")))

;;;; Plain List

(ert-deftest t-plain-list ()
  "Tests for `org-w3ctr-plain-list'."
  (t-check-element-values
   #'t-plain-list
   '(("- 123" "<ul>\n<li>123</li>\n</ul>")
     ("1. 123" "<ol>\n<li>123</li>\n</ol>")
     ("- x :: y" "<dl>\n<dt>x</dt><dd>y</dd>\n</dl>")
     ("#+name: test\n#+attr__: (data-test \"a joke\")\n- x"
      "<ul id=\"test\" data-test=\"a joke\">\n<li>x</li>\n</ul>")
     ("#+attr_html: :class foo\n- x"
      "<ul class=\"foo\">\n<li>x</li>\n</ul>")
     ("1. 123\n   - 2 3 4"
      "<ol>\n<li>123\n<ul>\n<li>2 3 4</li>\n</ul></li>\n</ol>"
      "<ul>\n<li>2 3 4</li>\n</ul>"))
   nil '(:html-prefer-user-labels t))
  ;; nil CONTENTS (e.g. from `org-export-with-backend') is empty.
  ($l (t-plain-list (t-get-element "- 123" 'plain-list)
                    nil nil)
      "<ul>\n</ul>")
  ($e!l (t-plain-list nil "123" nil)
        '(org-w3ctr-error "Unknown HTML list type: nil")))

;;;; Quote Block

(ert-deftest t-quote-block ()
  "Tests for `org-w3ctr-quote-block'."
  (t-check-element-values
   #'t-quote-block
   '(("#+begin_quote\n#+end_quote" "<blockquote></blockquote>")
     ("#+BEGIN_QUOTE\n#+END_QUOTE" "<blockquote></blockquote>")
     ("#+begin_quote\n123\n#+end_quote"
      "<blockquote>\n<p>123</p>\n</blockquote>")
     ("#+attr__: [test]\n#+BEGIN_QUOTE\n456\n#+END_QUOTE"
      "<blockquote class=\"test\">\n<p>456</p>\n</blockquote>")
     ("#+attr_html: :class test\n#+begin_quote\n456\n#+end_quote"
      "<blockquote class=\"test\">\n<p>456</p>\n</blockquote>")
     ("#+begin_quote\n\n\n#+end_quote" "<blockquote>\n\n</blockquote>")
     ("#+begin_quote\n\n\n\n\n\n\n\n\n\n#+end_quote"
      "<blockquote>\n\n</blockquote>")))
  ;; a name becomes an id when `:html-prefer-user-labels' is on
  (t-check-element-values
   #'t-quote-block
   '(("#+name: quote\n#+begin_quote\n456\n#+end_quote"
      "<blockquote id=\"quote\">\n<p>456</p>\n</blockquote>"))
   nil '(:html-prefer-user-labels t)))

;;;; Special Block

(ert-deftest t-html5-elements ()
  "Tests for `org-w3ctr-html5-elements'."
  ;; The list is the compatibility target: it must stay identical to
  ;; ox-html's, which the package already requires.
  ($l org-w3ctr-html5-elements org-html-html5-elements))

(ert-deftest t--special-block-builtin ()
  "Tests for `org-w3ctr--special-block-builtin'."
  (t-check-element-values
   #'t--special-block-builtin
   '(;; an HTML5 element keeps its name
     ("#+begin_section\nhello\n#+end_section"
      "<section>\n<p>hello</p>\n</section>")
     ;; any other type becomes a <div class="TYPE">
     ("#+begin_foo\nhello\n#+end_foo"
      "<div class=\"foo\">\n<p>hello</p>\n\n</div>")
     ;; user attributes replace the type class
     ("#+attr__: [bar]\n#+begin_foo\nhello\n#+end_foo"
      "<div class=\"bar\">\n<p>hello</p>\n\n</div>")
     ;; an empty #+attr__: gives a plain <div>
     ("#+attr__:\n#+begin_div\nhello\n#+end_div"
      "<div>\n<p>hello</p>\n\n</div>"))
   nil '(:html-prefer-user-labels t)))

(ert-deftest t--special-block-validate-registry ()
  "Tests for `org-w3ctr--special-block-validate-registry'."
  ($n (t--special-block-validate-registry nil))
  ($n (t--special-block-validate-registry
       '(("a-b") ("c-d" :src "x.js"))))
  ;; Not a cons, or a car that is not a string: signals.
  ($e!l (t--special-block-validate-registry '("a-b"))
        '(org-w3ctr-error
          "Malformed custom element registry entry: \"a-b\""))
  ($e!l (t--special-block-validate-registry '(42))
        '(org-w3ctr-error "Malformed custom element registry entry: 42"))
  ($e!l (t--special-block-validate-registry '((42 :src "x.js")))
        '(org-w3ctr-error
          "Malformed custom element registry entry: (42 :src \"x.js\")")))

(ert-deftest t--special-block-spec ()
  "Tests for `org-w3ctr--special-block-spec'."
  ($e!l (org-export-string-as
         "#+begin_x-old\nhi\n#+end_x-old" 'w3ctr t
         '(:html-special-block-custom-elements ("x-old")))
        '(org-w3ctr-error
          "Malformed custom element registry entry: \"x-old\""))
  ;; non-cons entry: signals
  ($e!l (org-export-string-as
         "#+begin_x-bad\nhi\n#+end_x-bad" 'w3ctr t
         '(:html-special-block-custom-elements (42)))
        '(org-w3ctr-error "Malformed custom element registry entry: 42"))
  ;; non-string name: signals
  ($e!l (org-export-string-as
         "#+begin_x-bad\nhi\n#+end_x-bad" 'w3ctr t
         '(:html-special-block-custom-elements ((42 :src "x.js"))))
        '(org-w3ctr-error
          "Malformed custom element registry entry: (42 :src \"x.js\")"))
  ;; malformed entry with no special block used: the <head> scan still
  ;; validates the registry, so the clean error is raised (not a
  ;; wrong-type-argument).
  ($e!l (org-export-string-as "hello" 'w3ctr nil
                              '(:html-special-block-custom-elements (42)))
        '(org-w3ctr-error "Malformed custom element registry entry: 42")))

(ert-deftest t--custom-element-name-p ()
  "Tests for `org-w3ctr--custom-element-name-p'."
  (dolist (s '("my-card" "x-" "a-b-c" "a1-b.c_d"))
    ($s (t--custom-element-name-p s)))
  (let ((case-fold-search t))
    (dolist (s '("card" "-card" "1-card" "My-card" "MY-CARD" "my card" ""
                 "é-card"))
      ($n (t--custom-element-name-p s)))))

(ert-deftest t--special-block-custom-template ()
  "Tests for the :template key of custom elements."
  (let ((tpl "<template shadowrootmode=\"open\"><slot></slot></template>"))
    (t-check-element-values
     #'t-special-block
     `(("#+begin_x-tpl\nhello\n#+end_x-tpl"
        ,($c "<x-tpl>\n" tpl "\n<p>hello</p>\n</x-tpl>"))
       ;; empty block: the template alone
       ("#+begin_x-tpl\n#+end_x-tpl"
        ,($c "<x-tpl>\n" tpl "\n</x-tpl>"))
       ;; entries without :template are unchanged
       ("#+begin_x-plain\nhello\n#+end_x-plain"
        "<x-plain>\n<p>hello</p>\n</x-plain>"))
     nil `(:html-special-block-custom-elements
           (("x-tpl" :template ,tpl :src "x.js") ("x-plain")))))
  ;; :template and :src coexist: template in the body, script in <head>
  (let* ((tpl "<template shadowrootmode=\"closed\"></template>")
         (out (org-export-string-as
               "#+begin_x-tpl\nhi\n#+end_x-tpl" 'w3ctr nil
               `(:html-special-block-custom-elements
                 (("x-tpl" :template ,tpl :src "x.js"))))))
    ($s (< (string-search "src=\"x.js\"" out)
           (string-search "</head>" out)
           (string-search "shadowrootmode=\"closed\"" out)))))

(ert-deftest t--special-block-custom ()
  "Tests for `org-w3ctr--special-block-custom'."
  (let* ((mk (lambda (type)
               (t-get-element (format "#+begin_%s\nhi\n#+end_%s" type type)
                              'special-block)))
         (tpl "<template shadowrootmode=\"open\"></template>")
         (info '(:html-prefer-user-labels t)))
    ;; no :template: the contents follow the opening tag directly
    ($l (t--special-block-custom (funcall mk "my-card") "<p>hi</p>\n"
                                 info '(nil . "my-card"))
        "<my-card>\n<p>hi</p>\n</my-card>")
    ;; a :template is normalized and inserted before the contents
    ($l (t--special-block-custom
         (funcall mk "my-card") "<p>hi</p>\n" info
         `((:template ,tpl) . "my-card"))
        ($c "<my-card>\n" tpl "\n<p>hi</p>\n</my-card>"))
    ;; a type that is not a valid custom element name signals
    ($e!l (t--special-block-custom
           (funcall mk "card") "x" info '(nil . "card"))
          '(org-w3ctr-error "Invalid custom element name: card"))))

(ert-deftest t-special-block-custom-elements ()
  "Tests for `:html-special-block-custom-elements'."
  (t-check-element-values
   #'t-special-block
   '(;; listed type: the custom element itself, no class added
     ("#+begin_my-card\nhello\n#+end_my-card"
      "<my-card>\n<p>hello</p>\n</my-card>")
     ("#+begin_my-card\n#+end_my-card" "<my-card>\n</my-card>")
     ;; attributes as for other elements
     ("#+name: nm\n#+attr__: [x]\n#+begin_my-card\nhello\n#+end_my-card"
      "<my-card id=\"nm\" class=\"x\">\n<p>hello</p>\n</my-card>")
     ;; unlisted types are unaffected
     ("#+begin_foo-bar\nhello\n#+end_foo-bar"
      "<div class=\"foo-bar\">\n<p>hello</p>\n\n</div>"))
   nil (list :html-prefer-user-labels t
             :html-special-block-custom-elements '(("my-card"))))
  ;; empty plist is equivalent to just registering the name
  (t-check-element-values
   #'t-special-block
   '(("#+begin_my-card\nhello\n#+end_my-card"
      "<my-card>\n<p>hello</p>\n</my-card>"))
   nil (list :html-prefer-user-labels t
             :html-special-block-custom-elements '(("my-card" . nil))))
  ;; a listed type that is not a valid custom element name
  ($e!l (org-export-string-as
         "#+begin_card\nx\n#+end_card" 'w3ctr t
         (list :html-special-block-custom-elements '(("card"))))
        '(org-w3ctr-error "Invalid custom element name: card"))
  ($e!l (org-export-string-as
         "#+begin_My-Card\nx\n#+end_My-Card" 'w3ctr t
         (list :html-special-block-custom-elements '(("My-Card"))))
        '(org-w3ctr-error "Invalid custom element name: My-Card")))

(ert-deftest t-special-block ()
  "Tests for `org-w3ctr-special-block'."
  ;; The extra blank line before </div> and the case-sensitive type
  ;; match are ox-html's behavior, kept for compatibility.
  (t-check-element-values
   #'t-special-block
   '(;; listed type: the element itself
     ("#+begin_section\nhello\n#+end_section"
      "<section>\n<p>hello</p>\n</section>")
     ;; other type: a <div> carrying the type as class
     ("#+begin_foo\nhello\n#+end_foo"
      "<div class=\"foo\">\n<p>hello</p>\n\n</div>")
     ("#+begin_foo\n#+end_foo" "<div class=\"foo\">\n\n</div>")
     ;; case-sensitive, as in ox-html
     ("#+begin_SECTION\nhello\n#+end_SECTION"
      "<div class=\"SECTION\">\n<p>hello</p>\n\n</div>")
     ;; user attributes (#+attr_html: or #+attr__:): full control,
     ;; no class added, as for center blocks
     ("#+attr_html: :class bar\n#+begin_foo\nhello\n#+end_foo"
      "<div class=\"bar\">\n<p>hello</p>\n\n</div>")
     ("#+attr_html: :class bar\n#+begin_section\nhello\n#+end_section"
      "<section class=\"bar\">\n<p>hello</p>\n</section>")
     ("#+attr__: [bar]\n#+begin_foo\nhello\n#+end_foo"
      "<div class=\"bar\">\n<p>hello</p>\n\n</div>")
     ;; an empty #+attr__: gives a plain <div>
     ("#+attr__:\n#+begin_div\nhello\n#+end_div"
      "<div>\n<p>hello</p>\n\n</div>")
     ("#+attr__: [bar]\n#+begin_section\nhello\n#+end_section"
      "<section class=\"bar\">\n<p>hello</p>\n</section>")
     ;; named blocks get an id, unless one is given explicitly
     ("#+name: nm\n#+begin_foo\nhello\n#+end_foo"
      "<div class=\"foo\" id=\"nm\">\n<p>hello</p>\n\n</div>")
     ("#+name: nm\n#+attr_html: :id own\n#+begin_foo\nhello\n#+end_foo"
      "<div id=\"own\">\n<p>hello</p>\n\n</div>")
     ("#+name: nm\n#+attr__: [bar]\n#+begin_aside\nhello\n#+end_aside"
      "<aside id=\"nm\" class=\"bar\">\n<p>hello</p>\n</aside>"))
   nil '(:html-prefer-user-labels t)))

(ert-deftest t--special-block-used-elements ()
  "Tests for `org-w3ctr--special-block-used-elements'."
  ;; Entries come back in registry order, without duplicates; the
  ;; registry is validated even when nothing matches.
  (let* ((registry '(("x-one" :src "one.js")
                     ("x-two" :tag "t")
                     ("x-unused")))
         (tree (with-temp-buffer
                 (insert ($c "#+begin_x-two\nb\n#+end_x-two\n"
                             "#+begin_x-one\na\n#+end_x-one\n"
                             "#+begin_x-two\nc\n#+end_x-two\n"))
                 (org-mode)
                 (org-element-parse-buffer)))
         (info (list :parse-tree tree
                     :html-special-block-custom-elements registry)))
    ($l (t--special-block-used-elements info)
        '(("x-one" :src "one.js") ("x-two" :tag "t"))))
  ;; no registry: nothing to report
  ($n (t--special-block-used-elements
       (list :html-special-block-custom-elements nil)))
  ;; a malformed registry signals even with no matching block
  ($e!l (t--special-block-used-elements
         (list :html-special-block-custom-elements '(42)))
        '(org-w3ctr-error "Malformed custom element registry entry: 42")))

(ert-deftest t--special-block-head-entry ()
  "Tests for `org-w3ctr--special-block-head-entry'."
  ;; The result is (MARKUP . SEEN); a :src already in SEEN is skipped.
  (let ((r (t--special-block-head-entry '("a-b" :src "a.js") nil)))
    ($l (car r) "<script type=\"module\" src=\"a.js\"></script>\n")
    ($l (cdr r) '("a.js")))
  (let ((r (t--special-block-head-entry '("a-b" :script "x()") nil)))
    ($l (car r) "<script type=\"module\">\nx()\n</script>\n")
    ($n (cdr r)))
  (let ((r (t--special-block-head-entry '("a-b" :src "a.js") '("a.js"))))
    ($l (car r) "")
    ($l (cdr r) '("a.js")))
  (let ((r (t--special-block-head-entry '("a-b") nil)))
    ($l (car r) "")
    ($n (cdr r))))

(ert-deftest t-special-block-head-default-function ()
  "Tests for `org-w3ctr-special-block-head-default-function'."
  ($l (t-special-block-head-default-function
       '(("a-b" :src "js/a.js")) nil)
      "<script type=\"module\" src=\"js/a.js\"></script>\n")
  ($l (t-special-block-head-default-function
       '(("a-b" :script "x()")) nil)
      "<script type=\"module\">\nx()\n</script>\n")
  ;; :src is escaped for the attribute
  ($l (t-special-block-head-default-function
       '(("a-b" :src "a.js?x=1&y=\"2\"")) nil)
      ($c "<script type=\"module\" src=\"a.js?x=1&amp;y=&quot;2&quot;\">"
          "</script>\n"))
  ;; a shared :src is emitted once, at its first entry
  ($l (t-special-block-head-default-function
       '(("a-b" :src "all.js") ("c-d" :src "c.js") ("e-f" :src "all.js"))
       nil)
      ($c "<script type=\"module\" src=\"all.js\"></script>\n"
          "<script type=\"module\" src=\"c.js\"></script>\n"))
  ;; deduplication does not touch :script
  ($l (t-special-block-head-default-function
       '(("a-b" :src "all.js" :script "a()")
         ("c-d" :src "all.js" :script "c()"))
       nil)
      ($c "<script type=\"module\" src=\"all.js\"></script>\n"
          "<script type=\"module\">\na()\n</script>\n"
          "<script type=\"module\">\nc()\n</script>\n"))
  ;; entries without known keys produce nothing
  ($n (t-special-block-head-default-function '(("a-b")) nil))
  ($n (t-special-block-head-default-function
       '(("a-b" :template "<template></template>")) nil)))

(ert-deftest t--special-block-head ()
  "Tests for `org-w3ctr--special-block-head'."
  (let ((registry '(("x-one" :src "one.js")
                    ("x-two" :src "two.js")
                    ("x-unused" :src "unused.js")))
        (doc ($c "#+begin_x-two\nb\n#+end_x-two\n\n"
                 "#+begin_x-one\na\n#+end_x-one\n\n"
                 "#+begin_x-two\nc\n#+end_x-two\n")))
    (cl-flet ((head (str &optional plist)
                (let ((out (org-export-string-as
                            str 'w3ctr nil
                            (append plist
                                    (list :html-special-block-custom-elements
                                          registry)))))
                  (substring out 0 (string-search "</head>" out)))))
      ;; used entries, registry order, no duplicates, unused skipped
      (let ((h (head doc)))
        ($l (t--find-all "src=\"[a-z]+\\.js\"" h)
            '("src=\"one.js\"" "src=\"two.js\"")))
      ;; blocks in a :noexport: subtree do not count
      ($n (string-search
           "one.js"
           (head "* a :noexport:\n#+begin_x-one\na\n#+end_x-one\n")))
      ;; no registered element used: nothing added
      ($n (string-search "<script type=\"module\"" (head "hello")))
      ;; the hook receives the used entries and its result is inserted
      (let ((h (head doc
                     (list :html-special-block-head-function
                           (lambda (specs _info)
                             (format "<!-- %s -->"
                                     (mapconcat #'car specs " ")))))))
        ($s (string-search "<!-- x-one x-two -->\n" h)))
      ;; after the user's own <head> contents
      (let ((h (head doc '(:html-head "<!-- user -->"))))
        ($s (< (string-search "<!-- user -->" h)
               (string-search "one.js" h))))
      ;; a nil hook falls back to the default head function
      (let ((h (head doc (list :html-special-block-head-function nil))))
        ($l (t--find-all "src=\"[a-z]+\\.js\"" h)
            '("src=\"one.js\"" "src=\"two.js\""))))
    ;; body-only exports have no <head>
    ($n (string-search
         "one.js"
         (org-export-string-as
          doc 'w3ctr t
          (list :html-special-block-custom-elements registry))))))

;;;; Table

(ert-deftest t--table-column-cookie ()
  "Tests for `org-w3ctr--table-column-cookie'."
  (cl-flet ((cookies (doc n)
              (let ((table (t-get-element doc 'table)))
                (mapcar (lambda (column)
                          (t--table-column-cookie table column nil))
                        (number-sequence 0 (1- n))))))
    ($l (cookies "| <l> | <c> | <r> |\n| a | b | c |" 3)
        '(left center right))
    ;; a width in the cookie is ignored; a width-only one is no alignment
    ($l (cookies "| <l5> | <r10> | <c3> |\n| a | b | c |" 3)
        '(left right center))
    ($l (cookies "| <5> | <10> |\n| a | b |" 2)
        '(nil nil))
    ;; the last special row wins
    ($l (cookies "| <l> | <c> |\n| <r> | <l> |\n| a | b |" 2)
        '(right left))
    ($l (cookies "| a | b |" 2) '(nil nil))
    ;; a column past the first row's width is nil
    ($l (cookies "| <l> |\n| a |" 3) '(left nil nil))))

(ert-deftest t--table-cell-align ()
  "Tests for `org-w3ctr--table-cell-align'."
  (cl-flet ((align (doc)
              (mapcar (lambda (cell)
                        (t--table-cell-align
                         cell (list :html-table-align-cache nil)))
                      (t-get-parsed-elements doc 'table-cell))))
    ;; every cell, cookies included, reports its column's alignment
    ($l (align "| <l> | <r> |\n| a | b |") '(left right left right))
    ($l (align "| a | b |") '(nil nil)))
  ;; the per-table cache is installed in INFO
  (let* ((cell (t-get-element "| <l> |\n| a |" 'table-cell))
         (info (list :html-table-align-cache nil)))
    ($q (t--table-cell-align cell info) 'left)
    ($s (hash-table-p (plist-get info :html-table-align-cache)))))

(ert-deftest t--table-cell-align-memo ()
  "Tests for the per-table memoization of `org-w3ctr--table-cell-align'.
A second lookup must come from the cache, `t--table-column-cookie' is
consulted once per column, and a cookie-less column is remembered as
the `none' marker."
  (let* ((cells (t-get-parsed-elements "| <l> | <r> |\n| a | b |\n| c | d |"
                                       'table-cell))
         (info (list :html-table-align-cache nil))
         (orig (symbol-function 't--table-column-cookie))
         (calls 0))
    ;; cells in order: <l>, <r>, a, b, c, d
    (cl-letf (((symbol-function 't--table-column-cookie)
               (lambda (table column info)
                 (setq calls (1+ calls))
                 (funcall orig table column info))))
      ;; the first call computes and stores; the second hits the cache
      ($q (t--table-cell-align (nth 2 cells) info) 'left)
      ($q (t--table-cell-align (nth 2 cells) info) 'left)
      ($q (t--table-cell-align (nth 3 cells) info) 'right)
      ($q (t--table-cell-align (nth 3 cells) info) 'right)
      ($l calls 2)
      ;; a later row's cell shares its column's cached value
      ($q (t--table-cell-align (nth 4 cells) info) 'left)
      ($q (t--table-cell-align (nth 5 cells) info) 'right)
      ($l calls 2)))
  ;; a cookie-less column is remembered as `none' and still returns nil
  (let* ((cells (t-get-parsed-elements "| a | b |\n| c | d |" 'table-cell))
         (info (list :html-table-align-cache nil))
         (orig (symbol-function 't--table-column-cookie))
         (calls 0))
    (cl-letf (((symbol-function 't--table-column-cookie)
               (lambda (table column info)
                 (setq calls (1+ calls))
                 (funcall orig table column info))))
      ($n (t--table-cell-align (car cells) info))
      ($n (t--table-cell-align (car cells) info))
      ($l calls 1)
      (let* ((table (org-export-get-parent-table (car cells)))
             (vec (gethash table (plist-get info :html-table-align-cache))))
        ($q (aref vec 0) 'none))))
  ;; a ragged row reaches a column past the first row's width; the
  ;; cache vector is extended for it
  (let* ((cells (t-get-parsed-elements "| a | b |\n| c | d | e |" 'table-cell))
         (info (list :html-table-align-cache nil))
         (table (org-export-get-parent-table (car cells))))
    ($n (t--table-cell-align (car cells) info))
    ($l (length (gethash table (plist-get info :html-table-align-cache))) 2)
    ($n (t--table-cell-align (nth 4 cells) info))
    ($l (length (gethash table (plist-get info :html-table-align-cache))) 3)))

(ert-deftest t--table-cell-attrs ()
  "Tests for `org-w3ctr--table-cell-attrs'."
  (cl-flet ((attrs (doc)
              (mapcar (lambda (cell)
                        (t--table-cell-attrs
                         cell (list :html-table-align-cache nil)))
                      (t-get-parsed-elements doc 'table-cell))))
    ;; every cell carries its column's cookie alignment
    ($l (attrs "| <l> | <r> |\n| a | b |")
        '(" style=\"text-align:left\"" " style=\"text-align:right\""
          " style=\"text-align:left\"" " style=\"text-align:right\""))
    ;; no cookie: the CSS decides, so no attribute
    ($l (attrs "| a | b |") '("" ""))))

(ert-deftest t--table-first-row-data-cells ()
  "Tests for `org-w3ctr--table-first-row-data-cells'."
  (cl-flet ((cells (doc)
              (mapcar #'org-element-contents
                      (t--table-first-row-data-cells
                       (t-get-element doc 'table) nil))))
    ($l (cells "| a | b |\n|---+---|\n| 1 | 2 |") '(("a") ("b")))
    ;; a leading rule row is skipped
    ($l (cells "|---+---|\n| a | b |") '(("a") ("b")))
    ;; a special column is dropped
    ($l (cells "| ! | a | b |\n|   | 1 | 2 |") '(("a") ("b")))))

(ert-deftest t--table-column-specs ()
  "Tests for `org-w3ctr--table-column-specs'."
  (cl-flet ((specs (doc)
              (t--table-column-specs (t-get-element doc 'table) nil)))
    ;; a table without markers is one group spanning every column
    ($l (specs "| a | b |") "\n<colgroup span=\"2\">")
    ;; a `/' row marks the group boundaries
    ($l (specs "| / | < | > | < | > |\n|   | a | b | c | d |")
        "\n<colgroup span=\"2\">\n<colgroup span=\"2\">")))

(ert-deftest t--table-caption ()
  "Tests for `org-w3ctr--table-caption'."
  (t-check-element-values
   #'t--table-caption
   '(("#+caption: Test caption\n| a |" "<caption>Test caption</caption>")
     ("| a |" "")
     ;; markup in the caption is exported, not escaped
     ("#+caption: *Bold* and /italic/\n| a |"
      "<caption><b>Bold</b> and <i>italic</i></caption>"))))

(ert-deftest t-table-cell ()
  "Tests for `org-w3ctr-table-cell'."
  (cl-flet ((cell (doc n contents &optional info)
              (t-table-cell (nth n (t-get-parsed-elements doc 'table-cell))
                            contents info)))
    ;; a header cell carries scope="col"
    ($l (cell "| Name |\n|------|\n| foo |" 0 "Name")
        "\n<th scope=\"col\">Name</th>")
    ;; a data cell
    ($l (cell "| a |" 0 "a") "\n<td>a</td>")
    ;; an alignment cookie adds the inline style, header or data
    ($l (cell "| <l> |\n| a |" 1 "a")
        "\n<td style=\"text-align:left\">a</td>")
    ($l (cell "| <l> | <r> |\n|------+-------|\n| foo | bar |" 0 "Name")
        "\n<th scope=\"col\" style=\"text-align:left\">Name</th>")
    ;; an empty cell becomes &nbsp;; nil is the bare-cell case
    ($l (cell "| |" 0 "") "\n<td>&#xa0;</td>")
    ($l (cell "| |" 0 nil) "\n<td>&#xa0;</td>")
    ;; the first column can carry row headers
    ($l (cell "| Name | Value |\n| foo | bar |" 2 "foo"
              '(:html-table-use-header-tags-for-first-column t))
        "\n<th scope=\"row\">foo</th>")
    ($l (cell "| <l> | <r> |\n|------+-------|\n| foo | bar |" 2 "foo"
              '(:html-table-use-header-tags-for-first-column t))
        "\n<th scope=\"row\" style=\"text-align:left\">foo</th>")))

(ert-deftest t-table-row ()
  "Tests for `org-w3ctr-table-row'."
  (cl-flet ((row (doc n contents)
              (t-table-row (nth n (t-get-parsed-elements doc 'table-row))
                           contents nil)))
    ;; a header row opens <thead>
    ($l (row "| a | b |\n|---+---|\n| 1 | 2 |" 0
             "<th scope=\"col\">a</th><th scope=\"col\">b</th>")
        ($c "<thead>\n<tr>"
            "<th scope=\"col\">a</th><th scope=\"col\">b</th>"
            "\n</tr>\n</thead>"))
    ;; a body row carries <tbody>
    ($l (row "| a | b |\n|---+---|\n| 1 | 2 |" 2 "<td>1</td><td>2</td>")
        "<tbody>\n<tr><td>1</td><td>2</td>\n</tr>\n</tbody>")
    ;; multi-row body: only the first row opens its group, only the last
    ;; one closes it
    ($l (row "| a |\n|---|\n| 1 |\n| 2 |" 2 "<td>1</td>")
        "<tbody>\n<tr><td>1</td>\n</tr>")
    ($l (row "| a |\n|---|\n| 1 |\n| 2 |" 3 "<td>2</td>")
        "\n<tr><td>2</td>\n</tr>\n</tbody>")
    ;; without a header the body still opens and closes
    ($l (row "| a | b |" 0 "<td>a</td><td>b</td>")
        "<tbody>\n<tr><td>a</td><td>b</td>\n</tr>\n</tbody>")))

(ert-deftest t-table ()
  "Tests for `org-w3ctr-table'."
  (t-check-element-values
   #'t-table
   `(;; no caption, one column group
     ("| a | b |"
      ,($c "<table>\n\n\n<colgroup span=\"2\">"
           "\n<tbody>\n<tr>\n<td>a</td>\n<td>b</td>\n</tr>\n</tbody>"
           "\n</table>"))
     ;; a rule makes the first row a header
     ("| a | b |\n|---+---|\n| 1 | 2 |"
      ,($c "<table>\n\n\n<colgroup span=\"2\">"
           "\n<thead>\n<tr>"
           "\n<th scope=\"col\">a</th>\n<th scope=\"col\">b</th>"
           "\n</tr>\n</thead>"
           "\n<tbody>\n<tr>\n<td>1</td>\n<td>2</td>\n</tr>\n</tbody>"
           "\n</table>"))
     ;; a name becomes the id; the caption comes first
     ("#+name: t\n#+caption: Cap\n| a |"
      ,($c "<table id=\"t\">\n<caption>Cap</caption>"
           "\n\n<colgroup span=\"1\">"
           "\n<tbody>\n<tr>\n<td>a</td>\n</tr>\n</tbody>"
           "\n</table>"))
     ;; alignment cookies style the data cells
     ("| <l> | <r> |\n| a | b |"
      ,($c "<table>\n\n\n<colgroup span=\"2\">"
           "\n<tbody>\n<tr>"
           "\n<td style=\"text-align:left\">a</td>"
           "\n<td style=\"text-align:right\">b</td>"
           "\n</tr>\n</tbody>\n</table>"))
     ;; attr_html is honoured
     ("#+attr_html: :class data\n| a |"
      ,($c "<table class=\"data\">\n\n\n<colgroup span=\"1\">"
           "\n<tbody>\n<tr>\n<td>a</td>\n</tr>\n</tbody>"
           "\n</table>"))
     ;; a `/' row makes two column groups
     ("| / | < | > | < | > |\n|   | a | b | c | d |"
      ,($c "<table>\n\n\n<colgroup span=\"2\">\n<colgroup span=\"2\">"
           "\n<tbody>\n<tr>"
           "\n<td>a</td>\n<td>b</td>\n<td>c</td>\n<td>d</td>"
           "\n</tr>\n</tbody>\n</table>")))
   nil '(:html-prefer-user-labels t)))

;;; Lesser elements

;;;; Example Block

(ert-deftest t-example-block ()
  "Tests for `org-w3ctr-example-block'."
  (t-check-element-values
   #'t-example-block
   '(("#+name: t\n#+begin_example\n#+end_example"
      "<div id=\"t\" class=\"example\">\n<pre>\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n1\n2\n3\n#+end_example"
      "<div id=\"t\" class=\"example\">\n<pre>\n1\n2\n3\n</pre>\n</div>")
     ("#+name: t\n#+attr__: [ex]\n#+BEGIN_EXAMPLE\n123\n#+END_EXAMPLE"
      "<div id=\"t\" class=\"ex\">\n<pre>\n123\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n 1\n 2\n 3\n#+end_example"
      "<div id=\"t\" class=\"example\">\n<pre>\n1\n2\n3\n</pre>\n</div>")
     ("#+name:t\n#+begin_example\n\n\n\n#+end_example"
      "<div id=\"t\" class=\"example\">\n<pre>\n\n\n\n</pre>\n</div>")
     ;; `:attr_html' is a user attribute too: no class="example"
     ("#+attr_html: :class foo\n#+begin_example\n1\n#+end_example"
      "<div class=\"foo\">\n<pre>\n1\n</pre>\n</div>")
     ;; an empty `#+attr__:' still counts as user control
     ("#+attr__:\n#+begin_example\n1\n#+end_example"
      "<div>\n<pre>\n1\n</pre>\n</div>"))
   nil '(:html-prefer-user-labels t)))

;;;; Export Block

(ert-deftest t-export-block ()
  "Tests for `org-w3ctr-export-block'."
  (t-check-element-values
   #'t-export-block
   '(;; HTML
     ("#+begin_export html\nanythinghere\n#+end_export" "anythinghere\n")
     ("#+begin_export html\n#+end_export" "")
     ("#+begin_export html\n\n#+end_export" "\n")
     ("#+begin_export html\n\n\n\n#+end_export" "\n\n\n")
     ("#+begin_export html\n\n\n\n\n\n\n#+end_export" "\n\n\n\n\n\n")
     ;; CSS
     ("#+begin_export css\np {color: red;}\n#+end_export"
      "<style>\np {color: red;}\n</style>")
     ("#+begin_export css\n#+end_export" "<style>\n</style>")
     ("#+begin_export CSS\n.test {margin: auto;}\n#+end_export"
      "<style>\n.test {margin: auto;}\n</style>")
     ;; JS
     ("#+begin_export js\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export js\n#+end_export" "<script>\n</script>")
     ("#+begin_export javascript\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export javascript\n#+end_export" "<script>\n</script>")
     ;; Elisp
     ("#+begin_export emacs-lisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export emacs-lisp\n#+end_export" "")
     ("#+begin_export elisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export elisp\n#+end_export" "")
     ;; Lisp data
     ("#+begin_export lisp-data\n (p() \"123\")\n#+end_export"
      "<p>123</p>")
     ("#+begin_export lisp-data\n (br)\n#+end_export" "<br>")
     ("#+BEGIN_EXPORT lisp-data\n (br)\n#+END_EXPORT" "<br>")
     ;; Unsupported type
     ("#+begin_export wtf\n no exported\n#+end_export" "")
     ("#+begin_export\n not exported\n#+end_export" ""))
   t)
  ;; Error handling: malformed Lisp signals t-error with line number.
  ($e!l (org-export-string-as
         "#+begin_export emacs-lisp\n(broken\n#+end_export\n" 'w3ctr t)
        (list 'org-w3ctr-error
              ($c "EMACS-LISP block at line 1: "
                  "End of file during parsing")))
  ($e!l (org-export-string-as
         ($c "text\n#+begin_export lisp-data\n(broken\n"
             "#+end_export\n") 'w3ctr t)
        (list 'org-w3ctr-error
              ($c "LISP-DATA block at line 2: "
                  "End of file during parsing")))
  ;; an eval failure is reported like a read failure
  ($e!l (org-export-string-as
         "#+begin_export emacs-lisp\n(error \"boom\")\n#+end_export\n"
         'w3ctr t)
        '(org-w3ctr-error "EMACS-LISP block at line 1: boom"))
  ;; a nested org-w3ctr-error keeps its clean message -- no type name
  ;; and quotes, which error-message-string would re-render it with
  ($e!l (org-export-string-as
         ($c "#+begin_export emacs-lisp\n"
             "(signal 'org-w3ctr-error (list \"clean\"))\n#+end_export\n")
         'w3ctr t)
        '(org-w3ctr-error "EMACS-LISP block at line 1: clean")))

(ert-deftest t-export-snippet ()
  "Tests for `org-w3ctr-export-snippet'."
  (t-check-element-values
   #'t-export-snippet
   '(("@@h:<span>123</span>@@" "<span>123</span>")
     ("@@h:@@" "")
     ("@@html:<span>123</span>@@" "<span>123</span>")
     ("@@html:@@" "")
     ("@@e:@@" "")
     ("@@e:(+ 1 2)@@" "3")
     ("@@e:'(1 2 3)@@" "")
     ("@@d:@@" "")
     ("@@d:(span() \"nothing\")")
     ("@@d:(wbr)@@" "<wbr>")
     ("@@d:(wbr())@@" "<wbr>")
     ;; Otherwise
     ("@@wtf::hello@@" ""))
   t)
  ;; Error handling: malformed Lisp signals t-error.
  ($e!l (org-export-string-as "@@e:(broken@@" 'w3ctr t)
        '(org-w3ctr-error "@@e snippet at line 1: End of file during parsing"))
  ($e!l (org-export-string-as "@@d:(broken@@" 'w3ctr t)
        '(org-w3ctr-error "@@d snippet at line 1: End of file during parsing")))

;;;; Fixed Width

(ert-deftest t-fixed-width ()
  "Tests for `org-w3ctr-fixed-width'."
  (t-check-element-values
   #'t-fixed-width
   '((":           " "<pre></pre>")
     (": 1\n" "<pre>\n1\n</pre>")
     (": 1\n: 2\n" "<pre>\n1\n2\n</pre>")
     (":  1\n:  2\n:   3\n" "<pre>\n1\n2\n 3\n</pre>")
     (": 1\n: \n" "<pre>\n1\n\n</pre>")
     ("#+name: t\n#+attr__: [test]\n: 1\n : 2\n: 3"
      "<pre id=\"t\" class=\"test\">\n1\n2\n3\n</pre>")
     ;; `:attr_html' goes through the same attribute builder
     ("#+attr_html: :class foo\n: 1" "<pre class=\"foo\">\n1\n</pre>")
     (":\n:\n:\n:\n" "<pre>\n\n\n</pre>"))
   nil '(:html-prefer-user-labels t)))

;;;; Horizontal Rule

(ert-deftest t-horizontal-rule ()
  "Tests for `org-w3ctr-horizontal-rule'."
  (t-check-element-values
   #'t-horizontal-rule
   '(("-")
     ("--")
     ("---")
     ("----")
     ("-----" "<hr>")
     ("------" "<hr>")
     ("-------" "<hr>")
     ("--------" "<hr>")
     ("---------" "<hr>")
     ("----------" "<hr>")
     ("-------------------------------" "<hr>")
     ("#+attr__: [thick]\n-----" "<hr class=\"thick\">")
     ("#+attr_html: :class foo\n-----" "<hr class=\"foo\">")
     ("#+attr__: (id foo)\n-----" "<hr id=\"foo\">"))))

;;;; Keyword

(ert-deftest t-keyword ()
  "Tests for `org-w3ctr-keyword'."
  (t-check-element-values
   #'t-keyword
   '(;; H
     ("#+h: " "")
     ("#+h: <p>123</p>" "<p>123</p>")
     ("#+h: <br>\n#+h: <br>" "<br>" "<br>")
     ;; HTML
     ("#+html: " "")
     ("#+html: <p>123</p>" "<p>123</p>")
     ("#+html: <a href=\"https://example.com\">Example</a>"
      "<a href=\"https://example.com\">Example</a>")
     ;; E
     ("#+e: " "")
     ("#+e: (concat \"1\" nil \"2\")" "12")
     ("#+e: (string-join '(\"a\" \"b\") \",\")" "a,b")
     ;; D
     ("#+d: " "")
     ("#+d: (br)" "<br>")
     ("#+d: (p((data-x \"1\"))123)" "<p data-x=\"1\">123</p>")
     ;; Otherwise
     ("#+hello: world" nil))
   t)
  ;; Error handling: malformed Lisp signals t-error.
  ($e!l (org-export-string-as "#+e: (broken" 'w3ctr t)
        '(org-w3ctr-error "#+E keyword at line 1: End of file during parsing"))
  ($e!l (org-export-string-as "text\n#+d: (broken" 'w3ctr t)
        '(org-w3ctr-error "#+D keyword at line 2: End of file during parsing"))
  ;; TOC goes through org-w3ctr--keyword-toc
  (cl-letf (((symbol-function 't--list-of-tables) (lambda (_i) "TABLES")))
    ($l (t-keyword (t-get-element "#+TOC: tables" 'keyword) nil nil)
        "TABLES")))

;;;; LaTeX

(ert-deftest t-math-custom-default-render-function ()
  "Tests for `org-w3ctr-math-custom-default-render-function'."
  ;; the default renderer is the identity
  ($l (t-math-custom-default-render-function "$x$" nil) "$x$"))

(ert-deftest t--normalize-latex ()
  "Tests for `org-w3ctr--normalize-latex'."
  ($l (t--normalize-latex "$x$") "\\(x\\)")
  ($l (t--normalize-latex "$$x$$") "\\[x\\]")
  ($l (t--normalize-latex "\\(x\\)") "\\(x\\)")
  ($l (t--normalize-latex "\\[x\\]") "\\[x\\]")
  ($l (t--normalize-latex "\\begin{equation}\nx=1\n\\end{equation}")
      "\\begin{equation}\nx=1\n\\end{equation}"))

(ert-deftest t--format-latex ()
  "Tests for `org-w3ctr--format-latex'."
  ;; nil and verbatim return the fragment unchanged
  ($l (t--format-latex "$x$" nil nil) "$x$")
  ($l (t--format-latex "$x$" 'verbatim nil) "$x$")
  ;; mathjax normalizes the delimiters for client-side MathJax
  ($l (t--format-latex "$x$" 'mathjax nil) "\\(x\\)")
  ;; custom calls the render function on the fragment and INFO
  (let ((info '(:html-math-custom-render-function
                (lambda (f _i) (format "<M>%s</M>" f)))))
    ($l (t--format-latex "$x$" 'custom info) "<M>$x$</M>"))
  ;; the RPC modes call the jstools MathJax helpers with the
  ;; normalized fragment
  (cl-flet ((rpc (mode)
              (let (got)
                (cl-letf (((symbol-function 't--jcall)
                           (lambda (client method params)
                             (setq got (list client method params))
                             "<M>")))
                  ($l (t--format-latex "$x$" mode nil) "<M>"))
                got)))
    ($l (rpc 'mathml-by-mathjax)
        (list t--jstools 'tex2mml (list :fragment "\\(x\\)")))
    ($l (rpc 'svg-by-mathjax)
        (list t--jstools 'tex2svg (list :fragment "\\(x\\)"))))
  ;; any other mode signals org-w3ctr-error
  ($e!l (t--format-latex "$x$" 'bogus nil)
        '(org-w3ctr-error "Unknown LaTeX mode: bogus")))

(ert-deftest t-latex-fragment ()
  "Tests for `org-w3ctr-latex-fragment'."
  ;; NB: for `verbatim', Org expands the fragment itself (ox.el) and
  ;; never calls the back-end transcoder, so only `mathjax' is tested.
  (t-check-element-values
   #'t-latex-fragment
   '(("$x^2$" "\\(x^2\\)"))
   nil '(:with-latex mathjax))
  ;; under `tex:nil' ox.el prunes the fragment: no call at all
  (t-check-element-values
   #'t-latex-fragment
   '(("$x^2$"))
   nil '(:with-latex nil)))

(ert-deftest t-latex-environment ()
  "Tests for `org-w3ctr-latex-environment'."
  (t-check-element-values
   #'t-latex-environment
   '(("\\begin{equation}\nx=1\n\\end{equation}"
      "\\begin{equation}\nx=1\n\\end{equation}")
     ;; the value keeps its content but loses common indentation
     ("  \\begin{equation}\n  x=1\n  \\end{equation}"
      "\\begin{equation}\nx=1\n\\end{equation}"))
   nil '(:with-latex mathjax)))

;;;; Paragraph

(ert-deftest t-paragraph ()
  "Tests for `org-w3ctr-paragraph'."
  (t-check-element-values
   #'t-paragraph
   '(("123" "<p>123</p>")
     ("123\n 234" "<p>123\n 234</p>")
     ;; trim
     ("    123" "<p>123</p>")
     ("123\n\t234" "<p>123\n\011234</p>")
     ;; two newline, two paragraph
     ("123\n\n234" "<p>234</p>" "<p>123</p>")
     ;; unordered list item's first object
     ("- 123 234" "123 234")
     ("- [ ] 123" "123")
     ("- 123\n 234" "123\n234")
     ("- 123\n\n   234" "<p>234</p>" "123")
     ;; first object with attributes
     ("-\n  #+attr__: [example]\n  123"
      "<span class=\"example\">123</span>")
     ("-\n  #+name: id\n  123\n\n  #+name: id2\n  456"
      "<p id=\"id2\">456</p>" "<span id=\"id\">123</span>")
     ;; standalone image
     ;; since `org-export-data' doesn't apply `:filter-parse-tree'
     ;; no newlines for test data.
     ("[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\"></figure>")
     ("#+name: id\n#+caption:cap\n[[./1.png]]"
      "<figure id=\"id\">\n<img src=\"./1.png\" alt=\"1.png\"><figcaption>cap</figcaption>\n</figure>")
     ;; empty caption
     ("#+caption: \n[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\"></figure>")
     ("#+attr__:[sidefigure]\n[[./2.gif]]"
      "<figure class=\"sidefigure\">\n<img src=\"./2.gif\" alt=\"2.gif\"></figure>")
     ("[[https://example.com/1.jpg]]"
      "<figure>\n<img src=\"https://example.com/1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[file:1.jpg]]" "<figure>\n<img src=\"1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[./1.png][name]]" "<p><a href=\"./1.png\">name</a></p>")
     ("[[https://example.com/1.jpg][file:1.jpg]]"
      "<figure>\n<a href=\"https://example.com/1.jpg\"><img src=\"1.jpg\" alt=\"1.jpg\"></a></figure>")
     ;; `:attr_html' applies to the image element, not the figure
     ("#+attr_html: :class foo\n[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\" class=\"foo\"></figure>")
     ("#+attr_html: :class foo\n[[https://example.com/1.jpg][file:1.jpg]]"
      "<figure>\n<a href=\"https://example.com/1.jpg\"><img src=\"1.jpg\" alt=\"1.jpg\" class=\"foo\"></a></figure>")
     ;; in a non-standalone paragraph `:attr_html' stays on the <p>
     ("#+attr_html: :class foo\n[[./1.png]] [[./2.png]]"
      "<p class=\"foo\"><img src=\"./1.png\" alt=\"1.png\"> <img src=\"./2.png\" alt=\"2.png\"></p>")
     ;; `:attr__' applies to the figure
     ("#+attr__: [bar]\n[[https://example.com/1.jpg][file:1.jpg]]"
      "<figure class=\"bar\">\n<a href=\"https://example.com/1.jpg\"><img src=\"1.jpg\" alt=\"1.jpg\"></a></figure>"))
   nil '(:html-prefer-user-labels t))
  ;; nil CONTENTS: math under `tex:nil' is pruned before the
  ;; transcoder runs, and the empty paragraph is ""
  (t-check-element-values
   #'t-paragraph
   '(("$x^2$" ""))
   nil '(:with-latex nil)))

;;;; Verse Block

(ert-deftest t-verse-block ()
  "Tests for `org-w3ctr-verse-block'."
  (t-check-element-values
   #'t-verse-block
   '(("#+begin_verse\n#+end_verse" "<p>\n</p>")
     ("#+BEGIN_VERSE\n#+END_VERSE" "<p>\n</p>")
     ("#+begin_verse\n1  2  3\n#+end_verse" "<p>\n1  2  3<br>\n</p>")
     ("#+begin_verse\n 1\n  2\n   3\n#+end_verse"
      "<p>\n1<br>\n&#xa0;2<br>\n&#xa0;&#xa0;3<br>\n</p>")
     ("#+name: this\n#+begin_verse\n#+end_verse"
      "<p id=\"this\">\n</p>")
     ("#+attr__:[hi]\n#+begin_verse\n\n\n#+end_verse"
      "<p class=\"hi\">\n<br>\n<br>\n</p>"))
   nil '(:html-prefer-user-labels t)))

;;;; Engrave-faces subset

(ert-deftest t--engrave-buffer ()
  "Tests for `org-w3ctr--engrave-buffer'."
  (with-temp-buffer
    (insert "abc def")
    (put-text-property 1 4 'face 'font-lock-keyword-face)
    (let ((out (generate-new-buffer " *engrave-out*")))
      (unwind-protect
          (progn
            (t--engrave-buffer (current-buffer) out)
            ($l (with-current-buffer out (buffer-string))
                "<span class=\"ef-k\">abc</span> def"))
        (kill-buffer out)))))

(ert-deftest t--engrave-next-face-change ()
  "Tests for `org-w3ctr--engrave-next-face-change'."
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 1 4 'face 'font-lock-keyword-face)
    ($l (t--engrave-next-face-change 1) 4)
    ($l (t--engrave-next-face-change 4) (point-max))))

(ert-deftest t--engrave-overlay-faces-at ()
  "Tests for `org-w3ctr--engrave-overlay-faces-at'."
  (with-temp-buffer
    (insert "abc")
    ($n (t--engrave-overlay-faces-at 2))
    (let ((ov (make-overlay 1 4)))
      (overlay-put ov 'face 'font-lock-keyword-face)
      ($l (t--engrave-overlay-faces-at 2) '(font-lock-keyword-face)))))

(ert-deftest t--engrave-face-transformer ()
  "Tests for `org-w3ctr--engrave-face-transformer'."
  ($l (t--engrave-face-transformer 'font-lock-keyword-face "defun")
      "<span class=\"ef-k\">defun</span>")
  ($l (t--engrave-face-transformer 'font-lock-keyword-face "<&>")
      "<span class=\"ef-k\">&lt;&amp;&gt;</span>")
  ;; Unfaced, unknown and default faces are emitted unwrapped.
  ($l (t--engrave-face-transformer nil "<&>") "&lt;&amp;&gt;")
  ($l (t--engrave-face-transformer 'no-such-face "<&>") "&lt;&amp;&gt;")
  ($l (t--engrave-face-transformer 'default "<&>") "&lt;&amp;&gt;")
  ;; Whitespace-only runs are never wrapped.
  ($l (t--engrave-face-transformer 'font-lock-keyword-face "  \n ")
      "  \n "))

(ert-deftest t--engrave-get-style ()
  "Tests for `org-w3ctr--engrave-get-style'."
  ($n (t--engrave-get-style nil))
  ($n (t--engrave-get-style 'default))
  ($n (t--engrave-get-style 'no-such-face))
  ($l (t--engrave-get-style 'font-lock-keyword-face)
      '(font-lock-keyword-face :slug "k"))
  ($l (t--engrave-get-style '(font-lock-comment-face))
      '(font-lock-comment-face :slug "c"))
  ($l (t--engrave-get-style 'css-property)
      '(css-property :slug "f")))

(ert-deftest t--engrave-fontify-code ()
  "Tests for `org-w3ctr--engrave-fontify-code'."
  (let ((out (t--engrave-fontify-code "(defun foo () 1)" "emacs-lisp")))
    (should (string-match-p "ef-k" out))
    ($n (string-match-p "<code" out)))
  ($l (t--engrave-fontify-code "(a < b)" "no-such-lang") "(a &lt; b)")
  ($l (t--engrave-fontify-code "(a < b)" nil) "(a &lt; b)"))

;;;; Source block

(ert-deftest t-fontify-code ()
  "Tests for `org-w3ctr-fontify-code'."
  (let ((out (t-fontify-code "(defun foo () 1)" "emacs-lisp")))
    (should (string-match-p "ef-k" out))
    ($n (string-match-p "<code" out)))
  (let ((t-fontify-method nil))
    ($l (t-fontify-code "(a < b)" "emacs-lisp") "(a &lt; b)"))
  ($l (t-fontify-code "" "emacs-lisp") "")
  ($l (t-fontify-code "(a < b)" nil) "(a &lt; b)"))

(ert-deftest t--src-code ()
  "Tests for `org-w3ctr--src-code'."
  (cl-flet ((f (str) (t-get-element str 'src-block)))
    (let ((out (t--src-code (f "#+begin_src emacs-lisp\n(defun foo () 1)\n#+end_src")
                            "emacs-lisp")))
      (should (string-match-p "ef-k" out))
      ($n (string-match-p "<code" out)))))

(ert-deftest t--src-code-tag ()
  "Tests for `org-w3ctr--src-code-tag'."
  ($l (t--src-code-tag "emacs-lisp" "body")
      "<code class=\"src src-emacs-lisp\">body</code>")
  ($l (t--src-code-tag nil "body") "<code>body</code>"))

(ert-deftest t--src-block-attrs ()
  "Tests for `org-w3ctr--src-block-attrs' (uncaptioned, deterministic)."
  (t-check-element-values
   #'t--src-block-attrs
   '(("#+begin_src emacs-lisp\nx\n#+end_src" "")
     ("#+attr__: [foo]\n#+begin_src emacs-lisp\nx\n#+end_src"
      " class=\"foo\"")
     ("#+name: nm\n#+begin_src emacs-lisp\nx\n#+end_src"
      " id=\"nm\""))
   nil '(:html-prefer-user-labels t)))

(ert-deftest t-src-block ()
  "Tests for `org-w3ctr-src-block'."
  (let ((out (org-export-string-as
              "#+begin_src emacs-lisp\n(defun foo () 1)\n#+end_src"
              'w3ctr t)))
    (should (string-match-p "<pre>\n<code class=\"src src-emacs-lisp\">" out)))
  (let ((out (org-export-string-as
              "#+caption: C\n#+begin_src emacs-lisp\nx\n#+end_src"
              'w3ctr t)))
    (should (string-match-p "<div id=\"org[^\"]*\" class=\"example\">" out))
    (should (string-match-p "self-link" out)))
  (let ((out (org-export-string-as
              "#+attr_html: :textarea t\n#+begin_src emacs-lisp\nx\n#+end_src"
              'w3ctr t)))
    (should (string-match-p "<textarea" out))))

(ert-deftest t-inline-src-block ()
  "Tests for `org-w3ctr-inline-src-block'."
  (let ((org-export-babel-evaluate nil))
    (let ((out (org-export-string-as "src_emacs-lisp{(+ 1 2)}" 'w3ctr t)))
      (should (string-match-p "<code class=\"src-inline src-emacs-lisp\">" out))
      ;; Single wrapper: no nested <code>.
      ($n (string-match-p "src-inline[^\"]*\"><code" out)))))

;;; Objects

;;;; Entity

(ert-deftest t-entity ()
  "Tests for `org-w3ctr-entity'."
  (t-check-element-values
   #'t-entity
   '(("\\alpha \\beta \\eta \\gamma \\epsilon"
      "&epsilon;" "&gamma;" "&eta;" "&beta;" "&alpha;")
     ("\\AA" "&Aring;")
     ("\\real \\image \\imath \\jmath"
      "&jmath;" "&imath;" "&image;" "&real;")
     ("\\quot \\acute \\bdquo \\raquo"
      "&raquo;" "&bdquo;" "&acute;" "&quot;")
     ("\\Dagger \\ddag \\** \\dollar \\copy \\reg"
      "&reg;" "&copy;" "$" "&Dagger;" "&Dagger;")
     ("\\frac12 \\frac14 \\frac34 \\radic \\prop \\sim"
      "&sim;" "&prop;" "&radic;" "&frac34;" "&frac14;" "&frac12;"))))

;;;; Line Break

(ert-deftest t-line-break ()
  "Tests for `org-w3ctr-line-break'."
  ($l (t-line-break nil nil nil) "<br>\n"))

;;;; Target

(ert-deftest t-target ()
  "Tests for `org-w3ctr-target'."
  (cl-letf* ((counter 0)
             ((symbol-function 't--reference)
              (lambda (_d _i &optional _n)
                (number-to-string (cl-incf counter)))))
    ($l (t-target nil nil nil) "<span id=\"1\"></span>")
    ($l (t-target nil nil nil) "<span id=\"2\"></span>")
    (t-check-element-values
     #'t-target
     '(("<<th1>> <<th2>> <<th3>>"
        "<span id=\"5\"></span>"
        "<span id=\"4\"></span>"
        "<span id=\"3\"></span>")))))

;;;; Radio Target

(ert-deftest t-radio-target ()
  "Tests for `org-w3ctr-radio-target'."
  (cl-letf* ((counter 0)
             ((symbol-function 't--reference)
              (lambda (_d _i &optional _n)
                (number-to-string (cl-incf counter)))))
    ($l (t-radio-target nil "hello" nil)
        "<span id=\"1\">hello</span>")
    ($l (t-radio-target nil "world" nil)
        "<span id=\"2\">world</span>")
    ($l (t-radio-target nil nil nil)
        "<span id=\"3\"></span>")
    (t-check-element-values
     #'t-radio-target
     '(("<<<th1>>> <<<th2>>> <<<th3>>>"
        "<span id=\"6\">th3</span>"
        "<span id=\"5\">th2</span>"
        "<span id=\"4\">th1</span>")))))

;;;; Statistics Cookie

(ert-deftest t-statistics-cookie ()
  "Tests for `org-w3ctr-statistics-cookie'."
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n &optional _d _f) n)))
    ($l (t-statistics-cookie "" nil nil) "<code></code>")
    ($l (t-statistics-cookie "y" nil nil) "<code>y</code>")
    ($l (t-statistics-cookie ()()()) "<code>nil</code>"))
  (t-check-element-values
   #'t-statistics-cookie
   '(("- hello [/]" "<code>[/]</code>")
     ("- hello [0/1]\n  - [ ] helllo" "<code>[0/1]</code>")
     ("- hello [33%]\n  - [X] hello" "<code>[33%]</code>")
     ("- hello :: abc [0/1]\n  - [ ] this is what"
      "<code>[0/1]</code>")
     ("1. hello [50%]\n   1. [ ] hello1\n   2. [X] hello2"
      "<code>[50%]</code>"))))

;;;; Subscript

(ert-deftest t-subscript ()
  "Tests for `org-w3ctr-subscript'."
  ($l (t-subscript nil "123" nil) "<sub>123</sub>")
  ($l (t-subscript nil "" nil) "<sub></sub>")
  ($l (t-subscript nil t nil) "<sub>t</sub>")
  ($l (t-subscript nil nil nil) "<sub>nil</sub>")
  (t-check-element-values
   #'t-subscript
   '(("1_2" "<sub>2</sub>")
     ("x86_64" "<sub>64</sub>")
     ("f_{1}" "<sub>1</sub>"))))

;;;; Superscript

(ert-deftest t-superscript ()
  "Tests for `org-w3ctr-superscript'."
  ($l (t-superscript nil "123" nil) "<sup>123</sup>")
  ($l (t-superscript nil "" nil) "<sup></sup>")
  ($l (t-superscript nil t nil) "<sup>t</sup>")
  ($l (t-superscript nil nil nil) "<sup>nil</sup>")
  (t-check-element-values
   #'t-superscript
   '(("1^2" "<sup>2</sup>")
     ("x86^64" "<sup>64</sup>")
     ("f^{1}" "<sup>1</sup>"))))

;;;; Timestamp

(ert-deftest t--timezone-to-offset ()
  "Tests for `org-w3ctr--timezone-to-offset'."
  ($it t--timezone-to-offset
    ($l (it "local") 'local)
    ($l (it "LOCAL") 'local)
    ($l (it "LoCaL") 'local)
    ($l (it "lOcAl") 'local)
    ($l (it "UTC+8") (* 8 3600))
    ($l (it "UTC+08") 28800)
    ($l (it "GMT-5") (* -5 3600))
    ($l (it "+0530") (+ (* 5 3600) (* 30 60)))
    ($l (it "-0830") (+ (* -8 3600) (* -30 60)))
    ($n (it "+10"))
    ($n (it "-11"))
    ($n (it "INVALID"))
    ($n (it "UTC+123"))
    ($n (it "+12345"))
    ($n (it "+1400"))
    ($n (it "UTC+13"))
    ($n (it "UTC-13"))
    ($n (it "+0860"))))

(ert-deftest t--get-info-timezone-offset ()
  "Tests for `org-w3ctr--get-info-timezone-offset'."
  ($it t--get-info-timezone-offset
    (let ((info0 '(:html-timezone "local")))
      ($l (it info0) 'local))
    (let ((info1 '(:html-timezone 28800)))
      ($l (it info1) 28800))
    (let ((info2 '(:html-timezone -18000)))
      ($l (it info2) -18000))
    (let ((info3 '(:html-timezone "UTC+8")))
      ($l (it info3) 28800)
      ($s (numberp (t--pget info3 :html-timezone)))
      ($l (t--pget info3 :html-timezone) 28800))
    (let ((info4 '(:html-timezone "-0500")))
      ($l (it info4) -18000)
      ($s (numberp (t--pget info4 :html-timezone)))
      ($l (t--pget info4 :html-timezone) -18000))
    (let ((info5 '(:html-timezone "UTC+0")))
      ($l (it info5) 0)
      ($l (t--pget info5 :html-timezone) 0))
    (let ((info6 '(:html-timezone "+0530")))
      ($l (it info6) 19800)
      ($l (t--pget info6 :html-timezone) 19800))
    (let ((info7 '(:html-timezone "Invalid")))
      ($e! (it info7)))
    (let ((info8 '(:html-timezone 3600 :other "value")))
      ($l (it info8) 3600)
      ($l info8 '(:html-timezone 3600 :other "value")))))

(ert-deftest t--get-info-export-timezone-offset ()
  "Tests for `org-w3ctr--get-info-export-timezone-offset'."
  ($it t--get-info-export-timezone-offset
    ;; When :html-export-timezone is nil, use :html-timezone
    (let ((info1 '(:html-timezone 28800)))
      ($l (it info1) 28800))
    (let ((info2 '( :html-timezone "UTC+8"
                    :html-export-timezone nil)))
      ($l (it info2) 28800))
    ;; When :html-timezone is "local", always use 'local
    (let ((info3 '( :html-timezone "local"
                    :html-export-timezone 3600)))
      ($q (it info3) 'local))
    (let ((info4 '( :html-timezone "local"
                    :html-export-timezone "UTC+5")))
      ($q (it info4) 'local))
    (let ((info41 '( :html-timezone local
                     :html-export-timezone "+0100")))
      ($q (it info41) 'local))
    ;; :html-export-timezone is local
    (let ((info42 '( :html-timezone 0
                     :html-export-timezone "local")))
      ($q (it info42) 'local))
    (let ((info43 '( :html-timezone 0
                     :html-export-timezone local)))
      ($q (it info43) 'local))
    ;; When :html-export-timezone is number, use directly
    (let ((info5 '( :html-timezone 28800
                    :html-export-timezone -18000)))
      ($l (it info5) -18000))
    ;; When :html-export-timezone is string, convert and cache
    (let ((info6 '( :html-timezone 28800
                    :html-export-timezone "-0500")))
      ($l (it info6) -18000)
      ($s (fixnump (t--pget info6 :html-export-timezone)))
      ($l (t--pget info6 :html-export-timezone) -18000))
    (let ((info7 '( :html-timezone 28800
                    :html-export-timezone "+0530")))
      ($l (it info7) 19800)
      ($l (t--pget info7 :html-export-timezone) 19800))
    (let ((info8 '( :html-timezone 0
                    :html-export-timezone "UTC+0")))
      ($l (it info8) 0))
    ;; Invalid
    (let ((info9 '( :html-timezone 3600
                    :html-export-timezone "Invalid")))
      ($e! (it info9)))
    (let ((info9 '( :html-timezone "WTF"
                    :html-export-timezone "+0000")))
      ($e! (it info9)))
    ;; Test optional argument
    (let ((info10 '(:html-export-timezone "+0000")))
      ($q (it info10 'local) 'local))
    (let ((info11 '(:html-export-timezone "UTC+8")))
      ($q (it info11 10) 28800))))

(ert-deftest t--get-info-timezone-delta ()
  "Tests for `org-w3ctr--get-info-timezone-delta'."
  ($it t--get-info-timezone-delta
    (let ((info '( :html-timezone 2
                   :html-export-timezone 1)))
      ($l (it info) -1))
    (let ((info '( :html-timezone 28800
                   :html-export-timezone 0)))
      ($l (it info) -28800))
    (let ((info '( :html-timezone local
                   :html-export-timezone 3600)))
      ($l (it info) 0))
    (let ((info '( :html-timezone local
                   :html-export-timezone 3600)))
      ($l (it info) 0))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info) 77467))
    (let ((info '( :html-timezone "WTF"
                   :html-export-timezone "INVALID")))
      ($e! (it info)))
    (let ((info '( :html-timezone 0
                   :html-export-timezone "INVALID")))
      ($e! (it info)))
    ($e! (it nil))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info 1 2) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info nil 114515) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info 191980) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it nil 114514 191981) 77467))))

(ert-deftest t--get-datetime-format ()
  "Tests for `org-w3ctr--get-datetime-format'."
  ($it t--get-datetime-format
    ($l (it 28800 's-none) "%F %R+0800")
    ($l (it 18000 's-none) "%F %R+0500")
    ($l (it -28800 's-none) "%F %R-0800")
    ($l (it -18000 's-none) "%F %R-0500")
    ($l (it 0 's-none) "%F %R+0000")
    ($l (it 0 's-none-zulu) "%F %RZ")
    ($l (it 0 's-colon) "%F %R+00:00")
    ($l (it 0 's-colon-zulu) "%F %RZ")
    ($l (it 4800 's-colon-zulu) "%F %R+01:20")
    ($l (it 0 'T-colon) "%FT%R+00:00")
    ($l (it 0 'T-none) "%FT%R+0000")
    ($l (it 0 'T-colon) "%FT%R+00:00")
    ($l (it 0 'T-colon-zulu) "%FT%RZ")
    ($l (it 3600 's-colon) "%F %R+01:00")
    ($l (it -900 's-colon) "%F %R-00:15")
    ($l (it 19800 'T-colon) "%FT%R+05:30")
    ($l (it -16200 'T-colon) "%FT%R-04:30")
    ($l (it 50400 's-none) "%F %R+1400")
    ($l (it -43200 's-none) "%F %R-1200")
    ($l (it 37800 'T-colon-zulu) "%FT%R+10:30")
    ($n (it 0 nil))
    ($l (it 'local nil t) "%F")
    ($l (it 'local 's-none) "%F %R")
    ($l (it 'local 's-none-zulu) "%F %R")
    ($l (it 'local 's-colon) "%F %R")
    ($l (it 'local 's-colon-zulu) "%F %R")
    ($l (it 'local 'T-none) "%FT%R")
    ($l (it 'local 'T-none-zulu) "%FT%R")
    ($l (it 'local 'T-colon) "%FT%R")
    ($l (it 'local 'T-colon-zulu) "%FT%R")))

(ert-deftest t--format-datetime ()
  "Tests for `org-w3ctr--format-datetime'."
  ;; Basic test with space separator and +HHMM timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info1 '( :html-timezone 28800
                  :html-export-timezone 28800
                  :html-datetime-option s-none)))
    ($l (t--format-datetime test-time info1)
        "2023-01-01 12:00+0800"))
  ;; Test with colon in time and -HHMM timezone
  (let ((test-time (encode-time 0 30 9 15 6 2023))
        (info2 '( :html-timezone -14400
                  :html-export-timezone -14400
                  :html-datetime-option s-colon)))
    ($l (t--format-datetime test-time info2)
        "2023-06-15 09:30-04:00"))
  ;; Test UTC with Zulu timezone
  (let ((test-time (encode-time 0 0 0 1 1 2023))
        (info3 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option s-none-zulu)))
    ($l (t--format-datetime test-time info3)
        "2023-01-01 00:00Z"))
  ;; Test with T separator and +HH:MM timezone
  (let ((test-time (encode-time 0 45 18 31 12 2023))
        (info4 '( :html-timezone 19800
                  :html-export-timezone 19800
                  :html-datetime-option T-colon)))
    ($l (t--format-datetime test-time info4)
        "2023-12-31T18:45+05:30"))
  ;; Test with T separator and Zulu timezone for UTC
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info5 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option T-colon-zulu)))
    ($l (t--format-datetime test-time info5)
        "2023-01-01T12:00Z"))
  ;; Test local timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info6 '( :html-timezone "local"
                  :html-export-timezone "local"
                  :html-datetime-option s-none)))
    ($l "2023-01-01 12:00"
        (t--format-datetime test-time info6)))
  ;; Test invalid timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info7 '(:html-timezone "Invalid")))
    ($e! (t--format-datetime test-time info7)))
  ;; time out of range
  (let ((info8 '(:html-timezone 0 :html-datetime-option s-none)))
    ($e!l (t--format-datetime -65536 info8)
          '(org-w3ctr-error "Invalid time value: -65536"))))

(ert-deftest t--call-with-invalid-time-spec-handler ()
  "Tests for `org-w3ctr--call-with-invalid-time-spec-handler'."
  (let ((ts (t-get-parsed-elements
             "[2000-01-01] [1945-08-15] [1145-05-14]--[1919-08-10]"
             'timestamp)))
    ($e!l (t--call-with-invalid-time-spec-handler
           (lambda (_ts) (error "Invalid time specification"))
           (nth 0 ts))
          '(org-w3ctr-error "Invalid timestamp: [2000-01-01]"))
    ($e!l (t--call-with-invalid-time-spec-handler
           #'org-timestamp-to-time (nth 1 ts))
          '(org-w3ctr-error "Invalid timestamp: [1945-08-15]"))
    ($e!l (t--call-with-invalid-time-spec-handler
           #'org-element-timestamp-interpreter (nth 2 ts) nil)
          '(org-w3ctr-error "Invalid timestamp: [1145-05-14]--[1919-08-10]")))
  (let ((ts (t-get-parsed-elements
             "[2038-01-19 03:14:07] [2025-06-17 16:40]" 'timestamp)))
    ($s (t--call-with-invalid-time-spec-handler
         #'org-timestamp-to-time (nth 0 ts)))
    ($s (t--call-with-invalid-time-spec-handler
         #'org-element-interpret-data (nth 1 ts)))))

(ert-deftest t--format-ts-datetime ()
  "Tests for `org-w3ctr--format-ts-datetime'."
  (let* ((info '( :html-timezone 28800 :html-export-timezone 0
                  :html-datetime-option T-none-zulu))
         (ts0 (t-get-parsed-elements "[1900-01-01]" 'timestamp))
         (ts1 (t-get-parsed-elements "[2025-06-17 16:49]" 'timestamp))
         (ts2 (t-get-parsed-elements "[2038-01-01]" 'timestamp))
         (ts3 (t-get-parsed-elements
               "[2025-06-17]--[2035-06-17]" 'timestamp))
         (ts4 (t-get-parsed-elements
               "[2025-01-01 18:00]--[2026-01-01 15:00]" 'timestamp))
         (ts5 (t-get-parsed-elements
               "<2022-06-07>--<2022-06-08 21:00>" 'timestamp))
         (ts6 (t-get-parsed-elements
               "[2022-06-07 09:00]--[2022-06-08]" 'timestamp)))
    ($e!l (t--format-ts-datetime (nth 0 ts0) info)
          '(org-w3ctr-error "Invalid timestamp: [1900-01-01]"))
    ($l (t--format-ts-datetime (nth 0 ts1) info)
        " datetime=\"2025-06-17T08:49Z\"")
    ($l (t--format-ts-datetime (nth 0 ts2) info)
        " datetime=\"2038-01-01\"")
    ($l (t--format-ts-datetime (nth 0 ts3) info)
        " datetime=\"2025-06-17\"")
    ($l (t--format-ts-datetime (nth 0 ts3) info t)
        " datetime=\"2035-06-17\"")
    ($l (t--format-ts-datetime (nth 0 ts4) info)
        " datetime=\"2025-01-01T10:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts4) info t)
        " datetime=\"2026-01-01T07:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts5) info)
        " datetime=\"2022-06-07\"")
    ($l (t--format-ts-datetime (nth 0 ts5) info t)
        " datetime=\"2022-06-08\"")
    ($l (t--format-ts-datetime (nth 0 ts6) info)
        " datetime=\"2022-06-07T01:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts6) info t)
        " datetime=\"2022-06-08T01:00Z\"")))

(ert-deftest t--interpret-timestamp ()
  "Tests for `org-w3ctr--interpret-timestamp'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (g (x) (t--interpret-timestamp (f x))))
    ($l (g "[2000-01-01]") "[2000-01-01 Sat]")
    ($l (g "[1970-01-02]") "[1970-01-02 Fri]")
    ($l (g "[1972-02-21]") "[1972-02-21 Mon]")
    ($l (g "<1989-11-09 WHAT>") "<1989-11-09 Thu>")
    ($l (g "[1991-12-26] thu") "[1991-12-26 Thu]")
    ($l (g "[2001-09-11] WTF") "[2001-09-11 Tue]")
    ($l (g "[2020-03-11] NUL") "[2020-03-11 Wed]")
    ($l (g "[1983-01-01]") "[1983-01-01 Sat]")
    ($l (g "[2022-11-30]") "[2022-11-30 Wed]")
    ($l (g "[2011-03-11]") "[2011-03-11 Fri]")
    ($l (g "[2008-02-30]") "[2008-03-01 Sat]")
    ($l (g "[2029-02-29]") "[2029-03-01 Thu]")
    ($l (g "[2029-02-30]") "[2029-03-02 Fri]")
    ($l (g "[2038-01-19]") "[2038-01-19 Tue]")
    ($l (g "[2038-01-19 03:14:07 UTC]") "[2038-01-19 Tue 03:14]")
    ($l (g "[2022-11-30 12:13 UTC+8]") "[2022-11-30 Wed 12:13]")
    ($l (g "[1976-09-09 00:10 UTC+8]") "[1976-09-09 Thu 00:10]")
    ($l (g "[2024-12-31 24:00]") "[2025-01-01 Wed 00:00]")
    ($l (g "[2025-06-18 99:99]") "[2025-06-22 Sun 04:39]")
    ($l (g "[2000-07-09 YY 19:25]") "[2000-07-09 Sun 19:25]")
    ($l (g "[2099-12-31 23:59:59]") "[2099-12-31 Thu 23:59]")
    ($l (g "[2025-06-18 00:00-03:07]") "[2025-06-18 Wed 00:00-03:07]")
    ($l (g "[2025-06-06 14:00-25:00]") "[2025-06-06 Fri 14:00-25:00]")
    ($l (g "[2000-01-01]--[2020-01-01]")
        "[2000-01-01 Sat]--[2020-01-01 Wed]")
    ($l (g "<1981-01-01>--<2020-01-01>")
        "<1981-01-01 Thu>--<2020-01-01 Wed>")
    ($l (g "[2000-01-01]--<2000-01-02>")
        "[2000-01-01 Sat]--[2000-01-02 Sun]")
    ($l (g "<2000-01-01>--[2000-01-02]")
        "<2000-01-01 Sat>--<2000-01-02 Sun>")
    ($l (g "[2020-01-01]--[2000-01-01]")
        "[2020-01-01 Wed]--[2000-01-01 Sat]")
    ($l (g "[2025-02-01 00:00]--[2025-06-01 00:00]")
        "[2025-02-01 Sat 00:00]--[2025-06-01 Sun 00:00]")
    ($l (g "[2025-02-01 00:00]--[2025-05-01]")
        "[2025-02-01 Sat 00:00]--[2025-05-01 Thu 00:00]")
    ($l (g "<2020-01-01>--[2025-01-01 12:23]")
        "<2020-01-01 Wed>--<2025-01-01 Wed 12:23>")
    ($l (g "[2020-01-01 12:23]--<1999-10-10>")
        "[2020-01-01 Wed 12:23]--[1999-10-10 Sun 12:23]")
    ($l (g "[1999-01-01]--[1999-01-02 12:00]")
        "[1999-01-01 Fri]--[1999-01-02 Sat 12:00]")
    ($l (g "[1999-01-01 12:00-13:00]--[2000-01-01 13:00-14:00]")
        "[1999-01-01 Fri 12:00]--[2000-01-01 Sat 13:00]"))
  (cl-flet ((f (s) (t-get-element s 'timestamp))
            (g (x) (t--interpret-timestamp x)))
    ($e!l (g (f "[1949-10-01]"))
          '(org-w3ctr-error "Invalid timestamp: [1949-10-01]"))
    ($e! (let ((ts (f "[2000-01-01]")))
           (setf (org-element-property :year-start ts) nil)
           (g ts))))
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (g (x) (t--interpret-timestamp (f x))))
    ($l (g "<2007-05-16 12:30 +1h>") "<2007-05-16 Wed 12:30 +1h>")
    ($l (g "<2007-05-16 12:30 +1d>") "<2007-05-16 Wed 12:30 +1d>")
    ($l (g "<2007-05-16 12:30 +1w>") "<2007-05-16 Wed 12:30 +1w>")
    ($l (g "<2007-05-16 12:30 +1m>") "<2007-05-16 Wed 12:30 +1m>")
    ($l (g "<2007-05-16 12:30 +1y>") "<2007-05-16 Wed 12:30 +1y>")))

(ert-deftest t--format-timestamp-diary ()
  "Tests for `org-w3ctr--format-timestamp-diary'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (g (x info) (t--format-timestamp-diary (f x) info))
             (mk (w o) `( :html-timestamp-wrapper ,w
                          :html-timestamp-option ,o)))
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'span 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'time 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'org))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'fmt))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'cus))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'fun))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'wtf))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 4 2) 22:00-23:00>" (mk 'none 'org))
        "&lt;%%(diary-float t 4 2) 22:00-23:00&gt;")
    ($l (g "<%%(diary-float t 4 2) 22:00>--<2222-02-22 23:00>"
           (mk 'none 'org))
        "&lt;%%(diary-float t 4 2) 22:00&gt;")))

(ert-deftest t--format-ts-span-time ()
  "Tests for `org-w3ctr--format-ts-span-time'."
  ;; <span> branch (time = nil)
  ($l (t--format-ts-span-time "hello" nil)
      "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">hello</span></span>")
  ($l (t--format-ts-span-time "a < b" nil)
      "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">a &lt; b</span></span>")
  ;; <time> branch (time = non-nil) -- returns template with %s
  ($l (t--format-ts-span-time "hello" nil t) "<time%s>hello</time>")
  ($l (t--format-ts-span-time "2024-01-01" nil t) "<time%s>2024-01-01</time>")
  ;; special strings via t-plain-text
  ($l (t--format-ts-span-time "a -- b" '(:with-special-strings t))
      "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">a &#x2013; b</span></span>")
  ;; preserve breaks via t-plain-text
  ($l (t--format-ts-span-time "a\nb" '(:preserve-breaks t))
      "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">a<br>\nb</span></span>")
  ;; template filled via format
  ($l (format (t--format-ts-span-time "2024" nil t) " datetime=\"2024\"")
      "<time datetime=\"2024\">2024</time>"))

(ert-deftest t--format-timestamp-raw-1 ()
  "Tests for `org-w3ctr--format-timestamp-raw-1'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (g (x y info) (t--format-timestamp-raw-1 (f x) y info))
             (p (w) `( :html-timestamp-wrapper ,w)))
    ($e!l (g "[2000-01-01]" "[0000-00-00]"(p 'wtf))
          '(org-w3ctr-error "Unknown timestamp wrapper: wtf"))
    ;; test none
    (let ((ts "[2000-01-01]"))
      ($l (g ts "[2000-01-01 test]" (p 'none)) "[2000-01-01 test]")
      ($l (g ts "test" (p 'none)) "test")
      ($l (g ts "<time>123</time>" (p 'none))
          "&lt;time&gt;123&lt;/time&gt;")
      ($l (g ts "&&&&&" (p 'none)) "&amp;&amp;&amp;&amp;&amp;"))
    ;; test span
    (let ((ts "[2000-01-01]"))
      ($l (g ts "[2000-01-01 test]" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2000-01-01 test]" "</span></span>"))
      ($l (g ts "test" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "test" "</span></span>"))
      ($l (g ts "<time>123</time>" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;time&gt;123&lt;/time&gt;"
              "</span></span>"))
      ($l (g ts "&" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&amp;" "</span></span>")))
    ;; test time
    (let ((info '( :html-timestamp-wrapper time
                   :html-datetime-option T-none
                   :html-timezone 28800))
          (t1 "[1970-01-02]")
          (t2 "[1970-01-02 08:00]")
          (t3 "[1970-01-02 08:00-13:00]")
          (t4 "[1970-01-02 08:00]--[2000-01-02 09:00]")
          (b1 "[0000-00-00]")
          (b2 "[0000-00-00]--[0000-00-00]"))
      ($l (g t1 b1 info)
          "<time datetime=\"1970-01-02\">[0000-00-00]</time>")
      ($l (g t1 b2 info)
          ($c "<time datetime=\"1970-01-02\">[0000-00-00]</time>--"
              "<time datetime=\"1970-01-02\">[0000-00-00]</time>"))
      ($l (g t2 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t2 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time datetime=\"1970-01-02T08:00+0800\">"
              "[0000-00-00]</time>"))
      ($l (g t3 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t3 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time datetime=\"1970-01-02T13:00+0800\">"
              "[0000-00-00]</time>"))
      ($l (g t4 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t4 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time datetime=\"2000-01-02T09:00+0800\">"
              "[0000-00-00]</time>"))
      (t--pput info :with-special-strings t)
      ($l (g t1 b2 info)
          ($c "<time datetime=\"1970-01-02\">[0000-00-00]</time>&#x2013;"
              "<time datetime=\"1970-01-02\">[0000-00-00]</time>"))
      ($l (g t2 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>&#x2013;<time datetime=\"1970-01-02T08:00+0800\">"
              "[0000-00-00]</time>"))
      ($l (g t3 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>&#x2013;<time datetime=\"1970-01-02T13:00+0800\">"
              "[0000-00-00]</time>"))
      ($l (g t4 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>&#x2013;<time datetime=\"2000-01-02T09:00+0800\">"
              "[0000-00-00]</time>")))))

(ert-deftest t--format-timestamp-raw ()
  "Tests for `org-w3ctr--format-timestamp-raw'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (g (x info) (t--format-timestamp-raw (f x) info))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone local)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 (p 'none)) "[2011-11-18]")
      ($l (g t1 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-18]" "</span></span>"))
      ($l (g t1 (p 'time))
          "<time datetime=\"2011-11-18\">[2011-11-18]</time>")
      ($l (g t2 (p 'none)) "&lt;2011-11-18 14:54&gt;")
      ($l (g t2 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;2011-11-18 14:54&gt;" "</span></span>"))
      ($l (g t2 (p 'time))
          ($c "<time datetime=\"2011-11-18T14:54\">"
              "&lt;2011-11-18 14:54&gt;</time>"))
      ($l (g t3 (p 'none)) "[2011-11-18 06:54-14:54]")
      ($l (g t3 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-18 06:54-14:54]" "</span></span>"))
      ($l (g t3 (p 'time))
          ($c "<time datetime=\"2011-11-18T06:54\">"
              "[2011-11-18 06:54-14:54]</time>"))
      ($l (g t4 (p 'none))
          "&lt;2011-11-18 06:54&gt;--[2011-11-18 14:54]")
      ($l (g t4 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;2011-11-18 06:54&gt;--[2011-11-18 14:54]"
              "</span></span>"))
      ($l (g t4 (p 'time))
          ($c "<time datetime=\"2011-11-18T06:54\">&lt;"
              "2011-11-18 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54\">"
              "[2011-11-18 14:54]</time>"))
      ($l (g "[2000-01-01 <> 13:13]" (p 'time))
          ($c "<time datetime=\"2000-01-01\">"
              "[2000-01-01 &lt;&gt;</time>")))))

(ert-deftest t--format-timestamp-int ()
  "Tests for `org-w3ctr--format-timestamp-int'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x opt) (t--format-timestamp-int (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 'none) "[2011-11-18 Fri]")
      ($l (g t1 'time)
          "<time datetime=\"2011-11-18\">[2011-11-18 Fri]</time>")
      ($l (g t2 'none) "&lt;2011-11-18 Fri 14:54&gt;")
      ($l (g t2 'time)
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "&lt;2011-11-18 Fri 14:54&gt;</time>"))
      ($l (g t3 'none) "[2011-11-18 Fri 06:54-14:54]")
      ($l (g t3 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">"
              "[2011-11-18 Fri 06:54-14:54]</time>"))
      ($l (g t4 'none)
          "&lt;2011-11-18 Fri 06:54&gt;--&lt;2011-11-18 Fri 14:54&gt;")
      ($l (g t4 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">&lt;"
              "2011-11-18 Fri 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54Z\">&lt;"
              "2011-11-18 Fri 14:54&gt;</time>"))
      ($l (g "[2000-01-01]--[2000-02-02 13:00]" 'time)
          ($c "<time datetime=\"2000-01-01\">[2000-01-01 Sat]"
              "</time>--<time datetime=\"2000-02-02\">"
              "[2000-02-02 Wed 13:00]</time>"))
      ($l (g "[2000-01-01 11:00]--[2000-01-02]" 'time)
          ($c "<time datetime=\"2000-01-01T11:00Z\">"
              "[2000-01-01 Sat 11:00]</time>--"
              "<time datetime=\"2000-01-02T11:00Z\">"
              "[2000-01-02 Sun 11:00]</time>")))))

(ert-deftest t--format-timestamp-fmt ()
  "Tests for `org-w3ctr--format-timestamp-fmt'"
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (p (m) `( :html-timestamp-wrapper none
                       :html-datetime-option T-none-zulu
                       :html-timezone 0
                       :html-timestamp-formats ,m))
             (g (x opt) (t--format-timestamp-fmt (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 '("%y" . "%y %m")) "[11]")
      ($l (g t1 '("%C" . "")) "[20]")
      ($l (g t1 '("%F %m" ' "%F %R")) "[2011-11-18 11]")
      ($l (g t1 '("[%F]")) "[2011-11-18]")
      ($l (g t1 '("<%F>")) "[2011-11-18]")
      ($l (g t2 '(nil . "%F %R")) "&lt;2011-11-18 14:54&gt;")
      ($l (g t2 '(nil . "%j")) "&lt;322&gt;")
      ($l (g t3 '(nil . "%F %R")) "[2011-11-18 06:54-14:54]")
      ($l (g t3 '(nil . "%D %U %W %V")) "[11/18/11 46 46 46-14:54]")
      ($l (g t4 '(nil . "%F %R"))
          "&lt;2011-11-18 06:54&gt;--&lt;2011-11-18 14:54&gt;")
      ($l (g t4 '(nil . "%M")) "&lt;54&gt;--&lt;54&gt;"))
    ($e!l (t--format-timestamp-fmt (f "[2000-01-01]") nil)
          '(org-w3ctr-error "Invalid timestamp formats: nil"))))

(ert-deftest t--format-timestamp-fix ()
  "Tests for `org-w3ctr--format-timestamp-fix'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x y opt) (t--format-timestamp-fix (f x) y (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 "%F%R" 'none) "2011-11-1800:00")
      ($l (g t1 "<%F%R>" 'none) "&lt;2011-11-1800:00&gt;")
      ($l (g t2 "%F" 'span)
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "2011-11-18</span></span>"))
      ($l (g t2 "%F %a" 'time)
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "2011-11-18 Fri</time>"))
      ($e!l (g t2 "%F" 'wtf)
            '(org-w3ctr-error "Unknown timestamp wrapper: wtf"))
      ($l (g t3 "{%F%a%R}" 'none)
          "{2011-11-18Fri06:54}--{2011-11-18Fri14:54}")
      ($e!l (g t3 "%a" 'abc)
            '(org-w3ctr-error "Unknown timestamp wrapper: abc"))
      ($l (g t3 "[%F%R]" 'span)
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-1806:54]--[2011-11-1814:54]</span></span>"))
      ($l (g t3 "<%F %R>" 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">"
              "&lt;2011-11-18 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54Z\">"
              "&lt;2011-11-18 14:54&gt;</time>"))
      ($l (g t3 "%F %R" 'none) (g t4 "%F %R" 'none))
      ($l (g t3 "<%F %R" 'span) (g t4 "<%F %R" 'span))
      ($l (g t3 "[%F%a%R]" 'time) (g t4 "[%F%a%R]" 'time)))))

(ert-deftest t--format-timestamp-org ()
  "Tests for `org-w3ctr--format-timestamp-org'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x opt) (t--format-timestamp-org (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      (dlet ((org-display-custom-times nil))
        ($l (g t1 'none) "[2011-11-18 Fri]")
        ($l (g t2 'span)
            ($c "<span class=\"timestamp-wrapper\">"
                "<span class=\"timestamp\">"
                "&lt;2011-11-18 Fri 14:54&gt;</span></span>"))
        ($l (g t3 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "[2011-11-18 Fri 06:54-14:54]</time>"))
        ($l (g t4 'none)
            ($c "&lt;2011-11-18 Fri 06:54&gt;"
                "--&lt;2011-11-18 Fri 14:54&gt;"))
        ($l (g t4 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "&lt;2011-11-18 Fri 06:54&gt;</time>--"
                "<time datetime=\"2011-11-18T14:54Z\">"
                "&lt;2011-11-18 Fri 14:54&gt;</time>")))
      (dlet ((org-display-custom-times t)
             (org-timestamp-custom-formats
              '("%m/%d/%y" . "%m/%d/%y %H:%M")))
        ($l (g t1 'none) "&lt;11/18/11&gt;")
        ($l (g t2 'span)
            ($c "<span class=\"timestamp-wrapper\">"
                "<span class=\"timestamp\">&lt;11/18/11 14:54&gt;"
                "</span></span>"))
        ($l (g t3 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "&lt;11/18/11 06:54&gt;</time>--"
                "<time datetime=\"2011-11-18T14:54Z\">"
                "&lt;11/18/11 14:54&gt;</time>"))
        ($l (g t3 'time) (g t4 'time))
        ($l (g t4 'none)
            "&lt;11/18/11 06:54&gt;--&lt;11/18/11 14:54&gt;")))))

(ert-deftest t--format-timestamp-cus ()
  "Tests for `org-w3ctr--format-timestamp-cus'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp))
             (p (w f) `( :html-timestamp-wrapper ,w
                         :html-datetime-option T-none-zulu
                         :html-timestamp-formats ,f
                         :with-special-strings t
                         :html-timezone 0))
             (g (x info) (t--format-timestamp-cus (f x) info)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 (p 'none '("[%F%a]"))) "[2011-11-18Fri]")
      ($l (g t1 (p 'none '("<%F%a>"))) "&lt;2011-11-18Fri&gt;")
      ($l (g t1 (p 'none '("{%F%a}"))) "2011-11-18Fri")
      ($e! (g t1 (p 'none '("%F%a"))))
      ($l (g t2 (p 'time '(nil . "{[%F %R]}")))
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "[2011-11-18 14:54]</time>"))
      ($l (g t2 (p 'none '(nil . "[[[[%R]]]]"))) "[[[[14:54]]]]")
      ($l (g t3 (p 'none '(nil . "<<<%F%R>")))
          ($c "&lt;&lt;&lt;2011-11-1806:54&gt;"
              "&#x2013;&lt;&lt;&lt;2011-11-1814:54&gt;"))
      ($l (g t3 (p 'none '(nil . "<%F%R>")))
          (g t4 (p 'none '(nil . "<%F%R>")))))))

(ert-deftest t-ts-default-format-function ()
  "Tests for `org-w3ctr-ts-default-format-function'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($it t-ts-default-format-function
        ($l (it (f t1) nil) t1)
        ($l (it (f t2) nil) t2)
        ($l (it (f t3) nil) t3)
        ($l (it (f t4) nil) t4)))))

(ert-deftest t--format-timestamp-fun ()
  "Tests for `org-w3ctr--format-timestamp-fun'."
  (cl-flet* ((f (s) (t-get-element s 'timestamp)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]")
          (info '(:html-timestamp-format-function (lambda (_a _b) "hello"))))
      ($it t--format-timestamp-fun
        ($l (it (f t1) info) "hello")
        ($l (it (f t2) info) "hello")
        ($l (it (f t3) info) "hello")
        ($l (it (f t4) info) "hello")
        ($e! (it (f t1) nil))))))

;; FIXME: make it more useful.
(ert-deftest t-timestamp ()
  "Tests for `org-w3ctr-timestamp'."
  (t-check-element-values
   #'t-timestamp
   '(("[2020-02-02]" "<time datetime=\"2020-02-02\">[2020-02-02]</time>")
     ("<2006-01-02>" "<time datetime=\"2006-01-02\">&lt;2006-01-02&gt;</time>")
     ("[2006-01-02 15:04:05]"
      "<time datetime=\"2006-01-02 15:04+0800\">[2006-01-02 15:04]</time>")
     ("<2006-01-02 15:04:05>"
      "<time datetime=\"2006-01-02 15:04+0800\">&lt;2006-01-02 15:04&gt;</time>")
     ("[2025-03-30]--[2025-03-31]"
      "<time datetime=\"2025-03-30\">[2025-03-30]</time>&#x2013;\
<time datetime=\"2025-03-31\">[2025-03-31]</time>")
     ("<2025-03-30>--<2025-03-31>"
      "<time datetime=\"2025-03-30\">&lt;2025-03-30&gt;</time>&#x2013;\
<time datetime=\"2025-03-31\">&lt;2025-03-31&gt;</time>")
     ("[2006-01-02 15:04:05]--[2006-01-03 04:05:06]"
      "<time datetime=\"2006-01-02 15:04+0800\">[2006-01-02 15:04]</time>\
&#x2013;<time datetime=\"2006-01-03 04:05+0800\">[2006-01-03 04:05]</time>")
     ("<2006-01-02 15:04:05>--<2006-01-03 04:05:06>"
      "<time datetime=\"2006-01-02 15:04+0800\">&lt;2006-01-02 15:04&gt;</time>\
&#x2013;<time datetime=\"2006-01-03 04:05+0800\">&lt;2006-01-03 04:05&gt;</time>")
     ("[2024-02-02]--<2025-02-02>"
      "<time datetime=\"2024-02-02\">[2024-02-02]</time>&#x2013;\
<time datetime=\"2025-02-02\">[2025-02-02]</time>")
     ("<2024-02-02>--[2025-02-02]"
      "<time datetime=\"2024-02-02\">&lt;2024-02-02&gt;</time>&#x2013;\
<time datetime=\"2025-02-02\">&lt;2025-02-02&gt;</time>")
     ("[2000-01-01]" "<time datetime=\"2000-01-01\">[2000-01-01]</time>"))
   nil '( :html-timestamp-option fmt :html-timestamp-wrapper time
          :html-timestamp-formats ("%F" . "%F %R")
          :html-timezone "UTC+8" :html-datetime-option s-none)))

;;;; Link

(ert-deftest t-inline-image-path-regexp ()
  "Tests for `org-w3ctr-inline-image-path-regexp'."
  (let ((case-fold-search t))
    (dolist (p '("img.png" "img.PNG" "img.jpeg" "img.jpg" "img.jfif"
                 "img.gif" "img.svg" "img.webp" "img.avif" "img.jxl"
                 "img.bmp" "img.ico" "img.apng"
                 "img.png?x=1" "img.png#frag"))
      ($s (string-match-p t-inline-image-path-regexp p)))
    (dolist (p '("img.png.txt" "img.tiff" "img.heic" "img.jp2"))
      ($n (string-match-p t-inline-image-path-regexp p)))))

(ert-deftest t--wrap-image ()
  "Tests for `org-w3ctr--wrap-image'."
  ($l (t--wrap-image "" nil "" "") "<figure>\n</figure>")
  ($l (t--wrap-image "hello" nil "" "")
      "<figure>\nhello</figure>")
  ($l (t--wrap-image "hello" nil " abc" "")
      "<figure>\nhello<figcaption>abc</figcaption>\n</figure>")
  ($l (t--wrap-image "" nil "\ntest\n" "1")
      "<figure1>\n<figcaption>test</figcaption>\n</figure>"))

(ert-deftest t-inline-image-p ()
  "Tests for `org-w3ctr-inline-image-p'."
  (let* ((info '(:html-inline-image-rules (("file" . "\\.png\\'"))))
         (p (lambda (s)
              (with-temp-buffer
                (org-mode)
                (insert s)
                (org-w3ctr-inline-image-p
                 (t-parse1 'link)
                 info)))))
    ($s (funcall p "[[file:img.png]]"))
    ($n (funcall p "[[https://example.com][ ]]"))
    ($n (funcall p "[[https://example.com][  x  ]]"))
    ($n (funcall p "[[https://example.com][]]"))
    ;; Description = white space + exactly one image link.
    ($s (with-temp-buffer
          (org-mode)
          (insert "[[https://example.com][file:img.png]]")
          (let ((tree (org-element-parse-buffer)))
            (org-export-insert-image-links
             tree info org-w3ctr-inline-image-rules)
            (let ((link (car (org-element-map tree 'link #'identity))))
              (org-element-set-contents
               link (cons " " (org-element-contents link)))
              (org-w3ctr-inline-image-p link info)))))))

(ert-deftest t--link-org-files-as-html ()
  "Tests for `org-w3ctr--link-org-files-as-html'."
  (let ((info '(:html-link-org-files-as-html t :html-extension "html")))
    ($l (t--link-org-files-as-html "foo.org" info) "foo.html")
    ($l (t--link-org-files-as-html "dir/foo.org" info) "dir/foo.html")
    ($l (t--link-org-files-as-html "foo.txt" info) "foo.txt"))
  ($l (t--link-org-files-as-html
       "foo.org" '(:html-link-org-files-as-html nil :html-extension "html"))
      "foo.org"))

(ert-deftest t--link-path ()
  "Tests for `org-w3ctr--link-path'."
  ;; No search option: the path is returned unchanged.
  (let* ((link (with-temp-buffer
                 (org-mode) (insert "[[file:other.org]]") (t-parse1 'link)))
         (info (list :html-link-org-files-as-html t :html-extension "html")))
    ($l (t--link-path link info) "other.html"))
  ;; Strict mode: #custom-id -> direct fragment.
  (let* ((link (with-temp-buffer
                 (org-mode) (insert "[[file:other.org::#cid]]") (t-parse1 'link)))
         (info (list :html-honor-ox-external-links nil
                     :html-link-org-files-as-html t :html-extension "html")))
    ($l (t--link-path link info) "other.html#cid"))
  ;; Strict mode: *heading / untyped fuzzy -> org-w3ctr-error.
  (dolist (opt '("*heading" "fuzzy"))
    (let* ((link (with-temp-buffer
                   (org-mode) (insert (format "[[file:other.org::%s]]" opt))
                   (t-parse1 'link)))
           (info (list :html-honor-ox-external-links nil
                       :html-link-org-files-as-html t :html-extension "html")))
      ($q (car (should-error (t--link-path link info))) 'org-w3ctr-error)))
  ;; Compatibility mode resolves through org-publish.
  (let* ((link (with-temp-buffer
                 (org-mode) (insert "[[file:other.org::*heading]]") (t-parse1 'link)))
         (info (list :html-honor-ox-external-links t
                     :html-link-org-files-as-html t :html-extension "html")))
    (cl-letf (((symbol-function 'org-publish-resolve-external-link)
               (lambda (option path &optional _prefer)
                 ($l option "*heading")
                 ($l path "other.org")
                 "FRAG")))
      ($l (t--link-path link info) "other.html#FRAG"))))

(ert-deftest t--link-to-file ()
  "Tests for `org-w3ctr--link-to-file'."
  (let ((info (list :html-link-org-files-as-html t :html-extension "html")))
    ($l (t--link-to-file "other.org" "xyz" "desc" "" info)
        "<a href=\"other.html#ID-xyz\">desc</a>")
    ($l (t--link-to-file "other.org" "xyz" nil "" info)
        "<a href=\"other.html#ID-xyz\">other.org</a>")))

(ert-deftest t--link-dispatch ()
  "Tests for `org-w3ctr--link-dispatch'."
  ;; Strict mode rejects a cross-file id: link.
  (let* ((link (with-temp-buffer
                 (org-mode) (insert "[[id:xyz]]") (t-parse1 'link)))
         (info (list :html-honor-ox-external-links nil
                     :id-alist '(("xyz" . "other.org")))))
    ($q (car (should-error (t--link-dispatch link nil info "")))
        'org-w3ctr-error))
  ;; Compatibility mode builds the ID- fragment via t--link-to-file.
  (let* ((link (with-temp-buffer
                 (org-mode) (insert "[[id:xyz]]") (t-parse1 'link)))
         (info (list :html-honor-ox-external-links t
                     :html-link-org-files-as-html t :html-extension "html"
                     :id-alist '(("xyz" . "other.org")))))
    ($l (t--link-dispatch link "desc" info "")
        "<a href=\"other.html#ID-xyz\">desc</a>")))

(ert-deftest t--link-equation ()
  "Tests for `org-w3ctr--link-equation'."
  (t-check-element-values
   #'t-link
   '(("#+name: eq\n\\begin{equation}\nx=1\n\\end{equation}\n\nSee [[eq]]."
      "\\eqref{eq}"))
   t '(:with-latex mathjax :html-prefer-user-labels t)))

(ert-deftest t--link-external ()
  "Tests for `org-w3ctr--link-external'."
  ($l (t--link-external "https://example.com" "desc" "")
      "<a href=\"https://example.com\">desc</a>")
  ($l (t--link-external "https://example.com" nil "")
      "<a href=\"https://example.com\">https://example.com</a>"))

(ert-deftest t-link ()
  "Tests for `org-w3ctr-link'."
  (t-check-element-values
   #'t-link
   '(("[[https://example.com][desc]]"
      "<a href=\"https://example.com\">desc</a>")
     ("[[https://example.com]]"
      "<a href=\"https://example.com\">https://example.com</a>")
     ("[[file:other.org][other]]"
      "<a href=\"other.html\">other</a>")
     ("[[file:img.png]]"
      "<img src=\"img.png\" alt=\"img.png\">")
     ;; Custom ID link to a headline.
     ("* Head\n:PROPERTIES:\n:CUSTOM_ID: custom\n:END:\n\nSee [[#custom]]."
      "<a href=\"#custom\">1</a>")
     ;; Fuzzy link to a target.
     ("A <<foo>> target. See [[foo]]."
      "<a href=\"#foo\">No description for this link</a>")
     ;; Fuzzy link to a named element.
     ("#+name: tab\n| a |\n\nSee [[tab]]."
      "<a href=\"#tab\">No description for this link</a>")
     ;; Radio target link.
     ("<<<radio>>>\n\nSee radio here."
      "<a href=\"#radio\">radio</a>"))
   t '(:with-latex verbatim :html-prefer-user-labels t)))

;;; Smallest objects

(ert-deftest t--get-markup-format ()
  "Tests for `org-w3ctr--get-markup-format'."
  (let ((info '(:html-text-markup-alist ((a . 2) (b . 3) (c . 4)))))
    ($l (t--get-markup-format 'a info) 2)
    ($l (t--get-markup-format 'b info) 3)
    ($l (t--get-markup-format 'c info) 4))
  ($l (t--get-markup-format 'anything nil) "%s"))

;;;; Bold

(ert-deftest t-bold ()
  "Tests for `org-w3ctr-bold'."
  (t-check-element-values
   #'t-bold
   '(("*abc*" "<b>abc</b>")
     ("**abc**"
      "<b><b>abc</b></b>"
      "<b>abc</b>")
     ("**" . nil)
     ("***" "<b>*</b>")
     ("****" "<b>**</b>")
     ("*****"
      "<b><b>*</b></b>"
      "<b>*</b>")
     ("*\\star\\star\\star*" "<b>***</b>")
     ("*hello world this world*"
      "<b>hello world this world</b>")
     ("*hello\nworld*" "<b>hello\nworld</b>"))))

;;;; Italic

(ert-deftest t-italic ()
  "Tests for `org-w3ctr-italic'."
  (t-check-element-values
   #'t-italic
   '(("/abc/" "<i>abc</i>")
     ("//abc//"
      "<i><i>abc</i></i>" "<i>abc</i>")
     ("//" . nil)
     ("///" "<i>/</i>")
     ("////" "<i>//</i>")
     ("/////"
      "<i><i>/</i></i>" "<i>/</i>")
     ("/\\slash\\slash\\slash/" "<i>///</i>")
     ("/hello world this world/"
      "<i>hello world this world</i>")
     ("/hello\nworld/" "<i>hello\nworld</i>"))))

;;;; Underline

(ert-deftest t-underline ()
  "Tests for `org-w3ctr-underline'."
  (t-check-element-values
   #'t-underline
   '(("_abc_" "<u>abc</u>")
     ("__abc__"
      "<u><u>abc</u></u>"
      "<u>abc</u>")
     ("__" . nil)
     ("___" "<u>_</u>")
     ("____" "<u>__</u>")
     ("_____"
      "<u><u>_</u></u>"
      "<u>_</u>")
     ("_\\under\\under\\under_"
      "<u>___</u>")
     ("_hello world this world_"
      "<u>hello world this world</u>")
     ("_hello\nworld_"
      "<u>hello\nworld</u>"))))

;;;; Verbatim

(ert-deftest t-verbatim ()
  "Tests for `org-w3ctr-verbatim'."
  (t-check-element-values
   #'t-verbatim
   '(("=abc=" "<code>abc</code>")
     ("==abc==" "<code>=abc=</code>")
     ("==" . nil)
     ("===" "<code>=</code>")
     ("====" "<code>==</code>")
     ("=====" "<code>===</code>")
     ("=\\slash\\slash\\slash="
      "<code>\\slash\\slash\\slash</code>")
     ("=hello world this world="
      "<code>hello world this world</code>")
     ("=hello\nworld=" "<code>hello\nworld</code>"))))

;;;; Code

(ert-deftest t-code ()
  "Tests for `org-w3ctr-code'."
  (t-check-element-values
   #'t-code
   '(("~abc~" "<code>abc</code>")
     ("~~abc~~" "<code>~abc~</code>")
     ("~~" . nil)
     ("~~~" "<code>~</code>")
     ("~~~~" "<code>~~</code>")
     ("~~~~~" "<code>~~~</code>")
     ("~\\slash\\slash\\slash~"
      "<code>\\slash\\slash\\slash</code>")
     ("~hello world this world~"
      "<code>hello world this world</code>")
     ("~hello\nworld~" "<code>hello\nworld</code>"))))

;;;; Strike-Through

(ert-deftest t-strike-through ()
  "Tests for `org-w3ctr-strike-through'."
  (t-check-element-values
   #'t-strike-through
   '(("+abc+" "<s>abc</s>")
     ("++abc++"
      "<s><s>abc</s></s>"
      "<s>abc</s>")
     ("++" . nil)
     ("+++" "<s>+</s>")
     ("++++" "<s>++</s>")
     ("+++++"
      "<s><s>+</s></s>"
      "<s>+</s>")
     ("+\\plus\\plus\\plus+" "<s>+++</s>")
     ("+hello world this world+"
      "<s>hello world this world</s>")
     ("+hello\nworld+" "<s>hello\nworld</s>"))))

;;;; Plain Text

(ert-deftest t--convert-special-strings ()
  "Tests for `org-w3ctr--convert-special-strings'."
  (dolist (a '(("hello..." . "hello&#x2026;")
               ("......" . "&#x2026;&#x2026;")
               ("\\\\-" . "\\&#x00ad;")
               ("---abc" . "&#x2014;abc")
               ("--abc" . "&#x2013;abc")))
    ($l (t--convert-special-strings (car a)) (cdr a))))

(ert-deftest t-plain-text ()
  "Tests for `org-w3ctr-plain-text'."
  ($l (t-plain-text "a < b & c > d" '())
      "a &lt; b &amp; c &gt; d")
  ($l (t-plain-text "\"hello\"" '(:with-smart-quotes t))
      "\"hello\"")
  ($l (t-plain-text "a -- b" '(:with-special-strings t))
      "a &#x2013; b")
  ($l (t-plain-text "line1\nline2" '(:preserve-breaks t))
      "line1<br>\nline2")
  ($l (t-plain-text
       "\"a < b\" -- c\nd"
       '( :with-smart-quotes t
          :with-special-strings t
          :preserve-breaks t))
      "\"a &lt; b\" &#x2013; c<br>\nd"))

;;; Headline and Section

;;;; Section

(ert-deftest t-section ()
  "Tests for `org-w3ctr-section'."
  (cl-letf (((symbol-function 't-section)
             (lambda (_s c _info) c)))
    (t-check-element-values
     #'t-section
     '(("123" "<p>123</p>\n")
       ("\n\n123" "<p>123</p>\n")
       ("#+a:b\n\n123 234\n\n" "<p>123 234</p>\n")
       ("123\n* test\n\n456" "<p>456</p>\n" "<p>123</p>\n"))))
  (t-check-element-values
   #'t-section
   '(("* test1\n\n123 234\n\n" "<p>123 234</p>\n")
     ("* test2\n\n#+a:b\n\n456\n" "<p>456</p>\n")
     ("* test3\n\n\n\n" . nil)))
  ;; The zeroth section returns nil and stores its output in INFO.
  (let* ((sec (t-get-element "zeroth" 'section))
         (info '(:html-toc-element ul)))
    ($n (t-section sec "<p>567</p>
" info))
    ($l (t--pget info :zeroth-section-output) "<p>567</p>
")))

;;;; Todo

(ert-deftest t--todo ()
  "Tests for `org-w3ctr--todo'."
  ($l (t--todo nil nil) nil)
  ($l (t--todo "TODO" nil)
      "<span class=\"todo TODO\">TODO</span>")
  ($l (t--todo "TODO" '(:html-todo-kwd-class-prefix "org1-"))
      "<span class=\"todo org1-TODO\">TODO</span>")
  (let ((org-done-keywords '("DONE")))
    ($l (t--todo "DONE" '(:html-todo-kwd-class-prefix "status-"))
        "<span class=\"done status-DONE\">DONE</span>"))
  ($l (t--todo "TODO" '(:html-todo-kwd-class-prefix "org-status-"))
      "<span class=\"todo org-status-TODO\">TODO</span>")
  (let ((org-done-keywords '("WTF")))
    ($l (t--todo "WTF" '(:html-todo-kwd-class-prefix "status-"))
        "<span class=\"done status-WTF\">WTF</span>"))
  ;; custom format function
  ($l (t--todo "TODO" '( :html-todo-format-function
                         (lambda (todo _info)
                           (format "<b>%s</b>" todo))))
      "<b>TODO</b>")
  ($l (t--todo "DONE" '( :html-todo-format-function
                         (lambda (todo _info)
                           (format "<i class=\"done\">%s</i>" todo))))
      "<i class=\"done\">DONE</i>"))

;;;; Priority

(ert-deftest t--priority ()
  "Tests for `org-w3ctr--priority'."
  ($l (t--priority nil nil) nil)
  ($l (t--priority 66 nil)
      "<span class=\"priority\">[B]</span>")
  ($l (t--priority 65 nil)
      "<span class=\"priority\">[A]</span>")
  ($l (t--priority 67 nil)
      "<span class=\"priority\">[C]</span>")
  ($e!l (t--priority -1 nil) '(error "Invalid priority value `-1'"))
  ;; custom format function
  ($l (t--priority 66 '(:html-priority-format-function
                        (lambda (p _i) (format "<i>%c</i>" p))))
      "<i>B</i>"))

;;;; Tags

(ert-deftest t--tags ()
  "Tests for `org-w3ctr--tags'."
  ($l (t--tags nil nil) nil)
  ($l (t--tags '("a") nil)
      "<span class=\"tag\"><span class=\"a\">a</span></span>")
  ($l (t--tags '("a" "b") nil)
      ($c "<span class=\"tag\"><span class=\"a\">a</span>&#xa0;"
          "<span class=\"b\">b</span></span>"))
  ($l (t--tags '("a" "b") '(:html-tag-class-prefix "org-tag-"))
      ($c "<span class=\"tag\"><span class=\"org-tag-a\">a</span>&#xa0;"
          "<span class=\"org-tag-b\">b</span></span>"))
  ;; custom format function
  ($l (t--tags '("a" "b") '(:html-tags-format-function
                            (lambda (tags _i) (string-join tags ","))))
      "a,b"))

;;;; Headline

(ert-deftest t--headline-todo ()
  "Tests for `org-w3ctr--headline-todo'."
  (t-check-element-values
   #'t--headline-todo
   '(("* TODO a" "TODO")
     ("* DONE b" "DONE")
     ("* c" nil)
     ;; The keyword goes through `org-export-data', so HTML-significant
     ;; characters in it are escaped.
     ("#+TODO: TODO WAIT<SEEN | DONE\n* WAIT<SEEN d" "WAIT&lt;SEEN"))
   t '(:with-todo-keywords t :with-toc nil))
  (t-check-element-values
   #'t--headline-todo
   '(("* TODO a" nil))
   t '(:with-todo-keywords nil :with-toc nil)))

(ert-deftest t--headline-priority ()
  "Tests for `org-w3ctr--headline-priority'."
  (t-check-element-values
   #'t--headline-priority
   '(("* [#A] a" 65)
     ("* [#1] b" 1)
     ("* c" nil))
   t '(:with-priority t :with-toc nil))
  (t-check-element-values
   #'t--headline-priority
   '(("* [#A] a" nil))
   t '(:with-priority nil :with-toc nil)))

(ert-deftest t--headline-tags ()
  "Tests for `org-w3ctr--headline-tags'."
  (t-check-element-values
   #'t--headline-tags
   '(("* a :x:" ("x"))
     ("* b :x:y:" ("x" "y"))
     ("* c" nil))
   t '(:with-tags t :with-toc nil))
  (t-check-element-values
   #'t--headline-tags
   '(("* a :x:" nil))
   t '(:with-tags nil :with-toc nil)))

(ert-deftest t--build-bare-headline ()
  "Tests for `org-w3ctr--build-bare-headline'."
  (t-check-element-values
   #'t--build-bare-headline
   '(("* TODO [#A] text :x:" "TODO|todo|65|text|(x)")
     ("* DONE b" "DONE|done|nil|b|nil")
     ("* text" "nil|nil|nil|text|nil"))
   t '(:with-todo-keywords t :with-priority t :with-tags t
                           :html-format-headline-function
                           (lambda (todo todo-type priority text tags _info)
                             (format "%s|%s|%s|%s|%s" todo todo-type priority text tags))
                           :with-toc nil))
  ;; A nil format function falls back to the default.
  (t-check-element-values
   #'t--build-bare-headline
   '(("* TODO a" "<span class=\"todo TODO\">TODO</span> a"))
   t '(:with-todo-keywords t :with-toc nil
                           :html-format-headline-function nil)))

(ert-deftest t--build-base-headline ()
  "Tests for `org-w3ctr--build-base-headline'."
  (t-check-element-values
   #'t--build-base-headline
   `(("* test" "test")
     ("* TODO test1" ,($c "<span class=\"todo org-status-TODO\">"
                          "TODO</span> test1"))
     ("* DONE test2" ,($c "<span class=\"done org-status-DONE\">"
                          "DONE</span> test2"))
     ("* [#1] test3" "<span class=\"priority\">[1]</span> test3")
     ("* [#A] test4" "<span class=\"priority\">[A]</span> test4")
     ("* test5 :a:" ,($c "test5&#xa0;&#xa0;&#xa0;<span class=\"tag\">"
                         "<span class=\"a\">a</span></span>"))
     ("* test6 :a:b" "test6 :a:b")
     ("* test6 :a:b:" ,($c "test6&#xa0;&#xa0;&#xa0;<span class="
                           "\"tag\"><span class=\"a\">a</span>&#xa0;"
                           "<span class=\"b\">b</span></span>"))
     ("* TODO [#F] test7 :tag1:tag2:"
      ,($c "<span class=\"todo org-status-TODO\">TODO</span> "
           "<span class=\"priority\">[F]</span> "
           "test7&#xa0;&#xa0;&#xa0;<span class=\"tag\">"
           "<span class=\"tag1\">tag1</span>&#xa0;<span class=\"tag2\">tag2</span></span>")))
   t '(:html-format-headline-function
       t-format-headline-default-function
       :with-todo-keywords t :with-priority t :with-tags t
       :html-todo-kwd-class-prefix "org-status-"
       :html-tags-format-function t-tags-default-format-function)))

(ert-deftest t--get-headline-hlevel ()
  "Tests for `org-w3ctr--get-headline-hlevel'."
  ($it t--get-headline-hlevel
    (cl-flet ((f (str) (t-get-parsed-elements str 'headline)))
      ;; Bare plist: :headline-offset 0, so relative = absolute level.
      ;; Inputs are rooted at level 1 to match real exports.
      (let* ((i '(:html-toplevel-hlevel 2))
             (g (lambda (h) (it h i))))
        ($l (mapcar g (f "* 123")) '(2))
        ($l (mapcar g (f "* a\n* b\n**** c\n***** d\n* e\n**** f"))
            (mapcar #'1+ '(1 1 4 5 1 4)))
        ($l (mapcar g (f "* a\n** b\n*** c\n**** d\n***** e\n****** f"))
            '(2 3 4 5 6 7)))
      ;; Boundary: 6 is the last valid value; 1 and 7 are rejected.
      ($l (it (car (f "* a")) '(:html-toplevel-hlevel 6)) 6)
      ($e!l (it (car (f "* a")) '(:html-toplevel-hlevel 1))
            '(org-w3ctr-error "Invalid HTML top level: 1"))
      ($e!l (it (car (f "* a")) '(:html-toplevel-hlevel 7))
            '(org-w3ctr-error "Invalid HTML top level: 7"))
      (t-check-element-values
       't--get-headline-hlevel
       '(("** a\n** b\n" 2 2 2 2)
         ("* a\n** b\n*** c\n" 2 2 3 3 4 4)
         ("* a\n* b\n**** c\n" 2 2 5 5 2 2))
       t '( :html-toplevel-hlevel 2
            :with-toc nil)))))

(ert-deftest t--low-level-headline-p ()
  "Tests for `org-w3ctr--low-level-headline-p'."
  ($it t--low-level-headline-p
    (let ((i1 '( :html-toplevel-hlevel 2
                 :html-honor-ox-headline-levels nil))
          (i2 '( :html-toplevel-hlevel 2
                 :html-honor-ox-headline-levels t
                 :headline-levels 4)))
      (cl-flet ((f (str) (t-get-element str 'headline)))
        ;; i1
        ($l (it (f "* a") i1) nil)
        ($l (it (f "** a") i1) nil)
        ($l (it (f "*** a") i1) nil)
        ($l (it (f "**** a") i1) nil)
        ($l (it (f "***** a") i1) nil)
        ($l (it (f "****** a") i1) t)
        ;; i2
        ($l (it (f "* a") i2) nil)
        ($l (it (f "** a") i2) nil)
        ($l (it (f "*** a") i2) nil)
        ($l (it (f "**** a") i2) nil)
        ($l (it (f "***** a") i2) t)
        ($l (it (f "****** a") i2) t))))
  (t-check-element-values
   #'t--low-level-headline-p
   '(("* a\n** b\n*** c\n**** d\n***** e\n" nil nil nil t t)
     ("** a\n*** b\n**** c\n***** d\n****** e\n" nil nil nil t t))
   t '( :html-honor-ox-headline-levels t
        :headline-levels 3
        :with-toc nil)))

(ert-deftest t--build-low-level-headline ()
  "Tests for `org-w3ctr--build-low-level-headline'."
  ($it t--build-low-level-headline
    (cl-letf* (((symbol-function 'org-export-numbered-headline-p)
                (lambda (_h _i) t))
               ((symbol-function 't--build-base-headline)
                (lambda (_h _i) "test"))
               ((symbol-function 't--reference)
                (lambda (_h _i) "0"))
               ((symbol-function 'org-export-first-sibling-p)
                (lambda (_h _i) t))
               ((symbol-function 'org-export-last-sibling-p)
                (lambda (_h _i) t)))
      ;; Bare item: id on the <li>, text unwrapped.
      ($l (it nil nil nil)
          "<ol>\n<li id=\"0\">test</li>\n</ol>\n")
      ;; With contents: trimmed, after a <br>, </li> on its own line.
      ($l (it nil "a" nil)
          "<ol>\n<li id=\"0\">test<br>\na\n</li>\n</ol>\n")
      ;; Classes: :HTML_CONTAINER_CLASS: on the <li>, :HTML_HEADLINE_CLASS:
      ;; wrapping the text in a <span>.
      (cl-flet ((hl (props)
                  (t-get-element
                   (concat "* x\n:PROPERTIES:\n" props ":END:\n")
                   'headline)))
        ($l (it (hl ":HTML_CONTAINER_CLASS: cc\n") nil nil)
            "<ol>\n<li id=\"0\" class=\"cc\">test</li>\n</ol>\n")
        ($l (it (hl ":HTML_HEADLINE_CLASS: hc\n") nil nil)
            "<ol>\n<li id=\"0\"><span class=\"hc\">test</span></li>\n</ol>\n")
        ($l (it (hl ":HTML_CONTAINER_CLASS: cc\n:HTML_HEADLINE_CLASS: hc\n") nil nil)
            ($c "<ol>\n<li id=\"0\" class=\"cc\">"
                "<span class=\"hc\">test</span></li>\n</ol>\n"))
        ($l (it (hl ":HTML_CONTAINER_CLASS: cc\n:HTML_HEADLINE_CLASS: hc\n") "cont" nil)
            ($c "<ol>\n<li id=\"0\" class=\"cc\">"
                "<span class=\"hc\">test</span><br>\ncont\n</li>\n</ol>\n")))))
  (let ((counter 0))
    (cl-letf (((symbol-function 't--reference)
               (lambda (n i &optional b)
                 (when (org-element-type-p n 'headline)
                   (number-to-string (incf counter))))))
      (t-check-element-values
       #'t--build-low-level-headline
       `((,($c "* a\n** b\n*** c\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n"
               "*** d\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\nabc\n"
               "*** e\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n")
          "<li id=\"3\">e</li>\n</ul>\n"
          "<li id=\"2\">d<br>\n<p>abc</p>\n</li>\n"
          "<ul>\n<li id=\"1\">c</li>\n"))
       t '( :html-honor-ox-headline-levels t
            :headline-levels 2
            :html-format-headline-function
            t-format-headline-default-function)))))

(ert-deftest t-heading-default-format-function ()
  "Tests for `org-w3ctr-heading-default-format-function'."
  ;; Full: secno + title in <hN>, self-link beside, class on <hN>.
  (cl-letf* (((symbol-function 't--headline-secno)
              (lambda (_h _i) "<span class=\"secno\">1. </span>"))
             ((symbol-function 't--headline-self-link)
              (lambda (_id _i) "<a class=\"self-link\"></a>\n")))
    ($l (t-heading-default-format-function
         'hl "title" "h2" "id" "cls" nil)
        ($c "<div class=\"header-wrapper\">\n"
            "<h2 class=\"cls\">"
            "<span class=\"secno\">1. </span>title</h2>\n"
            "<a class=\"self-link\"></a>\n"
            "</div>\n")))
  ;; Minimal: no secno, no self-link, no class.
  (cl-letf* (((symbol-function 't--headline-secno) (lambda (_h _i) nil))
             ((symbol-function 't--headline-self-link)
              (lambda (_id _i) nil)))
    ($l (t-heading-default-format-function
         'hl "title" "h3" "id" nil nil)
        ($c "<div class=\"header-wrapper\">\n"
            "<h3>title</h3>\n"
            "</div>\n"))))

(ert-deftest t--headline-container ()
  "Tests for `org-w3ctr--headline-container'."
  (t-check-element-values
   #'t--headline-container
   '(("* abc" "section")) t '(:html-container "section"))
  (t-check-element-values
   #'t--headline-container
   '(("* abc" "article")) t '(:html-container "article"))
  (t-check-element-values
   #'t--headline-container
   '(("* abc" "div")) t '(:html-container nil))
  (t-check-element-values
   #'t--headline-container
   '(("* abc\n:PROPERTIES:\n:HTML_CONTAINER: aside\n:END:\n" "aside"))
   t '(:html-container nil))
  ;; :HTML_CONTAINER: overrides :html-container.
  (t-check-element-values
   #'t--headline-container
   '(("* abc\n:PROPERTIES:\n:HTML_CONTAINER: aside\n:END:\n" "aside"))
   t '(:html-container "section")))

(ert-deftest t--headline-self-link ()
  "Tests for `org-w3ctr--headline-self-link'."
  ($it t--headline-self-link
    (let ((i1 '(:html-self-link-headlines t))
          (i2 '(:html-self-link-headlines nil))
          (i3 '()))
      ($l (it "0" i1) ($c "<a class=\"self-link\" href=\"#0\" "
                          "aria-label=\"Link to this section\"></a>\n"))
      ($l (it "0" i2) nil)
      ;; An absent :html-self-link-headlines is falsy too.
      ($l (it "0" i3) nil))))

(ert-deftest t--headline-secno ()
  "Tests for `org-w3ctr--headline-secno'."
  ($it t--headline-secno
    (cl-letf (((symbol-function 'org-export-numbered-headline-p)
               (lambda (_h _i) t))
              ((symbol-function 'org-export-get-headline-number)
               (lambda (_h _i) '(1 1 4 5 1 4))))
      ($l (it nil nil) "<span class=\"secno\">1.1.4.5.1.4. </span>"))
    ;; :PROPERTIES:\n:UNNUMBERED: t\n:END:
    (t-check-element-values
     #'t--headline-secno
     '(("* a\n** b\n*** c\n"
        "<span class=\"secno\">1. </span>"
        "<span class=\"secno\">1.1. </span>"
        "<span class=\"secno\">1.1.1. </span>")
       ("* a\n** b\n\n*** c\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n"
        "<span class=\"secno\">1. </span>"
        "<span class=\"secno\">1.1. </span>" nil)
       ("* a\n** b\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n*** c\n"
        "<span class=\"secno\">1. </span>" nil nil)
       ("* a\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n** b\n*** c\n"
        nil nil nil))
     t '( :with-toc nil))))

(ert-deftest t--headline-hN ()
  "Tests for `org-w3ctr--headline-hN'."
  ($it t--headline-hN
    (cl-letf (((symbol-function 't--get-headline-hlevel)
               (lambda (_h _i) 3)))
      ($l (it nil nil) "h3"))
    (cl-letf (((symbol-function 't--get-headline-hlevel)
               (lambda (_h _i) 7)))
      ($l (it nil nil) "h6")))
  (t-check-element-values
   #'t--headline-hN
   '(("* a\n** b\n*** c\n**** d\n***** e\n****** f\n"
      "h2" "h3" "h4" "h5" "h6"))
   t '( :html-honor-ox-headline-levels nil
        :html-toplevel-hlevel 2)))

(ert-deftest t--build-normal-headline ()
  "Tests for `org-w3ctr--build-normal-headline'."
  (cl-letf (((symbol-function 't--headline-secno)
             (lambda (_h _i) "1. "))
            ((symbol-function 't--headline-hN)
             (lambda (_h _i) "h2"))
            ((symbol-function 't--build-base-headline)
             (lambda (_h _i) "text"))
            ((symbol-function 't--reference)
             (lambda (_h _i &optional n) "pid"))
            ((symbol-function 't--headline-container)
             (lambda (_h _i) "section"))
            ((symbol-function 'org-element-property)
             (lambda (p _h)
               (pcase p
                 (`:HTML_CONTAINER_CLASS "c1")
                 (`:HTML_HEADLINE_CLASS "c2")
                 (_ "xx"))))
            ((symbol-function 't--headline-self-link)
             (lambda (_id _info) "<a href=\"#x\"></a>\n")))
    ($l (t--build-normal-headline nil nil nil)
        ($c "<section id=\"pid\">\n<div class=\"header-wrapper\">\n"
            "<h2>1. text</h2>\n<a href=\"#x\"></a>\n"
            "</div>\n</section>\n"))
    ($l (t--build-normal-headline nil "<p>hello world</p>\n" nil)
        ($c "<section id=\"pid\">\n<div class=\"header-wrapper\">\n"
            "<h2>1. text</h2>\n<a href=\"#x\"></a>\n"
            "</div>\n<p>hello world</p>\n</section>\n")))
  ;; :PROPERTIES:\n:UNNUMBERED:t\n:CUSTOM_ID:1\n:END:\n
  (t-check-element-values
   #'t--build-normal-headline
   `(("* a\n:PROPERTIES:\n:UNNUMBERED: t\n:CUSTOM_ID: 1\n:END:\n"
      ,($c "<section id=\"1\">\n<div class=\"header-wrapper\">\n"
           "<h2>a</h2>\n<a class=\"self-link\" href=\"#1\" "
           "aria-label=\"Link to this section\"></a>\n</div>\n</section>\n"))
     ("* a\n:PROPERTIES:\n:UNNUMBERED: t\n:CUSTOM_ID: 1\n:END:\n123456"
      ,($c "<section id=\"1\">\n<div class=\"header-wrapper\">\n"
           "<h2>a</h2>\n<a class=\"self-link\" href=\"#1\" "
           "aria-label=\"Link to this section\"></a>\n</div>\n"
           "<p>123456</p>\n</section>\n")))))

(ert-deftest t-headline ()
  "Tests for `org-w3ctr-headline'."
  ($it t-headline
    ;; Low-level headline is rendered as a list item.
    (cl-letf* (((symbol-function 't--low-level-headline-p)
                (lambda (_h _i) t))
               ((symbol-function 't--build-low-level-headline)
                (lambda (_h _c _i) "LOW"))
               ((symbol-function 't--build-normal-headline)
                (lambda (_h _c _i) "NORMAL")))
      ($l (it '(headline nil) "contents" 'info) "LOW"))
    ;; Normal headline is rendered as a section.
    (cl-letf* (((symbol-function 't--low-level-headline-p)
                (lambda (_h _i) nil))
               ((symbol-function 't--build-low-level-headline)
                (lambda (_h _c _i) "LOW"))
               ((symbol-function 't--build-normal-headline)
                (lambda (_h _c _i) "NORMAL")))
      ($l (it '(headline nil) "contents" 'info) "NORMAL"))
    ;; A footnote section yields nil without reaching the builders.
    (cl-letf* (((symbol-function 't--low-level-headline-p)
                (lambda (&rest _) (error "unexpected")))
               ((symbol-function 't--build-low-level-headline)
                (lambda (&rest _) (error "unexpected")))
               ((symbol-function 't--build-normal-headline)
                (lambda (&rest _) (error "unexpected"))))
      ($l (it '(headline (:footnote-section-p t)) "contents" 'info) nil))))

;;; Template and Inner Template

;;;; <meta> tags export.

(ert-deftest t--build-meta-entry ()
  "Tests for `org-w3ctr--build-meta-entry'."
  ($it t--build-meta-entry
    ($l (it "name" "author")
        "<meta name=\"author\">\n")
    ($l (it "property" "og:title" "My Title")
        "<meta property=\"og:title\" content=\"My Title\">\n")
    ($l (it "name" "description" "Version %s" "1.0")
        "<meta name=\"description\" content=\"Version 1.0\">\n")
    ($l (it "name" "quote" "He said \"Hello\"")
        "<meta name=\"quote\" content=\"He said &quot;Hello&quot;\">\n")
    ($l (it "name" "chars" "a & b < c > d")
        "<meta name=\"chars\" content=\"a &amp; b &lt; c &gt; d\">\n")
    ($l (it "name" "version" "v%s.%s" "1" "2")
        "<meta name=\"version\" content=\"v1.2\">\n")
    ($l (it "name" "version" "'%s'" "v1.2")
        "<meta name=\"version\" content=\"&apos;v1.2&apos;\">\n")))

(ert-deftest t--get-info-file-timestamp ()
  "Tests for `org-w3ctr--get-info-file-timestamp'."
  ($n (t--get-info-file-timestamp nil))
  ($e!l
   (t--get-info-file-timestamp '( :time-stamp-file t
                                  :html-file-timestamp-function nil))
   '(org-w3ctr-error "Invalid file timestamp function: nil"))
  ($e!l
   (t--get-info-file-timestamp '( :time-stamp-file t
                                  :html-file-timestamp-function "bad"))
   '(org-w3ctr-error "Invalid file timestamp function: bad"))
  (t-check-element-values
   #'t--get-info-file-timestamp
   `(("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t))
     ("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t))
     ("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t)))
   nil
   '( :html-file-timestamp-function t-file-timestamp-default-function
      :time-stamp-file t)))

(ert-deftest t--ensure-charset-utf8 ()
  "Tests for `org-w3ctr--ensure-charset-utf8'."
  (cl-labels ((test (x) (let ((org-w3ctr-coding-system x))
                          (t--ensure-charset-utf8))))
    ($e!l (test nil) '(t-error "Invalid coding system: nil"))
    ($l (test 'utf-8-unix) "utf-8")
    ($l (test 'utf-8-dos) "utf-8")
    ($l (test 'utf-8-mac) "utf-8")
    ($e!l (test 'gbk) '(t-error "Invalid coding system: gbk"))
    ($e!l (test 'chinese-gbk)
          '(t-error "Invalid coding system: chinese-gbk"))
    ($e!l (test 'big5) '(t-error "Invalid coding system: big5"))
    ($e!l (test 'utf-7) '(t-error "Invalid coding system: utf-7"))
    ($e!l (test 'gb18030) '(t-error "Invalid coding system: gb18030"))
    ($e!l (test 'iso-latin-2)
          '(t-error "Invalid coding system: iso-latin-2"))
    ($e!l (test 'japanese-shift-jis)
          '(t-error "Invalid coding system: japanese-shift-jis"))
    ($e!l (test 'japanese-iso-8bit)
          '(t-error "Invalid coding system: japanese-iso-8bit"))
    ($l (test 'cp65001) "utf-8")
    ($e!l (test 'wtf) '(t-error "Invalid coding system: wtf"))
    ($e!l (test "UTF-8") '(t-error "Invalid coding system: UTF-8"))
    ($e!l (test [1]) '(t-error "Invalid coding system: [1]"))))

(ert-deftest t--build-viewport-options ()
  "Tests for `org-w3ctr--build-viewport-options'."
  ($n (t--build-viewport-options nil))
  (cl-flet ((f (ls) (let ((info `(:html-viewport ,ls)))
                      (t--build-viewport-options info))))
    ($l (f '(("a" ""))) nil)
    ($l (f '(("a" "b")))
        "<meta name=\"viewport\" content=\"a=b\">\n")
    ($l (f '(("a" "b") ("b" "") ("c" "d")))
        "<meta name=\"viewport\" content=\"a=b, c=d\">\n")
    ($n (f '(("a" nil) ("b" nil) ("c" "  ")))))
  (t-check-element-values
   #'t--build-viewport-options
   `(("" ,($c "<meta name=\"viewport\" content=\"width=device-width,"
              " initial-scale=1\">\n")))
   nil '(:html-viewport ((width "device-width")
                         (initial-scale "1")
                         (minimum-scale "")
                         (maximum-scale "")
                         (user-scalable "")))))

(ert-deftest t--get-info-title-raw ()
  "Tests for `org-w3ctr--get-info-title-raw'."
  (t-check-element-values
   #'t--get-info-title-raw
   '(("#+title: he" "he")
     ("#+title:he" "he")
     ("#+title: \t" "&lrm;")
     ("#+title: \s\s\t" "&lrm;")
     ("#+title:   3   " "3")
     ;; zero width space
     ("#+title:​" "​")
     ("#+TITLE: hello\sworld" "hello world")
     ("#+title: a & b" "a &amp; b"))))

(ert-deftest t--get-info-author-raw ()
  "Tests for `org-w3ctr--get-info-author-raw'."
  ($it t--get-info-author-raw
    ($n (it nil))
    (let ((info '(:with-author nil)))
      ($n (it info)))
    (let ((info '(:with-author nil :author "test")))
      ($n (it info)))
    (let ((info '(:with-author t :author "test")))
      ($l (it info) "test"))
    (let ((info '(:with-author t :author "  ")))
      ($n (it info)))
    (let ((info '(:with-author t :author "a & b")))
      ($l (it info) "a & b"))))

(ert-deftest t-meta-tags-default ()
  "Tests for `org-w3ctr-meta-tags-default'."
  (let ((info-with-author '(:with-author t :author ("Alice")))
        (info-with-desc '(:description "Test doc"))
        (info-with-keywords '(:keywords "org, test"))
        (info-empty '()))
    ($it t-meta-tags-default
      ($l (it info-with-author)
          '(("name" "author" "Alice") nil nil
            ("name" "generator" "Org Mode")))
      ($l (it info-with-desc)
          '(nil ("name" "description" "Test doc") nil
                ("name" "generator" "Org Mode")))
      ($l (it info-with-keywords)
          '(nil nil ("name" "keywords" "org, test")
                ("name" "generator" "Org Mode")))
      ($l (it info-empty)
          '(nil nil nil ("name" "generator" "Org Mode"))))))

(ert-deftest t--build-meta-tags ()
  "Tests for `org-w3ctr--build-meta-tags'."
  (let ((t-meta-tags '(("a" "b" "test"))))
    ($l (t--build-meta-tags nil) "<meta a=\"b\" content=\"test\">\n"))
  (let ((t-meta-tags '(("a" "b" nil))))
    ($l (t--build-meta-tags nil) "<meta a=\"b\">\n"))
  (let ((t-meta-tags '(("a" "b" "c") nil ("d" "e" "f"))))
    ($l (t--build-meta-tags nil)
        ($c "<meta a=\"b\" content=\"c\">\n"
            "<meta d=\"e\" content=\"f\">\n")))
  (let ((t-meta-tags #'t-meta-tags-default))
    ($l (t--build-meta-tags '(:with-author t :author "Alice"))
        ($c "<meta name=\"author\" content=\"Alice\">\n"
            "<meta name=\"generator\" content=\"Org Mode\">\n"))))

(ert-deftest t--build-meta-info ()
  "Tests for `org-w3ctr--build-meta-info'."
  (let ((t-meta-tags '(("name" "generator" "Org Mode"))))
    (cl-letf (((symbol-function 't--get-info-file-timestamp)
               (lambda (_info) "2026-01-01T00:00Z")))
      ($l (t--build-meta-info '(:title "Test"))
          ($c "<!-- 2026-01-01T00:00Z -->\n"
              "<meta charset=\"utf-8\">\n"
              "<title>Test</title>\n"
              "<meta name=\"generator\" content=\"Org Mode\">\n")))
    ($l (t--build-meta-info '(:title "Test"))
        ($c "<meta charset=\"utf-8\">\n"
            "<title>Test</title>\n"
            "<meta name=\"generator\" content=\"Org Mode\">\n"))
    ($l (t--build-meta-info '(:title "Test" :html-viewport (("a" "b"))))
        ($c "<meta charset=\"utf-8\">\n"
            "<meta name=\"viewport\" content=\"a=b\">\n"
            "<title>Test</title>\n"
            "<meta name=\"generator\" content=\"Org Mode\">\n"))))

;;;; Default CSS export.

(ert-deftest t--load-css ()
  "Tests for `org-w3ctr--load-css'."
  (let ((t-style nil) (t-style-file nil) (t--style-cache nil))
    ($q (t--load-css nil) nil))
  (let ((t-style "123") (t--style-cache nil))
    ($l (t--load-css nil) "123"))
  (let ((t-style "")
        (t-style-file nil)
        (t--style-cache nil))
    ($l (t--load-css nil) nil))
  (let ((t-style "    \t")
        (t-style-file nil)
        (t--style-cache nil))
    ($l (t--load-css nil) nil))
  ;; user CSS takes precedence over the cache
  (let ((t-style "user") (t--style-cache "cached"))
    ($l (t--load-css nil) "user"))
  ;; a non-whitespace cache is returned without touching t--load-file
  (let ((t-style nil) (t--style-cache "<style>cached</style>"))
    ($l (t--load-css nil) "<style>cached</style>"))
  (cl-letf (((symbol-function 't--load-file)
             #'identity)
            (t-style "")
            (t--style-cache nil))
    ($l (t--load-css nil)
        (format "<style>\n%s\n</style>\n" t-style-file))
    ($l t--style-cache (format "<style>\n%s\n</style>\n" t-style-file))))

;;;; Math config

(ert-deftest t-math-head-default-function ()
  "Tests for `org-w3ctr-math-head-default-function'."
  ($l (t-math-head-default-function '(:with-latex nil)) "")
  ($l (t-math-head-default-function '(:with-latex verbatim)) "")
  ($l (t-math-head-default-function '(:with-latex mathml-by-mathjax)) "")
  ($l (t-math-head-default-function '(:with-latex svg-by-mathjax))
      t-svg-math-style)
  ($l (t-math-head-default-function
       '(:with-latex mathjax :html-mathjax-config "JX"))
      "JX"))

(ert-deftest t--build-math-config ()
  "Tests for `org-w3ctr--build-math-config'."
  (let ((f '(:html-math-head-function t-math-head-default-function)))
    ($l (t--build-math-config (append f '(:with-latex nil))) "")
    ($l (t--build-math-config
         (append f '(:with-latex mathjax :html-mathjax-config "JX")))
        "JX")
    ($l (t--build-math-config
         '(:with-latex custom :html-math-head-function (lambda (_i) "H")))
        "H")
    ;; nil :html-math-head-function falls back to the default function
    ($l (t--build-math-config '(:with-latex mathjax :html-mathjax-config "JX"))
        "JX")))

;;;; Rest of <head>

(ert-deftest t--use-default-style-p ()
  "Tests for `org-w3ctr--use-default-style-p'."
  ($n (t--use-default-style-p nil))
  ($s (t--use-default-style-p '(:html-head-include-style t))))

(ert-deftest t--has-math-p ()
  "Tests for `org-w3ctr--has-math-p'."
  (cl-flet ((mkinfo (str) `( :with-latex t
                             :parse-tree
                             ,(with-temp-buffer
                                (save-excursion (insert str))
                                (org-element-parse-buffer)))))
    ($n (t--has-math-p (mkinfo "123")))
    ($n (t--has-math-p (mkinfo "$1+2")))
    ($s (t--has-math-p (mkinfo "$1+2$")))
    ($s (t--has-math-p (mkinfo "\\(1+2\\)")))
    ($s (t--has-math-p (mkinfo "\\[1+2\\]")))
    ($s (t--has-math-p (mkinfo "\\begin_equation\n123\n\\end_equation")))))

(ert-deftest t--normalize-string-or-function ()
  "Tests for `org-w3ctr--normalize-string-or-function'."
  ($l (t--normalize-string-or-function "abc") "abc\n")
  ($l (t--normalize-string-or-function "abc\n") "abc\n")
  ($n (t--normalize-string-or-function nil))
  ($l (t--normalize-string-or-function (lambda () "fn")) "fn\n")
  ($n (t--normalize-string-or-function (lambda () nil)))
  ($l (t--normalize-string-or-function (lambda () 42)) "42\n"))

(ert-deftest t--build-head ()
  "Tests for `org-w3ctr--build-head'."
  (cl-letf (((symbol-function 't--build-meta-info)
             (lambda (_info) "META\n"))
            ((symbol-function 't--use-default-style-p)
             (lambda (_info) t))
            ((symbol-function 't--load-css)
             (lambda (_info) "CSS\n"))
            ((symbol-function 't--has-math-p)
             (lambda (_info) t))
            ((symbol-function 't--build-math-config)
             (lambda (_info) "MATH\n")))
    ($l (t--build-head '(:html-head "H\n" :html-head-extra "E\n"))
        ($c "<head>\n" "META\n" "CSS\n" "MATH\n" "H\n" "E\n" "</head>\n")))
  (cl-letf (((symbol-function 't--build-meta-info)
             (lambda (_info) "META\n"))
            ((symbol-function 't--use-default-style-p)
             (lambda (_info) nil))
            ((symbol-function 't--has-math-p)
             (lambda (_info) nil)))
    ($l (t--build-head nil)
        ($c "<head>\n" "META\n" "</head>\n")))
  ;; :html-head can be a function of INFO
  (cl-letf (((symbol-function 't--build-meta-info)
             (lambda (_info) "META\n"))
            ((symbol-function 't--use-default-style-p)
             (lambda (_info) nil))
            ((symbol-function 't--has-math-p)
             (lambda (_info) nil)))
    ($l (t--build-head '(:html-head (lambda (_info) "FN\n")))
        ($c "<head>\n" "META\n" "FN\n" "</head>\n"))))

;;;; Navbar

(ert-deftest t--format-home/up ()
  "Tests for `org-w3ctr--format-home/up'."
  ;; The first %s is UP, the second HOME.
  ($l (t--format-home/up "%s|%s" "u" "h") "u|h")
  ;; Links go in verbatim, without HTML escaping.
  ($l (t--format-home/up "%s %s" "<u>" "&h") "<u> &h")
  ;; A literal %% survives.
  ($l (t--format-home/up "100%% %s" "u" "h") "100% u")
  ;; A non-string FMT signals `org-w3ctr-error' with our own message.
  ($e!l (t--format-home/up nil "u" "h")
        '(org-w3ctr-error "Invalid :html-home/up-format: nil"))
  ($e!l (t--format-home/up 'foo "u" "h")
        '(org-w3ctr-error "Invalid :html-home/up-format: foo"))
  ;; `format' rejections are wrapped in `org-w3ctr-error' too.  Assert
  ;; the symbol only: the message text is `format''s own and may drift.
  ($q (car (should-error (t--format-home/up "100%" "u" "h")))
      'org-w3ctr-error)
  ($q (car (should-error (t--format-home/up "%q%s" "u" "h")))
      'org-w3ctr-error))

(ert-deftest t--format-legacy-navbar ()
  "Tests for `org-w3ctr--format-legacy-navbar'."
  ;; Missing and blank links both mean "no bar".
  ($n (t--format-legacy-navbar '(:html-home/up-format "%s|%s")))
  ($n (t--format-legacy-navbar '(:html-link-up " " :html-link-home "\t")))
  (let ((info '(:html-link-up "" :html-link-home "")))
    ($n (t--format-legacy-navbar info)))
  (let ((info `( :html-link-up "1" :html-link-home "2"
                 :html-home/up-format ,t-home/up-format)))
    ($l (t--format-legacy-navbar info) "\
<nav id=\"navbar\">\n <a href=\"1\"> UP </a>
 <a href=\"2\"> HOME </a>\n</nav>\n")
    (setq info (plist-put info :html-link-home ""))
    ($l (t--format-legacy-navbar info) "\
<nav id=\"navbar\">\n <a href=\"1\"> UP </a>
 <a href=\"1\"> HOME </a>\n</nav>\n")
    (setq info (plist-put info :html-link-up ""))
    (setf (plist-get info :html-link-home) "2")
    ($l (t--format-legacy-navbar info) "\
<nav id=\"navbar\">\n <a href=\"2\"> UP </a>
 <a href=\"2\"> HOME </a>\n</nav>\n"))
  ;; A custom format string: UP first, HOME second, and the result
  ;; normalized to exactly one trailing newline.
  ($l (t--format-legacy-navbar
       '(:html-link-up "u" :html-link-home "h"
                       :html-home/up-format "[%s][%s]"))
      "[u][h]\n")
  ($l (t--format-legacy-navbar
       '(:html-link-up "u" :html-link-home "h"
                       :html-home/up-format "<%s %s>\n\n\n"))
      "<u h>\n")
  ;; A bad format string errors when the bar is built, and goes
  ;; unnoticed when it is not.
  ($q (car (should-error
            (t--format-legacy-navbar
             '(:html-link-up "u" :html-home/up-format "100%"))))
      'org-w3ctr-error)
  ($n (t--format-legacy-navbar '(:html-home/up-format nil)))
  (t-check-element-values
   #'t--format-legacy-navbar
   `(("#+html_link_up: https://example.com"
      ,($c "<nav id=\"navbar\">\n <a href=\"https://example.com\"> UP "
           "</a>\n <a href=\"https://example.com\"> HOME </a>\n</nav>\n"))
     ("#+html_link_home: https://a.com"
      ,($c "<nav id=\"navbar\">\n <a href=\"https://a.com\"> UP "
           "</a>\n <a href=\"https://a.com\"> HOME </a>\n</nav>\n"))
     ("#+html_link_home: a\n#+html_link_up:b"
      ,($c "<nav id=\"navbar\">\n <a href=\"b\"> UP "
           "</a>\n <a href=\"a\"> HOME </a>\n</nav>\n"))
     ("#+html_link_home: \n#+html_link_up:"
      nil)
     ;; The HTML_HOME/UP_FORMAT keyword, ox-html compatible: one line,
     ;; or several lines joined with newlines.
     (,($c "#+html_link_up: u\n#+html_link_home: h\n"
           "#+html_home/up_format: [%s][%s]")
      "[u][h]\n")
     (,($c "#+html_link_up: u\n#+html_link_home: h\n"
           "#+html_home/up_format: <nav>\n"
           "#+html_home/up_format: %s %s\n"
           "#+html_home/up_format: </nav>")
      "<nav>\nu h\n</nav>\n"))
   nil `( :html-link-up "" :html-link-home ""
          :html-link-navbar nil
          :html-home/up-format ,t-home/up-format)))

(ert-deftest t--wrap-navbar ()
  "Tests for `org-w3ctr--wrap-navbar'."
  ($l (t--wrap-navbar "") "<nav id=\"navbar\">\n\n</nav>\n")
  ($l (t--wrap-navbar "1") "<nav id=\"navbar\">\n1\n</nav>\n")
  ;; Content passes through verbatim; the wrapper only adds the
  ;; element and its surrounding newlines.
  ($l (t--wrap-navbar "<a href=\"u\">U</a>\n<a href=\"h\">H</a>")
      ($c "<nav id=\"navbar\">\n<a href=\"u\">U</a>\n"
          "<a href=\"h\">H</a>\n</nav>\n"))
  ;; Surrounding whitespace and newlines are trimmed: the block shape
  ;; does not depend on how the caller formats its input.  Internal
  ;; newlines are kept.
  ($l (t--wrap-navbar "a\n") "<nav id=\"navbar\">\na\n</nav>\n")
  ($l (t--wrap-navbar "\n  a\nb  \n\n") "<nav id=\"navbar\">\na\nb\n</nav>\n")
  ;; Blank input is wrapped as an empty shell.
  ($l (t--wrap-navbar "  \n ") "<nav id=\"navbar\">\n\n</nav>\n"))

(ert-deftest t--format-navbar-vector ()
  "Tests for `org-w3ctr--format-navbar-vector'."
  ($l "" (t--format-navbar-vector []))
  ($l (t--format-navbar-vector [("a" . "b")]) "\
<nav id=\"navbar\">
<a href=\"a\">b</a>
</nav>\n")
  ($l (t--format-navbar-vector [("a" . "b") ("c" . "d")]) "\
<nav id=\"navbar\">
<a href=\"a\">b</a>
<a href=\"c\">d</a>
</nav>\n")
  ;; Link and name go in verbatim, without HTML escaping.
  ($l (t--format-navbar-vector [("<u>" . "N")])
      "<nav id=\"navbar\">\n<a href=\"<u>\">N</a>\n</nav>\n")
  ;; An entry that is not a (URL . NAME) cons of strings is an error.
  ($q (car (should-error (t--format-navbar-vector [(a . "b")])))
      'org-w3ctr-error)
  ($q (car (should-error (t--format-navbar-vector [("a" . b)])))
      'org-w3ctr-error))

(ert-deftest t--format-navbar-list ()
  "Tests for `org-w3ctr--format-navbar-list'."
  ;; A nil list and an all-blank list both mean "no links".
  ($l (t--format-navbar-list nil nil) "")
  (cl-letf (((symbol-function 'org-export-data)
             (lambda (x _info) x))
            ((symbol-function 't--wrap-navbar)
             (lambda (x) x)))
    ($l (t--format-navbar-list '("a") nil) "a")
    ($l (t--format-navbar-list '("a" "b" "c") nil) "a\nb\nc")
    ($l (t--format-navbar-list '("a" " " "\t" "\n" "e") nil) "a\ne")
    ($l (t--format-navbar-list '(" " "\t") nil) "")))

(ert-deftest t-navbar-default-format-function ()
  "Tests for `org-w3ctr-navbar-default-format-function'."
  ($it t-navbar-default-format-function
    (let ((info '(:html-link-navbar [("a" . "b")])))
      ($l (it info) "<nav id=\"navbar\">\n<a href=\"a\">b</a>\n</nav>\n"))
    (let ((info '(:html-link-navbar [("a" . "b") ("c" . "d")])))
      ($l (it info)
          ($c "<nav id=\"navbar\">\n<a href=\"a\">b</a>\n"
              "<a href=\"c\">d</a>\n</nav>\n")))
    ;; An entry that is not a (URL . NAME) cons of strings is an error.
    ($q (car (should-error (it '(:html-link-navbar [(a . "b")]))))
        'org-w3ctr-error)
    ($q (car (should-error (it '(:html-link-navbar [("a" . b)]))))
        'org-w3ctr-error)
    ($q (car (should-error (it '(:html-link-navbar [(a . b)]))))
        'org-w3ctr-error)
    ;; Neither a vector nor a list is an error.
    ($q (car (should-error (it '(:html-link-navbar "str"))))
        'org-w3ctr-error)
    ;; No links anywhere is "".
    ($l (it '(:html-link-navbar [])) "")
    ;; An empty vector and a blank link list fall back to the legacy bar.
    ($l (it '( :html-link-navbar [] :html-link-up "u" :html-link-home "h"
               :html-home/up-format "%s|%s"))
        "u|h\n")
    (cl-letf (((symbol-function 'org-export-data) (lambda (x _info) x)))
      ($l (it '( :html-link-navbar (" " "\t") :html-link-up "u"
                 :html-link-home "h" :html-home/up-format "%s|%s"))
          "u|h\n")))
  (t-check-element-values
   #'t-navbar-default-format-function
   `(("" ,($c "<nav id=\"navbar\">\n<a href=\"https://example.com\">"
              "example</a>\n</nav>\n")))
   nil '( :html-link-navbar [("https://example.com" . "example")]
          :html-navbar-format-function
          t-navbar-default-format-function))
  (t-check-element-values
   #'t-navbar-default-format-function
   `(("#+html_link_navbar: [[https://a.com][b]]"
      "<nav id=\"navbar\">\n<a href=\"https://a.com\">b</a>\n</nav>\n")
     ("#+html_link_navbar: [[https://a.com]]"
      ,($c "<nav id=\"navbar\">\n<a href=\"https://a.com\">"
           "https://a.com</a>\n</nav>\n"))
     (,($c "#+html_link_navbar: [[https://a.com]]\n"
           "#+html_link_navbar: [[https://b.com]]")
      ,($c "<nav id=\"navbar\">\n<a href=\"https://a.com\">"
           "https://a.com</a>\n<a href=\"https://b.com\">"
           "https://b.com</a>\n</nav>\n"))
     ("#+html_link_navbar: [[https://a.com]] [[https://b.com]]"
      ,($c "<nav id=\"navbar\">\n<a href=\"https://a.com\">"
           "https://a.com</a>\n<a href=\"https://b.com\">"
           "https://b.com</a>\n</nav>\n"))
     ("#+html_link_navbar: 1 2 3"
      "<nav id=\"navbar\">\n1 2 3\n</nav>\n")
     ("#+html_link_navbar: \n#+html_link_home: 123"
      ,($c "<nav id=\"navbar\">\n <a href=\"123\"> UP </a>\n"
           " <a href=\"123\"> HOME </a>\n</nav>\n"))
     ("#+html_link_up: 456"
      ,($c "<nav id=\"navbar\">\n <a href=\"456\"> UP </a>\n"
           " <a href=\"456\"> HOME </a>\n</nav>\n"))
     ("#+html_link_home: 123\n#+html_link_up: 456"
      ,($c "<nav id=\"navbar\">\n <a href=\"456\"> UP </a>\n"
           " <a href=\"123\"> HOME </a>\n</nav>\n")))
   nil `(:html-navbar-format-function
         t-navbar-default-format-function
         :html-link-up "" :html-link-home ""
         :html-home/up-format ,t-home/up-format)))

;;;; CC license badges

(ert-deftest t--load-cc-svg ()
  "Tests for `org-w3ctr--load-cc-svg'."
  ;; Round-trip the real file: base64 with no line breaks, decoding
  ;; back to SVG markup.
  (let ((raw (t--load-cc-svg "by")))
    ($s (string-match-p "\\`[A-Za-z0-9+/=]+\\'" raw))
    ($s (string-match-p "<svg" (base64-decode-string raw))))
  ;; Every shipped icon reads.
  (dolist (a '("by" "cc" "nc" "nd" "pdm" "sa" "zero"))
    ($s (t--load-cc-svg a)))
  ;; A missing icon is an error.
  ($q (car (should-error (t--load-cc-svg "no-such-icon")))
      'org-w3ctr-error))

(ert-deftest t--load-cc-svg-utf8 ()
  "Non-ASCII SVG bytes survive the base64 step."
  (let ((svg "<svg>\u00e9</svg>"))
    (cl-letf (((symbol-function 't--load-file) (lambda (_file) svg)))
      ($l (base64-decode-string (t--load-cc-svg "fake"))
          (encode-coding-string svg 'utf-8)))))

(ert-deftest t--load-cc-svg-once ()
  "Tests for `org-w3ctr--load-cc-svg-once'."
  ;; Each name is read once; repeat calls come from the cache.
  (let ((reads 0))
    (cl-letf (((symbol-function 't--load-cc-svg)
               (lambda (name) (setq reads (1+ reads)) (concat "b64:" name)))
              (t--cc-svg-cache (make-hash-table :test 'equal)))
      ($l (t--load-cc-svg-once "by") "b64:by")
      ($l (t--load-cc-svg-once "by") "b64:by")
      ($l (t--load-cc-svg-once "cc") "b64:cc")
      ($l reads 2)
      ($l (gethash "by" t--cc-svg-cache) "b64:by"))))

(ert-deftest t--build-cc-img ()
  "Tests for `org-w3ctr--build-cc-img'."
  ($l (t--build-cc-img "by" "")
      ($c "<img style=\"height:1.4em;margin-left:0.2em;"
          "vertical-align:text-bottom;\" "
          "src=\"data:image/svg+xml;base64,\" alt=\"BY\">"))
  ($l (t--build-cc-img "zero" "test")
      ($c "<img style=\"height:1.4em;margin-left:0.2em;"
          "vertical-align:text-bottom;\" "
          "src=\"data:image/svg+xml;base64,test\" alt=\"CC0\">")))

(ert-deftest t--cc-icon-names ()
  "Tests for `org-w3ctr--cc-icon-names'."
  ($l (t--cc-icon-names 'cc0) '("cc" "zero"))
  ($l (t--cc-icon-names 'public-domain-mark) '("pdm"))
  ;; Components name the icons; the version digits are dropped.
  ($l (t--cc-icon-names 'cc-by-nc-sa-4.0) '("cc" "by" "nc" "sa"))
  ($l (t--cc-icon-names 'cc-by-nc-nd-3.0) '("cc" "by" "nc" "nd"))
  ;; Non-CC entries and non-symbol values have none.
  ($n (t--cc-icon-names 'all-rights-reserved))
  ($n (t--cc-icon-names nil))
  ($n (t--cc-icon-names "cc-by-4.0"))
  ($n (t--cc-icon-names 42)))

(ert-deftest t-cc-badges-default-format-function ()
  "Tests for `org-w3ctr-cc-badges-default-format-function'."
  (cl-letf (((symbol-function 't--load-cc-svg-once)
             #'identity)
            ((symbol-function 't--build-cc-img)
             (lambda (name _base64) name)))
    ($it t-cc-badges-default-format-function
      ($l (it 'cc0 nil) "cczero")
      ($l (it 'cc-by-4.0 nil) "ccby")
      ($l (it 'cc-by-sa-4.0 nil) "ccbysa")
      ($l (it 'cc-by-nc-sa-4.0 nil) "ccbyncsa")
      ($l (it 'cc-by-nc-nd-4.0 nil) "ccbyncnd")
      ($l (it 'public-domain-mark nil) "pdm")
      ($l (it 'all-rights-reserved nil) ""))))

(ert-deftest t-format-public-license ()
  "Tests for `org-w3ctr-format-public-license'."
  (cl-letf (((symbol-function 't-license-default-format-function)
             (lambda (_info) "DEFAULT")))
    ;; The hook from INFO is called with INFO.
    ($l (t-format-public-license
         (list :x 1 :html-license-format-function
               (lambda (info) (format "CUSTOM:%s" (plist-get info :x)))))
        "CUSTOM:1")
    ;; Without the option it falls back to the default renderer.
    ($l (t-format-public-license (list :x 1)) "DEFAULT")))

(ert-deftest t--get-info-author ()
  "Tests for `org-w3ctr--get-info-author'."
  (t-check-element-values
   #'t--get-info-author
   '(("#+AUTHOR: /hello/" "<i>hello</i>")
     ("#+AUTHOR: " nil))
   nil '( :with-author t :html-license-format-function
          t-license-default-format-function))
  ;; The gate and the missing author are direct-call cases: a full
  ;; export defaults :author to `user-full-name'.
  (cl-letf (((symbol-function 'org-export-data) (lambda (d _info) d)))
    ($n (t--get-info-author '(:with-author nil :author "x")))
    ($n (t--get-info-author '(:with-author t)))
    ($l (t--get-info-author '(:with-author t :author "x")) "x")))

(ert-deftest t-license-default-format-function ()
  "Tests for `org-w3ctr-license-default-format-function'."
  (cl-letf (((symbol-function 't--get-info-author)
             (lambda (info) (plist-get info :author))))
    (cl-flet ((test (info)
                (t-license-default-format-function (copy-sequence info))))
      ;; No :html-cc-badges-format-function here on purpose: the call
      ;; site falls back to the default renderer.
      (let ((info (list :html-license nil)))
        ($l (test info) "Not Specified")
        (setq info (plist-put info :html-license 'all-rights-reserved))
        ($l (test info) "All Rights Reserved")
        (setq info (plist-put info :html-license 'all-rights-reversed))
        ($l (test info) "All Rights Reversed")
        (setq info (plist-put info :html-license 'cc-by-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/4.0/\">CC BY-NC 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-nd-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/4.0/\">CC BY-NC-ND 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-sa-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">CC BY-NC-SA 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nd-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/4.0/\">CC BY-ND 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-sa-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/3.0/\">CC BY 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/3.0/\">CC BY-NC 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-nd-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/3.0/\">CC BY-NC-ND 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-sa-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/3.0/\">CC BY-NC-SA 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nd-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/3.0/\">CC BY-ND 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-sa-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/3.0/\">CC BY-SA 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-4.0))
        (setq info (plist-put info :author "test"))
        ($l (test info)
            "This work by test is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>")
        (setq info (plist-put info :html-use-cc-badges t))
        ($l (test info)
            ($c "This work by test is licensed under "
                "<a href=\"https://creativecommons.org/licenses/by/4.0/\">"
                "CC BY 4.0</a>"
                " " (t-cc-badges-default-format-function 'cc-by-4.0 nil)))
        ;; The public-domain tools get their own sentences.
        (setq info (plist-put info :html-license 'cc0))
        ($l (test info)
            ($c "This work by test is dedicated to the public domain under "
                "<a href=\"https://creativecommons.org/publicdomain/zero/1.0/\">"
                "CC0 1.0 Universal</a>"
                " " (t-cc-badges-default-format-function 'cc0 nil)))
        (setq info (plist-put info :html-license 'public-domain-mark))
        ($l (test info)
            ($c "This work by test is marked as being in the public domain ("
                "<a href=\"https://creativecommons.org/publicdomain/mark/1.0/\">"
                "Public Domain Mark 1.0</a>)"
                " " (t-cc-badges-default-format-function 'public-domain-mark nil)))
        ;; An unknown license is an error.
        (setq info (plist-put info :html-license 'nope))
        ($q (car (should-error (test info))) 'org-w3ctr-error)))))

;;;; Preamble and Postamble

(ert-deftest t--pre/postamble-format-spec ()
  "Tests for `org-w3ctr--pre/postamble-format-spec'."
  (cl-flet ((test (str info expect)
              (t-check-element-values
               #'t--build-pre/postamble
               (list (list str "" expect)) nil info))
            (i (str &optional ls)
              `( :html-preamble ,str :html-postamble nil ,@ls)))
    (test "" (i "%t") "")
    (test "#+TITLE: 123" (i "%t") "123\n")
    (test "#+TITLE: " (i "%t") "")
    (test "#+SUBTITLE: 123" (i "%s") "123\n")
    (test "#+SUBTITLE: " (i "%s") "")
    (test "#+DATE: [2000-01-01]"
          (i "%d" '(:html-metadata-timestamp-format "%Y"))
          "2000\n")
    (test "#+DATE: [2000-01-01]--[2020-01-01]"
          (i "%d" '(:html-metadata-timestamp-format "%Y-%m-%d"))
          "2000-01-01\n")
    (test "" (i "%T" '(:html-metadata-timestamp-format "%F %R"))
          (format-time-string "%F %R\n"))
    (test "" (i "%a") "")
    (test "#+AUTHOR: " (i "%a") "")
    (test "#+AUTHOR: eXkeq" (i "%a") "eXkeq\n")
    (test "" (i "%a" '(:author "test")) "test\n")
    (let ((user-mail-address "hello@world.com"))
      (test "" (i "%e")
            "<a href=\"mailto:hello@world.com\">hello@world.com</a>\n"))
    (test "#+email: a@b.c, d@e.f" (i "%e")
          ($c "<a href=\"mailto:a@b.c\">a@b.c</a>, "
              "<a href=\"mailto:d@e.f\">d@e.f</a>\n"))
    (test "" (i "%e" '(:email "test"))
          "<a href=\"mailto:test\">test</a>\n")
    (test "" (i "%c" '(:creator "foo")) "foo\n")
    ;; A missing creator yields "", not "nil".
    (test "" (i "%c" '(:creator nil)) "")
    (test "" (i "%v" '(:html-validation-link "foo.com")) "foo.com\n")
    ;; No input file, no modification time.
    (test "" (i "%C") "")
    ;; A missing email yields "" and leaves the eager spec intact.
    (let ((user-mail-address nil))
      (test "" (i "%e") "")
      (test "#+TITLE: x" (i "%t") "x\n"))
    ;; Direct calls pin %C against a real file's mtime.
    (cl-letf (((symbol-function 'org-export-data) (lambda (d _info) d)))
      (let* ((file (make-temp-file "ox-w3ctr-mtime"))
             (fmt "%Y-%m-%d %H:%M")
             (time (encode-time 0 0 12 1 1 2000)))
        (unwind-protect
            (progn
              (set-file-times file time)
              ($l (cdr (assq ?C (t--pre/postamble-format-spec
                                 (list :input-file file
                                       :html-metadata-timestamp-format fmt))))
                  (format-time-string fmt time)))
          (delete-file file))))))

(ert-deftest t--build-pre/postamble ()
  "Tests for `org-w3ctr--build-pre/postamble'."
  (t-check-element-values
   #'t--build-pre/postamble
   '(("" "" "")) nil '(:html-preamble nil :html-postamble nil))
  (t-check-element-values
   #'t--build-pre/postamble
   '(("" "" "hello world\n"))
   nil '(:html-preamble "hello world" :html-postamble nil))
  (t-check-element-values
   #'t--build-pre/postamble
   '(("" "" "132\n")) nil
   '(:html-preamble (lambda (info) "132") :html-postamble nil))
  (cl-letf (((symbol-function 'foo) (lambda (_i) "foo")))
    (t-check-element-values
     #'t--build-pre/postamble
     '(("" "" "foo\n")) nil '(:html-preamble foo :html-postamble nil)))
  (dlet ((bar "foo bar baz"))
    (t-check-element-values
     #'t--build-pre/postamble
     '(("" "" "foo bar baz\n")) nil
     '(:html-preamble bar :html-postamble nil)))
  ($e!l (org-export-string-as "" 'w3ctr nil '(:html-preamble []))
        '(org-w3ctr-error "Invalid preamble: []"))
  ($e!l (org-export-string-as "" 'w3ctr nil '(:html-postamble []))
        '(org-w3ctr-error "Invalid postamble: []"))
  (dlet ((bar 'foo))
    ($e!l (org-export-string-as "" 'w3ctr nil '(:html-preamble bar))
          '(org-w3ctr-error "Invalid preamble symbol value: foo")))
  (dlet ((bar 'foo))
    ($e!l (org-export-string-as "" 'w3ctr nil '(:html-postamble bar))
          '(org-w3ctr-error "Invalid postamble symbol value: foo")))
  ;; A symbol without a value is our own error, not void-variable.
  ($e!l (org-export-string-as "" 'w3ctr nil '(:html-preamble no-such-var))
        '(org-w3ctr-error "Invalid preamble symbol: no-such-var"))
  ;; A whitespace value cell is unusable.
  (dlet ((bar "   "))
    ($e!l (org-export-string-as "" 'w3ctr nil '(:html-postamble bar))
          '(org-w3ctr-error "Invalid postamble symbol value:    ")))
  ;; Blank results normalize to the empty string.
  (t-check-element-values
   #'t--build-pre/postamble
   '(("" "" "")) nil
   '(:html-preamble (lambda (info) "  ") :html-postamble nil))
  (t-check-element-values
   #'t--build-pre/postamble
   '(("" "" "")) nil
   '(:html-preamble (lambda (info) nil) :html-postamble nil))
  ;; With no :html-postamble in the plist, the export environment
  ;; fills the default: the back-to-top arrow.
  (t-check-element-values
   #'t--build-pre/postamble
   `(("" ,($c "<p role=\"navigation\" id=\"back-to-top\">"
              "<a href=\"#title\"><abbr title=\"Back to Top\">↑"
              "</abbr></a></p>\n") ""))
   nil '(:html-preamble nil)))

(ert-deftest t--get-info-date ()
  "Tests for `org-w3ctr--get-info-date'."
  (t-check-element-values
   #'t--get-info-date
   '(("#+date: [2020-01-01]" "[2020-01-01 Wed]")
     ("#+date: [2000-01-01] [2000-01-02]" nil)
     ("#+date: 123" nil)
     ("#+date: " nil)
     ("" nil)
     ("#+date: [2000-01-01]--[2001-01-01]"
      "[2000-01-01 Sat]&#x2013;[2001-01-01 Mon]"))
   nil '(:html-timestamp-wrapper none)))

(ert-deftest t--get-info-mtime ()
  "Tests for `org-w3ctr--get-info-mtime'."
  ;; No input file, no modification time; likewise a missing one.
  ($n (t--get-info-mtime nil))
  ($n (t--get-info-mtime '(:input-file "no/such/file")))
  ($l (t--get-info-mtime `(:input-file ,(file-name-concat
                                         t--dir "ox-w3ctr-tests.el")))
      (format-time-string
       "%FT%RZ" (file-attribute-modification-time
                 (file-attributes (file-name-concat
                                   t--dir "ox-w3ctr-tests.el")))
       t)))

(ert-deftest t--preamble-default-function ()
  "Tests for `org-w3ctr-preamble-default-function'."
  (cl-letf (((symbol-function 't--get-info-date) #'ignore)
            ((symbol-function 't--get-info-mtime) #'ignore)
            ((symbol-function 't-format-public-license) #'ignore))
    (let ((expected ($c "<details open>\n"
                        "<summary>More details about this document</summary>\n"
                        "<dl>\n"
                        "<dt>Drafting to Completion / Publication:</dt> "
                        "<dd>[Not Specified]</dd>\n"
                        "<dt>Date of last modification:</dt> <dd>[Not Specified]</dd>\n"
                        "<dt>Creation Tools:</dt> <dd>[Not Specified]</dd>\n"
                        "<dt>Public License:</dt> <dd></dd>\n"
                        "</dl>\n</details>\n<hr>")))
      ;; Every row falls back to the placeholder.
      ($l (t-preamble-default-function nil) expected)
      ;; A blank creator is missing too.
      ($l (t-preamble-default-function '(:creator "   ")) expected)))
  ;; The rows fill in when the values exist.
  (cl-letf (((symbol-function 't--get-info-date) (lambda (_i) "DATE"))
            ((symbol-function 't--get-info-mtime) (lambda (_i) "MTIME"))
            ((symbol-function 't-format-public-license)
             (lambda (_i) "LICENSE")))
    ($l (t-preamble-default-function '(:creator "MAKER"))
        ($c "<details open>\n"
            "<summary>More details about this document</summary>\n"
            "<dl>\n"
            "<dt>Drafting to Completion / Publication:</dt> <dd>DATE</dd>\n"
            "<dt>Date of last modification:</dt> <dd>MTIME</dd>\n"
            "<dt>Creation Tools:</dt> <dd>MAKER</dd>\n"
            "<dt>Public License:</dt> <dd>LICENSE</dd>\n"
            "</dl>\n</details>\n<hr>"))))

;;;; Table of Contents

(ert-deftest t--toc-headline-secno ()
  "Tests for `org-w3ctr--toc-headline-secno'."
  (cl-letf (((symbol-function 'org-export-numbered-headline-p)
             (lambda (_h _i) t))
            ((symbol-function 'org-export-get-headline-number)
             (lambda (_h _i) '(1 1 4))))
    ($l (t--toc-headline-secno nil nil)
        "<span class=\"secno\">1.1.4</span>"))
  ;; Unnumbered headlines have no span.
  (cl-letf (((symbol-function 'org-export-numbered-headline-p)
             (lambda (_h _i) nil)))
    ($n (t--toc-headline-secno nil nil))))

(ert-deftest t--build-toc-headline ()
  "Tests for `org-w3ctr--build-toc-headline'."
  (cl-letf (((symbol-function 't--build-bare-headline)
             (lambda (_h text _i) text)))
    ;; The title goes through the TOC entry backend: a link becomes
    ;; its description, or its raw path when none is set.
    (t-check-element-values
     #'t--build-toc-headline
     '(("* Hello" "Hello")
       ("* [[https://example.com][desc]]" "desc")
       ("* [[https://example.com]]" "https://example.com"))
     nil '(:with-toc 2))
    ;; The alternative title wins over the regular one.
    (t-check-element-values
     #'t--build-toc-headline
     '(("* Regular\n:PROPERTIES:\n:ALT_TITLE: Alternative\n:END:"
        "Alternative"))
     nil '(:with-toc 2))))

(ert-deftest t-toc-headline-default-format-function ()
  "Tests for `org-w3ctr-toc-headline-default-format-function'."
  (cl-letf (((symbol-function 't--reference) (lambda (_h _i) "id"))
            ((symbol-function 't--build-toc-headline)
             (lambda (_h _i) "TITLE"))
            ((symbol-function 't--toc-headline-secno)
             (lambda (_h _i) "1.2"))
            ((symbol-function 't--low-level-headline-p)
             (lambda (_h _i) nil)))
    ($l (t-toc-headline-default-format-function nil nil)
        "<a href=\"#id\">1.2TITLE</a>"))
  ;; Low-level headlines carry no section number.
  (cl-letf (((symbol-function 't--reference) (lambda (_h _i) "id"))
            ((symbol-function 't--build-toc-headline)
             (lambda (_h _i) "TITLE"))
            ((symbol-function 't--toc-headline-secno)
             (lambda (_h _i) "1.2"))
            ((symbol-function 't--low-level-headline-p)
             (lambda (_h _i) t)))
    ($l (t-toc-headline-default-format-function nil nil)
        "<a href=\"#id\">TITLE</a>")))

(ert-deftest t--get-info-toc-element ()
  "Tests for `org-w3ctr--get-info-toc-element'."
  ($l (t--get-info-toc-element '(:html-toc-element ul)) "ul")
  ($l (t--get-info-toc-element '(:html-toc-element ol)) "ol")
  ($q (car (should-error (t--get-info-toc-element '(:html-toc-element dl))))
      'org-w3ctr-error))

(ert-deftest t--toc-alist-to-text ()
  "Tests for `org-w3ctr--toc-alist-to-text'."
  (let ((info '(:html-toc-element ul)))
    ;; A flat list at the top level.
    ($l (t--toc-alist-to-text '(("a" . 1) ("b" . 1)) info t)
        "\n<ul class=\"toc\">\n<li>a</li>\n<li>b</li>\n</ul>\n")
    ;; Entering a deeper level nests a list.
    ($l (t--toc-alist-to-text '(("a" . 1) ("b" . 2)) info t)
        ($c "\n<ul class=\"toc\">\n<li>a\n<ul class=\"toc\">\n<li>b"
            "</li>\n</ul>\n</li>\n</ul>\n"))
    ;; Leaving a level closes it before the entry.
    ($l (t--toc-alist-to-text '(("a" . 1) ("b" . 2) ("c" . 1)) info t)
        ($c "\n<ul class=\"toc\">\n<li>a\n<ul class=\"toc\">\n<li>b"
            "</li>\n</ul>\n</li>\n<li>c</li>\n</ul>\n"))
    ;; The two cases of Org commit 332695e85, where the scope semantics
    ;; was fixed upstream.
    ($l (t--toc-alist-to-text '(("1" . 1) ("1.1" . 2) ("2" . 1)) info t)
        "\n<ul class=\"toc\">\n<li>1\n<ul class=\"toc\">\n<li>1.1</li>\n</ul>\n</li>\n<li>2</li>\n</ul>\n")
    ;; A first entry below the top level wraps in empty lists.
    ($l (t--toc-alist-to-text '(("1" . 2) ("1.1" . 3) ("2" . 1)) info t)
        "\n<ul class=\"toc\">\n<li>\n<ul class=\"toc\">\n<li>1\n<ul class=\"toc\">\n<li>1.1</li>\n</ul>\n</li>\n</ul>\n</li>\n<li>2</li>\n</ul>\n")
    ;; Without top, the first entry's level sets the depth.
    ($l (t--toc-alist-to-text '(("a" . 3)) info)
        "\n<ul class=\"toc\">\n<li>a</li>\n</ul>\n")))

(ert-deftest t--build-toc ()
  "Tests for `org-w3ctr--build-toc'."
  (cl-letf (((symbol-function 'org-export-collect-headlines)
             (lambda (_i _d &optional _s) '(h1 h2)))
            ((symbol-function 't-toc-headline-default-format-function)
             (lambda (h _i) (symbol-name h)))
            ((symbol-function 'org-export-get-relative-level)
             (lambda (h _i) (if (eq h 'h1) 1 2))))
    ($l (t--build-toc 2 '(:html-toc-element ul))
        ($c "\n<ul class=\"toc\">\n<li>h1\n<ul class=\"toc\">\n<li>h2"
            "</li>\n</ul>\n</li>\n</ul>\n")))
  ;; No headline in range, no table.
  (cl-letf (((symbol-function 'org-export-collect-headlines)
             (lambda (_i _d &optional _s) nil)))
    ($n (t--build-toc 2 '(:html-toc-element ul))))
  ;; The hook formats the entry; without one, the default is used.
  (cl-letf (((symbol-function 'org-export-collect-headlines)
             (lambda (_i _d &optional _s) '(h1)))
            ((symbol-function 'org-export-get-relative-level)
             (lambda (_h _i) 1))
            ((symbol-function 't-toc-headline-default-format-function)
             (lambda (h _i) (format "DEFAULT %s" h))))
    ($l (t--build-toc 2 '(:html-toc-element ul))
        "\n<ul class=\"toc\">\n<li>DEFAULT h1</li>\n</ul>\n")
    ($l (t--build-toc 2 (list :html-toc-element 'ul
                              :html-toc-headline-format-function
                              (lambda (h _i) (format "HOOK %s" h))))
        "\n<ul class=\"toc\">\n<li>HOOK h1</li>\n</ul>\n"))
  ;; SCOPE also shifts the nesting start: a scoped table begins one
  ;; level below its first entry, a full one at level zero.
  (cl-letf (((symbol-function 'org-export-collect-headlines)
             (lambda (_i _d &optional _s) '(h1)))
            ((symbol-function 'org-export-get-relative-level)
             (lambda (_h _i) 2))
            ((symbol-function 't-toc-headline-default-format-function)
             (lambda (h _i) (symbol-name h))))
    ($l (t--build-toc 2 '(:html-toc-element ul))
        "\n<ul class=\"toc\">\n<li>\n<ul class=\"toc\">\n<li>h1</li>\n</ul>\n</li>\n</ul>\n")
    ($l (t--build-toc 2 '(:html-toc-element ul) 'scope-el)
        "\n<ul class=\"toc\">\n<li>h1</li>\n</ul>\n")))

(ert-deftest t--build-toc-restores-channel-slots ()
  "`org-w3ctr--build-toc' restores the swapped channel slots."
  (with-temp-buffer
    (insert "* One\n")
    (org-mode)
    (let* ((env (org-export-get-environment 'w3ctr))
           (tree (org-element-parse-buffer))
           (backend (org-export-get-backend 'w3ctr))
           (alist (org-export-get-all-transcoders 'w3ctr))
           (make-info
            (lambda ()
              (org-combine-plists
               env
               (list :parse-tree tree
                     :back-end backend
                     :translate-alist alist
                     :exported-data (make-hash-table :test 'eq)
                     :internal-references nil)))))
      ;; Normal return: the three slots come back unchanged.
      (let* ((info (funcall make-info))
             (hash (plist-get info :exported-data)))
        (t--build-toc 1 info)
        ($q (plist-get info :back-end) backend)
        ($q (plist-get info :translate-alist) alist)
        ($q (plist-get info :exported-data) hash))
      ;; A signal mid-loop still restores them.
      (let* ((info (funcall make-info))
             (hash (plist-get info :exported-data)))
        (plist-put info :html-toc-headline-format-function
                   (lambda (_h _i) (error "boom")))
        ($e! (t--build-toc 1 info))
        ($q (plist-get info :back-end) backend)
        ($q (plist-get info :translate-alist) alist)
        ($q (plist-get info :exported-data) hash)))))

(ert-deftest t--build-toc-keeps-oinfo-pid ()
  "`org-w3ctr--build-toc' keeps INFO's identity, so OINFO pids stay put.

A title rendered for the TOC reads cached keys through the same INFO
plist, so the cache hits and the oclosure's pid does not flip."
  (skip-unless t--oinfo-cache-p)
  (with-temp-buffer
    (insert "* One\n")
    (org-mode)
    (let* ((info (org-combine-plists
                  (org-export-get-environment 'w3ctr)
                  (list :parse-tree (org-element-parse-buffer)
                        :back-end (org-export-get-backend 'w3ctr)
                        :translate-alist (org-export-get-all-transcoders 'w3ctr)
                        :exported-data (make-hash-table :test 'eq)
                        :internal-references nil)))
           (o (t--oinfo-oget :with-smart-quotes)))
      (t--pget info :with-smart-quotes)
      ($q (t--oinfo--pid o) info)
      (t--build-toc 1 info)
      ($q (t--oinfo--pid o) info))))

(ert-deftest t--toc-entry-channel-assumptions ()
  "The TOC-entry swap in `org-w3ctr--build-toc' leans on ox.el internals.

Guard the cheap properties the swap relies on, so an Org version bump
that changes them fails here instead of corrupting a TOC silently."
  ;; The three swapped slots must stay outside the OINFO cache; a
  ;; cached slot would make `org-w3ctr--pput' write to the oclosure
  ;; instead of the plist.
  ($n (memq :back-end t--oinfo-cache-props))
  ($n (memq :translate-alist t--oinfo-cache-props))
  ($n (memq :exported-data t--oinfo-cache-props))
  ;; The TOC-entry backend is unnamed, so filters get nil -- the same
  ;; as through `org-export-data-with-backend'.
  ($n (org-export-backend-name t--toc-entry-backend))
  ;; Its full table puts the overrides before the inherited transcoders,
  ;; which `org-export-transcoder' picks with `assq'.  The link override
  ;; is a compiled function here, not a symbol, so check it is a function
  ;; and not the w3ctr link transcoder.
  ($s (functionp (cdr (assq 'link t--toc-entry-translate-alist))))
  ($nq (cdr (assq 'link t--toc-entry-translate-alist)) 't-link)
  ($l (cdr (assq 'footnote-reference t--toc-entry-translate-alist))
      'ignore)
  ($l (cdr (assq 'target t--toc-entry-translate-alist))
      'ignore)
  ($s (functionp (cdr (assq 'radio-target t--toc-entry-translate-alist))))
  ;; The w3ctr transcoders still follow the overrides.
  ($l (cdr (assq 'plain-text t--toc-entry-translate-alist))
      't-plain-text))

(ert-deftest t--build-table-of-contents ()
  "Tests for `org-w3ctr--build-table-of-contents'."
  (let (seen)
    (cl-letf (((symbol-function 't--build-toc)
               (lambda (d _i &optional _s) (setq seen d) "TOC")))
      ;; The depth comes from :with-toc.
      ($l (t--build-table-of-contents
           '(:with-toc 2 :html-toplevel-hlevel 3 :html-toc-title "Table of Contents"))
          "<nav id=\"toc\">\n<h3>Table of Contents</h3>TOC</nav>\n")
      ($l seen 2)))
  ;; A missing title falls back to `org-w3ctr-toc-title'.
  (cl-letf (((symbol-function 't--build-toc)
             (lambda (_d _i &optional _s) "TOC")))
    ($l (t--build-table-of-contents '(:with-toc 2 :html-toplevel-hlevel 3))
        "<nav id=\"toc\">\n<h3>Table of Contents</h3>TOC</nav>\n"))
  ;; No entries, no block.
  (cl-letf (((symbol-function 't--build-toc) (lambda (_d _i &optional _s) nil)))
    ($n (t--build-table-of-contents '(:with-toc 2 :html-toplevel-hlevel 3 :html-toc-title "Table of Contents"))))
  ;; A nil :with-toc means no table at all: the builder is not even
  ;; asked (its nil depth means "unlimited" instead).
  (cl-letf (((symbol-function 't--build-toc)
             (lambda (_d _i &optional _s) "TOC")))
    ($n (t--build-table-of-contents
         '(:with-toc nil :html-toplevel-hlevel 3 :html-toc-title "Table of Contents")))))

(ert-deftest t--list-of-elements ()
  "Tests for `org-w3ctr--list-of-elements'."
  (cl-letf (((symbol-function 'org-export-get-caption)
             (lambda (_e &optional _s) "CAP"))
            ((symbol-function 'org-export-data) (lambda (c _i) c))
            ((symbol-function 't--reference)
             (lambda (_e _i &optional _n) "id")))
    ($l (t--list-of-elements (lambda (_i) '(e1)) nil)
        "<ul class=\"index\">\n<li><a href=\"#id\">CAP</a></li>\n</ul>"))
  ;; Without a label the entry is plain text.
  (cl-letf (((symbol-function 'org-export-get-caption)
             (lambda (_e &optional _s) "CAP"))
            ((symbol-function 'org-export-data) (lambda (c _i) c))
            ((symbol-function 't--reference)
             (lambda (_e _i &optional _n) nil)))
    ($l (t--list-of-elements (lambda (_i) '(e1)) nil)
        "<ul class=\"index\">\n<li>CAP</li>\n</ul>"))
  ;; The short caption wins over the full one.
  (cl-letf (((symbol-function 'org-export-get-caption)
             (lambda (_e &optional short) (if short "SHORT" "FULL")))
            ((symbol-function 'org-export-data) (lambda (c _i) c))
            ((symbol-function 't--reference)
             (lambda (_e _i &optional _n) nil)))
    ($l (t--list-of-elements (lambda (_i) '(e1)) nil)
        "<ul class=\"index\">\n<li>SHORT</li>\n</ul>"))
  ;; Without a short caption, the full one is used.
  (cl-letf (((symbol-function 'org-export-get-caption)
             (lambda (_e &optional short) (unless short "FULL")))
            ((symbol-function 'org-export-data) (lambda (c _i) c))
            ((symbol-function 't--reference)
             (lambda (_e _i &optional _n) nil)))
    ($l (t--list-of-elements (lambda (_i) '(e1)) nil)
        "<ul class=\"index\">\n<li>FULL</li>\n</ul>"))
  ;; No entries, no list.
  ($n (t--list-of-elements (lambda (_i) nil) nil)))

(ert-deftest t--list-of-listings ()
  "Tests for `org-w3ctr--list-of-listings'."
  (let (seen)
    (cl-letf (((symbol-function 't--list-of-elements)
               (lambda (fn _i) (setq seen fn) "X")))
      ($l (t--list-of-listings nil) "X")
      ($q seen #'org-export-collect-listings))))

(ert-deftest t--list-of-tables ()
  "Tests for `org-w3ctr--list-of-tables'."
  (let (seen)
    (cl-letf (((symbol-function 't--list-of-elements)
               (lambda (fn _i) (setq seen fn) "X")))
      ($l (t--list-of-tables nil) "X")
      ($q seen #'org-export-collect-tables))))

(ert-deftest t--keyword-toc ()
  "Tests for `org-w3ctr--keyword-toc'."
  (cl-letf (((symbol-function 't--list-of-tables) (lambda (_i) "TABLES"))
            ((symbol-function 't--list-of-listings) (lambda (_i) "LISTINGS"))
            ((symbol-function 't--build-toc)
             (lambda (d _i &optional s) (format "TOC %S %S" d s))))
    ($l (t--keyword-toc nil "tables" nil) "TABLES")
    ($l (t--keyword-toc nil "listings" nil) "LISTINGS")
    ($l (t--keyword-toc nil "headlines" nil) "TOC nil nil")
    ($l (t--keyword-toc nil "headlines 3" nil) "TOC 3 nil")
    ;; Case is not significant for the list kinds or "headlines".
    ($l (t--keyword-toc nil "Tables" nil) "TABLES")
    ($l (t--keyword-toc nil "HEADLINES 2" nil) "TOC 2 nil")
    ;; :target resolves the link; local scopes to KEYWORD.
    (cl-letf (((symbol-function 'org-export-resolve-link)
               (lambda (l _i) (format "RESOLVED %s" l))))
      ($l (t--keyword-toc 'kw "headlines 2 :target \"file:foo.org\"" nil)
          "TOC 2 \"RESOLVED file:foo.org\"")
      ($l (t--keyword-toc 'kw "headlines local" nil) "TOC nil kw")
      ;; The depth is the number right after "headlines", not any
      ;; number in the value (a :target path may carry some).
      ($l (t--keyword-toc nil "headlines :target \"report-2.org\"" nil)
          "TOC nil \"RESOLVED report-2.org\"")
      ;; A :target takes precedence over local.
      ($l (t--keyword-toc 'kw "headlines local :target \"x\"" nil)
          "TOC nil \"RESOLVED x\""))
    ;; No match, no output.
    ($n (t--keyword-toc nil "nothing" nil))))

;;;; Template

(ert-deftest t-inner-template ()
  "Tests for `org-w3ctr-inner-template'."
  ;; The zeroth section lands before the table of contents, and a
  ;; document without one does not inherit the previous export's.
  (let ((with (org-export-string-as "ZEROTH\n\n* H\nbody" 'w3ctr nil))
        (without (org-export-string-as "* H\nbody" 'w3ctr nil)))
    ($s (string-match-p "ZEROTH" with))
    ($n (string-match-p "ZEROTH" without)))
  ;; The zeroth section's property drawer is dropped, not rendered.
  (let ((out (org-export-string-as
              ":PROPERTIES:\n:HTML_CONTAINER: aside\n:END:\n\nzeroth\n\n* H\nbody"
              'w3ctr nil)))
    ($s (string-match-p "zeroth" out))
    ($n (string-match-p "HTML_CONTAINER" out))))

(ert-deftest t--build-title ()
  "Tests for `org-w3ctr--build-title'."
  (cl-letf (((symbol-function 'org-export-data) (lambda (d _i) d)))
    ;; A missing :with-title means no block at all.
    ($n (t--build-title '(:title "T")))
    ;; The title and the subtitle paragraph.
    ($l (t--build-title '(:with-title t :title "T" :subtitle "S"))
        "<h1 id=\"title\">T</h1>\n<p id=\"w3c-state\">S</p>\n")
    ;; A blank title keeps the anchor with an invisible mark, and a
    ;; blank subtitle emits no paragraph.
    ($l (t--build-title '(:with-title t :title "  " :subtitle ""))
        "<h1 id=\"title\">&lrm;</h1>\n")))

(ert-deftest t--load-fixup-js ()
  "Tests for `org-w3ctr--load-fixup-js'."
  ;; A non-whitespace cache is returned without touching t--load-file.
  (let ((t--fixup-js-cache "<script>cached</script>"))
    ($l (t--load-fixup-js) "<script>cached</script>"))
  ;; An empty cache is refilled from the asset and stored.
  (cl-letf (((symbol-function 't--load-file) (lambda (_file) "JS")))
    (let ((t--fixup-js-cache nil))
      ($l (t--load-fixup-js) "<script>\nJS\n</script>\n")
      ($l t--fixup-js-cache "<script>\nJS\n</script>\n"))))

(ert-deftest t-clear-js ()
  "Tests for `org-w3ctr-clear-js'."
  (let ((t--fixup-js-cache "<script>x</script>"))
    (t-clear-js)
    ($q t--fixup-js-cache nil)))

(ert-deftest t-fixup-js-switch ()
  "The `fixup-js' OPTIONS key controls the shipped script."
  (let ((on (org-export-string-as "* H\nbody" 'w3ctr nil))
        (off (org-export-string-as
              "#+OPTIONS: fixup-js:nil\n* H\nbody" 'w3ctr nil)))
    ($s (string-match-p "<script>" on))
    ($n (string-match-p "<script>" off))))

(ert-deftest t-template-1 ()
  "Tests for `org-w3ctr-template-1'."
  (cl-letf (((symbol-function 't--build-head) (lambda (_i) "HEAD"))
            ((symbol-function 't--build-title) (lambda (_i) "TITLE"))
            ((symbol-function 't--load-fixup-js) (lambda () "DEFAULT-JS"))
            ((symbol-function 't--build-pre/postamble)
             (lambda (type _i) (upcase (symbol-name type)))))
    ;; The parts assemble in order; the document's fixup script wins.
    ($l (t-template-1 "BODY" (list :language "en"
                                   :html-navbar-format-function
                                   (lambda (_i) "NAV")
                                   :html-include-fixup-js t
                                   :html-fixup-js "JS();"))
        ($c "<!DOCTYPE html>\n<html lang=\"en\">\nHEAD<body>\nNAV"
            "<div class=\"head\">\nTITLEPREAMBLE</div>\nBODY"
            "POSTAMBLE"
            "JS();\n</body>\n</html>"))
    ;; A nil navbar function suppresses the navbar; a document without
    ;; its own fixup script falls back to the shipped default.
    ($l (t-template-1 "BODY" '(:language "en" :html-include-fixup-js t))
        ($c "<!DOCTYPE html>\n<html lang=\"en\">\nHEAD<body>\n"
            "<div class=\"head\">\nTITLEPREAMBLE</div>\nBODY"
            "POSTAMBLE"
            "DEFAULT-JS\n</body>\n</html>"))
    ;; The switch off drops the script.
    ($l (t-template-1 "BODY" '(:language "en" :html-include-fixup-js nil))
        ($c "<!DOCTYPE html>\n<html lang=\"en\">\nHEAD<body>\n"
            "<div class=\"head\">\nTITLEPREAMBLE</div>\nBODY"
            "POSTAMBLE</body>\n</html>"))))

(ert-deftest t-template ()
  "Tests for `org-w3ctr-template'."
  (let (cleaned)
    (cl-letf (((symbol-function 't-template-1)
               (lambda (c _i) (format "<%s>" c)))
              ((symbol-function 't--oinfo-cleanup)
               (lambda () (setq cleaned t))))
      ($l (t-template "X" nil) "<X>")
      ($s cleaned))))

(ert-deftest t--file-extension ()
  "Tests for `org-w3ctr--file-extension'."
  (dlet ((t-extension "html"))
    ;; The default extension, and the per-call override.
    ($l (t--file-extension nil) ".html")
    ($l (t--file-extension '(:html-extension "xhtml")) ".xhtml"))
  (dlet ((t-extension nil))
    ;; Without a default, the fallback is "html".
    ($l (t--file-extension nil) "html"))
  (dlet ((t-extension ""))
    ;; An empty default means no dot.
    ($l (t--file-extension nil) "")))

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "org-w3ctr-") ("$" . "org-w3ctr:test-"))
;; coding: utf-8-unix
;; End:
