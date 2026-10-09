;;; test-isolation.el --- flag order-dependent ERT tests  -*- lexical-binding:t -*-
;; Run a test file's suite in definition order, then in several shuffled
;; orders, each in a separate Emacs process, and report every test whose
;; result differs.  Separate processes are what makes the check honest:
;; state a test leaves behind cannot carry into the next order, so a
;; difference is the test's dependence on what ran before it.
;;
;; Heuristic, not a gate: a test may be legitimately order-dependent, and
;; a run that fails for an unrelated reason can mask a difference.  See
;; the skill's SKILL.md and references/self-check.md.
;;
;; Usage:
;;   emacs --batch -L . -l test-isolation.el -- TESTS.el [--seed S] [--rounds N]
;;
;; The child repeats this process's `-L' and `--eval' options, so pass the
;; suite's environment controls there (this repo:
;;   --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))").
;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ti--self (or load-file-name buffer-file-name)
  "This script, so a child can load it.")

(defun ti--args ()
  "The command-line arguments after `--'."
  (let ((args command-line-args-left))
    (if (member "--" args) (cdr (member "--" args)) args)))

(defun ti--option (name args)
  "Return the value after NAME in ARGS, or nil."
  (cadr (member name args)))

(defun ti--forward-args ()
  "This process's `-L' and `--eval' options, to repeat in a child."
  (let ((args command-line-args) out)
    (while args
      (let ((a (pop args)))
        (cond ((and (equal a "-L") args) (push a out) (push (pop args) out))
              ((equal a "--eval") (push a out) (push (pop args) out))
              ((and (stringp a) (string-prefix-p "-L" a)) (push a out))
              ((and (stringp a) (string-prefix-p "--eval" a)) (push a out)))))
    (nreverse out)))

(defun ti--kind (test)
  "TEST's most recent result, as `pass', `skip' or `fail'."
  (let ((r (ert-test-most-recent-result test)))
    (cond ((ert-test-result-type-p r :passed) 'pass)
          ((ert-test-result-type-p r :skipped) 'skip)
          (t 'fail))))

(defun ti--load (file)
  "Load test FILE and return its tests, in definition order."
  (load (expand-file-name file) nil t)
  (ert-select-tests t t))

(defun ti--shuffle (tests seed round)
  "TESTS in a deterministic order, seeded by SEED and ROUND."
  (random (format "test-isolation-%s-%s" seed round))
  (let* ((vec (vconcat tests))
         (n (length vec)))
    (dotimes (i n)
      (let ((j (+ i (random (- n i)))))
        (cl-rotatef (aref vec i) (aref vec j))))
    (append vec nil)))

(defun ti--emit (file kind seed round)
  "Child mode: run FILE in KIND order and print one `ISO' line per test.
KIND is \"suite\" (one block, definition order) or \"shuffled\"."
  (let ((tests (ti--load file)))
    (if (equal kind "shuffled")
        (dolist (test (ti--shuffle tests seed (string-to-number round)))
          (ert-run-tests (list 'eql test) #'ignore))
      (ert-run-tests t #'ignore))
    (dolist (test tests)
      (princ (format "ISO %s %s\n"
                     (ti--kind test)
                     (format "%s" (ert-test-name test)))))))

(defun ti--parse (text)
  "Parse child TEXT into an alist of (NAME . KIND)."
  (let (out)
    (dolist (line (split-string text "\n"))
      (when (string-match "\\`ISO \\([a-z]+\\) \\(.+\\)\\'" line)
        (push (cons (match-string 2 line) (intern (match-string 1 line)))
              out)))
    (nreverse out)))

(defun ti--child (file kind seed round)
  "Run FILE in a child Emacs in KIND order (SEED, ROUND); return its alist."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (dir (file-name-directory (expand-file-name file))))
    (with-temp-buffer
      (let ((code (apply #'call-process emacs nil t nil
                         (append (list "--batch")
                                 (ti--forward-args)
                                 (list "-L" dir "-L" default-directory
                                       "-l" (expand-file-name ti--self) "--"
                                       "--emit" file kind
                                       (format "%s" seed) (format "%s" round))))))
        (unless (zerop code)
          (error "child Emacs failed with status %d" code)))
      (ti--parse (buffer-string)))))

(defun ti--parent (file seed rounds)
  "Compare FILE's ordered suite with ROUNDS shuffled runs; report."
  (let ((suite (ti--child file "suite" 0 0))
        (diffs (make-hash-table :test 'equal))
        (n 0))
    (dotimes (r (max 1 rounds))
      (dolist (pair (ti--child file "shuffled" (or seed 0) r))
        (let ((base (cdr (assoc (car pair) suite))))
          (unless (eq base (cdr pair))
            (puthash (car pair) (list base (cdr pair)) diffs)))))
    (princ ";; test-isolation\n")
    (maphash
     (lambda (name kinds)
       (setq n (1+ n))
       (princ (format "ORDER-DEPENDENT %s  suite=%s shuffled=%s\n"
                      name (or (nth 0 kinds) "missing")
                      (or (nth 1 kinds) "missing"))))
     diffs)
    (princ (format ";; %d tests, %d order-dependent\n"
                   (length suite) n))))

;;; Dispatch

(let* ((args (ti--args))
       (mode (car args)))
  (cond
   ;; child mode: --emit FILE suite|shuffled SEED ROUND
   ((equal mode "--emit")
    (ti--emit (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)))
   ;; parent mode: TESTS.el [--seed S] [--rounds N]
   ((and mode (not (string-prefix-p "--" mode)))
    (ti--parent mode
                (ti--option "--seed" args)
                (string-to-number (or (ti--option "--rounds" args) "3"))))
   (t (error "usage: emacs --batch -L . -l test-isolation.el -- TESTS.el [--seed S] [--rounds N]"))))
;;; test-isolation.el ends here
