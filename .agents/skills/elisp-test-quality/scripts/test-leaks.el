;;; test-leaks.el --- run an ERT file and diff global state  -*- lexical-binding:t -*-
;; Load a test file, then run each test alone and diff the live buffers,
;; processes and advised functions around it, attributing a leak to the
;; test that caused it.  One-time lazy initialization (Org parsing, its
;; advice) shows on whichever test first triggers it: triage that
;; cluster, do not read it as that test's fault.
;;
;; Heuristic, not a gate: a suite may create a buffer on purpose, and
;; ERT itself may hold one; triage the output.  See the skill's
;; SKILL.md for the seeded-defect self-check every checker must pass.
;;
;; Usage:
;;   emacs --batch -L . -l test-leaks.el -- TESTS.el
;;; Code:

(require 'cl-lib)
(require 'ert)

(defun tl--advised ()
  "Return the symbols whose function cell currently carries advice."
  (let (out)
    (mapatoms
     (lambda (s)
       (when (and (fboundp s)
                  (condition-case nil
                      (advice--p (symbol-function s))
                    (error nil)))
         (push s out))))
    (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun tl--snapshot ()
  "Return the global state a test run must leave alone."
  (list :buffers (sort (delq nil (mapcar #'buffer-name (buffer-list)))
                       #'string<)
        :processes (sort (delq nil (mapcar #'process-name (process-list)))
                         #'string<)
        :advised (tl--advised)))

(defun tl--new (before after)
  "Elements of AFTER that are not in BEFORE, under EQUAL."
  (cl-set-difference after before :test #'equal))

(defun tl--run (test)
  "Run TEST alone, discarding its output.  Return its stats."
  (ert-run-tests (list 'eql test) #'ignore))

(defun tl--report (tag test item)
  "Print one leak line for ITEM, tagged TAG, under TEST."
  (princ (format "%-13s %s  %s\n" tag (ert-test-name test) item)))

(let* ((args command-line-args-left)
       (args (if (member "--" args) (cdr (member "--" args)) args))
       (file (car args))
       (tests nil)
       (failed 0)
       (leaks 0))
  (unless file
    (error "usage: emacs --batch -L . -l test-leaks.el -- TESTS.el"))
  (load (expand-file-name file) nil t)
  (setq tests (ert-select-tests t t))
  (princ ";; test-leaks\n")
  ;; Snapshot around each test, so a leak is attributed to it.  One-time
  ;; lazy initialization (Org parsing, advice) shows on whichever test is
  ;; first; triage that cluster, do not read it as that test's fault.
  (dolist (test tests)
    (let ((before (tl--snapshot)))
      (let ((stats (tl--run test)))
        (setq failed (+ failed (ert-stats-completed-unexpected stats))))
      (let ((after (tl--snapshot)))
        (dolist (b (tl--new (plist-get before :buffers)
                            (plist-get after :buffers)))
          (setq leaks (1+ leaks)) (tl--report "LEAK-BUFFER" test b))
        (dolist (p (tl--new (plist-get before :processes)
                            (plist-get after :processes)))
          (setq leaks (1+ leaks)) (tl--report "LEAK-PROCESS" test p))
        (dolist (a (cl-set-difference (plist-get after :advised)
                                      (plist-get before :advised)
                                      :test #'eq))
          (setq leaks (1+ leaks)) (tl--report "LEAK-ADVICE" test a)))))
  (princ (format ";; %d tests, %d unexpected; leaks: %d\n"
                 (length tests) failed leaks)))
;;; test-leaks.el ends here
