;;; test-cover.el --- testcover branch coverage for an ERT file  -*- lexical-binding:t -*-
;; Instrument SOURCE with `testcover', run the test file's suite, and
;; report every instrumented form that never executed (`UNCOVERED'), with
;; the function it belongs to.  `--one-value' also reports forms that
;; always returned the same value, which testcover treats as covered but
;; flags for review.
;;
;; The coverage is the real thing: the definitions are wrapped, the
;; suite runs against them, and testcover's own marks decide.  Heuristics
;; and caveats: `define-inline' and other non-edebug forms may not be
;; instrumented at all, a form can be missed if an indirection hides it,
;; and a test that aborts early leaves its remainder uncovered.
;;
;; Usage:
;;   emacs --batch -L . -l test-cover.el -- TESTS.el SOURCE.el [--lines MIN MAX] [--one-value] [--noreturn FN]...
;;
;; `--noreturn FN' declares a function that never returns (a wrapper
;; around `signal'/'error', e.g. this repo's `org-w3ctr-error');
;; testcover marks such a call red forever otherwise, because its
;; instrumentation never runs the form after it.
;;
;; Pass the suite's environment controls (this repo:
;;   --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))").
;; The test file is loaded first (it loads SOURCE), then SOURCE is
;; instrumented and re-evaluated, then the suite runs.
;;; Code:

(require 'subr-x)
(require 'testcover)
(require 'ert)

(defun tc--args ()
  "The command-line arguments after `--'."
  (let ((args command-line-args-left))
    (if (member "--" args) (cdr (member "--" args)) args)))

(defun tc--flag (name args)
  "Non-nil when NAME appears in ARGS."
  (and (member name args) t))

(defun tc--opt-all (name args)
  "Every value that follows NAME in ARGS."
  (let (out)
    (while args
      (when (equal (car args) name)
        (push (intern (cadr args)) out))
      (setq args (cdr args)))
    (nreverse out)))

(defun tc--patch-debug-specs ()
  "Give the conditional-compilation macros an edebug spec testcover likes.
`static-if' and `static-when' ship with `debug' = t, so edebug wraps the
call before it expands and testcover fails with \"Invalid call to
`edebug-after'\".  A `(sexp body)' spec instruments the body instead."
  (when (macrop 'static-if)
    (put 'static-if 'debug '(sexp sexp &rest sexp))
    (put 'static-if 'edebug-form-spec '(sexp sexp &rest sexp)))
  (when (macrop 'static-when)
    (put 'static-when 'debug '(sexp body))
    (put 'static-when 'edebug-form-spec '(sexp body))))

(defun tc--defs ()
  "Alist (DEF-MARK . SYMBOL) for the instrumented named definitions.
Anonymous lambdas (`edebug-anonNNNN') are left out so a form inside one
is attributed to the enclosing named function instead."
  (let (out)
    (dolist (x edebug-form-data)
      (let ((sym (car x)))
        (when (and (symbolp sym) (get sym 'edebug)
                   (not (string-prefix-p "edebug-anon" (symbol-name sym))))
          (push (cons (car (get sym 'edebug)) sym) out))))
    out))

(defun tc--owner (defs pos)
  "The symbol whose definition contains POS, or nil."
  (let (owner)
    (dolist (d defs owner)
      (when (and (<= (car d) pos)
                 (or (null owner) (> (car d) (car owner))))
        (setq owner d)))))

(defun tc--mark (sym)
  "Mark SYM's uncovered forms, ignoring a definition testcover cannot mark."
  (ignore-errors (testcover-mark sym)))

(defun tc--text (pos)
  "The trimmed source line at POS, shortened for the report."
  (let ((s (save-excursion
             (goto-char pos)
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))))
    (setq s (replace-regexp-in-string "[ \t]+" " " (string-trim s)))
    (if (> (length s) 60) (concat (substring s 0 57) "...") s)))

(defun tc--collect (buffer defs one-value min max)
  "Collect coverage marks in BUFFER.
DEFS maps positions to symbols; ONE-VALUE includes the tan marks;
MIN/MAX filter by line.  Each mark is (LINE KIND FUNCTION TEXT)."
  (with-current-buffer buffer
    (let (out)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (let* ((face (overlay-get ov 'face))
               (kind (cond ((eq face 'testcover-nohits) 'uncovered)
                           ((and one-value (eq face 'testcover-1value)) 'one-value)
                           (t nil)))
               (pos (overlay-start ov))
               (line (line-number-at-pos pos)))
          (when (and kind
                     (or (null min) (>= line min))
                     (or (null max) (<= line max)))
            (push (list line kind (cdr (tc--owner defs pos)) (tc--text pos)) out))))
      (nreverse out))))

(defun tc--report (source marks instrumented)
  "Print MARKS for SOURCE, with INSTRUMENTED definitions counted."
  (let ((seen (make-hash-table :test 'equal))
        (uncovered 0)
        (one 0)
        (funcs (make-hash-table :test 'eq)))
    (princ (format ";; test-cover: %s\n" source))
    (dolist (m marks)
      (let ((key (list (nth 0 m) (nth 1 m) (nth 2 m))))
        (unless (gethash key seen)
          (puthash key t seen)
          (when (eq (nth 1 m) 'uncovered)
            (setq uncovered (1+ uncovered))
            (puthash (nth 2 m) t funcs))
          (when (eq (nth 1 m) 'one-value) (setq one (1+ one)))
          (princ (format "%-10s %s:%d  %s  %s\n"
                         (if (eq (nth 1 m) 'uncovered) "UNCOVERED" "ONE-VALUE")
                         source (nth 0 m) (or (nth 2 m) "?") (or (nth 3 m) ""))))))
    (princ (format ";; %d instrumented, %d uncovered forms in %d functions; %d one-value\n"
                   instrumented uncovered (hash-table-count funcs) one))))

;;; Main

(let* ((args (tc--args))
       (tests-file (nth 0 args))
       (source (nth 1 args))
       (lines (member "--lines" args))
       (min (and lines (string-to-number (cadr lines))))
       (max (and lines (string-to-number (caddr lines))))
       (one-value (tc--flag "--one-value" args))
       (noreturns (tc--opt-all "--noreturn" args)))
  (unless (and tests-file source)
    (error "usage: emacs --batch -L . -l test-cover.el -- TESTS.el SOURCE.el [--lines MIN MAX] [--one-value]"))
  (load (expand-file-name tests-file) nil t)
  (dolist (fn noreturns)
    (add-to-list 'testcover-noreturn-functions fn))
  (tc--patch-debug-specs)
  ;; `testcover-start' re-evaluates SOURCE with `eval-buffer' while this
  ;; script is itself being `load'ed, so `load-in-progress' is t and
  ;; `load-file-name' names the script: a `(defconst DIR (if
  ;; load-in-progress (file-name-directory load-file-name) ...))' in the
  ;; source would then compute the script's directory.  Make the source's
  ;; own directory the answer.
  (let* ((abs (expand-file-name source))
         (load-in-progress nil)
         (default-directory (file-name-directory abs)))
    (testcover-start abs))
  ;; The instrumented buffer is the one the definition markers point at;
  ;; reopening SOURCE by name can land on a different buffer.
  (let* ((defs (tc--defs))
         (buffer (if defs
                     (marker-buffer (car (car defs)))
                   (find-file-noselect (expand-file-name source))))
         (stats nil))
    (setq stats (ert-run-tests t #'ignore))
    (dolist (x edebug-form-data)
      (when (get (car x) 'edebug) (tc--mark (car x))))
    (tc--report source
                (tc--collect buffer defs one-value min max)
                (length defs))
    (princ (format ";; suite: %d expected, %d unexpected\n"
                   (ert-stats-completed-expected stats)
                   (ert-stats-completed-unexpected stats)))))
;;; test-cover.el ends here
