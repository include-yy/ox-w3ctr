;;; test-map.el --- map source functions to the tests that reference them  -*- lexical-binding:t -*-
;; Report the source functions no test references (`UNCOVERED') and the
;; tests that reference no source function at all (`ORPHAN-TEST').  With
;; `--map', also print the function-to-test database
;; (`MAP FUNCTION TEST...'), which the coverage phase needs to pick the
;; tests for a function.
;;
;; Heuristic, not a gate.  A reference is the test's name (a test is
;; named after the function under test) or any occurrence of the function
;; symbol in its body, so a name in a quoted datum counts also.  The
;; source's `defun's are found by scanning for a def head at the start of
;; a line, so a definition nested in `eval-and-compile' (e.g. an inlined
;; OINFO helper) is found too, with its real line.
;;
;; Both files are read with their own `read-symbol-shorthands', so `t-foo'
;; in the source and in the tests resolve alike.
;;
;; Usage:
;;   emacs --batch -Q -l test-map.el -- SOURCE.el TESTS.el [--lines MIN MAX] [--map]
;;
;; `--lines MIN MAX' restricts `UNCOVERED'/`MAP' to functions whose def
;; starts on those lines; `ORPHAN-TEST' is always judged against every
;; source function.
;;; Code:

(require 'cl-lib)

(defconst tm--def-heads
  "defun\\|defsubst\\|define-inline\\|defmacro"
  "Function-defining heads whose test coverage is tracked.")

(defconst tm--var-heads
  "defconst\\|defvar\\|defvar-local\\|defcustom"
  "Variable-defining heads: tracked for `ORPHAN-TEST' only.")

(defun tm--args ()
  "The command-line arguments after `--'."
  (let ((args command-line-args-left))
    (if (member "--" args) (cdr (member "--" args)) args)))

(defun tm--option (name args)
  "Return the value after NAME in ARGS, or nil."
  (cadr (member name args)))

(defun tm--flag (name args)
  "Non-nil when NAME appears in ARGS."
  (and (member name args) t))

(defun tm--shorthands (file)
  "Read `read-symbol-shorthands' from FILE's local variables, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (and (re-search-forward "read-symbol-shorthands:[ \t]*" nil t)
         (read (current-buffer)))))

(defun tm--expand (name shorthands)
  "Expand shorthand symbol NAME to its full symbol, or NAME as a symbol.
SHORTHANDS is the same (SHORT . LONG) alist `read' would apply."
  (let (best)
    (dolist (pair shorthands)
      (let ((short (car pair)))
        (when (and (string-prefix-p short name)
                   (or (null best)
                       (> (length short) (length (car best)))))
          (setq best pair))))
    (if best
        (intern (concat (cdr best) (substring name (length (car best)))))
      (intern name))))

(defun tm--read-forms (file shorthands)
  "Return (LINE . FORM) for each top-level form in FILE.
SHORTHANDS, when non-nil, is bound for the reads."
  (with-temp-buffer
    (when shorthands (setq-local read-symbol-shorthands shorthands))
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (skip-chars-forward " \t\n\r")
            (let ((beg (point)))
              (push (cons (line-number-at-pos beg) (read (current-buffer)))
                    forms)))
        (end-of-file nil))
      (nreverse forms))))

(defun tm--scan-defs (file shorthands)
  "List (NAME LINE KIND) for every def form in FILE.
KIND is `fn' for a function and `var' for a variable.  A def head at the
start of a line is found at any nesting depth, so a definition inside
`eval-and-compile' keeps its real line."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((regexp (format "^[ \t]*(\\(%s\\|%s\\)[ \t\n]+\\([^ \t\n()]+\\)"
                          tm--def-heads tm--var-heads))
          out)
      (while (re-search-forward regexp nil t)
        (let* ((head (match-string 1))
               (kind (if (string-match-p tm--var-heads head) 'var 'fn)))
          (push (list (tm--expand (match-string 2) shorthands)
                      (line-number-at-pos (match-beginning 0))
                      kind)
                out)))
      (nreverse out))))

(defun tm--walk (form fn)
  "Call FN on FORM and every sub-form (elements and vectors)."
  (funcall fn form)
  (cond ((consp form)
         (tm--walk (car form) fn)
         (tm--walk (cdr form) fn))
        ((and (vectorp form) (not (recordp form)))
         (mapc (lambda (x) (tm--walk x fn)) form))))

(defun tm--symbols (form)
  "Every symbol in FORM, including inside quoted data."
  (let (out)
    (tm--walk form (lambda (f) (when (symbolp f) (push f out))))
    out))

(defun tm--tests (forms)
  "List (NAME LINE SYMBOLS) for the `ert-deftest's in FORMS."
  (let (out)
    (dolist (pair forms)
      (let ((form (cdr pair)))
        (when (and (consp form) (eq (car form) 'ert-deftest))
          (push (list (cadr form) (car pair) (tm--symbols (cddr form))) out))))
    (nreverse out)))

(defun tm--report (source tests-file all known range tests map)
  "Print the findings for RANGE, judged against ALL functions and TESTS.
KNOWN is every defined name (functions and variables); MAP prints the map."
  (let ((fnames (make-hash-table :test 'eq))
        (knownset (make-hash-table :test 'eq))
        (covered (make-hash-table :test 'eq))
        (uncovered 0)
        (orphan 0))
    ;; Index the function set for coverage, and every def for `orphan'.
    (dolist (fn all) (puthash (car fn) t fnames))
    (dolist (name known) (puthash name t knownset))
    ;; Record each test's references: its name (a test is named after the
    ;; function under test) plus every symbol in its body.  A test that
    ;; names a function several times is recorded once for it.
    (dolist (test tests)
      (let (seen)
        (dolist (s (cons (nth 0 test) (nth 2 test)))
          (when (and (gethash s fnames) (not (memq s seen)))
            (push s seen)
            (puthash s (cons (nth 0 test) (gethash s covered)) covered)))))
    (princ ";; test-map\n")
    (dolist (fn range)
      (let ((ts (nreverse (gethash (car fn) covered))))
        (cond (ts (when map
                   (princ (format "MAP %s %s\n" (car fn)
                                  (mapconcat #'symbol-name ts " ")))))
              (t (setq uncovered (1+ uncovered))
                 (princ (format "UNCOVERED %s:%d  %s\n"
                                source (nth 1 fn) (car fn)))))))
    (dolist (test tests)
      (unless (cl-some (lambda (s) (gethash s knownset))
                       (cons (nth 0 test) (nth 2 test)))
        (setq orphan (1+ orphan))
        (princ (format "ORPHAN-TEST %s:%d  %s\n"
                       tests-file (nth 1 test) (nth 0 test)))))
    (princ (format ";; %d functions, %d uncovered; %d tests, %d orphan\n"
                   (length range) uncovered (length tests) orphan))))

;;; Main

(let* ((args (tm--args))
       (source (nth 0 args))
       (tests-file (nth 1 args))
       (lines (member "--lines" args))
       (min (and lines (string-to-number (cadr lines))))
       (max (and lines (string-to-number (caddr lines))))
       (map (tm--flag "--map" args)))
  (unless (and source tests-file)
    (error "usage: emacs --batch -Q -l test-map.el -- SOURCE.el TESTS.el [--lines MIN MAX] [--map]"))
  (let* ((defs (tm--scan-defs source (tm--shorthands source)))
         (all (cl-remove-if-not (lambda (d) (eq (nth 2 d) 'fn)) defs))
         (known (mapcar #'car defs))
         (tests (tm--tests
                 (tm--read-forms tests-file (tm--shorthands tests-file))))
         (range (cl-remove-if-not
                 (lambda (fn)
                   (and (or (null min) (>= (nth 1 fn) min))
                        (or (null max) (<= (nth 1 fn) max))))
                 all)))
    ;; ORPHAN-TEST is judged against ALL definitions, so --lines does not
    ;; make a test look orphan just because its function is out of range.
    (tm--report source tests-file all known range tests map)))
;;; test-map.el ends here
