;;; order-check.el --- check that tests follow the source function order  -*- lexical-binding:t -*-
;; Report the tests that appear out of source order inside their
;; section, and the sections out of source order, in a suite that
;; mirrors the source: a `;;;;' section per source section, and inside a
;; section one test per source function in the function's source order,
;; a variant test (whose name the function's name prefixes) next to its
;; target.
;;
;; The check is textual.  A test is mapped to the longest source name it
;; names (equal, or the source name followed by `-'); a test that names
;; nothing in its section is skipped -- that is `test-map.el''s
;; `ORPHAN-TEST'.  Variables and constants without a test of their own
;; are skipped, so they never break the order.
;;
;; Both files are read with their own `read-symbol-shorthands', so `t-foo'
;; in the source and in the tests expand alike.
;;
;; Usage:
;;   emacs --batch -Q -l order-check.el -- SOURCE.el TESTS.el [--part NAME]
;;
;; `--part NAME' restricts the check to sections under that major part
;; (e.g. "Greater elements").
;;; Code:

(require 'cl-lib)

(defconst oc--def-re
  "^[ \t]*(\\(?:defun\\|defsubst\\|define-inline\\|defmacro\\|defconst\\|defvar\\|defvar-local\\|defcustom\\)[ \t]+\\([^ \t\n()]+\\)"
  "Regexp matching a def head and capturing the defined name.")

(defconst oc--test-re
  "^[ \t]*(ert-deftest[ \t]+\\([^ \t\n()]+\\)"
  "Regexp matching an `ert-deftest' head and capturing its name.")

(defun oc--args ()
  "The command-line arguments after `--'."
  (let ((args command-line-args-left))
    (if (member "--" args) (cdr (member "--" args)) args)))

(defun oc--option (name args)
  "Return the value after NAME in ARGS, or nil."
  (cadr (member name args)))

(defun oc--shorthands (file)
  "Read `read-symbol-shorthands' from FILE's local variables, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (and (re-search-forward "read-symbol-shorthands:[ \t]*" nil t)
         (read (current-buffer)))))

(defun oc--expand (name shorthands)
  "Expand shorthand NAME to its full string, or return NAME.
SHORTHANDS is the same (SHORT . LONG) alist `read' would apply."
  (let (best)
    (dolist (pair shorthands)
      (let ((short (car pair)))
        (when (and (string-prefix-p short name)
                   (or (null best)
                       (> (length short) (length (car best)))))
          (setq best pair))))
    (if best
        (concat (cdr best) (substring name (length (car best))))
      name)))

(defun oc--collect (file regexp shorthands)
  "Return ((PART . SECTION) . ITEMS) for FILE, in order of appearance.
ITEMS are (LINE NAME) for each REGEXP match in the section, NAME
expanded through SHORTHANDS."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (out part section)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^;;; \\(.+\\)$" line)
            (setq part (match-string 1 line) section nil))
           ((string-match "^;;;; \\(.+\\)$" line)
            (setq section (match-string 1 line)))
           ((and section (string-match regexp line))
            (let* ((key (cons part section))
                   (cell (assoc key out))
                   (item (list (line-number-at-pos)
                               (oc--expand (match-string 1 line) shorthands))))
              (if cell
                  (setcdr cell (cons item (cdr cell)))
                (push (cons key (list item)) out))))))
        (forward-line 1))
      (dolist (cell out) (setcdr cell (nreverse (cdr cell))))
      (nreverse out))))

(defun oc--sections (file)
  "Return (KEY . LINE) for every `;;;;' section in FILE, in order.
KEY is (PART . SECTION), PART being the enclosing `;;;' header."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (out part)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^;;; \\(.+\\)$" line)
            (setq part (match-string 1 line)))
           ((string-match "^;;;; \\(.+\\)$" line)
            (push (cons (cons part (match-string 1 line))
                        (line-number-at-pos))
                  out))))
        (forward-line 1))
      (nreverse out))))

(defun oc--match (test names)
  "Return the longest name in NAMES that TEST names, or nil.
TEST names a name when the two are equal, or the name is followed by
`-' in TEST (a variant test)."
  (let (best)
    (dolist (name names)
      (when (and (string-prefix-p name test)
                 (or (= (length name) (length test))
                     (eq ?- (aref test (length name)))))
        (when (or (null best) (> (length name) (length best)))
          (setq best name))))
    best))

;;; Main

(let* ((args (oc--args))
       (source (nth 0 args))
       (tests-file (nth 1 args))
       (part (oc--option "--part" args)))
  (unless (and source tests-file)
    (error "usage: emacs --batch -Q -l order-check.el -- SOURCE.el TESTS.el [--part NAME]"))
  (let* ((src (oc--collect source oc--def-re (oc--shorthands source)))
         (tst (oc--collect tests-file oc--test-re (oc--shorthands tests-file)))
         (src-by-key (make-hash-table :test 'equal))
         (sec-index (make-hash-table :test 'equal))
         (checked 0)
         (flagged 0))
    (dolist (cell src) (puthash (car cell) (cdr cell) src-by-key))
    (let ((i 0))
      (dolist (cell (oc--sections source))
        (puthash (car cell) (cons i (cdr cell)) sec-index)
        (setq i (1+ i))))
    (princ ";; order-check\n")
    ;; Section order: the tested sections must follow the source order.
    (let ((max -1) (prev nil))
      (dolist (cell (oc--sections tests-file))
        (let ((info (gethash (car cell) sec-index)))
          (when (and info (or (null part) (equal (caar cell) part)))
            (when (< (car info) max)
              (setq flagged (1+ flagged))
              (princ (format "SECTION-ORDER %s:%d  %s  maps to source line %d, after %s (source line %d)\n"
                             tests-file (cdr cell) (cdr (car cell))
                             (cdr info) (car prev) (cdr prev))))
            (when (> (car info) max)
              (setq max (car info)
                    prev (cons (cdr (car cell)) (cdr info))))))))
    ;; Function order inside each section.
    (dolist (cell tst)
      (let ((names (mapcar #'cadr (gethash (car cell) src-by-key))))
        (when (and names (or (null part) (equal (caar cell) part)))
          (setq checked (1+ checked))
          (let ((index (make-hash-table :test 'equal))
                (srcline (make-hash-table :test 'equal))
                (j 0)
                (max -1)
                (prev nil)
                (bad nil))
            (dolist (item (gethash (car cell) src-by-key))
              (puthash (cadr item) j index)
              (puthash (cadr item) (car item) srcline)
              (setq j (1+ j)))
            (dolist (item (cdr cell))
              (let ((mapped (oc--match (cadr item) names)))
                (when mapped
                  (let ((idx (gethash mapped index)))
                    (when (< idx max)
                      (setq bad t)
                      (princ (format "OUT-OF-ORDER %s:%d  %s  %s: maps to %s (source line %d), after %s (source line %d)\n"
                                     tests-file (car item) (cadr item)
                                     (cdr (car cell))
                                     mapped (gethash mapped srcline)
                                     (car prev) (cdr prev))))
                    (when (> idx max)
                      (setq max idx
                            prev (cons mapped (gethash mapped srcline))))))))
            (when bad (setq flagged (1+ flagged)))))))
    (princ (format ";; %d sections checked, %d out of order\n" checked flagged))))
;;; order-check.el ends here
