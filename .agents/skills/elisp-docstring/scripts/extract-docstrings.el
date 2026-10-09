;;; extract-docstrings.el --- Dump the prose of a source region  -*- lexical-binding: t -*-

;; Usage:
;;   emacs --batch -l extract-docstrings.el FILE START-REGEX END-REGEX OUT [SHORTHAND]
;;
;; Prints the prose of a source file's region -- comment lines verbatim,
;; then every definition's kind, name, argument list (or default value),
;; `declare' forms and docstring -- and nothing else, so that a "reader"
;; model can be asked what the text alone does not answer.  See
;; references/two-role-pass.md for the pass this feeds.
;;
;; START-REGEX matches the first line of the region, END-REGEX the line
;; that ends it.  The region must hold whole top-level forms.  SHORTHAND,
;; when given (for example "t-"), is read as a symbol shorthand for the
;; package prefix, so that printed names are the real ones.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar xd-kinds
  '(defun defmacro defsubst defvar defconst defcustom defvar-local
    define-inline oclosure-define cl-defstruct cl-defgeneric)
  "Definition forms whose documentation `xd-extract' prints.")

(defvar xd-sig-kinds '(defun defmacro defsubst define-inline cl-defgeneric)
  "Definition forms whose argument list `xd-extract' prints.")

(defun xd-region (file start end)
  "Return the lines of FILE between the START and END regexps."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (re-search-forward start nil t)
      (error "Start regexp %S not found in %s" start file))
    (let ((beg (line-beginning-position)))
      (unless (re-search-forward end nil t)
        (error "End regexp %S not found in %s" end file))
      (split-string (buffer-substring-no-properties beg (line-beginning-position))
                    "\n"))))

(defun xd-read-forms (text shorthand)
  "Read the top-level forms of TEXT.  SHORTHAND is a symbol prefix or nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((read-symbol-shorthands (and shorthand (list (cons shorthand "org-w3ctr-"))))
          (forms nil))
      (condition-case err
          (while t (push (read (current-buffer)) forms))
        ((end-of-file quit) nil)
        ((error) (message "Stopped reading: %S" err)))
      (nreverse forms))))

(defun xd-flatten (forms)
  "Splice `eval-and-compile', `eval-when-compile' and `progn' wrappers out of FORMS."
  (let (leaves)
    (dolist (form forms (nreverse leaves))
      (if (and (consp form)
               (memq (car form) '(eval-and-compile eval-when-compile progn)))
          (dolist (leaf (xd-flatten (cdr form)))
            (push leaf leaves))
        (push form leaves)))))

(defun xd-field (form)
  "Return (KIND NAME SIGNATURE DECLARATIONS DOC) for FORM, or nil.
FORM must be one of `xd-kinds'."
  (when (and (consp form) (memq (car form) xd-kinds))
    (let* ((kind (car form))
           (name (format "%s" (cadr form)))
           (rest (cddr form))
           (sig (cond ((memq kind xd-sig-kinds)
                       (if (null (car rest))
                           "()"
                         (format "%S" (car rest))))
                      ((memq kind '(defvar defconst defcustom defvar-local))
                       (format "default: %s" (truncate-string-to-width
                                              (format "%S" (car rest)) 60 nil nil "...")))))
           (decl (cl-find-if (lambda (x) (and (consp x) (eq (car x) 'declare)))
                             rest)))
      (list kind name sig (and decl (format "%S" (cdr decl)))
            (cl-find-if #'stringp rest)))))

(defun xd-extract (file start end out shorthand)
  "Write the prose of FILE's region to OUT and print a one-line tally."
  (let ((lines (xd-region file start end))
        (count 0))
    (with-temp-file out
      (insert ";; ---- comments ----\n")
      (dolist (l lines)
        (when (or (string-empty-p (string-trim l))
                  (string-prefix-p ";" (string-trim-left l)))
          (insert (string-trim-right l) "\n")))
      (insert "\n;; ---- docstrings ----\n")
      (dolist (form (xd-flatten (xd-read-forms (string-join lines "\n") shorthand)))
        (pcase-let ((`(,kind ,name ,sig ,decl ,doc) (xd-field form)))
          (when doc
            (cl-incf count)
            (insert (format ";;; [%s] %s%s\n%s\n%s\n"
                            kind name (if sig (concat "  " sig) "")
                            doc (if decl (format ";;; declares: %s" decl) "")))))))
    (princ (format "wrote %s (%d bytes, %d docstrings)\n"
                   out (file-attribute-size (file-attributes out)) count))))

(let ((args command-line-args-left))
  (if (< (length args) 4)
      (princ "usage: emacs --batch -l extract-docstrings.el FILE START-REGEX END-REGEX OUT [SHORTHAND]\n")
    (xd-extract (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args))))
