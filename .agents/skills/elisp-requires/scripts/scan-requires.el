;;; scan-requires.el --- report require coverage of an Elisp file -*- lexical-binding: t; -*-

;; Scan an Emacs Lisp file and report, for its `require' forms:
;;
;;   * REQUIRED: every `(require 'LIB)' with the count of LIB's symbols
;;     that appear in the file's code; a count of 0 flags a likely
;;     redundant require.
;;   * USED BUT NOT REQUIRED: libraries whose symbols appear in the
;;     code but are not required.  `feature=' is t when the library is
;;     already loaded (preloaded or pulled in transitively), nil when
;;     the code relies on autoload alone.
;;
;; Usage (from the repository root):
;;
;;   emacs --batch -L . -l .agents/skills/elisp-requires/scripts/scan-requires.el -- FILE
;;
;; FILE defaults to ox-w3ctr.el.  The scanner reads the file's own
;; `read-symbol-shorthands' (from its Local Variables) so t-* symbols
;; resolve like they do in the source.

(require 'seq)

(setq load-prefer-newer t)

(defun sr--target ()
  "Return the file named after `--' in the command line, or the default."
  (let ((args command-line-args-left))
    (or (cadr (member "--" args))
        (car args)
        "ox-w3ctr.el")))

(defun sr--read-shorthand (file)
  "Read `read-symbol-shorthands' from FILE's local variables, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (and (re-search-forward "read-symbol-shorthands:[ \t]*" nil t)
         (read (current-buffer)))))

(defun sr--collect (form table)
  "Record every non-keyword symbol in FORM into TABLE (a hash table)."
  (cond
   ((symbolp form)
    (unless (keywordp form)
      (puthash (symbol-name form) t table)))
   ((consp form)
    (sr--collect (car form) table)
    (sr--collect (cdr form) table))
   ((vectorp form)
    (seq-doseq (e form) (sr--collect e table))))
  table)

(defun sr--requires (text)
  "Return an alist (LIB . LINE) for each (require 'LIB) in TEXT."
  (let (reqs)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "(require[ \t\n]+'\\([^)]+\\))" nil t)
        (push (cons (match-string 1)
                    (line-number-at-pos (match-beginning 0)))
              reqs)))
    (nreverse reqs)))

(let* ((file (sr--target))
       (shorthand (sr--read-shorthand file))
       (self-lib (file-name-sans-extension (file-name-nondirectory file)))
       (text (with-temp-buffer
               (insert-file-contents file)
               (buffer-substring-no-properties (point-min) (point-max))))
       (reqs (sr--requires text))
       (table (make-hash-table :test 'equal)))
  ;; Load the target so `symbol-file' resolves transitive deps to their
  ;; real library (not the autoload file).
  (load (file-name-sans-extension file) nil t)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((read-symbol-shorthands shorthand))
      (condition-case nil
          (while t
            (setq table (sr--collect (read (current-buffer)) table)))
        (end-of-file nil))))
  (let ((by-lib (make-hash-table :test 'equal)))
    (maphash
     (lambda (name _)
       (let ((sym (intern-soft name)))
         (when (and sym (or (fboundp sym) (boundp sym)))
           (let ((src (symbol-file sym)))
             (when src
               (let ((lib (file-name-sans-extension
                           (file-name-nondirectory src))))
                 (unless (equal lib self-lib)
                   (push name (gethash lib by-lib)))))))))
     table)
    (let ((required (delete-dups (mapcar #'car reqs))))
      (princ "=== REQUIRED ===\n")
      (dolist (r reqs)
        (let ((syms (sort (delete-dups (gethash (car r) by-lib)) #'string<)))
          (princ (format "  %-16s line %4d  used=%-3d %s\n"
                         (car r) (cdr r) (length syms)
                         (if syms (mapconcat #'identity syms " ")
                           "UNUSED")))))
      (princ "\n=== USED BUT NOT REQUIRED ===\n")
      (let (cands)
        (maphash
         (lambda (lib syms)
           (unless (member lib required)
             (push (cons lib (sort (delete-dups syms) #'string<)) cands)))
         by-lib)
        (dolist (e (sort cands (lambda (a b) (string< (car a) (car b)))))
          (princ (format "  %-18s feature=%-6s %s\n"
                         (car e)
                         (prin1-to-string (featurep (intern (car e))))
                         (mapconcat #'identity (cdr e) " "))))))))

;;; scan-requires.el ends here
