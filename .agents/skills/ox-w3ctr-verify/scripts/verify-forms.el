;;; verify-forms.el --- Compare def* forms of two Elisp files  -*- lexical-binding: t; -*-
;;
;; Usage: emacs --batch -Q -l .agents/skills/ox-w3ctr-verify/scripts/verify-forms.el OLD.el NEW.el
;;
;; Use it to check a pure reorder / move of definitions: materialize the old
;; version (`git show <rev>:ox-w3ctr.el > /tmp/old.el') and compare it with
;; the new one.
;;
;; Reports whether every defgroup / defcustom / defconst / defvar /
;; defsubst / defun form in NEW.el has the identical Lisp structure
;; (prin1-to-string) as in OLD.el — i.e. a reorder changed nothing but
;; order.  Prints MISSING / ADDED / CHANGED symbol lists, then a
;; one-line RESULT.

(require 'cl-lib)

(defun vf-collect (file)
  "Collect ((NAME . PRIN1) ...) for every def* form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((form (ignore-errors (read (current-buffer))))
          result)
      (while form
        (when (and (listp form)
                   (memq (car form)
                         '(defgroup defcustom defconst defvar defsubst defun)))
          (push (cons (symbol-name (nth 1 form))
                      (prin1-to-string form))
                result))
        (setq form (ignore-errors (read (current-buffer)))))
      result)))

(defun vf-report (label items)
  (princ (format "%s (%d): %s\n"
                 label (length items)
                 (mapconcat #'identity (sort items #'string<) " "))))

(let* ((args command-line-args-left)
       (old (vf-collect (nth 0 args)))
       (new (vf-collect (nth 1 args)))
       (new-names (make-hash-table :test #'equal))
       (old-names (make-hash-table :test #'equal))
       missing added changed)
  (dolist (x old) (puthash (car x) (cdr x) old-names))
  (dolist (x new) (puthash (car x) (cdr x) new-names))
  (dolist (x old)
    (let ((n (car x)))
      (if (gethash n new-names)
          (unless (string= (cdr x) (gethash n new-names))
            (push n changed))
        (push n missing))))
  (dolist (x new)
    (unless (gethash (car x) old-names)
      (push (car x) added)))
  (princ (format "OLD %d forms, NEW %d forms\n" (length old) (length new)))
  (vf-report "MISSING" missing)
  (vf-report "ADDED" added)
  (vf-report "CHANGED" changed)
  (princ (if (and (null missing) (null added) (null changed))
             "RESULT: identical definitions, order-only change\n"
           "RESULT: DIFFERENCES FOUND\n")))
