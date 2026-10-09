;;; verify-options.el --- Compare :options-alist of two files  -*- lexical-binding: t; -*-
;;
;; Usage: emacs --batch -Q -l .agents/skills/ox-w3ctr-verify/scripts/verify-options.el OLD.el NEW.el
;;
;; Same idea as `verify-forms.el', for option lists: materialize the old file
;; first (`git show <rev>:ox-w3ctr.el > /tmp/old.el').
;;
;; Reads the org-export-define-backend form in each file, pulls out
;; :options-alist, and compares the entries by prin1-to-string after
;; sorting — so a pure reorder (and `( :kw` vs `(:kw` formatting)
;; reports identical.  Prints any entry that differs.

(require 'cl-lib)

(defun vo-get-options (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((form (ignore-errors (read (current-buffer))))
          opts)
      (while form
        (when (and (listp form) (eq (car form) 'org-export-define-backend))
          ;; (org-export-define-backend 'name transcoders &rest options)
          ;; → options plist starts at (nthcdr 3 form).
          (setq opts (cadr (plist-get (nthcdr 3 form) :options-alist))))
        (setq form (ignore-errors (read (current-buffer)))))
      (mapcar #'prin1-to-string opts))))

(let* ((args command-line-args-left)
       (old (cl-sort (vo-get-options (nth 0 args)) #'string<))
       (new (cl-sort (vo-get-options (nth 1 args)) #'string<)))
  (princ (format "OLD %d entries, NEW %d entries\n" (length old) (length new)))
  (if (equal old new)
      (princ "RESULT: identical option entries, order-only change\n")
    (dolist (d (cl-set-exclusive-or old new :test #'equal))
      (princ (format "DIFF: %s\n" d)))))
