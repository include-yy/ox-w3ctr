;;; scan-ftype.el --- count functions lacking a declare ftype  -*- lexical-binding:t -*-
(with-temp-buffer
  (setq-local read-symbol-shorthands '(("t-" . "org-w3ctr-")))
  (setq-local load-prefer-newer t)
  (insert-file-contents "ox-w3ctr.el")
  (goto-char (point-min))
  (let ((missing nil) (total 0) (have 0))
    (condition-case nil
        (while t
          (let ((f (read (current-buffer))))
            (when (and (consp f) (memq (car f) '(defun defsubst)))
              (setq total (1+ total))
              (let* ((name (cadr f))
                     (body (cddr f))
                     (decl (assq 'declare body))
                     (has-ftype (and decl (assq 'ftype (cdr decl))))
                     (interactive (assq 'interactive body)))
                (if has-ftype
                    (setq have (1+ have))
                  (unless (or (eq (car f) 'defsubst) interactive)
                    (push (symbol-name name) missing)))))))
      (end-of-file nil))
    (princ (format "total defun/defsubst: %d, with ftype: %d\n" total have))
    (princ (format "missing (non-defsubst, non-interactive): %d\n" (length missing)))
    (dolist (n (nreverse missing)) (princ (format "  %s\n" n)))))
;;; scan-ftype.el ends here
