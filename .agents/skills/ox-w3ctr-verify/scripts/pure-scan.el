;;; pure-scan.el --- find (pure t) fns reaching t--pget/t--pput  -*- lexical-binding:t -*-
(require 'cl-lib)

(defun ps-walk (form fn)
  (funcall fn form)
  (cond ((consp form) (ps-walk (car form) fn) (ps-walk (cdr form) fn))
        ((and (vectorp form) (not (recordp form)))
         (mapc (lambda (x) (ps-walk x fn)) form))))

(defun ps-collect (form fns pure)
  "Collect every defun/defsubst/define-inline anywhere in FORM."
  (cond
   ((and (consp form) (memq (car form) '(defun defsubst define-inline)))
    (puthash (cadr form) (cddr form) fns)
    (when (assq 'pure (cdr (assq 'declare (cddr form))))
      (puthash (cadr form) t pure))
    (dolist (x (cddr form)) (ps-collect x fns pure)))
   ((consp form) (ps-collect (car form) fns pure) (ps-collect (cdr form) fns pure))
   ((and (vectorp form) (not (recordp form)))
    (mapc (lambda (x) (ps-collect x fns pure)) form))))

(with-temp-buffer
  (setq-local read-symbol-shorthands '(("t-" . "org-w3ctr-")))
  (insert-file-contents "ox-w3ctr.el")
  (goto-char (point-min))
  (let (forms)
    (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
    (setq forms (nreverse forms))
    (let ((fns (make-hash-table :test 'eq))
          (pure (make-hash-table :test 'eq))
          pure-list)
      (dolist (f forms) (ps-collect f fns pure))
      (maphash (lambda (k _) (push k pure-list)) pure)
      (let ((callees (lambda (name)
                       (let (out)
                         (ps-walk (gethash name fns)
                                  (lambda (x)
                                    (when (and (symbolp x) (gethash x fns))
                                      (push x out))))
                         (cl-remove-duplicates out)))))
        (princ (format "functions: %d, pure: %d\n"
                       (hash-table-count fns) (hash-table-count pure)))
        (dolist (p pure-list)
          (let ((seen (list p)) (queue (list p)) (hit nil))
            (while (and queue (not hit))
              (let ((cur (pop queue)))
                (dolist (c (funcall callees cur))
                  (cond ((memq c '(org-w3ctr--pget org-w3ctr--pput)) (setq hit c))
                        ((not (memq c seen)) (push c seen) (push c queue))))))
            (when hit
              (princ (format "VIOLATION %s -> %s\n" p hit)))))))))
;;; pure-scan.el ends here
