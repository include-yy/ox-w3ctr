;;; indent-check.el --- report lines `indent-region' would change  -*- lexical-binding:t -*-
;; Report every line whose leading whitespace Emacs's own indentation
;; would change, i.e. every line that is not already canonical.
;;
;; Usage, from the repo root:
;;
;;   emacs --batch -L . -l scripts/indent-check.el ox-w3ctr.el
;;   emacs --batch -L . -l scripts/indent-check.el ox-w3ctr-tests.el
;;
;; The file is LOADed before indenting, so the helper macros'
;; `(declare (indent ...))' specs are in place.  Without that, the test
;; macros are unknown, `indent-region' falls back to generic indentation
;; and reports hundreds of false deviations (523 vs 32 on the test
;; file).  Some deliberate alignment (e.g. `org-w3ctr-public-license-alist')
;; is a fixed point of neither rule and is left alone.
(let* ((file (or (car command-line-args-left) "ox-w3ctr.el")))
  (load (expand-file-name file) nil t)
  (find-file (expand-file-name file))
  (emacs-lisp-mode)
  (let ((orig (split-string (buffer-string) "\n")) (n 0))
    (indent-region (point-min) (point-max))
    (let ((new (split-string (buffer-string) "\n")))
      (dotimes (i (length orig))
        (unless (equal (nth i orig) (nth i new))
          (setq n (1+ n))
          (princ (format "%s:%d: %S -> %S\n"
                         file (1+ i) (nth i orig) (nth i new))))))
    (princ (format "%s: %d non-canonical line(s)\n" file n))))
;;; indent-check.el ends here
