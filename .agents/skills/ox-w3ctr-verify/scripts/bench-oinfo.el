;;; bench-oinfo.el --- Measure the OINFO cache against `plist-get'  -*- lexical-binding: t; -*-
;;
;; Usage: from a build directory of ox-w3ctr, with this skill's scripts
;; directory on the load path (see references/harness.md):
;;
;;   SKILLDIR=.agents/skills/ox-w3ctr-verify/scripts
;;   OINFO=on emacs --batch -L . -L "$SKILLDIR" -l bench-oinfo
;;
;; Exports DRAFTS/micro-doc first to capture a *real* INFO plist, then times,
;; for a few keys, `plist-get' on that plist and — when the cache is built in
;; — `(funcall 'oclosure info)': funcall on the closure *symbol*, which is
;; exactly what the inlined `t--pget' call site compiles into.  The pair index
;; shows how far `plist-get' has to walk, which is what decides the outcome.

(require 'ox-w3ctr)
(require 'benchmark)
(require 'seq)

(defconst micro-n 1000000)
(defconst micro-doc
  (expand-file-name "2025-01-19-monads/index.org"
                    (or (getenv "DRAFTS")
                        "C:/Users/26633/OneDrive/egh0bww1/drafts/")))
(defvar micro-info nil)

(defun micro-capture (_contents info)
  "Remember the first INFO plist an export passes to the template."
  (unless micro-info (setq micro-info info)))

(defun micro-time (thunk)
  "Return the best of three timings of THUNK (MICRO-N iterations each)."
  (let (times)
    (dotimes (_ 3) (push (car (benchmark-run 1 (funcall thunk))) times))
    (apply #'min times)))

(defun micro-measure (info key)
  "Print the `plist-get' and (if built in) cache timings for KEY in INFO."
  (org-w3ctr--pget info key)                    ; warm the cache
  (let* ((pair (/ (or (seq-position info key #'eq) 0) 2))
         (plist (micro-time (lambda () (dotimes (_ micro-n) (plist-get info key)))))
         (cached (when (bound-and-true-p org-w3ctr--oinfo-cache-p)
                   (let ((sym (org-w3ctr--oinfo-oclosure key)))
                     (funcall sym info)    ; warm the closure too
                     (micro-time (lambda ()
                                   (dotimes (_ micro-n) (funcall sym info))))))))
    (princ (format "MICRO-REAL %-32s pair#%-4d plist=%.4f%s\n"
                   key pair plist
                   (if cached
                       (format " oclosure=%.4f ratio=%.2f" cached (/ cached plist))
                     " oclosure=n/a (cache off)")))))

(advice-add 'org-w3ctr-template :before #'micro-capture)
(let ((buf (find-file-noselect micro-doc)))
  (unwind-protect
      (with-current-buffer buf (org-export-as 'w3ctr))
    (kill-buffer buf)))
(advice-remove 'org-w3ctr-template #'micro-capture)

(princ (format "MICRO-REAL flavor=%s cache-p=%S plist-pairs=%d\n"
               (or (getenv "OINFO") "?")
               (and (bound-and-true-p org-w3ctr--oinfo-cache-p) t)
               (/ (length micro-info) 2)))
(dolist (key '(:title :with-latex :preserve-breaks :html-equation-reference-format
               :html-timezone :html-inline-image-rules))
  (micro-measure micro-info key))
