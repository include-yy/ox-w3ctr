;;; verify-oinfo.el --- Check the OINFO cache against a document corpus  -*- lexical-binding: t; -*-
;;
;; Usage: from a build directory of ox-w3ctr, with this skill's scripts
;; directory on the load path (the full recipe is in references/harness.md):
;;
;;   OINFO=on  emacs --batch -L . -L .agents/skills/ox-w3ctr-verify/scripts \
;;                  -l verify-oinfo
;;   OINFO=off emacs --batch -L . -L .agents/skills/ox-w3ctr-verify/scripts \
;;                  -l verify-oinfo
;;
;; Overridable: DRAFTS (the corpus root, default the author's drafts folder),
;; TARGET (one document under it, default 2026-01-01-gv-history/index.org).
;;
;; Export the target document with this build and report:
;;   FLAVOR/DOC/TIME   the build's flavour, output length and hashes, and
;;                     20 single-run timings
;;   PIDS              oclosures still holding INFO after a full export (0)
;;   STATS             lookups per key, keys used, keys never touched
;;   ABORT             an aborted export, and the opt-in
;;                     `org-w3ctr-oinfo-cleanup-before-export' hook
;;
;; This is the cache instrumentation only.  The 57-document corpus pass
;; (`verify-corpus.el') reports the hashes to compare between builds; running
;; both scripts is the recipe in references/harness.md.  Documents are only
;; read; nothing is written.

(setq enable-local-variables nil
      system-time-locale "C")

(require 'ox-w3ctr)
(require 'seq)
(require 'benchmark)

(defconst oinfo-flavor (or (getenv "OINFO") "?"))
(defconst drafts (or (getenv "DRAFTS")
                     "C:/Users/26633/OneDrive/egh0bww1/drafts/"))
(defconst target (expand-file-name
                  (or (getenv "TARGET") "2026-01-01-gv-history/index.org")
                  drafts))

(defun norm (s)
  "Hide the export timestamp and Org's random reference ids in S."
  (replace-regexp-in-string
   "org[0-9a-f]\\{4,\\}" "ORGREF"
   (replace-regexp-in-string
    "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}Z"
    "TIMESTAMP" s t t)))

(defun export-file (file)
  "Export FILE to a string.  No file is written."
  (let ((buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf (org-export-as 'w3ctr))
      (kill-buffer buf))))

(defun oinfo-closures ()
  (when (bound-and-true-p org-w3ctr--oinfo-cache-alist)
    (mapcar (lambda (x) (symbol-function (cdr x))) org-w3ctr--oinfo-cache-alist)))

(defun oinfo-stats ()
  (when (bound-and-true-p org-w3ctr--oinfo-cache-alist)
    (mapcar (lambda (x) (cons (car x) (org-w3ctr--oinfo--cnt (symbol-function (cdr x)))))
            org-w3ctr--oinfo-cache-alist)))

(defun oinfo-pids ()
  "How many oclosures still hold an INFO plist."
  (length (seq-filter (lambda (o) (org-w3ctr--oinfo--pid o)) (oinfo-closures))))

(defun print-stats (label)
  (let ((st (oinfo-stats)))
    (if (null st)
        (princ (format "STATS %s cache-off\n" label))
      (let ((used (seq-filter (lambda (x) (> (cdr x) 0)) st))
            (unused (seq-filter (lambda (x) (= (cdr x) 0)) st)))
        (princ (format "STATS %s lookups=%d keys-used=%d/%d unused=%S\n"
                       label (apply #'+ (mapcar #'cdr st)) (length used) (length st)
                       (mapcar #'car unused)))
        (princ (format "STATS %s top=%S\n" label
                       (seq-take (sort st (lambda (a b) (> (cdr a) (cdr b)))) 8)))))))

(princ (format "FLAVOR %s cache-p=%S\n" oinfo-flavor
               (and (bound-and-true-p org-w3ctr--oinfo-cache-p) t)))

;;; 1. the given document: output, timing, retention
(let* ((html (export-file target))
       (times nil))
  (princ (format "DOC %s len=%d hash=%s norm=%s\n"
                 (file-name-directory target) (length html)
                 (md5 html) (md5 (norm html))))
  (dotimes (_ 20)
    (push (car (benchmark-run 1 (export-file target))) times))
  (setq times (sort times #'<))
  (princ (format "TIME target n=20 min=%.4f med=%.4f max=%.4f sum=%.4f\n"
                 (car times) (nth 10 times) (car (last times)) (apply #'+ times)))
  (princ (format "PIDS after-one-full-export=%d\n" (oinfo-pids)))
  (print-stats "target"))

;;; 2. aborted export and the opt-in hook (cache builds only)
(when (bound-and-true-p org-w3ctr--oinfo-cache-p)
  (let* ((stale nil) (phase 1) (observed 'unset)
         (advice (lambda (_e _c info)
                   (if (= phase 1)
                       (progn (org-w3ctr--pget info :title)
                              (setq stale info phase 2)
                              (error "oinfo-check: aborted export"))
                     (setq observed (and (eq (org-w3ctr--oinfo--pid
                                              (symbol-function (org-w3ctr--oinfo-oclosure :title)))
                                             stale)
                                         t))))))
    (advice-add 'org-w3ctr-paragraph :before advice)
    (unwind-protect
        (progn
          (condition-case _ (export-file target) (error nil))
          (princ (format "ABORT dead-INFO-held=%d\n" (oinfo-pids)))
          (export-file target)          ; no hook installed
          (princ (format "ABORT next-export-still-cached=%S\n" observed))
          (org-w3ctr--oinfo-cleanup)
          (setq phase 1 observed 'unset)
          (add-hook 'org-export-before-processing-functions
                    #'org-w3ctr-oinfo-cleanup-before-export)
          (condition-case _ (export-file target) (error nil))
          (setq phase 2 observed 'unset)
          (export-file target)          ; with the hook installed
          (princ (format "ABORT with-hook-still-cached=%S\n" observed))
          (remove-hook 'org-export-before-processing-functions
                       #'org-w3ctr-oinfo-cleanup-before-export))
      (advice-remove 'org-w3ctr-paragraph advice)
      (org-w3ctr--oinfo-cleanup))))
