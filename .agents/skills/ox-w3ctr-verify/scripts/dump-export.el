;;; dump-export.el --- Export one Org file to an HTML file  -*- lexical-binding: t; -*-
;;
;; Usage: from a build directory of ox-w3ctr, with this skill's scripts
;; directory on the load path:
;;
;;   SKILL=.agents/skills/ox-w3ctr-verify/scripts
;;   ORG=/path/to/index.org OUT=/tmp/out.html emacs --batch -L . -L $SKILL \
;;      -l dump-export
;;
;; Reads ORG (never writes it) and writes the exported document to OUT, with
;; the export timestamp and Org's random reference ids normalized — handy for
;; diffing two builds' output of the same document.  Without the second rule
;; three corpus documents differ between builds and between runs (see
;; harness.md).  The ids are "org" plus four or more hex digits; in prose a
;; word like "org-mode" cannot match, and any prose collision only affects a
;; diff, not the export.
(setq enable-local-variables nil system-time-locale "C")
(require 'ox-w3ctr)
(defun norm (s)
  ;; Backslashes are doubled on purpose: inside a Lisp string an invalid
  ;; escape such as \{ drops the backslash, and the interval then silently
  ;; matches a literal brace instead of four hex digits.
  (replace-regexp-in-string
   "org[0-9a-f]\\{4,\\}" "ORGREF"
   (replace-regexp-in-string
    "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}Z" "TIMESTAMP" s t t)))
(let* ((file (getenv "ORG"))
       (buf (find-file-noselect file)))
  (unwind-protect
      (with-temp-file (getenv "OUT")
        (insert (norm (with-current-buffer buf (org-export-as 'w3ctr)))))
    (kill-buffer buf)))
(princ (format "dumped %s\n" (getenv "OUT")))
