;;; verify-corpus.el --- Hash and validate every corpus export in one pass  -*- lexical-binding: t; -*-
;;
;; Usage: from a build directory of ox-w3ctr, with this skill's scripts
;; directory on the load path (the full recipe is in references/harness.md):
;;
;;   emacs --batch -L . -L .agents/skills/ox-w3ctr-verify/scripts -l verify-corpus
;;
;; DRAFTS (the corpus root, default the author's drafts folder) can be
;; overridden as in verify-oinfo.el.
;;
;; Exports each DRAFTS/*/index.org **once** and reports, from that one pass:
;;
;;   BATCH <doc> hash=<md5 of the raw export> norm=<md5 of the normalized
;;         export> refs=<how many random reference ids it holds> len=<n>
;;   PARSE / DUPID / UNRESOLVED / ERROR <doc> …   one line per problem
;;   RESULT verify-corpus: flavor=… docs=… errors=… parse-errors=…
;;         dup-id-docs=… unresolved-docs=… unresolved-links=… len=… secs=…
;;
;; Compare the `norm=' column across the two flavour builds: it must be
;; identical, because the cache is transparent.  The recipe in
;; references/harness.md builds the second flavour only when the change is to
;; OINFO itself; the shipped build is the one checked routinely.
;;
;; `hash=' is the raw export and is **not** a baseline for a document with
;; anonymous elements: such a document gets fresh `orgXXXXXXX' ids on every
;; export, in one build too.  `norm=' normalizes exactly those ids and the
;; export timestamp; `refs=' says which documents have any (their raw hash is
;; the one that moves).
;;
;; Duplicate ids are counted on the **raw** ids: the placeholder that
;; normalization introduces makes two distinct ids collide, which is a false
;; positive this corpus used to hit (references/checker-design.md).  HTML
;; comments, <style> and <script> are stripped before scanning — the W3C
;; header blurb quotes ids and hrefs as examples.  Every document is only
;; read; nothing is written.

(setq enable-local-variables nil system-time-locale "C")
(require 'ox-w3ctr)
(require 'seq)

(defconst vc-drafts (or (getenv "DRAFTS")
                        "C:/Users/26633/OneDrive/egh0bww1/drafts/"))
(defconst vc-flavor (or (getenv "OINFO") "?"))

(defconst vc-ts-regexp
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}Z"
  "The export-timestamp comment's ISO time.")
(defconst vc-ref-regexp "org[0-9a-f]\\{4,\\}"
  "Org's random reference ids: `org' plus hex digits (7 or 8 in practice).")

(defun vc-refs (s)
  "How many random reference ids S holds."
  (let ((n 0) (pos 0))
    (while (string-match vc-ref-regexp s pos)
      (setq n (1+ n) pos (match-end 0)))
    n))

(defun vc-docs ()
  "Every DRAFTS/*/index.org, in directory order."
  (seq-filter #'file-readable-p
              (mapcar (lambda (d) (expand-file-name "index.org" d))
                      (directory-files vc-drafts t "\\`[0-9]"))))

(defun vc-norm (s)
  "Hide the export timestamp and Org's random reference ids in S.

Backslashes are doubled on purpose: inside a Lisp string an invalid escape
such as \\{ drops the backslash and the interval then silently matches a
literal brace instead of four or more hex digits."
  (replace-regexp-in-string
   vc-ref-regexp "ORGREF"
   (replace-regexp-in-string vc-ts-regexp "TIMESTAMP" s t t)))

(defun vc-strip-comments (s)
  "Return S without HTML comments and without style/script blocks.

The W3C header blurb lists required ids and hrefs inside a CSS comment, so a
plain tag scan would find them."
  (with-temp-buffer
    (insert s)
    (dolist (pair '(("<style" . "</style>")
                    ("<script" . "</script>")
                    ("<!--" . "-->")))
      (goto-char (point-min))
      (while (search-forward (car pair) nil t)
        (let ((start (match-beginning 0)))
          (if (search-forward (cdr pair) nil t)
              (delete-region start (point))
            (delete-region start (point-max))))))
    (buffer-string)))

(defun vc-export (file)
  "Export FILE to a string.  No file is written."
  (let ((buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf (org-export-as 'w3ctr))
      (kill-buffer buf))))

(defun vc-scan (html)
  "Return (PARSE IDS HREFS DUPS) for HTML, ignoring comments.

IDS and HREFS are the raw `id=\"…\"' and `href=\"#…\"' values; DUPS lists the
ids that occur more than once.  PARSE is `ok' or the libxml error."
  (let ((ids (make-hash-table :test 'equal)) (dups nil) (hrefs nil) (parse 'ok))
    (with-temp-buffer
      (insert (vc-strip-comments html))
      (when (fboundp 'libxml-parse-html-region)
        (setq parse (condition-case err
                        (progn (libxml-parse-html-region (point-min) (point-max)) 'ok)
                      (error (format "%S" err)))))
      (goto-char (point-min))
      (while (re-search-forward " id=\"\\([^\"]+\\)\"" nil t)
        (let ((id (match-string 1)))
          (if (gethash id ids) (push id dups) (puthash id t ids))))
      (goto-char (point-min))
      (while (re-search-forward " href=\"#\\([^\"]+\\)\"" nil t)
        (push (match-string 1) hrefs)))
    (list parse
          (sort (hash-table-keys ids) #'string<)
          (sort (delete-dups (copy-sequence hrefs)) #'string<)
          (sort (delete-dups dups) #'string<))))

(let* ((docs (vc-docs))
       (t0 (float-time))
       (n 0) (errs 0) (len 0) (bad-parse 0) (dup-docs 0)
       (unresolved-docs 0) (unresolved 0))
  (dolist (doc docs)
    (condition-case err
        (let* ((name (file-name-nondirectory
                      (directory-file-name (file-name-directory doc))))
               (html (vc-export doc))
               (r (vc-scan html))
               (parse (nth 0 r)) (ids (nth 1 r)) (hrefs (nth 2 r)) (dups (nth 3 r))
               (unres (seq-remove (lambda (h) (member h ids)) hrefs)))
          (setq n (1+ n) len (+ len (length html)))
          (princ (format "BATCH %s hash=%s norm=%s refs=%d len=%d\n"
                         name (md5 html) (md5 (vc-norm html)) (vc-refs html)
                         (length html)))
          (unless (eq parse 'ok)
            (setq bad-parse (1+ bad-parse))
            (princ (format "PARSE %s %s\n" name parse)))
          (when dups
            (setq dup-docs (1+ dup-docs))
            (princ (format "DUPID %s %S\n" name (seq-take dups 3))))
          (when unres
            (setq unresolved-docs (1+ unresolved-docs)
                  unresolved (+ unresolved (length unres)))
            (princ (format "UNRESOLVED %s n=%d %S\n"
                           name (length unres) (seq-take unres 4)))))
      (error (setq errs (1+ errs))
             (princ (format "ERROR %s %S\n" doc err)))))
  (princ (format (concat "RESULT verify-corpus: flavor=%s docs=%d errors=%d"
                         " parse-errors=%d dup-id-docs=%d unresolved-docs=%d"
                         " unresolved-links=%d len=%d secs=%.3f\n")
                 vc-flavor n errs bad-parse dup-docs unresolved-docs unresolved
                 len (- (float-time) t0))))
