;;; verify-html.el --- Structural checks on exported HTML  -*- lexical-binding: t; -*-
;;
;; Usage: from a build directory of ox-w3ctr, with this skill's scripts
;; directory on the load path (the full recipe is in references/harness.md):
;;
;;   emacs --batch -L . -L .agents/skills/ox-w3ctr-verify/scripts -l verify-html
;;   RAW=1 emacs --batch ..., -l verify-html     ; keep Org's random ids
;;
;; DRAFTS (the corpus root) can be overridden as in verify-oinfo.el; DOC=/path
;; checks a single document instead of the corpus.  The bulk pass — one export
;; per document, giving both the comparable hash and these checks — is
;; `verify-corpus.el'; use this script for one document, or to debug a single
;; check (`RAW=1` keeps Org's random ids in the duplicate-id test).
;;
;; Every DRAFTS/*/index.org is exported and checked:
;;   * `libxml-parse-html-region' parses the output (HTML structural sanity);
;;   * no id appears twice;
;;   * every internal "#..." link resolves to an id in the same document.
;; HTML comments, <style> and <script> blocks are stripped first — the W3C
;; header blurb lists ids and hrefs as examples and would otherwise be
;; counted as markup.  Org's random reference ids are normalized to one
;; value unless RAW is set, so that duplicate detection stays meaningful.
;; Prints one line per problem, then one RESULT line.

(require 'ox-w3ctr)
(require 'seq)

(setq enable-local-variables nil system-time-locale "C")
(defconst drafts (or (getenv "DRAFTS")
                     "C:/Users/26633/OneDrive/egh0bww1/drafts/"))

(defun norm (s)
  "Hide the export timestamp, and Org's random reference ids unless RAW."
  (let ((s (replace-regexp-in-string
            "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}Z"
            "TIMESTAMP" s)))
    (if (getenv "RAW") s
      (replace-regexp-in-string "org[0-9a-f]\\{7\\}" "orgRANDOM" s))))

(defun strip-comments (s)
  "Return S without HTML comments and without style/script blocks.

The W3C header blurb lists required ids and hrefs inside a CSS comment,
so a plain tag scan would find them."
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

(defun export-file (file)
  (let ((buf (find-file-noselect file)))
    (unwind-protect (with-current-buffer buf (org-export-as 'w3ctr))
      (kill-buffer buf))))

(defun scan (html)
  "Return (PARSE-IDS-HREFS-DUPS) for HTML, ignoring comments."
  (let ((ids (make-hash-table :test 'equal)) (dups nil) (hrefs nil) (parse 'ok))
    (with-temp-buffer
      (insert (strip-comments html))
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

(let* ((one (getenv "DOC"))
       (docs (if one
                 (list (expand-file-name one))
               (seq-filter #'file-readable-p
                           (mapcar (lambda (d) (expand-file-name "index.org" d))
                                   (directory-files drafts t "\\`[0-9]")))))
       (n 0) (bad-parse 0) (dup-docs 0) (unresolved-docs 0) (unresolved 0))
  (dolist (doc docs)
    (condition-case err
        (let* ((name (file-name-nondirectory (directory-file-name (file-name-directory doc))))
               (r (scan (norm (export-file doc))))
               (parse (nth 0 r)) (ids (nth 1 r)) (hrefs (nth 2 r)) (dups (nth 3 r))
               (unres (seq-remove (lambda (h) (member h ids)) hrefs)))
          (setq n (1+ n))
          (unless (eq parse 'ok) (setq bad-parse (1+ bad-parse))
                  (princ (format "PARSE %s %s\n" name parse)))
          (when dups (setq dup-docs (1+ dup-docs))
                (princ (format "DUPID %s %S\n" name (seq-take dups 3))))
          (when unres (setq unresolved-docs (1+ unresolved-docs) unresolved (+ unresolved (length unres)))
                (princ (format "UNRESOLVED %s n=%d %S\n" name (length unres) (seq-take unres 4)))))
      (error (princ (format "ERROR %s %S\n" doc err)))))
  (princ (format "RESULT verify-html: flavor=%s docs=%d parse-errors=%d dup-id-docs=%d unresolved-docs=%d unresolved-links=%d\n"
                 (or (getenv "OINFO") "?") n bad-parse dup-docs unresolved-docs unresolved)))
