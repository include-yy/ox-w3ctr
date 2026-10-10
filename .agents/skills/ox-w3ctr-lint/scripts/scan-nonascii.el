;;; scan-nonascii.el --- flag non-ASCII outside the whitelist  -*- lexical-binding:t -*-
;; Docstrings and comments must be ASCII (see the elisp-docstring
;; skill).  The one allowed non-ASCII character is the ↑ glyph (U+2191)
;; in the back-to-top link, which is rendered HTML content, not prose.
(with-temp-buffer
  (let ((coding-system-for-read 'utf-8-unix))
    (insert-file-contents "ox-w3ctr.el"))
  (goto-char (point-min))
  (let ((bad nil))
    (while (re-search-forward "[^\000-\177]" nil t)
      (let ((ch (string-to-char (match-string 0)))
            (line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        ;; The back-to-top ↑ (U+2191) is rendered HTML content, not prose.
        (unless (and (eq ch #x2191)
                     (string-search "back-to-top" line))
          (push (cons (line-number-at-pos) ch) bad))))
    (if (not bad)
        (princ "NON-ASCII-OK\n")
      (dolist (x (nreverse bad))
        (princ (format "NON-ASCII line %d: U+%04X\n" (car x) (cdr x))))
      (kill-emacs 1))))
;;; scan-nonascii.el ends here
