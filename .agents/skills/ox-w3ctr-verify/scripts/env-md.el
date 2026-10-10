;;; env-md.el --- read a machine fact from .agents/env.md  -*- lexical-binding:t -*-

;; The corpus scripts run under scratch/build/, but the machine facts
;; live in the repo's .agents/env.md.  env-md-value finds that file
;; from the current directory upward and returns the fact's value.

;;; Code:

(defun env-md-value (fact)
  "Return FACT's value from `.agents/env.md', or nil.
FACT is the first-column label (e.g. \"Drafts\").  The file is found
from `default-directory' upward, so a script run under scratch/build/
still reads the repo's env.md."
  (let ((root (locate-dominating-file default-directory ".agents/env.md")))
    (when root
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".agents/env.md" root))
        (goto-char (point-min))
        (and (re-search-forward
              (format "^| %s | `\\([^`]*\\)`" (regexp-quote fact)) nil t)
             (match-string 1))))))

(provide 'env-md)
;;; env-md.el ends here
