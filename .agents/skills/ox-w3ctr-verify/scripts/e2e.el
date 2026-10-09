;; -*- lexical-binding:t; -*-
;; End-to-end check of the jsonrpc.el <-> node pipeline.  Load the package,
;; then drive the two MathJax modes directly and through a real export.
(setq load-prefer-newer t)
(load "ox-w3ctr")

(princ (format "norm = %S\n" (org-w3ctr--normalize-latex "\\(x^2\\)")))
(princ (format "mml  = %s\n"
               (org-w3ctr--format-latex "\\(x^2\\)" 'mathml-by-mathjax nil)))
(princ (format "svg  = %s\n"
               (org-w3ctr--format-latex "\\(x^2\\)" 'svg-by-mathjax nil)))

(let ((org-w3ctr-with-latex 'mathml-by-mathjax))
  (let ((out (org-export-string-as "* H\n\\(x^2\\)" 'w3ctr nil)))
    (princ (format "e2e has <math: %s\n" (and (string-match-p "<math" out) t)))))

(princ (format "conn live: %s\n"
               (and (jsonrpc-running-p
                     (org-w3ctr--jrpc--conn org-w3ctr--jstools))
                    t)))
