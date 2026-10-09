;; -*- lexical-binding:t; -*-
(setq load-prefer-newer t)
(load "ox-w3ctr")
(defvar jp-here (file-name-directory (or load-file-name buffer-file-name)))
(let ((conn (org-w3ctr--jrpc-connect
             "test-jrpc"
             (list (or (executable-find "python") "python")
                   (expand-file-name "stderr_server.py" jp-here)))))
  (princ (format "ping:   %S\n" (jsonrpc-request conn "ping" :jsonrpc-omit)))
  (princ (format "stderr: %S\n"
                 (with-current-buffer (jsonrpc-stderr-buffer conn) (buffer-string))))
  (jsonrpc-shutdown conn t))
