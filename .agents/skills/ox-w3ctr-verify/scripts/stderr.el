;; -*- lexical-binding:t; -*-
(require 'jsonrpc)
(defvar jp-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar conn
  (make-instance 'jsonrpc-process-connection
    :name "test-stderr"
    :process (lambda (_c)
               (make-process
                :name "test-stderr"
                :command (list (or (executable-find "python") "python")
                               (expand-file-name "stderr_server.py" jp-here))
                ;; the name coupling: jsonrpc.el made this buffer first
                :stderr (get-buffer "*test-stderr stderr*")
                :noquery t :coding 'binary))))
(princ (format "ping: %S\n" (jsonrpc-request conn "ping" :jsonrpc-omit)))
(princ (format "stderr-contents: %S\n"
               (with-current-buffer (jsonrpc-stderr-buffer conn) (buffer-string))))
(jsonrpc-shutdown conn t)
