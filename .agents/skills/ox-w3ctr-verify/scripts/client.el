;; -*- lexical-binding:t; -*-
(require 'jsonrpc)
(defvar jp-here (file-name-directory (or load-file-name buffer-file-name)))

(defun jp-make (name)
  (make-instance 'jsonrpc-process-connection
    :name name
    :process (lambda (_conn)
               (make-process :name name
                             :command (list (or (executable-find "python") "python")
                                            (expand-file-name "server.py" jp-here))
                             :noquery t :coding 'binary))
    :on-shutdown (lambda (_conn) (message "on-shutdown fired"))))

(defvar jp nil)
(setq jp (jp-make "jp1"))

(message "ping      -> %S" (jsonrpc-request jp "ping" :jsonrpc-omit))
(message "add       -> %S" (jsonrpc-request jp "add" [2 3]))
(message "tex2mml   -> %S" (jsonrpc-request jp "tex2mml" '(:fragment "x^2")))
(message "running-p -> %S" (jsonrpc-running-p jp))
(condition-case e
    (jsonrpc-request jp "nope" :jsonrpc-omit)
  (jsonrpc-error (message "error     -> %S" (cdr e))))

;; Restart: shut the first connection down, then build a fresh one.
(jsonrpc-shutdown jp t)
(message "after shutdown running-p -> %S" (jsonrpc-running-p jp))
(setq jp (jp-make "jp2"))
(message "after restart ping -> %S" (jsonrpc-request jp "ping" :jsonrpc-omit))
(jsonrpc-shutdown jp t)
