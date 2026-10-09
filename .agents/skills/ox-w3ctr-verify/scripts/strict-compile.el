;; -*- lexical-binding:t; -*-
(setq load-prefer-newer t)
(setq byte-compile-error-on-warn t)
(setq byte-compile-warnings t)
(byte-compile-file "ox-w3ctr.el")
(princ "COMPILE-OK\n")
