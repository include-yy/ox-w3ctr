;;; test-scan.el --- static triage of an ERT test file  -*- lexical-binding:t -*-
;; Static, per-`ert-deftest' candidates for review.  Heuristic, not a
;; gate: each tag has false positives, so triage the output.  See the
;; skill's SKILL.md for the tags and for the seeded-defect self-check
;; every checker must pass.
;;
;; Usage:
;;   emacs --batch -l test-scan.el -- TESTS.el [ASSERT-HELPER...]
;;
;; ASSERT-HELPER names an assertion helper the suite calls instead of
;; `should' (for ox-w3ctr, `t-check-element-values'); pass those so a
;; test that only calls the helper is not flagged NO-ASSERTION.
;;
;; Output: one line per finding, `<TAG>  FILE:LINE  TEST  [DETAIL]`, and
;; a `;; N tests, M findings` summary.
;;; Code:

(require 'cl-lib)

(defconst ts--env-symbols
  '(current-time float-time time-convert format-time-string format-seconds
    sleep-for sit-for random
    make-temp-file make-temp-name
    url-retrieve url-retrieve-synchronously url-insert-file-contents
    start-process make-process make-network-process make-pipe-process
    shell-command shell-command-to-string call-process
    start-process-shell-command jsonrpc-request)
  "Calls whose result can depend on the clock, randomness, the
network, the filesystem or a subprocess.")

(defconst ts--assert-symbols
  '(should should-not should-error ert-fail)
  "Assertion macros recognised by name.  A call whose head is a symbol
starting with `$' (a common test-helper convention) also counts.")

(defconst ts--binding-heads
  '(let let* dlet lambda dolist dotimes
    when-let when-let* if-let if-let* and-let* while-let
    cl-letf cl-letf* cl-flet cl-flet* cl-labels cl-labels*)
  "Forms whose binders are collected, so a `setq' on them is local.")

(defconst ts--place-heads
  '(setq setq-default setq-local setf
    push pop incf decf cl-incf cl-decf add-to-list cl-pushnew cl-callf)
  "Forms that assign through a place.")

(defun ts--sym< (a b)
  "Order symbols A and B by name."
  (string< (symbol-name a) (symbol-name b)))

(defun ts--sym-list (syms)
  "SYMS as a sorted, deduplicated, space-separated string."
  (mapconcat #'symbol-name
             (sort (delete-dups (copy-sequence syms)) #'ts--sym<)
             " "))

(defun ts--walk (form fn)
  "Call FN on FORM and every sub-form (elements and vectors)."
  (funcall fn form)
  (cond ((consp form)
         (ts--walk (car form) fn)
         (ts--walk (cdr form) fn))
        ((and (vectorp form) (not (recordp form)))
         (mapc (lambda (x) (ts--walk x fn)) form))))

(defun ts--binding-names (form)
  "Symbols that binding FORM binds, or nil if FORM is not a binder."
  (pcase (car-safe form)
    ((or 'let 'let* 'dlet)
     (delq nil (mapcar (lambda (b) (if (consp b) (car b) b))
                       (let ((bs (cadr form))) (if (listp bs) bs nil)))))
    ('lambda (let ((args (cadr form))) (if (listp args) args nil)))
    ((or 'dolist 'dotimes) (list (car (cadr form))))
    ((or 'when-let 'when-let* 'if-let 'if-let* 'and-let* 'while-let)
     (delq nil (mapcar (lambda (b) (if (consp b) (car b) b))
                       (let ((bs (cadr form))) (if (listp bs) bs nil)))))
    ((or 'cl-letf 'cl-letf*)
     (delq nil (mapcar (lambda (b)
                         (when (consp b)
                           (let ((place (car b)))
                             (if (consp place) (car place) place))))
                       (let ((bs (cadr form))) (if (listp bs) bs nil)))))
    ((or 'cl-flet 'cl-flet* 'cl-labels 'cl-labels*)
     (delq nil (mapcar (lambda (b) (when (consp b) (car b)))
                       (let ((bs (cadr form))) (if (listp bs) bs nil)))))
    (_ nil)))

(defun ts--stubbed-functions (form)
  "The function symbols whose cell a `cl-letf' FORM rebinds.
A test that stubs `jsonrpc-request' with `cl-letf' does not reach the
network, so it must not be reported ENV-SENSITIVE for that symbol."
  (delq nil
        (mapcar
         (lambda (b)
           (when (consp b)
             (let ((place (car b)))
               (when (and (consp place) (eq (car place) 'symbol-function))
                 (let ((arg (cadr place)))
                   (when (and (consp arg)
                              (memq (car arg) '(quote function))
                              (symbolp (cadr arg)))
                     (cadr arg)))))))
         (let ((bs (cadr form))) (if (listp bs) bs nil)))))

(defun ts--place-names (form)
  "Variables FORM assigns through, for a place-mutating head."
  (pcase (car-safe form)
    ((or 'setq 'setq-default 'setq-local 'setf)
     (let (out (rest (cdr form)))
       (while (consp rest)
         (let ((p (pop rest)))
           (when (symbolp p) (push p out)))
         (when (consp rest) (pop rest)))     ; skip the value
       (nreverse out)))
    ((or 'push 'pop 'incf 'decf 'cl-incf 'cl-decf
         'add-to-list 'cl-pushnew 'cl-callf)
     (let ((p (cadr form))) (when (symbolp p) (list p))))
    (_ nil)))

(defun ts--scan-test (name line body assert-helpers)
  "Return findings (TAG LINE NAME DETAIL) for one test.
NAME is the test, at LINE; BODY is its forms; ASSERT-HELPERS are extra
assertion helper symbols."
  (let ((asserts 0)
        binders global locals advice-add advice-remove env-control stubbed
        fset fmakunbound getbuf killbuf proc delproc env skip out)
    (ts--walk
     (cons 'progn body)
     (lambda (f)
       (when (consp f)
         (let ((head (car f)))
           (when (or (memq head ts--assert-symbols)
                     (memq head assert-helpers)
                     (and (symbolp head)
                          (string-prefix-p "$" (symbol-name head))))
             (setq asserts (1+ asserts)))
           (when (memq head ts--binding-heads)
             (setq binders (append (ts--binding-names f) binders)))
           (when (memq head '(cl-letf cl-letf*))
             (setq stubbed (append (ts--stubbed-functions f) stubbed)))
           (when (memq head ts--place-heads)
             (let ((places (ts--place-names f)))
               (if (eq head 'setq-local)
                   (setq locals (append places locals))
                 (dolist (p places)
                   (unless (memq p binders) (push p global))))))
           (pcase head
             ('advice-add (setq advice-add t))
             ('advice-remove (setq advice-remove t))
             ('fset (setq fset t))
             ('fmakunbound (setq fmakunbound t))
             ('get-buffer-create (setq getbuf t))
             ('kill-buffer (setq killbuf t))
             ((or 'make-process 'start-process 'make-network-process)
              (setq proc t))
             ((or 'delete-process 'kill-process) (setq delproc t))
             ((or 'skip-unless 'skip-when 'ert-skip) (setq skip t))
             (_ nil))
           (when (memq head ts--env-symbols)
             (push head env)
             ;; (random "SEED") seeds deterministically; a bare (random N)
             ;; draws from it.  A test that seeds is not order-dependent.
             (when (and (eq head 'random) (stringp (cadr f)))
               (setq env-control t)))))))
    (when (zerop asserts)
      (push (list "NO-ASSERTION" line name "") out))
    (when global
      (push (list "GLOBAL-WRITE" line name (ts--sym-list global)) out))
    (when locals
      (push (list "SETQ-LOCAL" line name (ts--sym-list locals)) out))
    (when (and advice-add (not advice-remove))
      (push (list "ADVICE-LEAK" line name "") out))
    (when (and fset (not fmakunbound))
      (push (list "FSET-LEAK" line name "") out))
    (when (and getbuf (not killbuf))
      (push (list "BUFFER-LEAK" line name "") out))
    (when (and proc (not delproc))
      (push (list "PROCESS-LEAK" line name "") out))
    (when skip (push (list "SKIP" line name "") out))
    (when env-control (setq env (delq 'random env)))
    (setq env (cl-set-difference env stubbed))
    (when env
      (push (list "ENV-SENSITIVE" line name (ts--sym-list env)) out))
    (nreverse out)))

(defun ts--read-forms (file)
  "Return (LINE . FORM) for each top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (skip-chars-forward " \t\n\r")
            (let ((beg (point)))
              (push (cons (line-number-at-pos beg) (read (current-buffer)))
                    forms)))
        (end-of-file nil))
      (nreverse forms))))

(let* ((args command-line-args-left)
       (args (if (member "--" args) (cdr (member "--" args)) args))
       (file (pop args))
       (assert-helpers (mapcar #'intern args))
       (tests 0)
       (findings nil))
  (unless file
    (error "usage: emacs --batch -l test-scan.el -- TESTS.el [ASSERT-HELPER...]"))
  (dolist (pair (ts--read-forms file))
    (let ((line (car pair))
          (form (cdr pair)))
      (when (and (consp form) (eq (car form) 'ert-deftest))
        (setq tests (1+ tests))
        (setq findings (nconc findings
                              (ts--scan-test (cadr form) line (cddr form)
                                             assert-helpers))))))
  (princ (format ";; test-scan: %s\n" file))
  (dolist (f findings)
    (princ (format "%-14s %s:%d  %s  %s\n"
                   (nth 0 f) file (nth 1 f) (nth 2 f) (nth 3 f))))
  (princ (format ";; %d tests, %d findings\n" tests (length findings))))
;;; test-scan.el ends here
