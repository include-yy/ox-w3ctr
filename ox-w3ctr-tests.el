;;; -*- lexical-binding:t; no-byte-compile:t; -*-

(require 'ert)
(load "ox-w3ctr")

;; Test helper functions
(defun $c (&rest args) "concat" (apply #'concat args))
(defun $s (a) "should" (should a))
(defun $n (a) "should-not" (should-not a))
(defun $q (a b) "should-eq" (should (eq a b)))
(defun $l (a b) "should-equal" (should (equal a b)))
(defun $nq (a b) "should-not-eq" (should-not (eq a b)))
(defun $nl (a b) "should-not-equal" (should-not (equal a b)))
(defmacro $e! (exp) "should-error" `(should-error ,exp))
(defmacro $e!l (exp val)
  "should-error-equal"
  `(should (equal (should-error ,exp) ,val)))
(defmacro $it (f &rest body)
  "Bind function to symbol `it'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'it) ',f))
     ,@body))

(defun t--oinfo-oget (prop)
  "Get the oclosure object corresponeds to PROP."
  (when-let* ((f (alist-get prop t--oinfo-cache-alist)))
    (symbol-function f)))

(defvar t-test-values nil
  "A list to store return values during testing.")
(defun t-advice-return-value (result)
  "Advice function to save and return RESULT.
Pushes RESULT onto `org-w3ctr-test-values' and returns RESULT."
  (prog1 result
    (push (if (not (stringp result)) result
            (substring-no-properties result))
          t-test-values)))
(defun t-check-element-values (fn pairs &optional body-only plist)
  "Check that FN returns the expected values when exporting.

FN is a function to advice.  PAIRS is a list of the form
((INPUT . EXPECTED) ...).  INPUT is a string of Org markup to be
exported.  EXPECTED is a list of expected return values from FN.
BODY-ONLY and PLIST are optional arguments passed to
`org-export-string-as'."
  (advice-add fn :filter-return #'t-advice-return-value)
  (unwind-protect
      (dolist (test pairs t)
        (let (t-test-values)
          (ignore (org-export-string-as
                   (car test) 'w3ctr body-only plist))
          (should (equal t-test-values (cdr test)))))
    (advice-remove fn #'t-advice-return-value)))

(defun t-get-parsed-elements (str type)
  "Parse STR as an Org buffer and return a list of elements of TYPE.

STR is a string containing Org content.  TYPE is an Org element type
symbol (such as \\='headline, \\='paragraph, etc)."
  (thread-first
    (with-temp-buffer
      (save-excursion (insert str))
      (org-element-parse-buffer))
    (org-element-map type #'identity)))

(ert-deftest t--make-cache-oclosure ()
  "Tests for `org-w3ctr--make-cache-oclosure'."
  (let ((x (t--make-cache-oclosure :wtf)))
    ($l (t--oinfo--cnt x) 0)
    ($l (t--oinfo--pid x) nil)
    ($l (t--oinfo--val x) nil))
  (let ((info '(:a 1 :b 2 :c 3))
        (info2 '(:a 3 :b 2 :c 1))
        (oa (t--make-cache-oclosure :a))
        (ob (t--make-cache-oclosure :b))
        (oc (t--make-cache-oclosure :c)))
    ;; a
    ($l (funcall oa info) 1)
    ($q (t--oinfo--pid oa) info)
    ($l (t--oinfo--val oa) 1)
    ($l (t--oinfo--cnt oa) 1)
    ;; b
    ($l (funcall ob info) 2)
    ($q (t--oinfo--pid ob) info)
    ($l (t--oinfo--val ob) 2)
    ($l (t--oinfo--cnt ob) 1)
    ;; c
    ($l (funcall oc info) 3)
    ($q (t--oinfo--pid oc) info)
    ($l (t--oinfo--val oc) 3)
    ($l (t--oinfo--cnt oc) 1)
    ;; test cnt
    (funcall oa info)
    ($l (t--oinfo--cnt oa) 2)
    (funcall ob info) (funcall ob info)
    ($l (t--oinfo--cnt ob) 3)
    (funcall oc info) (funcall oc info) (funcall oc info)
    ($l (t--oinfo--cnt oc) 4)
    ;; change plist
    ($l (funcall oa info2) 3)
    ($q (t--oinfo--pid oa) info2)
    ($l (funcall ob info2) 2)
    ($q (t--oinfo--pid ob) info2)
    ($l (funcall oc info2) 1)
    ($q (t--oinfo--pid oc) info2)))

(ert-deftest t--oinfo-pget ()
  "Tests for `org-w3ctr--pget'."
  (dlet ((t--oinfo-cache-props '(:a :b))
         (t--oinfo-cache-alist nil)
         (info '(:a 1 :b 2 :c 3)))
    (dolist (a t--oinfo-cache-props)
      (let* ((kname (symbol-name a))
             (fname (intern (concat "org-w3ctr--oinfo-test" kname))))
        (fset fname (t--make-cache-oclosure a))
        (push (cons a fname) t--oinfo-cache-alist)))
    ($l (eval '(t--pget info :a)) 1)
    ($l (t--oinfo--cnt (t--oinfo-oget :a)) 1)
    ($l (eval '(t--pget info :b)) 2)
    ($l (t--oinfo--cnt (t--oinfo-oget :b)) 1)
    ($l (eval '(t--pget info :c)) 3)
    ($n (alist-get :c t--oinfo-cache-alist))))

(ert-deftest t--oinfo-pput ()
  "Tests for `org-w3ctr--pput'."
  (dlet ((t--oinfo-cache-props '(:a :b))
         (t--oinfo-cache-alist nil)
         (info '(:a 1 :b 2 :c 3))
         (val 1))
    (dolist (a t--oinfo-cache-props)
      (let* ((kname (symbol-name a))
             (fname (intern (concat "org-w3ctr--oinfo-test" kname))))
        (fset fname (t--make-cache-oclosure a))
        (push (cons a fname) t--oinfo-cache-alist)))
    ;; a
    ($l (eval '(t--pput info :a 2)) 2)
    ($l (eval '(t--pget info :a)) 2)
    ($l (t--oinfo--val (t--oinfo-oget :a)) 2)
    ($l (plist-get info :a) 1)
    ;; b
    ($l (eval '(t--pput info :b 3)) 3)
    ($l (eval '(t--pget info :b)) 3)
    ($l (t--oinfo--val (t--oinfo-oget :b)) 3)
    ($l (plist-get info :b) 2)
    ;; c
    ($l (eval '(t--pput info :c 4)) 4)
    ($l (eval '(t--pget info :c)) 4)
    ($l (plist-get info :c) 4)
    ;; test side effect c
    ($l (eval '(t--pput info :c (incf val))) 2)
    ($l (eval '(t--pget info :c)) 2)
    ($l (plist-get info :c) 2)
    ;; test side effect b
    ($l (eval '(t--pput info :b (incf val))) 3)
    ($l (eval '(t--pget info :b)) 3)
    ($l (plist-get info :b) 2)))

(ert-deftest t--oinfo-cleanup ()
  "Tests for `org-w3ctr--oinfo-cleanup'."
  (dlet ((t--oinfo-cache-props '(:a :b))
         (t--oinfo-cache-alist nil)
         (info '(:a 1 :b 2 :c 3)))
    (dolist (a t--oinfo-cache-props)
      (let* ((kname (symbol-name a))
             (fname (intern (concat "org-w3ctr--oinfo" kname))))
        (fset fname (t--make-cache-oclosure a))
        (push (cons a fname) t--oinfo-cache-alist)))
    ($l (eval '(t--pget info :a)) 1)
    ($l (eval '(t--pget info :b)) 2)
    ($l (eval '(t--pget info :c)) 3)
    ($q (t--oinfo--pid (t--oinfo-oget :a)) info)
    ($q (t--oinfo--pid (t--oinfo-oget :b)) info)
    ($q (t--oinfo--val (t--oinfo-oget :a)) 1)
    ($q (t--oinfo--val (t--oinfo-oget :b)) 2)
    ($q (t--oinfo--cnt (t--oinfo-oget :a)) 1)
    ($q (t--oinfo--cnt (t--oinfo-oget :b)) 1)
    (t--oinfo-cleanup)
    ($l (t--oinfo--pid (t--oinfo-oget :a)) nil)
    ($l (t--oinfo--pid (t--oinfo-oget :a)) nil)
    ($l (t--oinfo--val (t--oinfo-oget :a)) nil)
    ($l (t--oinfo--val (t--oinfo-oget :a)) nil)
    ($q (t--oinfo--cnt (t--oinfo-oget :a)) 1)
    ($q (t--oinfo--cnt (t--oinfo-oget :a)) 1)))

(ert-deftest t--maybe-contents ()
  "Tests for `org-w3ctr--maybe-contents'."
  ($l (t--maybe-contents nil) "")
  ($l (t--maybe-contents "") "\n")
  ($l (t--maybe-contents "abc") "\nabc")
  ($l (t--maybe-contents 123) "")
  ($l (t--maybe-contents '(1 2)) ""))

(ert-deftest t--nw-p ()
  "Tests for `org-w3ctr--nw-p'."
  ($l (t--nw-p "123") "123")
  ($l (t--nw-p " 1") " 1")
  ($l (t--nw-p "\t\r\n2") "\t\r\n2")
  ($n (t--nw-p ""))
  ($n (t--nw-p "\t\s\r\n")))

(ert-deftest t--2str ()
  "Tests for `org-w3ctr--2str'."
  ($q (t--2str nil) nil)
  ($l (t--2str 1) "1")
  ($l (t--2str 114.514) "114.514")
  ($l (t--2str ?a) "97")
  ($l (t--2str 'hello) "hello")
  ($l (t--2str 'has\ space) "has space")
  ($l (t--2str 'has\#) "has#")
  ($l (t--2str "string") "string")
  ($n (t--2str [1]))
  ($n (t--2str (make-char-table 'sub)))
  ($n (t--2str (make-bool-vector 3 t)))
  ($n (t--2str (make-hash-table)))
  ($n (t--2str (lambda (x) x))))

(ert-deftest t--read-attr ()
  "Tests for `org-w3ctr--read-attr'."
  ;; `org-element-property' use `org-element--property'
  ;; and defined using `define-inline'.
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n _deft _force) n)))
    ($l (org-element-property :attr__ 123) 123)
    ($l (org-element-property nil 1) 1)
    ($l (t--read-attr nil '("123")) '(123))
    ($l (t--read-attr nil '("1 2 3" "4 5 6")) '(1 2 3 4 5 6))
    ($l (t--read-attr nil '("(class data) [hello] (id ui)"))
        '((class data) [hello] (id ui)))
    ($l (t--read-attr nil '("\"123\"")) '("123")))
  (t-check-element-values
   #'t--read-attr
   '(("#+attr__: 1 2 3\n#+attr__: 4 5 6\nhello world"
      (1 2 3 4 5 6))
     ("#+attr__: [hello world] (id no1)\nhello"
      ([hello world] (id no1)))
     ("nothing but text" . nil)
     ("#+attr__: \"str\"\nstring" ("str"))
     ("#+attr__:\nempty" nil))))

(ert-deftest t--read-attr__ ()
  "Tests for `org-w3ctr--read-attr__'."
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n _deft _force) n)))
    ($l (t--read-attr__ '("1 2 3")) '(1 2 3))
    ($l (t--read-attr__ '("(class data) open"))
        '((class data) open))
    ($l (t--read-attr__ '("(class hello world)" "foo"))
        '((class hello world) foo))
    ($l (t--read-attr__ '("[nim zig]")) '(("class" "nim zig")))
    ($l (t--read-attr__ '("[]")) '(nil))
    ($l (t--read-attr__ '("[][][]")) '(()()())))
  (t-check-element-values
   #'t--read-attr__
   '(("#+attr__: 1 2 3\n#+attr__: 4\ntest" (1 2 3 4))
     ("#+attr__: [hello world] (id no1)\ntest"
      (("class" "hello world") (id no1)))
     ("test" . nil)
     ("#+attr__:\n#+attr__:\ntest" nil)
     ("#+attr__: []\ntest" (nil))
     ("#+attr__: [][][]\ntest" (nil nil nil)))))

(ert-deftest t--encode-plain-text ()
  "Tests for `org-w3ctr--encode-plain-text'."
  ($l (t--encode-plain-text "") "")
  ($l (t--encode-plain-text "123") "123")
  ($l (t--encode-plain-text "hello world") "hello world")
  ($l (t--encode-plain-text "&") "&amp;")
  ($l (t--encode-plain-text "<") "&lt;")
  ($l (t--encode-plain-text ">") "&gt;")
  ($l (t--encode-plain-text "<&>") "&lt;&amp;&gt;")
  (dolist (a '(("a&b&c" . "a&amp;b&amp;c")
               ("<div>" . "&lt;div&gt;")
               ("<span>" . "&lt;span&gt;")))
    ($l (t--encode-plain-text (car a)) (cdr a))))

(ert-deftest t--encode-plain-text* ()
  "Tests for `org-w3ctr--encode-plain-text*'."
  ($l (t--encode-plain-text* "&") "&amp;")
  ($l (t--encode-plain-text* "<") "&lt;")
  ($l (t--encode-plain-text* ">") "&gt;")
  ($l (t--encode-plain-text* "<&>") "&lt;&amp;&gt;")
  ($l (t--encode-plain-text* "'") "&apos;")
  ($l (t--encode-plain-text* "\"") "&quot;")
  ($l (t--encode-plain-text* "\"'&\"")
      "&quot;&apos;&amp;&quot;"))

(ert-deftest t--make-attr ()
  "Tests for `org-w3ctr--make-attr'."
  ($n (t--make-attr nil))
  ($n (t--make-attr '(nil 1)))
  ($n (t--make-attr '([x])))
  ($l (t--make-attr '(open)) " open")
  ($l (t--make-attr '("disabled")) " disabled")
  ($l (t--make-attr '(FOO)) " foo")
  ($l (t--make-attr '(a b)) " a=\"b\"")
  ($l (t--make-attr '(class "example two")) " class=\"example two\"")
  ($l (t--make-attr '(foo [bar] baz)) " foo=\"baz\"")
  ($l (t--make-attr '(data-A "base64...")) " data-a=\"base64...\"")
  ($l (t--make-attr '(data-tt "a < b && c"))
      " data-tt=\"a &lt; b &amp;&amp; c\"")
  ($l (t--make-attr '(data-he "\"hello world\""))
      " data-he=\"&quot;hello world&quot;\"")
  ($l (t--make-attr '(sig "''")) " sig=\"&apos;&apos;\"")
  ($l (t--make-attr '(test ">'\""))
      " test=\"&gt;&apos;&quot;\""))

(ert-deftest t--make-attr__ ()
  "Tests for `org-w3ctr--make-attr__'."
  ($l (t--make-attr__ nil) "")
  ($l (t--make-attr__ '(nil)) "")
  ($l (t--make-attr__ '(nil nil [])) "")
  ($l (t--make-attr__ '(a)) " a")
  ($l (t--make-attr__ '((id yy 123) (class a\ b) test))
      " id=\"yy123\" class=\"a b\" test")
  ($l (t--make-attr__ '((test this th&t <=>)))
      " test=\"thisth&amp;t&lt;=&gt;\""))

(ert-deftest t--make-attr__id ()
  "Tests for `org-w3ctr--make-attr__id'."
  (t-check-element-values
   #'t--make-attr__id
   '(("#+attr__:\ntest" "")
     ("#+name:test\n#+attr__: hello\ntest" " id=\"test\" hello")
     ("#+name:1\n#+attr__:[data] (style {a:b})\ntest"
      " id=\"1\" class=\"data\" style=\"{a:b}\"")
     ("#+name:1\n#+attr__:[hello world]\ntest"
      " id=\"1\" class=\"hello world\"")
     ("#+name:1\n#+attr__:[]\ntest" " id=\"1\"")
     ("#+name:1\n#+attr__:(data-test \"test double quote\")\nh"
      " id=\"1\" data-test=\"test double quote\"")
     ("#+name:1\n#+attr__:(something <=>)\nt"
      " id=\"1\" something=\"&lt;=&gt;\""))))

(ert-deftest t--make-attribute-string ()
  "Tests for `org-w3ctr--make-attribute-string'."
  ($l (t--make-attribute-string '(:a "1" :b "2"))
      "a=\"1\" b=\"2\"")
  ($l (t--make-attribute-string nil) "")
  ($l (t--make-attribute-string '(:a nil)) "")
  ($l (t--make-attribute-string '(:a "\"a\""))
      "a=\"&quot;a&quot;\"")
  ($l (t--make-attribute-string '(:open "open"))
      "open=\"open\"")
  ($l (t--make-attribute-string '(:test "'\"'"))
      "test=\"&apos;&quot;&apos;\"")
  (t-check-element-values
   #'t--make-attribute-string
   '(("#+attr_html: :open open :class a\ntest"
      "open=\"open\" class=\"a\"")
     ("#+attr_html: :id wo-1 :two\ntest" "id=\"wo-1\"")
     ("#+attr_html: :id :idd hhh\ntest" "idd=\"hhh\"")
     ("#+attr_html: :null nil :this test\ntest" "this=\"test\""))))

(ert-deftest t--make-attr_html ()
  "Tests for `org-w3ctr--make-attr_html'."
  (t-check-element-values
   #'t--make-attr_html
   '(("#+attr_html:\ntest" "")
     ("#+attr_html: :hello hello\ntest" " hello=\"hello\"")
     ("#+name: 1\n#+attr_html: :class data\ntest"
      " class=\"data\" id=\"1\"")
     ("#+attr_html: :id 1 :class data\ntest"
      " id=\"1\" class=\"data\"")
     ("#+name: 1\n#+attr_html: :id 2 :class data two\ntest"
      " id=\"2\" class=\"data two\"")
     ("#+attr_html: :data-id < > ? 2 =\ntest"
      " data-id=\"&lt; &gt; ? 2 =\""))))

(ert-deftest t--make-attr__id* ()
  "Tests for `org-w3ctr--make-attr__id*'."
  (t-check-element-values
   #'t--make-attr__id*
   '(("#+attr__:\n#+attr_html: :class a\ntest" "")
     ("#+attr_html: :class a\ntest" " class=\"a\"")
     ("#+name: 1\n#+attr__: (id 2)\n#+attr_html: :id 3\ntest"
      " id=\"2\"")
     ("#+name: 1\n#+attr_html: :id 3\ntest" " id=\"3\""))))

(ert-deftest t--trim ()
  "Tests for `org-w3ctr--trim'."
  ($l (t--trim "123") "123")
  ($l (t--trim " 123") "123")
  ($l (t--trim " 123 ") "123")
  ($l (t--trim "  123  ") "123")
  ($l (t--trim "  123\n 456\n") "123\n 456")
  ($l (t--trim "\n 123" t) " 123")
  ($l (t--trim "\n\n  123\n" t) "  123"))

(ert-deftest t--nw-trim ()
  "Tests for `org-w3ctr--nw-trim'."
  ($l (t--nw-trim " ") nil)
  ($l (t--nw-trim " 1 ") "1")
  ($l (t--nw-trim "234\n") "234")
  ($l (t--nw-trim 1) nil)
  ($l (t--nw-trim 'hello) nil))

(ert-deftest t--sexp2html ()
  "Tests for `org-w3ctr--sexp2html'."
  ($l (t--sexp2html nil) "")
  ;; Basic tag with no attributes
  ($l (t--sexp2html '(p () "123")) "<p>123</p>")
  ($l (t--sexp2html '(p t "123")) "<p>123</p>")
  ;; Tag with attributes
  ($l (t--sexp2html '(a ((href "https://example.com")) "link"))
      "<a href=\"https://example.com\">link</a>")
  ($l (t--sexp2html '(img ((src "../" "1.jpg") (alt "../1.jpg"))))
      "<img src=\"../1.jpg\" alt=\"../1.jpg\">")
  ;; Nested tags
  ($l (t--sexp2html '(div () (p () "Hello") (p () "World")))
      "<div><p>Hello</p><p>World</p></div>")
  ;; Symbol as tag name
  ($l (t--sexp2html '(my-tag () "content")) "<my-tag>content</my-tag>")
  ;; Empty tag
  ($l (t--sexp2html '(area ())) "<area>")
  ($l (t--sexp2html '(base ())) "<base>")
  ($l (t--sexp2html '(br ())) "<br>")
  ($l (t--sexp2html '(col ())) "<col>")
  ($l (t--sexp2html '(embed ())) "<embed>")
  ($l (t--sexp2html '(hr ())) "<hr>")
  ($l (t--sexp2html '(img ())) "<img>")
  ($l (t--sexp2html '(input ())) "<input>")
  ($l (t--sexp2html '(link ())) "<link>")
  ($l (t--sexp2html '(meta ())) "<meta>")
  ($l (t--sexp2html '(param ())) "<param>")
  ($l (t--sexp2html '(source ())) "<source>")
  ($l (t--sexp2html '(track ())) "<track>")
  ($l (t--sexp2html '(wbr ())) "<wbr>")
  ;; Number as content
  ($l (t--sexp2html '(span () 42)) "<span>42</span>")
  ;; Mixed content (text and elements)
  ($l (t--sexp2html '(div () "Text " (span () "inside") " more text"))
      "<div>Text <span>inside</span> more text</div>")
  ;; Ignore unsupported types (e.g., vectors)
  ($l (t--sexp2html '(div () [1 2 3])) "<div></div>")
  ;; Always downcase
  ($l (t--sexp2html '(DIV () "123")) "<div>123</div>")
  ;; Allow bare tags
  ($l (t--sexp2html '(p)) "<p></p>")
  ($l (t--sexp2html '(hr)) "<hr>")
  ;; Escape
  ($l (t--sexp2html '(p () "123<456>")) "<p>123&lt;456&gt;</p>")
  ($l (t--sexp2html '(p () (b () "a&b"))) "<p><b>a&amp;b</b></p>"))

(ert-deftest t--make-string ()
  "Tests for `org-w3ctr--make-string'."
  ($l (t--make-string 1 "a") "a")
  ($l (t--make-string 2 "a") "aa")
  ($l (t--make-string 3 "a") "aaa")
  ($l (t--make-string 2 "ab") "abab")
  ($l (t--make-string 0 "a") "")
  ($l (t--make-string -1 "a") "")
  ($l (t--make-string 100 "") "")
  ($e! (t--make-string 3 [?a ?b]))
  ($e! (t--make-string "a" "a")))

(ert-deftest t--normalize-string ()
  "Tests for `org-w3ctr--normalize-string'."
  ($n (t--normalize-string nil))
  ($l "" (t--normalize-string ""))
  ($l "a\n" (t--normalize-string "a"))
  ($l "a  \n" (t--normalize-string "a  \n\n\n")))

(ert-deftest t--load-file ()
  "Tests for `org-w3ctr--load-file'."
  (let ((ox (with-temp-buffer
              (insert-file-contents "ox-w3ctr.el")
              (buffer-substring-no-properties
               (point-min) (point-max)))))
    ($l ox (t--load-file "ox-w3ctr.el")))
  ($e! (t--load-file "not-exist")))

(ert-deftest t--insert-file ()
  "Tests for `org-w3ctr--insert-file'."
  ($e! (t--insert-file default-directory))
  ($e! (t--insert-file "no-exist")))

(ert-deftest t--find-all ()
  "Tests for `org-w3ctr--find-all."
  ($l (t--find-all "[0-9]" "114514") '("1" "1" "4" "5" "1" "4"))
  ($l (t--find-all "[0-9]\\{2\\}" "191981") '("19" "19" "81"))
  ($l (t--find-all "" "123") nil)
  ($l (t--find-all "1" "") nil)
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]") '("[2000-01-02]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-02]")
      '("[2000-01-02]" "[2000-01-02]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-03]" 1)
      '("[2000-01-03]"))
  ($l (t--find-all org-ts-regexp-both "[2000-01-02]--[2000-01-03]" -1)
      '("[2000-01-02]" "[2000-01-03]")))

(ert-deftest t-center-block ()
  "Tests for `org-w3ctr-center-block'."
  (t-check-element-values
   #'t-center-block
   '(("#+begin_center\n#+end_center"
      "<div style=\"text-align:center;\"></div>")
     ("#+begin_center\n123\n#+end_center"
      "<div style=\"text-align:center;\">\n<p>123</p>\n</div>")
     ("#+BEGIN_CENTER\n\n\n#+END_CENTER"
      "<div style=\"text-align:center;\">\n\n</div>")
     ("#+BEGIN_CENTER\n\n\n\n\n\n#+END_CENTER"
      "<div style=\"text-align:center;\">\n\n</div>"))))

(ert-deftest t-drawer ()
  "Tests for `org-w3ctr-drawer'."
  (t-check-element-values
   #'t-drawer
   '((":hello:\n:end:"
      "<details><summary>hello</summary></details>")
     ("#+caption: what can i say\n:test:\n:end:"
      "<details><summary>what can i say</summary></details>")
     ("#+name: id\n#+attr__: [example]\n:h:\n:end:"
      "<details id=\"id\" class=\"example\"><summary>\
h</summary></details>")
     ("#+attr__: (open)\n:h:\n:end:"
      "<details open><summary>h</summary></details>")
     (":try-this:\n=int a = 1;=\n:end:"
      "<details><summary>try-this</summary>\n<p><code>\
int a = 1;</code></p>\n</details>")
     ("#+CAPTION:\n:test:\n:end:"
      "<details><summary>test</summary></details>")
     ("#+caption: \n:test:\n:end:"
      "<details><summary>test</summary></details>")
     ("#+caption:         \t\n:test:\n:end:"
      "<details><summary>test</summary></details>"))))

(ert-deftest t-dynamic-block ()
  "Tests for `org-w3ctr-dynamic-block'."
  (t-check-element-values
   #'t-dynamic-block
   '(("#+begin: hello\n123\n#+end:" "<p>123</p>\n")
     ("#+begin: nothing\n#+end:" ""))))

(ert-deftest t--checkbox ()
  "Tests for `org-w3ctr-checkbox'."
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--checkbox 'on info) "&#x2611;")
    ($l (t--checkbox 'off info) "&#x2610;")
    ($l (t--checkbox 'trans info) "&#x2612;")
    ($l (t--checkbox nil info) nil)
    ($l (t--checkbox [1] info) nil)
    ($l (t--checkbox "hello" info) nil)
    ($l (t--checkbox "on" info) nil)
    ($l (t--checkbox "off" info) nil)
    ($l (t--checkbox "trans" info) nil))
  (let ((info '(:html-checkbox-type ascii)))
    ($l (t--checkbox 'off info) "<code>[&#xa0;]</code>")
    ($l (t--checkbox 'on info) "<code>[X]</code>")
    ($l (t--checkbox 'trans info) "<code>[-]</code>"))
  (let ((info '(:html-checkbox-type html)))
    ($l (t--checkbox 'off info) "<input type=\"checkbox\">")
    ($l (t--checkbox 'on info) "<input type=\"checkbox\" checked>")
    ($l (t--checkbox 'trans info) "<input type=\"checkbox\">")))

(ert-deftest t--format-checkbox ()
  "Tests for `org-w3ctr--format-checkbox.'"
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-checkbox 'off info) "&#x2610; ")
    ($l (t--format-checkbox 'on info) "&#x2611; ")
    ($l (t--format-checkbox 'trans info) "&#x2612; ")
    ($l (t--format-checkbox nil info) "")
    ($l (t--format-checkbox 'test info) "")
    ($l (t--format-checkbox [1] info) "")
    ($l (t--format-checkbox '(1 . 2) info) "")
    ($l (t--format-checkbox #s(hello wtf) info) "")))

(ert-deftest t--format-ordered-item ()
  "Tests for `org-w3ctr--format-ordered-item'."
  ($l (t--format-ordered-item "" nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "\n  \n" nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "\t\r\n " nil nil nil) "<li></li>")
  ($l (t--format-ordered-item "123" nil nil nil) "<li>123</li>")
  ($l (t--format-ordered-item " 123 " nil nil nil) "<li>123</li>")
  ($l (t--format-ordered-item "123" nil nil 10) "<li value=\"10\">123</li>")
  ($l (t--format-ordered-item "123" nil nil 114514)
      "<li value=\"114514\">123</li>")
  ($l (t--format-ordered-item "123" nil nil 191981)
      "<li value=\"191981\">123</li>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-ordered-item "123" 'off info nil)
        "<li>&#x2610; 123</li>")
    ($l (t--format-ordered-item "123" 'on info nil)
        "<li>&#x2611; 123</li>")
    ($l (t--format-ordered-item "123" 'trans info nil)
        "<li>&#x2612; 123</li>")
    ($l (t--format-ordered-item "123" 'on info 114)
        "<li value=\"114\">&#x2611; 123</li>")))

(ert-deftest t--format-unordered-item ()
  "Tests for `org-w3ctr--format-unordered-item'."
  ($l (t--format-unordered-item "" nil nil) "<li></li>")
  ($l (t--format-unordered-item "\n  \n" nil nil) "<li></li>")
  ($l (t--format-unordered-item "\t\r\n " nil nil) "<li></li>")
  ($l (t--format-unordered-item "123" nil nil) "<li>123</li>")
  ($l (t--format-unordered-item " 123 " nil nil) "<li>123</li>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-unordered-item "123" 'off info)
        "<li>&#x2610; 123</li>")
    ($l (t--format-unordered-item "123" 'on info)
        "<li>&#x2611; 123</li>")
    ($l (t--format-unordered-item "123" 'trans info)
        "<li>&#x2612; 123</li>")))

(ert-deftest t--format-descriptive-item ()
  "Tests for `org-w3ctr--format-descriptive-item'."
  ($l (t--format-descriptive-item "" nil nil nil)
      "<dt>(no term)</dt><dd></dd>")
  ($l (t--format-descriptive-item " " nil nil nil)
      "<dt>(no term)</dt><dd></dd>")
  ($l (t--format-descriptive-item "\r\n\t " nil nil nil)
      "<dt>(no term)</dt><dd></dd>")
  ($l (t--format-descriptive-item "123" nil nil nil)
      "<dt>(no term)</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil nil)
      "<dt>(no term)</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "ONE")
      "<dt>ONE</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "TWO")
      "<dt>TWO</dt><dd>123</dd>")
  ($l (t--format-descriptive-item " 123 " nil nil "THREE ")
      "<dt>THREE </dt><dd>123</dd>")
  (let ((info '(:html-checkbox-type unicode)))
    ($l (t--format-descriptive-item "123" 'off info nil)
        "<dt>&#x2610; (no term)</dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'on info nil)
        "<dt>&#x2611; (no term)</dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'trans info nil)
        "<dt>&#x2612; (no term)</dt><dd>123</dd>")
    ($l (t--format-descriptive-item "123" 'trans info " test ")
        "<dt>&#x2612;  test </dt><dd>123</dd>")))

(ert-deftest t-item-unordered ()
  "Tests for `org-w3ctr-item' ordered clause."
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item
     '(("- 123" "<li>123</li>")
       ("- hello \n 123" "<li>hello \n123</li>")
       ("- hello \n\n123" "<li>hello</li>")
       ("- hello \n\n 123" "<li>hello\n<p>123</p></li>")
       ("- hello \n\n     \t123" "<li>hello\n<p>123</p></li>")
       ("- [ ] 123" "<li>&#x2610; 123</li>")
       ("- [X] 123" "<li>&#x2611; 123</li>")
       ("- [ ] 123   \n 234" "<li>&#x2610; 123   \n234</li>")
       ("- [ ] 123 \n\n234" "<li>&#x2610; 123</li>")
       ("- [ ] 123 \n\n 234" "<li>&#x2610; 123\n<p>234</p></li>")
       ("- [ ] [@1] 123" "<li>&#x2610; [@1] 123</li>")
       ("- [ ] [@1]123" "<li>&#x2610; [@1]123</li>")
       ("- [@2] 123" "<li>123</li>")
       ("- [@1]123"  "<li>123</li>")
       ("- [@a] 123" "<li>123</li>")
       ("- [@1] [ ] 123" "<li>&#x2610; 123</li>")
       ("- [@a] [ ] 123" "<li>&#x2610; 123</li>")
       ("- [@pp] [ ] 123" "<li>[@pp] [ ] 123</li>")
       ;; zero width space
       ("- [​@1] [ ] 123" "<li>[​@1] [ ] 123</li>")))))

(ert-deftest t-item-ordered ()
  "Tests for `org-w3ctr-item' unordered item."
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item
     '(("1. 123" "<li>123</li>")
       ("1. hello \n 123" "<li>hello \n123</li>")
       ("1. hello \n\n123" "<li>hello</li>")
       ("1. hello \n\n 123" "<li>hello\n<p>123</p></li>")
       ("1. hello \n\n     \t123" "<li>hello\n<p>123</p></li>")
       ("1. [ ] 123" "<li>&#x2610; 123</li>")
       ("1. [X] 123" "<li>&#x2611; 123</li>")
       ("1. [ ] 123   \n 234" "<li>&#x2610; 123   \n234</li>")
       ("1. [ ] 123 \n\n234" "<li>&#x2610; 123</li>")
       ("1. [ ] 123 \n\n 234" "<li>&#x2610; 123\n<p>234</p></li>")
       ("1. [ ] [@1] 123" "<li>&#x2610; [@1] 123</li>")
       ("1. [ ] [@1]123" "<li>&#x2610; [@1]123</li>")
       ("1. [@2] 123" "<li value=\"2\">123</li>")
       ("1. [@1]123"  "<li value=\"1\">123</li>")
       ("1. [@a] 123" "<li value=\"1\">123</li>")
       ("1. [@1] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
       ("1. [@a] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
       ("1. [@z] [ ] 123" "<li value=\"26\">&#x2610; 123</li>")
       ("1. [@pp] [ ] 123" "<li>[@pp] [ ] 123</li>")
       ;; zero width space
       ("1. [​@1] [ ] 123" "<li>[​@1] [ ] 123</li>")))))

(ert-deftest t-item-descriptive ()
  "Tests for `org-w3ctr-item' descriptive clause."
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item
     '(("- 123 :: tag" "<dt>123</dt><dd>tag</dd>")
       ("- hello :: test \n 123" "<dt>hello</dt><dd>test \n123</dd>")
       ("- hello :: \n\n123" "<dt>hello</dt><dd></dd>")
       ("- hello :: \n\n 123" "<dt>hello</dt><dd>123</dd>")
       ("- hello :: \n\n  123" "<dt>hello</dt><dd>123</dd>")
       ("- hello :: \n\n\n  123" "<dt>hello</dt><dd></dd>")
       ("- hello :: \n\n     \t123" "<dt>hello</dt><dd>123</dd>")
       ("- hello :: world 123" "<dt>hello</dt><dd>world 123</dd>")
       ("- h :: w \n123" "<dt>h</dt><dd>w</dd>")
       ("- h :: w \n 123" "<dt>h</dt><dd>w \n123</dd>")
       ("- h :: w \n\n123" "<dt>h</dt><dd>w</dd>")
       ("- h :: w \n\n 123" "<dt>h</dt><dd>w\n<p>123</p></dd>")
       ("- h :: w \n\n 123\n 456" "<dt>h</dt><dd>w\n<p>123\n456</p></dd>")
       ("- [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
       ("- [X] 123 ::" "<dt>&#x2611; 123</dt><dd></dd>")
       ("- [ ] 123 ::  \n 234" "<dt>&#x2610; 123</dt><dd>234</dd>")
       ("- [ ] 123 :: \n\n234" "<dt>&#x2610; 123</dt><dd></dd>")
       ("- [ ] 123 :: \n\n 234" "<dt>&#x2610; 123</dt><dd>234</dd>")
       ("- [ ] [@1] 123 ::" "<dt>&#x2610; [@1] 123</dt><dd></dd>")
       ("- [ ] [@1]123 ::" "<dt>&#x2610; [@1]123</dt><dd></dd>")
       ("- [@2] 123 ::" "<dt>123</dt><dd></dd>")
       ("- [@1]123 ::"  "<dt>123</dt><dd></dd>")
       ("- [@1]123::" "<li>123::</li>")
       ("- [@a] 123 ::" "<dt>123</dt><dd></dd>")
       ("- [@1] [ ] 123 ::" "<dt>&#x2610; 123</dt><dd></dd>")
       ("- [@a] [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
       ("- [@pp] [ ] :: 123" "<dt>[@pp] [ ]</dt><dd>123</dd>")
       ;; zero width space
       ("- [​@1] [ ] 123 :: " "<dt>[​@1] [ ] 123</dt><dd></dd>")
       ("- a ::" "<dt>a</dt><dd></dd>")
       ("- :: 3" "<li>:: 3</li>")
       ("- a :: b\n- c" "<dt>(no term)</dt><dd>c</dd>"
        "<dt>a</dt><dd>b</dd>")))))

(ert-deftest t-item ()
  "Tests for `org-w3ctr-item'."
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item
     '(("- [@a] [ ] 123 :: 456" "<dt>&#x2610; 123</dt><dd>456</dd>")
       ("1. [@1] [ ] 123" "<li value=\"1\">&#x2610; 123</li>")
       ("- [ ] 123 \n\n 234" "<li>&#x2610; 123\n<p>234</p></li>"))))
  ($e! (t-item nil "123" nil)))

(ert-deftest t-plain-list ()
  "Tests for `org-w3ctr-plain-list'."
  (t-check-element-values
   #'t-plain-list
   '(("- 123" "<ul>\n<li>123</li>\n</ul>")
     ("1. 123" "<ol>\n<li>123</li>\n</ol>")
     ("- x :: y" "<dl>\n<dt>x</dt><dd>y</dd>\n</dl>")
     ("#+name: test\n#+attr__: (data-test \"a joke\")\n- x"
      "<ul id=\"test\" data-test=\"a joke\">\n<li>x</li>\n</ul>")
     ("1. 123\n   - 2 3 4"
      "<ol>\n<li>123\n<ul>\n<li>2 3 4</li>\n</ul></li>\n</ol>"
      "<ul>\n<li>2 3 4</li>\n</ul>")))
  ($e! (t-plain-list nil "123" nil)))

(ert-deftest t-quote-block ()
  "Tests for `org-w3ctr-quote-block'."
  (t-check-element-values
   #'t-quote-block
   '(("#+begin_quote\n#+end_quote" "<blockquote></blockquote>")
     ("#+BEGIN_QUOTE\n#+END_QUOTE" "<blockquote></blockquote>")
     ("#+begin_quote\n123\n#+end_quote"
      "<blockquote>\n<p>123</p>\n</blockquote>")
     ("#+attr__: [test]\n#+BEGIN_QUOTE\n456\n#+END_QUOTE"
      "<blockquote class=\"test\">\n<p>456</p>\n</blockquote>")
     ("#+begin_quote\n\n\n#+end_quote" "<blockquote>\n\n</blockquote>")
     ("#+begin_quote\n\n\n\n\n\n\n\n\n\n#+end_quote"
      "<blockquote>\n\n</blockquote>"))))

(ert-deftest t-example-block ()
  "Tests for `org-w3ctr-example-block'."
  (t-check-element-values
   #'t-example-block
   '(("#+name: t\n#+begin_example\n#+end_example"
      "<div id=\"t\">\n<pre>\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n1\n2\n3\n#+end_example"
      "<div id=\"t\">\n<pre>\n1\n2\n3\n</pre>\n</div>")
     ("#+name: t\n#+attr__: [ex]\n#+BEGIN_EXAMPLE\n123\n#+END_EXAMPLE"
      "<div id=\"t\" class=\"ex\">\n<pre>\n123\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n 1\n 2\n 3\n#+end_example"
      "<div id=\"t\">\n<pre>\n1\n2\n3\n</pre>\n</div>")
     ("#+name:t\n#+begin_example\n\n\n\n#+end_example"
      "<div id=\"t\">\n<pre>\n\n\n\n</pre>\n</div>"))))

(ert-deftest t-export-block ()
  "Tests for `org-w3ctr-export-block'."
  (t-check-element-values
   #'t-export-block
   '(;; HTML
     ("#+begin_export html\nanythinghere\n#+end_export" "anythinghere\n")
     ("#+begin_export html\n#+end_export" "")
     ("#+begin_export html\n\n#+end_export" "\n")
     ("#+begin_export html\n\n\n\n#+end_export" "\n\n\n")
     ("#+begin_export html\n\n\n\n\n\n\n#+end_export" "\n\n\n\n\n\n")
     ;; MHTML
     ("#+begin_export mhtml\nanythinghere\n#+end_export" "anythinghere\n")
     ("#+begin_export mhtml\n#+end_export" "")
     ("#+begin_export mhtml\n\n\n#+end_export" "\n\n")
     ;; CSS
     ("#+begin_export css\np {color: red;}\n#+end_export"
      "<style>\np {color: red;}\n</style>")
     ("#+begin_export css\n#+end_export" "<style>\n</style>")
     ("#+begin_export CSS\n.test {margin: auto;}\n#+end_export"
      "<style>\n.test {margin: auto;}\n</style>")
     ;; JS
     ("#+begin_export js\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export js\n#+end_export" "<script>\n</script>")
     ("#+begin_export javascript\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export javascript\n#+end_export" "<script>\n</script>")
     ;; Elisp
     ("#+begin_export emacs-lisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export emacs-lisp\n#+end_export" "")
     ("#+begin_export elisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export elisp\n#+end_export" "")
     ;; Lisp data
     ("#+begin_export lisp-data\n (p() \"123\")\n#+end_export"
      "<p>123</p>")
     ("#+begin_export lisp-data\n (br)\n#+end_export" "<br>")
     ("#+BEGIN_EXPORT lisp-data\n (br)\n#+END_EXPORT" "<br>")
     ;; Unsupported type
     ("#+begin_export wtf\n no exported\n#+end_export" "")
     ("#+begin_export\n not exported\n#+end_export" ""))
   t))

(ert-deftest t-fixed-width ()
  "Tests for `org-w3ctr-fixed-width'."
  (t-check-element-values
   #'t-fixed-width
   '((":           " "<pre></pre>")
     (": 1\n" "<pre>\n1\n</pre>")
     (": 1\n: 2\n" "<pre>\n1\n2\n</pre>")
     (":  1\n:  2\n:   3\n" "<pre>\n1\n2\n 3\n</pre>")
     (": 1\n: \n" "<pre>\n1\n\n</pre>")
     ("#+name: t\n#+attr__: [test]\n: 1\n : 2\n: 3"
      "<pre id=\"t\" class=\"test\">\n1\n2\n3\n</pre>")
     (":\n:\n:\n:\n" "<pre>\n\n\n</pre>"))))

(ert-deftest t-horizontal-rule ()
  "Tests for `org-w3ctr-horizontal-rule'."
  (t-check-element-values
   #'t-horizontal-rule
   '(("-")
     ("--")
     ("---")
     ("----")
     ("-----" "<hr>")
     ("------" "<hr>")
     ("-------" "<hr>")
     ("--------" "<hr>")
     ("---------" "<hr>")
     ("----------" "<hr>")
     ("-------------------------------" "<hr>"))))

(ert-deftest t-keyword ()
  "Tests for `org-w3ctr-keyword'."
  (t-check-element-values
   #'t-keyword
   '(;; H
     ("#+h: " "")
     ("#+h: <p>123</p>" "<p>123</p>")
     ("#+h: <br>\n#+h: <br>" "<br>" "<br>")
     ;; HTML
     ("#+html: " "")
     ("#+html: <p>123</p>" "<p>123</p>")
     ("#+html: <a href=\"https://example.com\">Example</a>"
      "<a href=\"https://example.com\">Example</a>")
     ;; E
     ("#+e: " "")
     ("#+e: (concat \"1\" nil \"2\")" "12")
     ("#+e: (string-join '(\"a\" \"b\") \",\")" "a,b")
     ;; D
     ("#+d: " "")
     ("#+d: (br)" "<br>")
     ("#+d: (p((data-x \"1\"))123)" "<p data-x=\"1\">123</p>")
     ;; L
     ("#+l: " "")
     ("#+l: (br) (br)" "<br><br>")
     ("#+l: (br) \" \" (br)" "<br> <br>")
     ("#+l: (p() 123) (p() 234)" "<p>123</p><p>234</p>")
     ;; Otherwise
     ("#+hello: world" ""))
   t))

(ert-deftest t--wrap-image ()
  "Tests for `org-w3ctr--wrap-image'."
  ($l (t--wrap-image "" nil "" "") "<figure>\n</figure>")
  ($l (t--wrap-image "hello" nil "" "")
      "<figure>\nhello</figure>")
  ($l (t--wrap-image "hello" nil " abc" "")
      "<figure>\nhello<figcaption>abc</figcaption>\n</figure>")
  ($l (t--wrap-image "" nil "\ntest\n" "1")
      "<figure1>\n<figcaption>test</figcaption>\n</figure>"))

(ert-deftest t-paragraph ()
  "Tests for `org-w3ctr-paragraph'."
  (t-check-element-values
   #'t-paragraph
   '(("123" "<p>123</p>")
     ("123\n 234" "<p>123\n 234</p>")
     ;; trim
     ("    123" "<p>123</p>")
     ("123\n\t234" "<p>123\n\011234</p>")
     ;; two newline, two paragraph
     ("123\n\n234" "<p>234</p>" "<p>123</p>")
     ;; unordered list item's first object
     ("- 123 234" "123 234")
     ("- [ ] 123" "123")
     ("- 123\n 234" "123\n234")
     ("- 123\n\n   234" "<p>234</p>" "123\n")
     ;; first object with attributes
     ("-\n  #+attr__: [example]\n  123"
      "<span class=\"example\">123</span>")
     ("-\n  #+name: id\n  123\n\n  #+name: id2\n  456"
      "<p id=\"id2\">456</p>" "<span id=\"id\">123\n</span>")
     ;; standalone image
     ;; since `org-export-data' doesn't apply `:filter-parse-tree'
     ;; no newlines for test data.
     ("[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\"></figure>")
     ("#+name: id\n#+caption:cap\n[[./1.png]]"
      "<figure id=\"id\">\n<img src=\"./1.png\" alt=\"1.png\"><figcaption>cap</figcaption>\n</figure>")
     ;; empty caption
     ("#+caption: \n[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\"></figure>")
     ("#+attr__:[sidefigure]\n[[./2.gif]]"
      "<figure class=\"sidefigure\">\n<img src=\"./2.gif\" alt=\"2.gif\"></figure>")
     ("[[https://example.com/1.jpg]]"
      "<figure>\n<img src=\"https://example.com/1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[file:1.jpg]]" "<figure>\n<img src=\"1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[./1.png][name]]" "<p><a href=\"./1.png\">name</a></p>")
     ("[[https://example.com/1.jpg][file:1.jpg]]"
      "<figure>\n<a href=\"https://example.com/1.jpg\"><img src=\"1.jpg\" alt=\"1.jpg\"></a></figure>"))))

(ert-deftest t-paragraph-filter ()
  "Tests for `org-w3ctr-paragraph-filter'."
  ($l (t-paragraph-filter "a\n\n" nil nil) "a\n")
  ($l (t-paragraph-filter "a" nil nil) "a\n")
  ($l (t-paragraph-filter "\na" nil nil) "\na\n"))

(ert-deftest t-verse-block ()
  "Tests for `org-w3ctr-verse-block'."
  (t-check-element-values
   #'t-verse-block
   '(("#+begin_verse\n#+end_verse" "<p>\n</p>")
     ("#+BEGIN_VERSE\n#+END_VERSE" "<p>\n</p>")
     ("#+begin_verse\n1  2  3\n#+end_verse" "<p>\n1  2  3<br>\n</p>")
     ("#+begin_verse\n 1\n  2\n   3\n#+end_verse"
      "<p>\n1<br>\n&#xa0;2<br>\n&#xa0;&#xa0;3<br>\n</p>")
     ("#+name: this\n#+begin_verse\n#+end_verse"
      "<p id=\"this\">\n</p>")
     ("#+attr__:[hi]\n#+begin_verse\n\n\n#+end_verse"
      "<p class=\"hi\">\n<br>\n<br>\n</p>"))))

(ert-deftest t-entity ()
  "Tests for `org-w3ctr-entity'."
  (t-check-element-values
   #'t-entity
   '(("\\alpha \\beta \\eta \\gamma \\epsilon"
      "&epsilon;" "&gamma;" "&eta;" "&beta;" "&alpha;")
     ("\\AA" "&Aring;")
     ("\\real \\image \\imath \\jmath"
      "&jmath;" "&imath;" "&image;" "&real;")
     ("\\quot \\acute \\bdquo \\raquo"
      "&raquo;" "&bdquo;" "&acute;" "&quot;")
     ("\\Dagger \\ddag \\** \\dollar \\copy \\reg"
      "&reg;" "&copy;" "$" "&Dagger;" "&Dagger;")
     ("\\frac12 \\frac14 \\frac34 \\radic \\prop \\sim"
      "&sim;" "&prop;" "&radic;" "&frac34;" "&frac14;" "&frac12;"))))

(ert-deftest t-export-snippet ()
  "Tests for `org-w3ctr-export-snippet'."
  (t-check-element-values
   #'t-export-snippet
   '(("@@h:<span>123</span>@@" "<span>123</span>")
     ("@@h:@@" "")
     ("@@html:<span>123</span>@@" "<span>123</span>")
     ("@@html:@@" "")
     ("@@e:@@" "")
     ("@@e:(+ 1 2)@@" "3")
     ("@@e:'(1 2 3)@@" "(1 2 3)")
     ("@@d:@@" "")
     ("@@d:(span() \"nothing\")")
     ("@@d:(wbr)@@" "<wbr>")
     ("@@d:(wbr())@@" "<wbr>")
     ("@@l:@@" "")
     ("@@l:1 2 3@@" "123")
     ("@@l:\"1 \" \"2\"@@" "1 2")
     ("@@wtf::hello@@" ""))))

(ert-deftest t-line-break ()
  "Tests for `org-w3ctr-line-break'."
  ($l (t-line-break nil nil nil) "<br>\n"))

(ert-deftest t-target ()
  "Tests for `org-w3ctr-target'."
  (cl-letf* ((counter 0)
             ((symbol-function 't--reference)
              (lambda (_d _i &optional _n)
                (number-to-string (cl-incf counter)))))
    ($l (t-target nil nil nil) "<span id=\"1\"></span>")
    ($l (t-target nil nil nil) "<span id=\"2\"></span>")
    (t-check-element-values
     #'t-target
     '(("<<th1>> <<th2>> <<th3>>"
        "<span id=\"5\"></span>"
        "<span id=\"4\"></span>"
        "<span id=\"3\"></span>")))))

(ert-deftest t-radio-target ()
  "Tests for `org-w3ctr-radio-target'."
  (cl-letf* ((counter 0)
             ((symbol-function 't--reference)
              (lambda (_d _i &optional _n)
                (number-to-string (cl-incf counter)))))
    ($l (t-radio-target nil "hello" nil)
        "<span id=\"1\">hello</span>")
    ($l (t-radio-target nil "world" nil)
        "<span id=\"2\">world</span>")
    ($l (t-radio-target nil nil nil)
        "<span id=\"3\"></span>")
    (t-check-element-values
     #'t-radio-target
     '(("<<<th1>>> <<<th2>>> <<<th3>>>"
        "<span id=\"6\">th3</span>"
        "<span id=\"5\">th2</span>"
        "<span id=\"4\">th1</span>")))))

(ert-deftest t-statistics-cookie ()
  "Tests for `org-w3ctr-statistics-cookie'."
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n &optional _d _f) n)))
    ($l (t-statistics-cookie "" nil nil) "<code></code>")
    ($l (t-statistics-cookie "y" nil nil) "<code>y</code>")
    ($l (t-statistics-cookie ()()()) "<code>nil</code>"))
  (t-check-element-values
   #'t-statistics-cookie
   '(("- hello [/]" "<code>[/]</code>")
     ("- hello [0/1]\n  - [ ] helllo" "<code>[0/1]</code>")
     ("- hello [33%]\n  - [X] hello" "<code>[33%]</code>")
     ("- hello :: abc [0/1]\n  - [ ] this is what"
      "<code>[0/1]</code>")
     ("1. hello [50%]\n   1. [ ] hello1\n   2. [X] hello2"
      "<code>[50%]</code>"))))

(ert-deftest t-subscript ()
  "Tests for `org-w3ctr-subscript'."
  ($l (t-subscript nil "123" nil) "<sub>123</sub>")
  ($l (t-subscript nil "" nil) "<sub></sub>")
  ($l (t-subscript nil t nil) "<sub>t</sub>")
  ($l (t-subscript nil nil nil) "<sub>nil</sub>")
  (t-check-element-values
   #'t-subscript
   '(("1_2" "<sub>2</sub>")
     ("x86_64" "<sub>64</sub>")
     ("f_{1}" "<sub>1</sub>"))))

(ert-deftest t-superscript ()
  "Tests for `org-w3ctr-superscript'."
  ($l (t-superscript nil "123" nil) "<sup>123</sup>")
  ($l (t-superscript nil "" nil) "<sup></sup>")
  ($l (t-superscript nil t nil) "<sup>t</sup>")
  ($l (t-superscript nil nil nil) "<sup>nil</sup>")
  (t-check-element-values
   #'t-superscript
   '(("1^2" "<sup>2</sup>")
     ("x86^64" "<sup>64</sup>")
     ("f^{1}" "<sup>1</sup>"))))

(ert-deftest t--get-markup-format ()
  "Tests for `org-w3ctr--get-markup-format'."
  (let ((info '(:html-text-markup-alist ((a . 2) (b . 3) (c . 4)))))
    ($l (t--get-markup-format 'a info) 2)
    ($l (t--get-markup-format 'b info) 3)
    ($l (t--get-markup-format 'c info) 4))
  ($l (t--get-markup-format 'anything nil) "%s"))

(ert-deftest t-bold ()
  "Tests for `org-w3ctr-bold'."
  (t-check-element-values
   #'t-bold
   '(("*abc*" "<strong>abc</strong>")
     ("**abc**"
      "<strong><strong>abc</strong></strong>"
      "<strong>abc</strong>")
     ("**" . nil)
     ("***" "<strong>*</strong>")
     ("****" "<strong>**</strong>")
     ("*****"
      "<strong><strong>*</strong></strong>"
      "<strong>*</strong>")
     ("*\\star\\star\\star*" "<strong>***</strong>")
     ("*hello world this world*"
      "<strong>hello world this world</strong>")
     ("*hello\nworld*" "<strong>hello\nworld</strong>"))))

(ert-deftest t-italic ()
  "Tests for `org-w3ctr-italic'."
  (t-check-element-values
   #'t-italic
   '(("/abc/" "<em>abc</em>")
     ("//abc//"
      "<em><em>abc</em></em>" "<em>abc</em>")
     ("//" . nil)
     ("///" "<em>/</em>")
     ("////" "<em>//</em>")
     ("/////"
      "<em><em>/</em></em>" "<em>/</em>")
     ("/\\slash\\slash\\slash/" "<em>///</em>")
     ("/hello world this world/"
      "<em>hello world this world</em>")
     ("/hello\nworld/" "<em>hello\nworld</em>"))))

(ert-deftest t-underline ()
  "Tests for `org-w3ctr-underline'."
  (t-check-element-values
   #'t-underline
   '(("_abc_" "<u>abc</u>")
     ("__abc__"
      "<u><u>abc</u></u>"
      "<u>abc</u>")
     ("__" . nil)
     ("___" "<u>_</u>")
     ("____" "<u>__</u>")
     ("_____"
      "<u><u>_</u></u>"
      "<u>_</u>")
     ("_\\under\\under\\under_"
      "<u>___</u>")
     ("_hello world this world_"
      "<u>hello world this world</u>")
     ("_hello\nworld_"
      "<u>hello\nworld</u>"))))

(ert-deftest t-verbatim ()
  "Tests for `org-w3ctr-verbatim'."
  (t-check-element-values
   #'t-verbatim
   '(("=abc=" "<code>abc</code>")
     ("==abc==" "<code>=abc=</code>")
     ("==" . nil)
     ("===" "<code>=</code>")
     ("====" "<code>==</code>")
     ("=====" "<code>===</code>")
     ("=\\slash\\slash\\slash="
      "<code>\\slash\\slash\\slash</code>")
     ("=hello world this world="
      "<code>hello world this world</code>")
     ("=hello\nworld=" "<code>hello\nworld</code>"))))

(ert-deftest t-code ()
  "Tests for `org-w3ctr-code'."
  (t-check-element-values
   #'t-code
   '(("~abc~" "<code>abc</code>")
     ("~~abc~~" "<code>~abc~</code>")
     ("~~" . nil)
     ("~~~" "<code>~</code>")
     ("~~~~" "<code>~~</code>")
     ("~~~~~" "<code>~~~</code>")
     ("~\\slash\\slash\\slash~"
      "<code>\\slash\\slash\\slash</code>")
     ("~hello world this world~"
      "<code>hello world this world</code>")
     ("~hello\nworld~" "<code>hello\nworld</code>"))))

(ert-deftest t-strike-through ()
  "Tests for `org-w3ctr-strike-through'."
  (t-check-element-values
   #'t-strike-through
   '(("+abc+" "<s>abc</s>")
     ("++abc++"
      "<s><s>abc</s></s>"
      "<s>abc</s>")
     ("++" . nil)
     ("+++" "<s>+</s>")
     ("++++" "<s>++</s>")
     ("+++++"
      "<s><s>+</s></s>"
      "<s>+</s>")
     ("+\\plus\\plus\\plus+" "<s>+++</s>")
     ("+hello world this world+"
      "<s>hello world this world</s>")
     ("+hello\nworld+" "<s>hello\nworld</s>"))))

(ert-deftest t--convert-special-strings ()
  "Tests for `org-w3ctr--convert-special-strings'."
  (dolist (a '(("hello..." . "hello&#x2026;")
               ("......" . "&#x2026;&#x2026;")
               ("\\\\-" . "\\&#x00ad;")
               ("---abc" . "&#x2014;abc")
               ("--abc" . "&#x2013;abc")))
    ($l (t--convert-special-strings (car a)) (cdr a))))

(ert-deftest t-plain-text ()
  "Tests for `org-w3ctr-plain-text'."
  ($l (t-plain-text "a < b & c > d" '())
      "a &lt; b &amp; c &gt; d")
  ($l (t-plain-text "\"hello\"" '(:with-smart-quotes t))
      "\"hello\"")
  ($l (t-plain-text "a -- b" '(:with-special-strings t))
      "a &#x2013; b")
  ($l (t-plain-text "line1\nline2" '(:preserve-breaks t))
      "line1<br>\nline2")
  ($l (t-plain-text
       "\"a < b\" -- c\nd"
       '( :with-smart-quotes t
          :with-special-strings t
          :preserve-breaks t))
      "\"a &lt; b\" &#x2013; c<br>\nd"))

(ert-deftest t--timezone-to-offset ()
  "Tests for `org-w3ctr--timezone-to-offset'."
  ($it t--timezone-to-offset
    ($l (it "local") 'local)
    ($l (it "LOCAL") 'local)
    ($l (it "LoCaL") 'local)
    ($l (it "lOcAl") 'local)
    ($l (it "UTC+8") (* 8 3600))
    ($l (it "UTC+08") 28800)
    ($l (it "GMT-5") (* -5 3600))
    ($l (it "+0530") (+ (* 5 3600) (* 30 60)))
    ($l (it "-0830") (+ (* -8 3600) (* -30 60)))
    ($n (it "+10"))
    ($n (it "-11"))
    ($n (it "INVALID"))
    ($n (it "UTC+123"))
    ($n (it "+12345"))
    ($n (it "+1400"))
    ($n (it "UTC+13"))
    ($n (it "UTC-13"))
    ($n (it "+0860"))))

(ert-deftest t--get-info-timezone-offset ()
  "Tests for `org-w3ctr--get-info-timezone-offset'."
  ($it t--get-info-timezone-offset
    (let ((info0 '(:html-timezone "local")))
      ($l (it info0) 'local))
    (let ((info1 '(:html-timezone 28800)))
      ($l (it info1) 28800))
    (let ((info2 '(:html-timezone -18000)))
      ($l (it info2) -18000))
    (let ((info3 '(:html-timezone "UTC+8")))
      ($l (it info3) 28800)
      ($s (numberp (t--pget info3 :html-timezone)))
      ($l (t--pget info3 :html-timezone) 28800))
    (let ((info4 '(:html-timezone "-0500")))
      ($l (it info4) -18000)
      ($s (numberp (t--pget info4 :html-timezone)))
      ($l (t--pget info4 :html-timezone) -18000))
    (let ((info5 '(:html-timezone "UTC+0")))
      ($l (it info5) 0)
      ($l (t--pget info5 :html-timezone) 0))
    (let ((info6 '(:html-timezone "+0530")))
      ($l (it info6) 19800)
      ($l (t--pget info6 :html-timezone) 19800))
    (let ((info7 '(:html-timezone "Invalid")))
      ($e! (it info7)))
    (let ((info8 '(:html-timezone 3600 :other "value")))
      ($l (it info8) 3600)
      ($l info8 '(:html-timezone 3600 :other "value")))))

(ert-deftest t--get-info-export-timezone-offset ()
  "Tests for `org-w3ctr--get-info-export-timezone-offset'."
  ($it t--get-info-export-timezone-offset
    ;; When :html-export-timezone is nil, use :html-timezone
    (let ((info1 '(:html-timezone 28800)))
      ($l (it info1) 28800))
    (let ((info2 '( :html-timezone "UTC+8"
                    :html-export-timezone nil)))
      ($l (it info2) 28800))
    ;; When :html-timezone is "local", always use 'local
    (let ((info3 '( :html-timezone "local"
                    :html-export-timezone 3600)))
      ($q (it info3) 'local))
    (let ((info4 '( :html-timezone "local"
                    :html-export-timezone "UTC+5")))
      ($q (it info4) 'local))
    (let ((info41 '( :html-timezone local
                     :html-export-timezone "+0100")))
      ($q (it info41) 'local))
    ;; :html-export-timezone is local
    (let ((info42 '( :html-timezone 0
                     :html-export-timezone "local")))
      ($q (it info42) 'local))
    (let ((info43 '( :html-timezone 0
                     :html-export-timezone local)))
      ($q (it info43) 'local))
    ;; When :html-export-timezone is number, use directly
    (let ((info5 '( :html-timezone 28800
                    :html-export-timezone -18000)))
      ($l (it info5) -18000))
    ;; When :html-export-timezone is string, convert and cache
    (let ((info6 '( :html-timezone 28800
                    :html-export-timezone "-0500")))
      ($l (it info6) -18000)
      ($s (fixnump (t--pget info6 :html-export-timezone)))
      ($l (t--pget info6 :html-export-timezone) -18000))
    (let ((info7 '( :html-timezone 28800
                    :html-export-timezone "+0530")))
      ($l (it info7) 19800)
      ($l (t--pget info7 :html-export-timezone) 19800))
    (let ((info8 '( :html-timezone 0
                    :html-export-timezone "UTC+0")))
      ($l (it info8) 0))
    ;; Invalid
    (let ((info9 '( :html-timezone 3600
                    :html-export-timezone "Invalid")))
      ($e! (it info9)))
    (let ((info9 '( :html-timezone "WTF"
                    :html-export-timezone "+0000")))
      ($e! (it info9)))
    ;; Test optional argument
    (let ((info10 '(:html-export-timezone "+0000")))
      ($q (it info10 'local) 'local))
    (let ((info11 '(:html-export-timezone "UTC+8")))
      ($q (it info11 10) 28800))))

(ert-deftest t--get-info-timezone-delta ()
  "Tests for `org-w3ctr--get-info-timezone-delta'."
  ($it t--get-info-timezone-delta
    (let ((info '( :html-timezone 2
                   :html-export-timezone 1)))
      ($l (it info) -1))
    (let ((info '( :html-timezone 28800
                   :html-export-timezone 0)))
      ($l (it info) -28800))
    (let ((info '( :html-timezone local
                   :html-export-timezone 3600)))
      ($l (it info) 0))
    (let ((info '( :html-timezone local
                   :html-export-timezone 3600)))
      ($l (it info) 0))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info) 77467))
    (let ((info '( :html-timezone "WTF"
                   :html-export-timezone "INVALID")))
      ($e! (it info)))
    (let ((info '( :html-timezone 0
                   :html-export-timezone "INVALID")))
      ($e! (it info)))
    ($e! (it nil))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info 1 2) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info nil 114515) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it info 191980) 1))
    (let ((info '( :html-timezone 114514
                   :html-export-timezone 191981)))
      ($l (it nil 114514 191981) 77467))))

(ert-deftest t--get-datetime-format ()
  "Tests for `org-w3ctr--get-datetime-format'."
  ($it t--get-datetime-format
    ($l (it 28800 's-none) "%F %R+0800")
    ($l (it 18000 's-none) "%F %R+0500")
    ($l (it -28800 's-none) "%F %R-0800")
    ($l (it -18000 's-none) "%F %R-0500")
    ($l (it 0 's-none) "%F %R+0000")
    ($l (it 0 's-none-zulu) "%F %RZ")
    ($l (it 0 's-colon) "%F %R+00:00")
    ($l (it 0 's-colon-zulu) "%F %RZ")
    ($l (it 4800 's-colon-zulu) "%F %R+01:20")
    ($l (it 0 'T-colon) "%FT%R+00:00")
    ($l (it 0 'T-none) "%FT%R+0000")
    ($l (it 0 'T-colon) "%FT%R+00:00")
    ($l (it 0 'T-colon-zulu) "%FT%RZ")
    ($l (it 3600 's-colon) "%F %R+01:00")
    ($l (it -900 's-colon) "%F %R-00:15")
    ($l (it 19800 'T-colon) "%FT%R+05:30")
    ($l (it -16200 'T-colon) "%FT%R-04:30")
    ($l (it 50400 's-none) "%F %R+1400")
    ($l (it -43200 's-none) "%F %R-1200")
    ($l (it 37800 'T-colon-zulu) "%FT%R+10:30")
    ($n (it 0 nil))
    ($l (it 'local nil t) "%F")
    ($l (it 'local 's-none) "%F %R")
    ($l (it 'local 's-none-zulu) "%F %R")
    ($l (it 'local 's-colon) "%F %R")
    ($l (it 'local 's-colon-zulu) "%F %R")
    ($l (it 'local 'T-none) "%FT%R")
    ($l (it 'local 'T-none-zulu) "%FT%R")
    ($l (it 'local 'T-colon) "%FT%R")
    ($l (it 'local 'T-colon-zulu) "%FT%R")))

(ert-deftest t--format-datetime ()
  "Tests for `org-w3ctr--format-datetime'."
  ;; Basic test with space separator and +HHMM timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info1 '( :html-timezone 28800
                  :html-export-timezone 28800
                  :html-datetime-option s-none)))
    ($l (t--format-datetime test-time info1)
        "2023-01-01 12:00+0800"))
  ;; Test with colon in time and -HHMM timezone
  (let ((test-time (encode-time 0 30 9 15 6 2023))
        (info2 '( :html-timezone -14400
                  :html-export-timezone -14400
                  :html-datetime-option s-colon)))
    ($l (t--format-datetime test-time info2)
        "2023-06-15 09:30-04:00"))
  ;; Test UTC with Zulu timezone
  (let ((test-time (encode-time 0 0 0 1 1 2023))
        (info3 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option s-none-zulu)))
    ($l (t--format-datetime test-time info3)
        "2023-01-01 00:00Z"))
  ;; Test with T separator and +HH:MM timezone
  (let ((test-time (encode-time 0 45 18 31 12 2023))
        (info4 '( :html-timezone 19800
                  :html-export-timezone 19800
                  :html-datetime-option T-colon)))
    ($l (t--format-datetime test-time info4)
        "2023-12-31T18:45+05:30"))
  ;; Test with T separator and Zulu timezone for UTC
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info5 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option T-colon-zulu)))
    ($l (t--format-datetime test-time info5)
        "2023-01-01T12:00Z"))
  ;; Test local timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info6 '( :html-timezone "local"
                  :html-export-timezone "local"
                  :html-datetime-option s-none)))
    ($l "2023-01-01 12:00"
        (t--format-datetime test-time info6)))
  ;; Test invalid timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info7 '(:html-timezone "Invalid")))
    ($e! (t--format-datetime test-time info7)))
  ;; time out of range
  (let ((info8 '(:html-timezone 0 :html-datetime-option s-none)))
    ($e!l (t--format-datetime -65536 info8)
          '(error "Time may be out of range: -65536"))))

(ert-deftest t--call-with-invalid-time-spec-handler ()
  "Tests for `org-w3ctr--call-with-invalid-time-spec-handler'."
  (let ((ts (t-get-parsed-elements
             "[2000-01-01] [1945-08-15] [1145-05-14]--[1919-08-10]"
             'timestamp)))
    ($e!l (t--call-with-invalid-time-spec-handler
           (lambda (_ts) (error "Invalid time specification"))
           (nth 0 ts))
          '(error "Timestamp [2000-01-01] encode failed"))
    ($e!l (t--call-with-invalid-time-spec-handler
           #'org-timestamp-to-time (nth 1 ts))
          '(error "Timestamp [1945-08-15] encode failed"))
    ($e!l (t--call-with-invalid-time-spec-handler
           #'org-element-timestamp-interpreter (nth 2 ts) nil)
          '(error "Timestamp [1145-05-14]--[1919-08-10] encode failed")))
  (let ((ts (t-get-parsed-elements
             "[2038-01-19 03:14:07] [2025-06-17 16:40]" 'timestamp)))
    ($s (t--call-with-invalid-time-spec-handler
         #'org-timestamp-to-time (nth 0 ts)))
    ($s (t--call-with-invalid-time-spec-handler
         #'org-element-interpret-data (nth 1 ts)))))

(ert-deftest t--format-ts-datetime ()
  "Tests for `org-w3ctr--format-ts-datetime'."
  (let* ((info '( :html-timezone 28800 :html-export-timezone 0
                  :html-datetime-option T-none-zulu))
         (ts0 (t-get-parsed-elements "[1900-01-01]" 'timestamp))
         (ts1 (t-get-parsed-elements "[2025-06-17 16:49]" 'timestamp))
         (ts2 (t-get-parsed-elements "[2038-01-01]" 'timestamp))
         (ts3 (t-get-parsed-elements
               "[2025-06-17]--[2035-06-17]" 'timestamp))
         (ts4 (t-get-parsed-elements
               "[2025-01-01 18:00]--[2026-01-01 15:00]" 'timestamp))
         (ts5 (t-get-parsed-elements
               "<2022-06-07>--<2022-06-08 21:00>" 'timestamp))
         (ts6 (t-get-parsed-elements
               "[2022-06-07 09:00]--[2022-06-08]" 'timestamp)))
    ($e!l (t--format-ts-datetime (nth 0 ts0) info)
          '(error "Timestamp [1900-01-01] encode failed"))
    ($l (t--format-ts-datetime (nth 0 ts1) info)
        " datetime=\"2025-06-17T08:49Z\"")
    ($l (t--format-ts-datetime (nth 0 ts2) info)
        " datetime=\"2038-01-01\"")
    ($l (t--format-ts-datetime (nth 0 ts3) info)
        " datetime=\"2025-06-17\"")
    ($l (t--format-ts-datetime (nth 0 ts3) info t)
        " datetime=\"2035-06-17\"")
    ($l (t--format-ts-datetime (nth 0 ts4) info)
        " datetime=\"2025-01-01T10:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts4) info t)
        " datetime=\"2026-01-01T07:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts5) info)
        " datetime=\"2022-06-07\"")
    ($l (t--format-ts-datetime (nth 0 ts5) info t)
        " datetime=\"2022-06-08\"")
    ($l (t--format-ts-datetime (nth 0 ts6) info)
        " datetime=\"2022-06-07T01:00Z\"")
    ($l (t--format-ts-datetime (nth 0 ts6) info t)
        " datetime=\"2022-06-08T01:00Z\"")))

(ert-deftest t--interpret-timestamp ()
  "Tests for `org-w3ctr--interpret-timestamp'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (g (x) (t--interpret-timestamp (f x))))
    ($l (g "[2000-01-01]") "[2000-01-01 Sat]")
    ($l (g "[1970-01-02]") "[1970-01-02 Fri]")
    ($l (g "[1972-02-21]") "[1972-02-21 Mon]")
    ($l (g "<1989-11-09 WHAT>") "<1989-11-09 Thu>")
    ($l (g "[1991-12-26] thu") "[1991-12-26 Thu]")
    ($l (g "[2001-09-11] WTF") "[2001-09-11 Tue]")
    ($l (g "[2020-03-11] NUL") "[2020-03-11 Wed]")
    ($l (g "[1983-01-01]") "[1983-01-01 Sat]")
    ($l (g "[2022-11-30]") "[2022-11-30 Wed]")
    ($l (g "[2011-03-11]") "[2011-03-11 Fri]")
    ($l (g "[2008-02-30]") "[2008-03-01 Sat]")
    ($l (g "[2029-02-29]") "[2029-03-01 Thu]")
    ($l (g "[2029-02-30]") "[2029-03-02 Fri]")
    ($l (g "[2038-01-19]") "[2038-01-19 Tue]")
    ($l (g "[2038-01-19 03:14:07 UTC]") "[2038-01-19 Tue 03:14]")
    ($l (g "[2022-11-30 12:13 UTC+8]") "[2022-11-30 Wed 12:13]")
    ($l (g "[1976-09-09 00:10 UTC+8]") "[1976-09-09 Thu 00:10]")
    ($l (g "[2024-12-31 24:00]") "[2025-01-01 Wed 00:00]")
    ($l (g "[2025-06-18 99:99]") "[2025-06-22 Sun 04:39]")
    ($l (g "[2000-07-09 YY 19:25]") "[2000-07-09 Sun 19:25]")
    ($l (g "[2099-12-31 23:59:59]") "[2099-12-31 Thu 23:59]")
    ($l (g "[2025-06-18 00:00-03:07]") "[2025-06-18 Wed 00:00-03:07]")
    ($l (g "[2025-06-06 14:00-25:00]") "[2025-06-06 Fri 14:00-25:00]")
    ($l (g "[2000-01-01]--[2020-01-01]")
        "[2000-01-01 Sat]--[2020-01-01 Wed]")
    ($l (g "<1981-01-01>--<2020-01-01>")
        "<1981-01-01 Thu>--<2020-01-01 Wed>")
    ($l (g "[2000-01-01]--<2000-01-02>")
        "[2000-01-01 Sat]--[2000-01-02 Sun]")
    ($l (g "<2000-01-01>--[2000-01-02]")
        "<2000-01-01 Sat>--<2000-01-02 Sun>")
    ($l (g "[2020-01-01]--[2000-01-01]")
        "[2020-01-01 Wed]--[2000-01-01 Sat]")
    ($l (g "[2025-02-01 00:00]--[2025-06-01 00:00]")
        "[2025-02-01 Sat 00:00]--[2025-06-01 Sun 00:00]")
    ($l (g "[2025-02-01 00:00]--[2025-05-01]")
        "[2025-02-01 Sat 00:00]--[2025-05-01 Thu 00:00]")
    ($l (g "<2020-01-01>--[2025-01-01 12:23]")
        "<2020-01-01 Wed>--<2025-01-01 Wed 12:23>")
    ($l (g "[2020-01-01 12:23]--<1999-10-10>")
        "[2020-01-01 Wed 12:23]--[1999-10-10 Sun 12:23]")
    ($l (g "[1999-01-01]--[1999-01-02 12:00]")
        "[1999-01-01 Fri]--[1999-01-02 Sat 12:00]")
    ($l (g "[1999-01-01 12:00-13:00]--[2000-01-01 13:00-14:00]")
        "[1999-01-01 Fri 12:00]--[2000-01-01 Sat 13:00]"))
  (cl-flet ((f (s) (car (t-get-parsed-elements s 'timestamp)))
            (g (x) (t--interpret-timestamp x)))
    ($e!l (g (f "[1949-10-01]"))
          '(error "Timestamp [1949-10-01] encode failed"))
    ($e! (let ((ts (f "[2000-01-01]")))
           (setf (org-element-property :year-start ts) nil)
           (g ts))))
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (g (x) (t--interpret-timestamp (f x))))
    ($l (g "<2007-05-16 12:30 +1h>") "<2007-05-16 Wed 12:30 +1h>")
    ($l (g "<2007-05-16 12:30 +1d>") "<2007-05-16 Wed 12:30 +1d>")
    ($l (g "<2007-05-16 12:30 +1w>") "<2007-05-16 Wed 12:30 +1w>")
    ($l (g "<2007-05-16 12:30 +1m>") "<2007-05-16 Wed 12:30 +1m>")
    ($l (g "<2007-05-16 12:30 +1y>") "<2007-05-16 Wed 12:30 +1y>")))

(ert-deftest t--format-timestamp-diary ()
  "Tests for `org-w3ctr--format-timestamp-diary'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (g (x info) (t--format-timestamp-diary (f x) info))
             (mk (w o) `( :html-timestamp-wrapper ,w
                          :html-timestamp-option ,o)))
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'span 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'time 'raw))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'org))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'fmt))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'cus))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'fun))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 42)>" (mk 'none 'wtf))
        "&lt;%%(diary-float t 42)&gt;")
    ($l (g "<%%(diary-float t 4 2) 22:00-23:00>" (mk 'none 'org))
        "&lt;%%(diary-float t 4 2) 22:00-23:00&gt;")
    ($l (g "<%%(diary-float t 4 2) 22:00>--<2222-02-22 23:00>"
           (mk 'none 'org))
        "&lt;%%(diary-float t 4 2) 22:00&gt;")))

(ert-deftest t--format-timestamp-raw-1 ()
  "Tests for `org-w3ctr--format-timestamp-raw-1'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (g (x y info) (t--format-timestamp-raw-1 (f x) y info))
             (p (w) `( :html-timestamp-wrapper ,w))
             (c (&rest args) (apply #'concat args)))
    ($e!l (g "[2000-01-01]" "[0000-00-00]"(p 'wtf))
          '(error "Unknown timestamp wrapper: wtf"))
    ;; test none
    (let ((ts "[2000-01-01]"))
      ($l (g ts "[2000-01-01 test]" (p 'none)) "[2000-01-01 test]")
      ($l (g ts "test" (p 'none)) "test")
      ($l (g ts "<time>123</time>" (p 'none))
          "&lt;time&gt;123&lt;/time&gt;")
      ($l (g ts "&&&&&" (p 'none)) "&amp;&amp;&amp;&amp;&amp;"))
    ;; test span
    (let ((ts "[2000-01-01]"))
      ($l (g ts "[2000-01-01 test]" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2000-01-01 test]" "</span></span>"))
      ($l (g ts "test" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "test" "</span></span>"))
      ($l (g ts "<time>123</time>" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;time&gt;123&lt;/time&gt;"
              "</span></span>"))
      ($l (g ts "&" (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&amp;" "</span></span>")))
    ;; test time
    (let ((info '( :html-timestamp-wrapper time
                   :html-datetime-option T-none
                   :html-timezone 28800))
          (t1 "[1970-01-02]")
          (t2 "[1970-01-02 08:00]")
          (t3 "[1970-01-02 08:00-13:00]")
          (t4 "[1970-01-02 08:00]--[2000-01-02 09:00]")
          (b1 "[0000-00-00]")
          (b2 "[0000-00-00]--[0000-00-00]"))
      ($l (g t1 b1 info)
          "<time datetime=\"1970-01-02\">[0000-00-00]</time>")
      ($l (g t1 b2 info)
          ($c "<time datetime=\"1970-01-02\">[0000-00-00]</time>--"
              "<time>[0000-00-00]</time>"))
      ($l (g t2 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t2 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time>[0000-00-00]</time>"))
      ($l (g t3 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t3 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time>[0000-00-00]</time>"))
      ($l (g t4 b1 info)
          "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]</time>")
      ($l (g t4 b2 info)
          ($c "<time datetime=\"1970-01-02T08:00+0800\">[0000-00-00]"
              "</time>--<time datetime=\"2000-01-02T09:00+0800\">"
              "[0000-00-00]</time>")))))

(ert-deftest t--format-timestamp-raw ()
  "Tests for `org-w3ctr--format-timestamp-raw'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (g (x info) (t--format-timestamp-raw (f x) info))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone local)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 (p 'none)) "[2011-11-18]")
      ($l (g t1 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-18]" "</span></span>"))
      ($l (g t1 (p 'time))
          "<time datetime=\"2011-11-18\">[2011-11-18]</time>")
      ($l (g t2 (p 'none)) "&lt;2011-11-18 14:54&gt;")
      ($l (g t2 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;2011-11-18 14:54&gt;" "</span></span>"))
      ($l (g t2 (p 'time))
          ($c "<time datetime=\"2011-11-18T14:54\">"
              "&lt;2011-11-18 14:54&gt;</time>"))
      ($l (g t3 (p 'none)) "[2011-11-18 06:54-14:54]")
      ($l (g t3 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-18 06:54-14:54]" "</span></span>"))
      ($l (g t3 (p 'time))
          ($c "<time datetime=\"2011-11-18T06:54\">"
              "[2011-11-18 06:54-14:54]</time>"))
      ($l (g t4 (p 'none))
          "&lt;2011-11-18 06:54&gt;--[2011-11-18 14:54]")
      ($l (g t4 (p 'span))
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "&lt;2011-11-18 06:54&gt;--[2011-11-18 14:54]"
              "</span></span>"))
      ($l (g t4 (p 'time))
          ($c "<time datetime=\"2011-11-18T06:54\">&lt;"
              "2011-11-18 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54\">"
              "[2011-11-18 14:54]</time>"))
      ($l (g "[2000-01-01 <> 13:13]" (p 'time))
          ($c "<time datetime=\"2000-01-01\">"
              "[2000-01-01 &lt;&gt;</time>")))))

(ert-deftest t--format-timestamp-int ()
  "Tests for `org-w3ctr--format-timestamp-int'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x opt) (t--format-timestamp-int (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 'none) "[2011-11-18 Fri]")
      ($l (g t1 'time)
          "<time datetime=\"2011-11-18\">[2011-11-18 Fri]</time>")
      ($l (g t2 'none) "&lt;2011-11-18 Fri 14:54&gt;")
      ($l (g t2 'time)
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "&lt;2011-11-18 Fri 14:54&gt;</time>"))
      ($l (g t3 'none) "[2011-11-18 Fri 06:54-14:54]")
      ($l (g t3 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">"
              "[2011-11-18 Fri 06:54-14:54]</time>"))
      ($l (g t4 'none)
          "&lt;2011-11-18 Fri 06:54&gt;--&lt;2011-11-18 Fri 14:54&gt;")
      ($l (g t4 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">&lt;"
              "2011-11-18 Fri 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54Z\">&lt;"
              "2011-11-18 Fri 14:54&gt;</time>"))
      ($l (g "[2000-01-01]--[2000-02-02 13:00]" 'time)
          ($c "<time datetime=\"2000-01-01\">[2000-01-01 Sat]"
              "</time>--<time datetime=\"2000-02-02\">"
              "[2000-02-02 Wed 13:00]</time>"))
      ($l (g "[2000-01-01 11:00]--[2000-01-02]" 'time)
          ($c "<time datetime=\"2000-01-01T11:00Z\">"
              "[2000-01-01 Sat 11:00]</time>--"
              "<time datetime=\"2000-01-02T11:00Z\">"
              "[2000-01-02 Sun 11:00]</time>")))))

(ert-deftest t--format-timestamp-fmt ()
  "Tests for `org-w3ctr--format-timestamp-fmt'"
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (p (m) `( :html-timestamp-wrapper none
                       :html-datetime-option T-none-zulu
                       :html-timezone 0
                       :html-timestamp-formats ,m))
             (g (x opt) (t--format-timestamp-fmt (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 '("%y" . "%y %m")) "[11]")
      ($l (g t1 '("%C" . "")) "[20]")
      ($l (g t1 '("%F %m" ' "%F %R")) "[2011-11-18 11]")
      ($l (g t1 '("[%F]")) "[2011-11-18]")
      ($l (g t1 '("<%F>")) "[2011-11-18]")
      ($l (g t2 '(nil . "%F %R")) "&lt;2011-11-18 14:54&gt;")
      ($l (g t2 '(nil . "%j")) "&lt;322&gt;")
      ($l (g t3 '(nil . "%F %R")) "[2011-11-18 06:54-14:54]")
      ($l (g t3 '(nil . "%D %U %W %V")) "[11/18/11 46 46 46-14:54]")
      ($l (g t4 '(nil . "%F %R"))
          "&lt;2011-11-18 06:54&gt;--&lt;2011-11-18 14:54&gt;")
      ($l (g t4 '(nil . "%M")) "&lt;54&gt;--&lt;54&gt;"))
    ($e!l (t--format-timestamp-fmt (f "[2000-01-01]") nil)
          '(error ":html-timestamp-formats not valid: nil"))))

(ert-deftest t--format-timestamp-fix ()
  "Tests for `org-w3ctr--format-timestamp-fix'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x y opt) (t--format-timestamp-fix (f x) y (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 "%F%R" 'none) "2011-11-1800:00")
      ($l (g t1 "<%F%R>" 'none) "&lt;2011-11-1800:00&gt;")
      ($l (g t2 "%F" 'span)
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "2011-11-18</span></span>"))
      ($l (g t2 "%F %a" 'time)
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "2011-11-18 Fri</time>"))
      ($e!l (g t2 "%F" 'wtf)
            '(error "Unknown timestamp wrap: wtf"))
      ($l (g t3 "{%F%a%R}" 'none)
          "{2011-11-18Fri06:54}--{2011-11-18Fri14:54}")
      ($e!l (g t3 "%a" 'abc)
            '(error "Unknown timestamp wrap: abc"))
      ($l (g t3 "[%F%R]" 'span)
          ($c "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              "[2011-11-1806:54]--[2011-11-1814:54]</span></span>"))
      ($l (g t3 "<%F %R>" 'time)
          ($c "<time datetime=\"2011-11-18T06:54Z\">"
              "&lt;2011-11-18 06:54&gt;</time>--"
              "<time datetime=\"2011-11-18T14:54Z\">"
              "&lt;2011-11-18 14:54&gt;</time>"))
      ($l (g t3 "%F %R" 'none) (g t4 "%F %R" 'none))
      ($l (g t3 "<%F %R" 'span) (g t4 "<%F %R" 'span))
      ($l (g t3 "[%F%a%R]" 'time) (g t4 "[%F%a%R]" 'time)))))

(ert-deftest t--format-timestamp-org ()
  "Tests for `org-w3ctr--format-timestamp-org'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (p (w) `( :html-timestamp-wrapper ,w
                       :html-datetime-option T-none-zulu
                       :html-timezone 0))
             (g (x opt) (t--format-timestamp-org (f x) (p opt))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      (dlet ((org-display-custom-times nil))
        ($l (g t1 'none) "[2011-11-18 Fri]")
        ($l (g t2 'span)
            ($c "<span class=\"timestamp-wrapper\">"
                "<span class=\"timestamp\">"
                "&lt;2011-11-18 Fri 14:54&gt;</span></span>"))
        ($l (g t3 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "[2011-11-18 Fri 06:54-14:54]</time>"))
        ($l (g t4 'none)
            ($c "&lt;2011-11-18 Fri 06:54&gt;"
                "--&lt;2011-11-18 Fri 14:54&gt;"))
        ($l (g t4 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "&lt;2011-11-18 Fri 06:54&gt;</time>--"
                "<time datetime=\"2011-11-18T14:54Z\">"
                "&lt;2011-11-18 Fri 14:54&gt;</time>")))
      (dlet ((org-display-custom-times t)
             (org-timestamp-custom-formats
              '("%m/%d/%y" . "%m/%d/%y %H:%M")))
        ($l (g t1 'none) "&lt;11/18/11&gt;")
        ($l (g t2 'span)
            ($c "<span class=\"timestamp-wrapper\">"
                "<span class=\"timestamp\">&lt;11/18/11 14:54&gt;"
                "</span></span>"))
        ($l (g t3 'time)
            ($c "<time datetime=\"2011-11-18T06:54Z\">"
                "&lt;11/18/11 06:54&gt;</time>--"
                "<time datetime=\"2011-11-18T14:54Z\">"
                "&lt;11/18/11 14:54&gt;</time>"))
        ($l (g t3 'time) (g t4 'time))
        ($l (g t4 'none)
            "&lt;11/18/11 06:54&gt;--&lt;11/18/11 14:54&gt;")))))

(ert-deftest t--format-timestamp-cus ()
  "Tests for `org-w3ctr--format-timestamp-cus'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp)))
             (p (w f) `( :html-timestamp-wrapper ,w
                         :html-datetime-option T-none-zulu
                         :html-timestamp-formats ,f
                         :html-timezone 0))
             (g (x info) (t--format-timestamp-cus (f x) info)))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($l (g t1 (p 'none '("[%F%a]"))) "[2011-11-18Fri]")
      ($l (g t1 (p 'none '("<%F%a>"))) "&lt;2011-11-18Fri&gt;")
      ($l (g t1 (p 'none '("{%F%a}"))) "2011-11-18Fri")
      ($e! (g t1 (p 'none '("%F%a"))))
      ($l (g t2 (p 'time '(nil . "{[%F %R]}")))
          ($c "<time datetime=\"2011-11-18T14:54Z\">"
              "[2011-11-18 14:54]</time>"))
      ($l (g t2 (p 'none '(nil . "[[[[%R]]]]"))) "[[[[14:54]]]]")
      ($l (g t3 (p 'none '(nil . "<<<%F%R>")))
          ($c "&lt;&lt;&lt;2011-11-1806:54&gt;"
              "--&lt;&lt;&lt;2011-11-1814:54&gt;"))
      ($l (g t3 (p 'none '(nil . "<%F%R>")))
          (g t4 (p 'none '(nil . "<%F%R>")))))))

(ert-deftest t-ts-default-format-function ()
  "Tests for `org-w3ctr-ts-default-format-function'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]"))
      ($it t-ts-default-format-function
        ($l (it (f t1) nil) t1)
        ($l (it (f t2) nil) t2)
        ($l (it (f t3) nil) t3)
        ($l (it (f t4) nil) t4)))))

(ert-deftest t--format-timestamp-fun ()
  "Tests for `org-w3ctr--format-timestamp-fun'."
  (cl-flet* ((f (s) (car (t-get-parsed-elements s 'timestamp))))
    (let ((t1 "[2011-11-18]")
          (t2 "<2011-11-18 14:54>")
          (t3 "[2011-11-18 06:54-14:54]")
          (t4 "<2011-11-18 06:54>--[2011-11-18 14:54]")
          (info '(:html-timestamp-format-function (lambda (_a _b) "hello"))))
      ($it t--format-timestamp-fun
        ($l (it (f t1) info) "hello")
        ($l (it (f t2) info) "hello")
        ($l (it (f t3) info) "hello")
        ($l (it (f t4) info) "hello")
        ($e! (it (f t1) nil))))))

(ert-deftest t-timestamp ()
  (ert-skip "skip now")
  (t-check-element-values
   #'t-timestamp
   '(("[2020-02-02]" "<time datetime=\"2020-02-02\">[2020-02-02]</time>")
     ("<2006-01-02>" "<time datetime=\"2006-01-02\"><2006-01-02></time>")
     ("[2006-01-02 15:04:05]"
      "<time datetime=\"2006-01-02 15:04+0800\">[2006-01-02 15:04]</time>")
     ("<2006-01-02 15:04:05>"
      "<time datetime=\"2006-01-02 15:04+0800\"><2006-01-02 15:04></time>")
     ("[2025-03-30]--[2025-03-31]"
      "<time datetime=\"2025-03-30\">[2025-03-30]</time>&#x2013;\
<time datetime=\"2025-03-31\">[2025-03-31]</time>")
     ("<2025-03-30>--<2025-03-31>"
      "<time datetime=\"2025-03-30\"><2025-03-30></time>&#x2013;\
<time datetime=\"2025-03-31\"><2025-03-31></time>")
     ("[2006-01-02 15:04:05]--[2006-01-03 04:05:06]"
      "<time datetime=\"2006-01-02 15:04+0800\">[2006-01-02 15:04]</time>\
&#x2013;<time datetime=\"2006-01-03 04:05+0800\">[2006-01-03 04:05]</time>")
     ("<2006-01-02 15:04:05>--<2006-01-03 04:05:06>"
      "<time datetime=\"2006-01-02 15:04+0800\"><2006-01-02 15:04></time>\
&#x2013;<time datetime=\"2006-01-03 04:05+0800\"><2006-01-03 04:05></time>")
     ("[2024-02-02]--<2025-02-02>"
      "<time datetime=\"2024-02-02\">[2024-02-02]</time>&#x2013;\
<time datetime=\"2025-02-02\">[2025-02-02]</time>")
     ("<2024-02-02>--[2025-02-02]"
      "<time datetime=\"2024-02-02\"><2024-02-02></time>&#x2013;\
<time datetime=\"2025-02-02\"><2025-02-02></time>")
     ("[2000-01-01]" "<time datetime=\"2000-01-01\">[2000-01-01]</time>"))))

(ert-deftest t--get-charset ()
  "Tests for `org-w3ctr--get-charset'."
  (cl-labels ((test (x) (let ((org-w3ctr-coding-system x))
                          (t--get-charset))))
    ($l (test nil) "utf-8")
    ($l (test 'utf-8-unix) "utf-8")
    ($l (test 'utf-8-dos) "utf-8")
    ($l (test 'utf-8-mac) "utf-8")
    ($l (test 'gbk) "gbk")
    ($l (test 'chinese-gbk) "gbk")
    ($l (test 'big5) "big5")
    ($l (test 'utf-7) "utf-7")
    ($l (test 'gb18030) "gb18030")
    ($l (test 'iso-latin-2) "iso-8859-2")
    ($l (test 'japanese-shift-jis) "shift_jis")
    ($l (test 'japanese-iso-8bit) "euc-jp")
    ($l (test 'cp936) "gbk")
    ($l (test 'cp65001) "utf-8")))

(ert-deftest t--get-info-author-raw ()
  "Tests for `org-w3ctr--get-author-raw'."
  ($n (t--get-info-author-raw nil))
  (let ((info '(:with-author nil)))
    ($n (t--get-info-author-raw info)))
  (let ((info '(:with-author nil :author "test")))
    ($n (t--get-info-author-raw info)))
  (let ((info '(:with-author t :author "test")))
    ($l (t--get-info-author-raw info) "test")))

(ert-deftest t--get-info-title ()
  "Tests for `org-w3ctr--get-info-title'."
  (t-check-element-values
   #'t--get-info-title
   '(("#+title: he" "he")
     ("#+title:he" "he")
     ("#+title: \t" "&lrm;")
     ("#+title: \s\s\t" "&lrm;")
     ("#+title:   3   " "3")
     ;; zero width space
     ("#+title:​" "​")
     ("#+TITLE: hello\sworld" "hello world"))))

(ert-deftest t--build-meta-entry ()
  "Tests for `org-w3ctr--build-meta-entry'."
  ($l (t--build-meta-entry "name" "author")
      "<meta name=\"author\">\n")
  ($l (t--build-meta-entry "property" "og:title" "My Title")
      "<meta property=\"og:title\" content=\"My Title\">\n")
  ($l (t--build-meta-entry "name" "description" "Version %s" "1.0")
      "<meta name=\"description\" content=\"Version 1.0\">\n")
  ($l (t--build-meta-entry "name" "quote" "He said \"Hello\"")
      "<meta name=\"quote\" content=\"He said &quot;Hello&quot;\">\n")
  ($l (t--build-meta-entry "name" "version" "v%s.%s" "1" "2")
      "<meta name=\"version\" content=\"v1.2\">\n")
  ($l (t--build-meta-entry "name" "version" "'%s'" "v1.2")
      "<meta name=\"version\" content=\"&apos;v1.2&apos;\">\n"))

(ert-deftest t--get-info-file-timestamp ()
  "Tests for `org-w3ctr--get-info-file-timestamp'."
  (t-check-element-values
   #'t--get-info-file-timestamp
   `(("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t))
     ("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t))
     ("" ,(format-time-string "%Y-%m-%dT%H:%MZ" nil t)))
   nil
   '( :html-file-timestamp t-file-timestamp-default
      :time-stamp-file t)))

(ert-deftest t-meta-tags-default ()
  "Tests for `org-w3ctr-meta-tags-default'."
  (let ((info-with-author '(:with-author t :author ("Alice")))
        (info-with-desc '(:description "Test doc"))
        (info-with-keywords '(:keywords "org, test"))
        (info-empty '()))
    ($l (t-meta-tags-default info-with-author)
        '(("name" "author" "Alice")
          ("name" "generator" "Org Mode")))
    ($l (t-meta-tags-default info-with-desc)
        '(("name" "description" "Test doc")
          ("name" "generator" "Org Mode")))
    ($l (t-meta-tags-default info-with-keywords)
        '(("name" "keywords" "org, test")
          ("name" "generator" "Org Mode")))
    ($l (t-meta-tags-default info-empty)
        '(("name" "generator" "Org Mode")))))

(ert-deftest t--build-meta-tags ()
  "Tests for `org-w3ctr--build-meta-tags'."
  (let ((t-meta-tags '(("a" "b" "test"))))
    ($l (t--build-meta-tags nil) "<meta a=\"b\" content=\"test\">\n"))
  (let ((t-meta-tags '(("a" "b" nil))))
    ($l (t--build-meta-tags nil) "<meta a=\"b\">\n")))

(ert-deftest t--build-viewport-options ()
  "Tests for `org-w3ctr--build-viewport-options'."
  ($n (t--build-viewport-options nil))
  (cl-flet ((f (ls) (let ((info `(:html-viewport ,ls)))
                      (t--build-viewport-options info))))
    ($l (f '(("a" ""))) nil)
    ($l (f '(("a" "b")))
        "<meta name=\"viewport\" content=\"a=b\">\n")
    ($l (f '(("a" "b") ("b" "") ("c" "d")))
        "<meta name=\"viewport\" content=\"a=b, c=d\">\n")
    ($n (f '(("a" nil) ("b" nil) ("c" "  ")))))
  (t-check-element-values
   #'t--build-viewport-options
   '(("" "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"))
   nil '(:html-viewport ((width "device-width")
                         (initial-scale "1")
                         (minimum-scale "")
                         (maximum-scale "")
                         (user-scalable "")))))

(ert-deftest t--load-css ()
  "Tests for `org-w3ctr--load-css'."
  (let ((t-default-style nil))
    ($e! (t--load-css nil)))
  (let ((t-default-style "123"))
    ($l (t--load-css nil) "<style>\n123\n</style>\n"))
  (let ((t-default-style "")
        (t-default-style-file nil))
    ($l (t--load-css nil) ""))
  (cl-letf (((symbol-function 't--load-file)
             #'identity)
            (t-default-style ""))
    ($l (t--load-css nil)
        (format "<style>\n%s\n</style>\n" t-default-style-file))
    ($l t-default-style t-default-style-file)))

(ert-deftest t--build-math-config ()
  "Test `t--build-math-config' function."
  (let ((info '( :with-latex nil)))
    ($l (t--build-math-config info) ""))
  (let ((info '( :with-latex mathjax
                 :html-mathjax-config "mathjax")))
    ($l (t--build-math-config info) "mathjax\n"))
  (let ((info '( :with-latex mathml
                 :html-mathml-config "mathml")))
    ($l (t--build-math-config info) "mathml\n"))
  (let ((info '(:with-latex invalid)))
    ($e! (t--build-math-config info)))
  (let ((info '( :with-latex custom
                 :html-math-custom-config-function
                 (lambda (_i) "test"))))
    ($l (t--build-math-config info) "test\n"))
  (let ((info '( :with-latex custom
                 :html-math-custom-config-function
                 t-math-custom-config-function-default)))
    ($l (t--build-math-config info) "")))

(ert-deftest t--has-math-p ()
  "Tests for `org-w3ctr--has-math-p'."
  (cl-flet ((mkinfo (str) `( :with-latex t
                             :parse-tree
                             ,(with-temp-buffer
                                (save-excursion (insert str))
                                (org-element-parse-buffer)))))
    ($n (t--has-math-p (mkinfo "123")))
    ($n (t--has-math-p (mkinfo "$1+2")))
    ($s (t--has-math-p (mkinfo "$1+2$")))
    ($s (t--has-math-p (mkinfo "\\(1+2\\)")))
    ($s (t--has-math-p (mkinfo "\\[1+2\\]")))
    ($s (t--has-math-p (mkinfo "\\begin_equation\n123\n\\end_equation")))))

(ert-deftest t--use-default-style-p ()
  "Tests for `org-w3ctr--use-default-styple-p'."
  ($n (t--use-default-style-p nil))
  ($s (t--use-default-style-p '(:html-head-include-default-style t))))

(ert-deftest t-legacy-format-home/up ()
  "Tests for `org-w3ctr-legacy-format-home/up'."
  (let ((info '(:html-link-up "" :html-link-home "")))
    ($n (t-legacy-format-home/up info)))
  (let ((info `( :html-link-up "1" :html-link-home "2"
                 :html-home/up-format ,t-home/up-format)))
    ($l (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"1\"> UP </a>
 <a href=\"2\"> HOME </a>\n</div>")
    (setq info (plist-put info :html-link-home ""))
    ($l (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"1\"> UP </a>
 <a href=\"1\"> HOME </a>\n</div>")
    (setq info (plist-put info :html-link-up ""))
    (setf (plist-get info :html-link-home) "2")
    ($l (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"2\"> UP </a>
 <a href=\"2\"> HOME </a>\n</div>"))
  (t-check-element-values
   #'t-legacy-format-home/up
   '(("#+html_link_up: https://example.com"
      "<div id=\"home-and-up\">\n <a href=\"https://example.com\"> UP </a>\n <a href=\"https://example.com\"> HOME </a>\n</div>")
     ("#+html_link_home: https://a.com"
      "<div id=\"home-and-up\">\n <a href=\"https://a.com\"> UP </a>\n <a href=\"https://a.com\"> HOME </a>\n</div>")
     ("#+html_link_home: a\n#+html_link_up:b"
      "<div id=\"home-and-up\">\n <a href=\"b\"> UP </a>\n <a href=\"a\"> HOME </a>\n</div>"))
   nil `( :html-link-up "" :html-link-home ""
          :html-link-home/up nil
          :html-home/up-format ,t-home/up-format)))

(ert-deftest t--format-home/up-nav ()
  "Tests for `org-w3ctr--format-home/up-nav'."
  ($l (t--format-home/up-nav "") "<nav id=\"home-and-up\">\n\n</nav>\n")
  ($l (t--format-home/up-nav "1") "<nav id=\"home-and-up\">\n1\n</nav>\n"))

(ert-deftest t--format-home/up-vector ()
  "Tests for `org-w3ctr--format-home/up-vector'."
  ($l "" (t--format-home/up-vector []))
  ($l (t--format-home/up-vector [("a" . "b")]) "\
<nav id=\"home-and-up\">
<a href=\"a\">b</a>
</nav>\n")
  ($l (t--format-home/up-vector [("a" . "b") ("c" . "d")]) "\
<nav id=\"home-and-up\">
<a href=\"a\">b</a>
<a href=\"c\">d</a>
</nav>\n"))

(ert-deftest t--format-home/up-list ()
  "Tests for `org-w3ctr--format-home/up-list'."
  ($s (t--format-home/up-list nil nil))
  (cl-letf (((symbol-function 'org-export-data)
             (lambda (x _info) x))
            ((symbol-function 't--format-home/up-nav)
             (lambda (x) x)))
    ($l (t--format-home/up-list '("a") nil) "a")
    ($l (t--format-home/up-list '("a" "b" "c") nil) "a\nb\nc")
    ($l (t--format-home/up-list '("a" " " "\t" "\n" "e") nil) "a\ne")))

(ert-deftest t-format-home/up-default-function ()
  "Tests for `org-w3ctr-format-home/up-default-function'."
  (let ((info '(:html-link-home/up [("a" . "b")])))
    ($l (t-format-home/up-default-function info)
        "<nav id=\"home-and-up\">\n<a href=\"a\">b</a>\n</nav>\n"))
  (let ((info '(:html-link-home/up [("a" . "b") ("c" . "d")])))
    ($l (t-format-home/up-default-function info)
        "<nav id=\"home-and-up\">\n<a href=\"a\">b</a>\n<a href=\"c\">d</a>\n</nav>\n"))
  (let ((info '(:html-link-home/up [(a . "b")])))
    ($e! (t-format-home/up-default-function info)))
  (let ((info '(:html-link-home/up [("a" . b)])))
    ($e! (t-format-home/up-default-function info)))
  (let ((info '(:html-link-home/up [(a . b)])))
    ($e! (t-format-home/up-default-function info)))
  (t-check-element-values
   #'t-format-home/up-default-function
   '(("" "<nav id=\"home-and-up\">\n<a href=\"https://example.com\">example</a>\n</nav>\n"))
   nil '( :html-link-home/up [("https://example.com" . "example")]
          :html-format-home/up-function
          t-format-home/up-default-function))
  (t-check-element-values
   #'t-format-home/up-default-function
   '(("#+html_link_homeup: [[https://a.com][b]]"
      "<nav id=\"home-and-up\">\n<a href=\"https://a.com\">b</a>\n</nav>\n")
     ("#+html_link_homeup: [[https://a.com]]"
      "<nav id=\"home-and-up\">\n<a href=\"https://a.com\">https://a.com</a>\n</nav>\n")
     ("#+html_link_homeup: [[https://a.com]]\n#+html_link_homeup: [[https://b.com]]"
      "<nav id=\"home-and-up\">\n<a href=\"https://a.com\">https://a.com</a>\n<a href=\"https://b.com\">https://b.com</a>\n</nav>\n")
     ("#+html_link_homeup: [[https://a.com]] [[https://b.com]]"
      "<nav id=\"home-and-up\">\n<a href=\"https://a.com\">https://a.com</a>\n<a href=\"https://b.com\">https://b.com</a>\n</nav>\n")
     ("#+html_link_homeup: 1 2 3"
      "<nav id=\"home-and-up\">\n1 2 3\n</nav>\n")
     ("#+html_link_homeup: \n#+html_link_home: 123"
      "<div id=\"home-and-up\">\n <a href=\"123\"> UP </a>\n <a href=\"123\"> HOME </a>\n</div>")
     ("#+html_link_up: 456"
      "<div id=\"home-and-up\">\n <a href=\"456\"> UP </a>\n <a href=\"456\"> HOME </a>\n</div>")
     ("#+html_link_home: 123\n#+html_link_up: 456"
      "<div id=\"home-and-up\">\n <a href=\"456\"> UP </a>\n <a href=\"123\"> HOME </a>\n</div>"))
   nil `(:html-format-home/up-function
         t-format-home/up-default-function
         :html-link-up "" :html-link-home ""
         :html-home/up-format ,t-home/up-format)))

(ert-deftest t--load-cc-svg ()
  "Tests for `org-w3ctr--load-cc-svg'."
  (cl-letf (((symbol-function 't--insert-file)
             (lambda (file) file)))
    (dolist (a '("by" "cc" "nc" "nd" "sa" "zero"))
      ($s (t--load-cc-svg a)))))

(ert-deftest t--load-cc-svg-once ()
  "Tests for `org-w3ctr--load-cc-svg-once'."
  (cl-letf (((symbol-function 't--insert-file)
             (lambda (file) file))
            (t--cc-svg-hashtable (make-hash-table :test 'equal)))
    (dolist (a '("by" "cc" "nc" "nd" "sa" "zero"))
      (t--load-cc-svg-once a))
    (dolist (a '("by" "cc" "nc" "nd" "sa" "zero"))
      ($l (gethash a t--cc-svg-hashtable) (t--load-cc-svg a)))))

(ert-deftest t-format-public-license-default-function ()
  "Tests for `org-w3ctr-format-public-license-default-function'."
  (cl-letf (((symbol-function 't--get-info-author)
             (lambda (info) (plist-get info :author))))
    (cl-flet ((test (info)
                (t-format-public-license-default-function info)))
      (let ((info (list :html-license nil)))
        ($l (test info) "Not Specified")
        (setq info (plist-put info :html-license 'all-rights-reserved))
        ($l (test info) "All Rights Reserved")
        (setq info (plist-put info :html-license 'all-rights-reversed))
        ($l (test info) "All Rights Reversed")
        (setq info (plist-put info :html-license 'cc-by-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/4.0/\">CC BY-NC 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-nd-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/4.0/\">CC BY-NC-ND 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-sa-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">CC BY-NC-SA 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nd-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/4.0/\">CC BY-ND 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-sa-4.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>")
        (setq info (plist-put info :html-license 'cc-by-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/3.0/\">CC BY 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/3.0/\">CC BY-NC 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-nd-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/3.0/\">CC BY-NC-ND 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nc-sa-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/3.0/\">CC BY-NC-SA 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-nd-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/3.0/\">CC BY-ND 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-sa-3.0))
        ($l (test info)
            "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/3.0/\">CC BY-SA 3.0</a>")
        (setq info (plist-put info :html-license 'cc-by-4.0))
        (setq info (plist-put info :author "test"))
        ($l (test info)
            "This work by test is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>")
        (setq info (plist-put info :html-use-cc-budget t))
        ($l (test info)
            (concat "This work by test is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>"
                    " " (t--get-cc-svgs 'cc-by-4.0)))))))

;; Add pre/postamble tests here.



(defun t-parse-mathml-string (strs)
  (with-work-buffer
    (dolist (a strs)
      (insert a "\n"))
    (goto-char (point-min))
    (xml-parse-tag)))

(defun t-check-mathml (pairs)
  (dolist (p pairs)
    (let ((xml (t-parse-mathml-string (cdr p)))
          (result (car p)))
      ($l result (t--mathml-to-oneline xml)))))

(ert-deftest t--mathml-to-oneline ()
  ;; https://www.w3.org/TR/2025/WD-mathml4-20250326/
  (t-check-mathml
   '(("<math><mrow>...</mrow></math>"
      "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
      "<mrow>...</mrow>"
      "</math>")
     ("<body>...<m:math><m:mrow>...</m:mrow></m:math>...</body>"
      "<body xmlns:m=\"http://www.w3.org/1998/Math/MathML\">"
      "  ..."
      "  <m:math><m:mrow>...</m:mrow></m:math>"
      "  ..."
      "</body>")
     ("<mtext>Theorem\n1:</mtext>"
      "<mtext>"
      "Theorem"
      "1:"
      "</mtext>")
     ("<msup><mrow><mo>(</mo><mrow><mi>f</mi><mo>+</mo><mi>g</mi></mrow><mo>)</mo></mrow><mo>′</mo></msup>"
      "<msup>"
      "<mrow><mo>(</mo><mrow><mi>f</mi><mo>+</mo><mi>g</mi></mrow><mo>)</mo></mrow>"
      "<mo>&#x2032;<!--PRIME--></mo>"
      "</msup>")
     )))


;; Local Variables:
;; read-symbol-shorthands: (("t-" . "org-w3ctr-") ("$" . "org-w3ctr:test-"))
;; coding: utf-8-unix
;; End:
