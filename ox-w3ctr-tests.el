;;; -*- lexical-binding:t; no-byte-compile:t; -*-

(load "ox-w3ctr")

;; Test helper functions
(defun t--oinfo-oget (prop)
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

(ert-deftest t--make-cache-oclosure ()
  "Tests for `org-w3ctr--make-cache-oclosure'."
  (let ((x (t--make-cache-oclosure :wtf)))
    (should (equal (t--oinfo--cnt x) 0))
    (should (equal (t--oinfo--pid x) nil))
    (should (equal (t--oinfo--val x) nil)))
  (let ((info '(:a 1 :b 2 :c 3))
        (info2 '(:a 3 :b 2 :c 1))
        (oa (t--make-cache-oclosure :a))
        (ob (t--make-cache-oclosure :b))
        (oc (t--make-cache-oclosure :c)))
    ;; a
    (should (equal (funcall oa info) 1))
    (should (eq (t--oinfo--pid oa) info))
    (should (equal (t--oinfo--val oa) 1))
    (should (equal (t--oinfo--cnt oa) 1))
    ;; b
    (should (equal (funcall ob info) 2))
    (should (eq (t--oinfo--pid ob) info))
    (should (equal (t--oinfo--val ob) 2))
    (should (equal (t--oinfo--cnt ob) 1))
    ;; c
    (should (equal (funcall oc info) 3))
    (should (eq (t--oinfo--pid oc) info))
    (should (equal (t--oinfo--val oc) 3))
    (should (equal (t--oinfo--cnt oc) 1))
    ;; test cnt
    (funcall oa info)
    (should (equal (t--oinfo--cnt oa) 2))
    (funcall ob info) (funcall ob info)
    (should (equal (t--oinfo--cnt ob) 3))
    (funcall oc info) (funcall oc info) (funcall oc info)
    (should (equal (t--oinfo--cnt oc) 4))
    ;; change plist
    (should (equal (funcall oa info2) 3))
    (should (eq (t--oinfo--pid oa) info2))
    (should (equal (funcall ob info2) 2))
    (should (eq (t--oinfo--pid ob) info2))
    (should (equal (funcall oc info2) 1))
    (should (eq (t--oinfo--pid oc) info2))))

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
    (should (equal (eval '(t--pget info :a)) 1))
    (should (equal (t--oinfo--cnt (t--oinfo-oget :a)) 1))
    (should (equal (eval '(t--pget info :b)) 2))
    (should (equal (t--oinfo--cnt (t--oinfo-oget :b)) 1))
    (should (equal (eval '(t--pget info :c)) 3))
    (should-not (alist-get :c t--oinfo-cache-alist))))

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
    (should (equal (eval '(t--pput info :a 2)) 2))
    (should (equal (eval '(t--pget info :a)) 2))
    (should (equal (t--oinfo--val (t--oinfo-oget :a)) 2))
    (should (equal (plist-get info :a) 1))
    ;; b
    (should (equal (eval '(t--pput info :b 3)) 3))
    (should (equal (eval '(t--pget info :b)) 3))
    (should (equal (t--oinfo--val (t--oinfo-oget :b)) 3))
    (should (equal (plist-get info :b) 2))
    ;; c
    (should (equal (eval '(t--pput info :c 4)) 4))
    (should (equal (eval '(t--pget info :c)) 4))
    (should (equal (plist-get info :c) 4))
    ;; test side effect c
    (should (equal (eval '(t--pput info :c (incf val))) 2))
    (should (equal (eval '(t--pget info :c)) 2))
    (should (equal (plist-get info :c) 2))
    ;; test side effect b
    (should (equal (eval '(t--pput info :b (incf val))) 3))
    (should (equal (eval '(t--pget info :b)) 3))
    (should (equal (plist-get info :b) 2))))

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
    (should (equal (eval '(t--pget info :a)) 1))
    (should (equal (eval '(t--pget info :b)) 2))
    (should (equal (eval '(t--pget info :c)) 3))
    (should (eq (t--oinfo--pid (t--oinfo-oget :a)) info))
    (should (eq (t--oinfo--pid (t--oinfo-oget :b)) info))
    (should (= (t--oinfo--val (t--oinfo-oget :a)) 1))
    (should (= (t--oinfo--val (t--oinfo-oget :b)) 2))
    (should (= (t--oinfo--cnt (t--oinfo-oget :a)) 1))
    (should (= (t--oinfo--cnt (t--oinfo-oget :b)) 1))
    (t--oinfo-cleanup)
    (should (eq (t--oinfo--pid (t--oinfo-oget :a)) nil))
    (should (eq (t--oinfo--pid (t--oinfo-oget :a)) nil))
    (should (equal (t--oinfo--val (t--oinfo-oget :a)) nil))
    (should (equal (t--oinfo--val (t--oinfo-oget :a)) nil))
    (should (equal (t--oinfo--cnt (t--oinfo-oget :a)) 1))
    (should (equal (t--oinfo--cnt (t--oinfo-oget :a)) 1))))

(ert-deftest t--maybe-contents ()
  "Tests for `org-w3ctr--maybe-contents'."
  (should (equal (t--maybe-contents nil) ""))
  (should (equal (t--maybe-contents "") "\n"))
  (should (equal (t--maybe-contents "abc") "\nabc"))
  (should (equal (t--maybe-contents 123) ""))
  (should (equal (t--maybe-contents '(1 2)) "")))

(ert-deftest t--nw-p ()
  "Tests for `org-w3ctr--nw-p'."
  (should (equal (t--nw-p "123") "123"))
  (should (equal (t--nw-p " 1") " 1"))
  (should (equal (t--nw-p "\t\r\n2") "\t\r\n2"))
  (should-not (t--nw-p ""))
  (should-not (t--nw-p "\t\s\r\n")))

(ert-deftest t--2str ()
  "Tests for `org-w3ctr--2str'."
  (should (eq (t--2str nil) nil))
  (should (string= (t--2str 1) "1"))
  (should (string= (t--2str 114.514) "114.514"))
  (should (string= (t--2str ?a) "97"))
  (should (string= (t--2str 'hello) "hello"))
  (should (string= (t--2str 'has\ space) "has space"))
  (should (string= (t--2str 'has\#) "has#"))
  (should (string= (t--2str "string") "string"))
  (should-not (t--2str [1]))
  (should-not (t--2str (make-char-table 'sub)))
  (should-not (t--2str (make-bool-vector 3 t)))
  (should-not (t--2str (make-hash-table)))
  (should-not (t--2str (lambda (x) x))))

(ert-deftest t--read-attr ()
  "Tests for `org-w3ctr--read-attr'."
  ;; `org-element-property' use `org-element--property'
  ;; and defined using `define-inline'.
  (cl-letf (((symbol-function 'org-element--property)
             (lambda (_p n _deft _force) n)))
    (should (equal (org-element-property :attr__ 123) 123))
    (should (equal (org-element-property nil 1) 1))
    (should (equal (t--read-attr nil '("123")) '(123)))
    (should (equal (t--read-attr nil '("1 2 3" "4 5 6"))
                   '(1 2 3 4 5 6)))
    (should (equal (t--read-attr nil '("(class data) [hello] (id ui)"))
                   '((class data) [hello] (id ui))))
    (should (equal (t--read-attr nil '("\"123\"")) '("123"))))
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
    (should (equal (t--read-attr__ '("1 2 3")) '(1 2 3)))
    (should (equal (t--read-attr__ '("(class data) open"))
                   '((class data) open)))
    (should (equal (t--read-attr__ '("(class hello world)" "foo"))
                   '((class hello world) foo)))
    (should (equal (t--read-attr__ '("[nim zig]"))
                   '(("class" "nim zig"))))
    (should (equal (t--read-attr__ '("[]")) '(nil)))
    (should (equal (t--read-attr__ '("[][][]")) '(()()()))))
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
  (should (equal (t--encode-plain-text "") ""))
  (should (equal (t--encode-plain-text "123") "123"))
  (should (equal (t--encode-plain-text "hello world") "hello world"))
  (should (equal (t--encode-plain-text "&") "&amp;"))
  (should (equal (t--encode-plain-text "<") "&lt;"))
  (should (equal (t--encode-plain-text ">") "&gt;"))
  (should (equal (t--encode-plain-text "<&>") "&lt;&amp;&gt;"))
  (dolist (a '(("a&b&c" . "a&amp;b&amp;c")
               ("<div>" . "&lt;div&gt;")
               ("<span>" . "&lt;span&gt;")))
    (should (string= (t--encode-plain-text (car a)) (cdr a)))))

(ert-deftest t--encode-plain-text* ()
  "Tests for `org-w3ctr--encode-plain-text*'."
  (should (equal (t--encode-plain-text* "&") "&amp;"))
  (should (equal (t--encode-plain-text* "<") "&lt;"))
  (should (equal (t--encode-plain-text* ">") "&gt;"))
  (should (equal (t--encode-plain-text* "<&>") "&lt;&amp;&gt;"))
  (should (equal (t--encode-plain-text* "'") "&apos;"))
  (should (equal (t--encode-plain-text* "\"") "&quot;"))
  (should (equal (t--encode-plain-text* "\"'&\"")
                 "&quot;&apos;&amp;&quot;")))

(ert-deftest t--make-attr ()
  "Tests for `org-w3ctr--make-attr'."
  (should-not (t--make-attr nil))
  (should-not (t--make-attr '(nil 1)))
  (should-not (t--make-attr '([x])))
  (should (string= (t--make-attr '(open)) " open"))
  (should (string= (t--make-attr '("disabled")) " disabled"))
  (should (string= (t--make-attr '(FOO)) " foo"))
  (should (string= (t--make-attr '(a b)) " a=\"b\""))
  (should (string= (t--make-attr '(class "example two"))
                   " class=\"example two\""))
  (should (string= (t--make-attr '(foo [bar] baz))
                   " foo=\"baz\""))
  (should (string= (t--make-attr '(data-A "base64..."))
                   " data-a=\"base64...\""))
  (should (string= (t--make-attr '(data-tt "a < b && c"))
                   " data-tt=\"a &lt; b &amp;&amp; c\""))
  (should (string= (t--make-attr '(data-he "\"hello world\""))
                   " data-he=\"&quot;hello world&quot;\""))
  (should (string= (t--make-attr '(sig "''")) " sig=\"&apos;&apos;\""))
  (should (string= (t--make-attr '(test ">'\""))
                   " test=\"&gt;&apos;&quot;\"")))

(ert-deftest t--make-attr__ ()
  "Tests for `org-w3ctr--make-attr__'."
  (should (equal (t--make-attr__ nil) ""))
  (should (equal (t--make-attr__ '(nil)) ""))
  (should (equal (t--make-attr__ '(nil nil [])) ""))
  (should (equal (t--make-attr__ '(a)) " a"))
  (should (equal (t--make-attr__ '((id yy 123) (class a\ b) test))
                 " id=\"yy123\" class=\"a b\" test"))
  (should (equal (t--make-attr__ '((test this th&t <=>)))
                 " test=\"thisth&amp;t&lt;=&gt;\"")))

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
  (should (equal (t--make-attribute-string '(:a "1" :b "2"))
                 "a=\"1\" b=\"2\""))
  (should (equal (t--make-attribute-string nil) ""))
  (should (equal (t--make-attribute-string '(:a nil)) ""))
  (should (equal (t--make-attribute-string '(:a "\"a\""))
                 "a=\"&quot;a&quot;\""))
  (should (equal (t--make-attribute-string '(:open "open"))
                 "open=\"open\""))
  (should (equal (t--make-attribute-string '(:test "'\"'"))
                 "test=\"&apos;&quot;&apos;\""))
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
  (should (equal (t--trim "123") "123"))
  (should (equal (t--trim " 123") "123"))
  (should (equal (t--trim " 123 ") "123"))
  (should (equal (t--trim "  123  ") "123"))
  (should (equal (t--trim "  123\n 456\n") "123\n 456"))
  (should (equal (t--trim "\n 123" t) " 123"))
  (should (equal (t--trim "\n\n  123\n" t) "  123")))

(ert-deftest t--nw-trim ()
  "Tests for `org-w3ctr--nw-trim'."
  (should (equal (t--nw-trim " ") nil))
  (should (equal (t--nw-trim " 1 ") "1"))
  (should (equal (t--nw-trim "234\n") "234"))
  (should (equal (t--nw-trim 1) nil))
  (should (equal (t--nw-trim 'hello) nil)))

(ert-deftest t--sexp2html ()
  "Tests for `org-w3ctr--sexp2html'."
  (should (string= (t--sexp2html nil) ""))
  ;; Basic tag with no attributes
  (should (string= (t--sexp2html '(p () "123")) "<p>123</p>"))
  (should (string= (t--sexp2html '(p t "123")) "<p>123</p>"))
  ;; Tag with attributes
  (should (string= (t--sexp2html
                    '(a ((href "https://example.com")) "link"))
                   "<a href=\"https://example.com\">link</a>"))
  (should (string= (t--sexp2html
                    '(img ((src "../" "1.jpg")
                           (alt "../1.jpg"))))
                   "<img src=\"../1.jpg\" alt=\"../1.jpg\">"))
  ;; Nested tags
  (should (string= (t--sexp2html
                    '(div () (p () "Hello") (p () "World")))
                   "<div><p>Hello</p><p>World</p></div>"))
  ;; Symbol as tag name
  (should (string= (t--sexp2html '(my-tag () "content"))
                   "<my-tag>content</my-tag>"))
  ;; Empty tag
  (should (string= (t--sexp2html '(area ())) "<area>"))
  (should (string= (t--sexp2html '(base ())) "<base>"))
  (should (string= (t--sexp2html '(br ())) "<br>"))
  (should (string= (t--sexp2html '(col ())) "<col>"))
  (should (string= (t--sexp2html '(embed ())) "<embed>"))
  (should (string= (t--sexp2html '(hr ())) "<hr>"))
  (should (string= (t--sexp2html '(img ())) "<img>"))
  (should (string= (t--sexp2html '(input ())) "<input>"))
  (should (string= (t--sexp2html '(link ())) "<link>"))
  (should (string= (t--sexp2html '(meta ())) "<meta>"))
  (should (string= (t--sexp2html '(param ())) "<param>"))
  (should (string= (t--sexp2html '(source ())) "<source>"))
  (should (string= (t--sexp2html '(track ())) "<track>"))
  (should (string= (t--sexp2html '(wbr ())) "<wbr>"))
  ;; Number as content
  (should (string= (t--sexp2html '(span () 42)) "<span>42</span>"))
  ;; Mixed content (text and elements)
  (should (string= (t--sexp2html
                    '(div () "Text " (span () "inside") " more text"))
                   "<div>Text <span>inside</span> more text</div>"))
  ;; Ignore unsupported types (e.g., vectors)
  (should (string= (t--sexp2html '(div () [1 2 3])) "<div></div>"))
  ;; Always downcase
  (should (string= (t--sexp2html '(DIV () "123"))
                   "<div>123</div>"))
  ;; Allow bare tags
  (should (string= (t--sexp2html '(p)) "<p></p>"))
  (should (string= (t--sexp2html '(hr)) "<hr>"))
  ;; Escape
  (should (string= (t--sexp2html '(p () "123<456>"))
                   "<p>123&lt;456&gt;</p>"))
  (should (string= (t--sexp2html '(p () (b () "a&b")))
                   "<p><b>a&amp;b</b></p>")))

(ert-deftest t--make-string ()
  "Tests for `org-w3ctr--make-string'."
  (should (equal (t--make-string 1 "a") "a"))
  (should (equal (t--make-string 2 "a") "aa"))
  (should (equal (t--make-string 3 "a") "aaa"))
  (should (equal (t--make-string 2 "ab") "abab"))
  (should (equal (t--make-string 0 "a") ""))
  (should (equal (t--make-string -1 "a") ""))
  (should (equal (t--make-string 100 "") ""))
  (should-error (t--make-string 3 [?a ?b]))
  (should-error (t--make-string "a" "a")))

(ert-deftest t--normalize-string ()
  "Tests for `org-w3ctr--normalize-string'."
  (should-not (t--normalize-string nil))
  (should (equal "" (t--normalize-string "")))
  (should (equal "a\n" (t--normalize-string "a")))
  (should (equal "a  \n" (t--normalize-string "a  \n\n\n"))))

(ert-deftest t--load-file ()
  "Tests for `org-w3ctr--load-file'."
  (let ((ox (with-temp-buffer
              (insert-file-contents "ox-w3ctr.el")
              (buffer-substring-no-properties
               (point-min) (point-max)))))
    (should (equal ox (t--load-file "ox-w3ctr.el"))))
  (should-error (t--load-file "not-exist")))

(ert-deftest t--insert-file ()
  "Tests for `org-w3ctr--insert-file'."
  (should-error (t--insert-file default-directory))
  (should-error (t--insert-file "no-exist")))

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
    (should (equal (t--checkbox 'on info) "&#x2611;"))
    (should (equal (t--checkbox 'off info) "&#x2610;"))
    (should (equal (t--checkbox 'trans info) "&#x2612;"))
    (should (equal (t--checkbox nil info) nil))
    (should (equal (t--checkbox [1] info) nil))
    (should (equal (t--checkbox "hello" info) nil))
    (should (equal (t--checkbox "on" info) nil))
    (should (equal (t--checkbox "off" info) nil))
    (should (equal (t--checkbox "trans" info) nil)))
  (let ((info '(:html-checkbox-type ascii)))
    (should (equal (t--checkbox 'off info) "<code>[&#xa0;]</code>"))
    (should (equal (t--checkbox 'on info) "<code>[X]</code>"))
    (should (equal (t--checkbox 'trans info) "<code>[-]</code>")))
  (let ((info '(:html-checkbox-type html)))
    (should (equal (t--checkbox 'off info)
                   "<input type=\"checkbox\">"))
    (should (equal (t--checkbox 'on info)
                   "<input type=\"checkbox\" checked>"))
    (should (equal (t--checkbox 'trans info)
                   "<input type=\"checkbox\">"))))

(ert-deftest t--format-checkbox ()
  "Tests for `org-w3ctr--format-checkbox.'"
  (let ((info '(:html-checkbox-type unicode)))
    (should (equal (t--format-checkbox 'off info) "&#x2610; "))
    (should (equal (t--format-checkbox 'on info) "&#x2611; "))
    (should (equal (t--format-checkbox 'trans info) "&#x2612; "))
    (should (equal (t--format-checkbox nil info) ""))
    (should (equal (t--format-checkbox 'test info) ""))
    (should (equal (t--format-checkbox [1] info) ""))
    (should (equal (t--format-checkbox '(1 . 2) info) ""))
    (should (equal (t--format-checkbox #s(hello wtf) info) ""))))

(ert-deftest t--format-ordered-item ()
  "Tests for `org-w3ctr--format-ordered-item'."
  (should (equal (t--format-ordered-item "" nil nil nil)
                 "<li></li>"))
  (should (equal (t--format-ordered-item "\n  \n" nil nil nil)
                 "<li></li>"))
  (should (equal (t--format-ordered-item "\t\r\n " nil nil nil)
                 "<li></li>"))
  (should (equal (t--format-ordered-item "123" nil nil nil)
                 "<li>123</li>"))
  (should (equal (t--format-ordered-item " 123 " nil nil nil)
                 "<li>123</li>"))
  (should (equal (t--format-ordered-item "123" nil nil 10)
                 "<li value=\"10\">123</li>"))
  (should (equal (t--format-ordered-item "123" nil nil 114514)
                 "<li value=\"114514\">123</li>"))
  (should (equal (t--format-ordered-item "123" nil nil 191981)
                 "<li value=\"191981\">123</li>"))
  (let ((info '(:html-checkbox-type unicode)))
    (should (equal (t--format-ordered-item "123" 'off info nil)
                   "<li>&#x2610; 123</li>"))
    (should (equal (t--format-ordered-item "123" 'on info nil)
                   "<li>&#x2611; 123</li>"))
    (should (equal (t--format-ordered-item "123" 'trans info nil)
                   "<li>&#x2612; 123</li>"))
    (should (equal (t--format-ordered-item "123" 'on info 114)
                   "<li value=\"114\">&#x2611; 123</li>"))))

(ert-deftest t--format-unordered-item ()
  "Tests for `org-w3ctr--format-unordered-item'."
  (should (equal (t--format-unordered-item "" nil nil)
                 "<li></li>"))
  (should (equal (t--format-unordered-item "\n  \n" nil nil)
                 "<li></li>"))
  (should (equal (t--format-unordered-item "\t\r\n " nil nil)
                 "<li></li>"))
  (should (equal (t--format-unordered-item "123" nil nil)
                 "<li>123</li>"))
  (should (equal (t--format-unordered-item " 123 " nil nil)
                 "<li>123</li>"))
  (let ((info '(:html-checkbox-type unicode)))
    (should (equal (t--format-unordered-item "123" 'off info)
                   "<li>&#x2610; 123</li>"))
    (should (equal (t--format-unordered-item "123" 'on info)
                   "<li>&#x2611; 123</li>"))
    (should (equal (t--format-unordered-item "123" 'trans info)
                   "<li>&#x2612; 123</li>"))))

(ert-deftest t--format-descriptive-item ()
  "Tests for `org-w3ctr--format-descriptive-item'."
  (should (equal (t--format-descriptive-item "" nil nil nil)
                 "<dt>(no term)</dt><dd></dd>"))
  (should (equal (t--format-descriptive-item " " nil nil nil)
                 "<dt>(no term)</dt><dd></dd>"))
  (should (equal (t--format-descriptive-item "\r\n\t " nil nil nil)
                 "<dt>(no term)</dt><dd></dd>"))
  (should (equal (t--format-descriptive-item "123" nil nil nil)
                 "<dt>(no term)</dt><dd>123</dd>"))
  (should (equal (t--format-descriptive-item " 123 " nil nil nil)
                 "<dt>(no term)</dt><dd>123</dd>"))
  (should (equal (t--format-descriptive-item " 123 " nil nil "ONE")
                 "<dt>ONE</dt><dd>123</dd>"))
  (should (equal (t--format-descriptive-item " 123 " nil nil "TWO")
                 "<dt>TWO</dt><dd>123</dd>"))
  (should (equal (t--format-descriptive-item " 123 " nil nil "THREE ")
                 "<dt>THREE </dt><dd>123</dd>"))
  (let ((info '(:html-checkbox-type unicode)))
    (should (equal (t--format-descriptive-item "123" 'off info nil)
                   "<dt>&#x2610; (no term)</dt><dd>123</dd>"))
    (should (equal (t--format-descriptive-item "123" 'on info nil)
                   "<dt>&#x2611; (no term)</dt><dd>123</dd>"))
    (should (equal (t--format-descriptive-item "123" 'trans info nil)
                   "<dt>&#x2612; (no term)</dt><dd>123</dd>"))
    (should (equal (t--format-descriptive-item "123" 'trans info " test ")
                   "<dt>&#x2612;  test </dt><dd>123</dd>"))))

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
  (should-error (t-item nil "123" nil)))

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
  (should-error (t-plain-list nil "123" nil)))

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
  (should (equal (t--wrap-image "" nil "" "") "<figure>\n</figure>"))
  (should (equal (t--wrap-image "hello" nil "" "")
                 "<figure>\nhello</figure>"))
  (should (equal
           (t--wrap-image "hello" nil " abc" "")
           "<figure>\nhello<figcaption>abc</figcaption>\n</figure>"))
  (should (equal
           (t--wrap-image "" nil "\ntest\n" "1")
           "<figure1>\n<figcaption>test</figcaption>\n</figure>")))

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
  (should (equal (t-paragraph-filter "a\n\n" nil nil) "a\n"))
  (should (equal (t-paragraph-filter "a" nil nil) "a\n"))
  (should (equal (t-paragraph-filter "\na" nil nil) "\na\n")))

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
  (should (equal (t-line-break nil nil nil) "<br>\n")))

(ert-deftest t-target ()
  "Tests for `org-w3ctr-target'."
  (cl-letf* ((counter 0)
             ((symbol-function 't--reference)
              (lambda (_d _i &optional _n)
                (number-to-string (cl-incf counter)))))
    (should (equal (t-target nil nil nil) "<span id=\"1\"></span>"))
    (should (equal (t-target nil nil nil) "<span id=\"2\"></span>"))
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
    (should (equal (t-radio-target nil "hello" nil)
                   "<span id=\"1\">hello</span>"))
    (should (equal (t-radio-target nil "world" nil)
                   "<span id=\"2\">world</span>"))
    (should (equal (t-radio-target nil nil nil)
                   "<span id=\"3\"></span>"))
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
    (should (equal (t-statistics-cookie "" nil nil) "<code></code>"))
    (should (equal (t-statistics-cookie "y" nil nil) "<code>y</code>"))
    (should (equal (t-statistics-cookie ()()()) "<code>nil</code>")))
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
  (should (equal (t-subscript nil "123" nil) "<sub>123</sub>"))
  (should (equal (t-subscript nil "" nil) "<sub></sub>"))
  (should (equal (t-subscript nil t nil) "<sub>t</sub>"))
  (should (equal (t-subscript nil nil nil) "<sub>nil</sub>"))
  (t-check-element-values
   #'t-subscript
   '(("1_2" "<sub>2</sub>")
     ("x86_64" "<sub>64</sub>")
     ("f_{1}" "<sub>1</sub>"))))

(ert-deftest t-superscript ()
  "Tests for `org-w3ctr-superscript'."
  (should (equal (t-superscript nil "123" nil) "<sup>123</sup>"))
  (should (equal (t-superscript nil "" nil) "<sup></sup>"))
  (should (equal (t-superscript nil t nil) "<sup>t</sup>"))
  (should (equal (t-superscript nil nil nil) "<sup>nil</sup>"))
  (t-check-element-values
   #'t-superscript
   '(("1^2" "<sup>2</sup>")
     ("x86^64" "<sup>64</sup>")
     ("f^{1}" "<sup>1</sup>"))))

(ert-deftest t--get-markup-format ()
  "Tests for `org-w3ctr--get-markup-format'."
  (let ((info '(:html-text-markup-alist ((a . 2) (b . 3) (c . 4)))))
    (should (equal (t--get-markup-format 'a info) 2))
    (should (equal (t--get-markup-format 'b info) 3))
    (should (equal (t--get-markup-format 'c info) 4)))
  (should (equal (t--get-markup-format 'anything nil) "%s")))

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
    (should (string= (t--convert-special-strings (car a))
                     (cdr a)))))

(ert-deftest t-plain-text ()
  "Tests for `org-w3ctr-plain-text'."
  (should (equal (t-plain-text "a < b & c > d" '())
                 "a &lt; b &amp; c &gt; d"))
  (should (equal (t-plain-text "\"hello\"" '(:with-smart-quotes t))
                 "\"hello\""))
  (should (equal (t-plain-text "a -- b" '(:with-special-strings t))
                 "a &#x2013; b"))
  (should (equal (t-plain-text "line1\nline2" '(:preserve-breaks t))
                 "line1<br>\nline2"))
  (should (equal (t-plain-text "\"a < b\" -- c\nd"
                               '( :with-smart-quotes t
                                  :with-special-strings t
                                  :preserve-breaks t))
                 "\"a &lt; b\" &#x2013; c<br>\nd")))

(ert-deftest t--timezone-to-offset ()
  "Tests for `org-w3ctr--timezone-to-offset'."
  (should (eq (t--timezone-to-offset "local") 'local))
  (should (eq (t--timezone-to-offset "LOCAL") 'local))
  (should (eq (t--timezone-to-offset "LoCaL") 'local))
  (should (eq (t--timezone-to-offset "lOcAl") 'local))
  (should (= (t--timezone-to-offset "UTC+8") (* 8 3600)))
  (should (= (t--timezone-to-offset "UTC+08") 28800))
  (should (= (t--timezone-to-offset "GMT-5") (* -5 3600)))
  (should (= (t--timezone-to-offset "+0530")
             (+ (* 5 3600) (* 30 60))))
  (should (= (t--timezone-to-offset "-0830")
             (+ (* -8 3600) (* -30 60))))
  (should-not (t--timezone-to-offset "+10"))
  (should-not (t--timezone-to-offset "-11"))
  (should-not (t--timezone-to-offset "INVALID"))
  (should-not (t--timezone-to-offset "UTC+123"))
  (should-not (t--timezone-to-offset "+12345"))
  (should-not (t--timezone-to-offset "+1400"))
  (should-not (t--timezone-to-offset "UTC+13"))
  (should-not (t--timezone-to-offset "UTC-13"))
  (should-not (t--timezone-to-offset "+0860")))

(ert-deftest t--get-info-timezone-offset ()
  "Tests for `org-w3ctr--get-info-timezone-offset'."
  (let ((info0 '(:html-timezone "local")))
    (should (eq (t--get-info-timezone-offset info0) 'local)))
  (let ((info1 '(:html-timezone 28800)))
    (should (= (t--get-info-timezone-offset info1) 28800)))
  (let ((info2 '(:html-timezone -18000)))
    (should (= (t--get-info-timezone-offset info2) -18000)))
  (let ((info3 '(:html-timezone "UTC+8")))
    (should (= (t--get-info-timezone-offset info3) 28800))
    (should (numberp (t--pget info3 :html-timezone)))
    (should (= (t--pget info3 :html-timezone) 28800)))
  (let ((info4 '(:html-timezone "-0500")))
    (should (= (t--get-info-timezone-offset info4) -18000))
    (should (numberp (t--pget info4 :html-timezone)))
    (should (= (t--pget info4 :html-timezone) -18000)))
  (let ((info5 '(:html-timezone "UTC+0")))
    (should (= (t--get-info-timezone-offset info5) 0))
    (should (= (t--pget info5 :html-timezone) 0)))
  (let ((info6 '(:html-timezone "+0530")))
    (should (= (t--get-info-timezone-offset info6) 19800))
    (should (= (t--pget info6 :html-timezone) 19800)))
  (let ((info7 '(:html-timezone "Invalid")))
    (should-error (t--get-info-timezone-offset info7)))
  (let ((info8 '(:html-timezone 3600 :other "value")))
    (should (= (t--get-info-timezone-offset info8) 3600))
    (should (equal info8 '(:html-timezone 3600 :other "value")))))

(ert-deftest t--get-info-export-timezone-offset ()
  "Tests for `org-w3ctr--get-info-export-timezone-offset'."
  ;; When :html-export-timezone is nil, use :html-timezone
  (let ((info1 '(:html-timezone 28800)))
    (should (= (t--get-info-export-timezone-offset info1) 28800)))
  (let ((info2 '(:html-timezone "UTC+8" :html-export-timezone nil)))
    (should (= (t--get-info-export-timezone-offset info2) 28800)))
  ;; When :html-timezone is "local", always use 'local
  (let ((info3 '(:html-timezone "local" :html-export-timezone 3600)))
    (should (eq (t--get-info-export-timezone-offset info3) 'local)))
  (let ((info4 '(:html-timezone "local" :html-export-timezone "UTC+5")))
    (should (eq (t--get-info-export-timezone-offset info4) 'local)))
  (let ((info41 '(:html-timezone local :html-export-timezone "+0100")))
    (should (eq (t--get-info-export-timezone-offset info41) 'local)))
  ;; :html-export-timezone is local
  (let ((info42 '(:html-timezone 0 :html-export-timezone "local")))
    (should (eq (t--get-info-export-timezone-offset info42) 'local)))
  (let ((info43 '(:html-timezone 0 :html-export-timezone local)))
    (should (eq (t--get-info-export-timezone-offset info43) 'local)))
  ;; When :html-export-timezone is number, use directly
  (let ((info5 '(:html-timezone 28800 :html-export-timezone -18000)))
    (should (= (t--get-info-export-timezone-offset info5) -18000)))
  ;; When :html-export-timezone is string, convert and cache
  (let ((info6 '(:html-timezone 28800 :html-export-timezone "-0500")))
    (should (= (t--get-info-export-timezone-offset info6) -18000))
    (should (fixnump (t--pget info6 :html-export-timezone)))
    (should (= (t--pget info6 :html-export-timezone) -18000)))
  (let ((info7 '(:html-timezone 28800 :html-export-timezone "+0530")))
    (should (= (t--get-info-export-timezone-offset info7) 19800))
    (should (= (t--pget info7 :html-export-timezone) 19800)))
  (let ((info8 '(:html-timezone 0 :html-export-timezone "UTC+0")))
    (should (= (t--get-info-export-timezone-offset info8) 0)))
  ;; Invalid
  (let ((info9 '(:html-timezone 3600 :html-export-timezone "Invalid")))
    (should-error (t--get-info-export-timezone-offset info9)))
  (let ((info9 '(:html-timezone "WTF" :html-export-timezone "+0000")))
    (should-error (t--get-info-export-timezone-offset info9)))
  ;; Test optional argument
  (let ((info10 '(:html-export-timezone "+0000")))
    (should (eq (t--get-info-export-timezone-offset info10 'local) 'local)))
  (let ((info11 '(:html-export-timezone "UTC+8")))
    (should (= (t--get-info-export-timezone-offset info11 10) 28800))))

(ert-deftest t--get-info-timezone-delta ()
  "Tests for `org-w3ctr--get-info-timezone-delta'."
  (let ((info '(:html-timezone 2 :html-export-timezone 1)))
    (should (= (t--get-info-timezone-delta info) -1)))
  (let ((info '(:html-timezone 28800 :html-export-timezone 0)))
    (should (= (t--get-info-timezone-delta info) -28800)))
  (let ((info '(:html-timezone local :html-export-timezone 3600)))
    (should (= (t--get-info-timezone-delta info) 0)))
  (let ((info '(:html-timezone local :html-export-timezone 3600)))
    (should (= (t--get-info-timezone-delta info) 0)))
  (let ((info '(:html-timezone 114514 :html-export-timezone 191981)))
    (should (= (t--get-info-timezone-delta info) 77467)))
  (let ((info '(:html-timezone "WTF" :html-export-timezone "INVALID")))
    (should-error (t--get-info-timezone-delta info)))
  (let ((info '(:html-timezone 0 :html-export-timezone "INVALID")))
    (should-error (t--get-info-timezone-delta info)))
  (should-error (t--get-info-timezone-delta nil))
  (let ((info '(:html-timezone 114514 :html-export-timezone 191981)))
    (should (= (t--get-info-timezone-delta info 1 2) 1)))
  (let ((info '(:html-timezone 114514 :html-export-timezone 191981)))
    (should (= (t--get-info-timezone-delta info nil 114515) 1)))
  (let ((info '(:html-timezone 114514 :html-export-timezone 191981)))
    (should (= (t--get-info-timezone-delta info 191980) 1)))
  (let ((info '(:html-timezone 114514 :html-export-timezone 191981)))
    (should (= (t--get-info-timezone-delta nil 114514 191981) 77467))))

(ert-deftest t--get-datetime-format ()
  "Tests for `org-w3ctr--get-datetime-format'."
  (should (string= (t--get-datetime-format 28800 'space-none)
                   "%F %R+0800"))
  (should (string= (t--get-datetime-format 18000 'space-none)
                   "%F %R+0500"))
  (should (string= (t--get-datetime-format -28800 'space-none)
                   "%F %R-0800"))
  (should (string= (t--get-datetime-format -18000 'space-none)
                   "%F %R-0500"))
  (should (string= (t--get-datetime-format 0 'space-none)
                   "%F %R+0000"))
  (should (string= (t--get-datetime-format 0 'space-none-zulu)
                   "%F %RZ"))
  (should (string= (t--get-datetime-format 0 'space-colon)
                   "%F %R+00:00"))
  (should (string= (t--get-datetime-format 0 'space-colon-zulu)
                   "%F %RZ"))
  (should (string= (t--get-datetime-format 4800 'space-colon-zulu)
                   "%F %R+01:20"))
  (should (string= (t--get-datetime-format 0 'T-colon)
                   "%FT%R+00:00"))
  (should (string= (t--get-datetime-format 0 'T-none)
                   "%FT%R+0000"))
  (should (string= (t--get-datetime-format 0 'T-colon)
                   "%FT%R+00:00"))
  (should (string= (t--get-datetime-format 0 'T-colon-zulu)
                   "%FT%RZ"))
  (should (string= (t--get-datetime-format 3600 'space-colon)
                   "%F %R+01:00"))
  (should (string= (t--get-datetime-format -900 'space-colon)
                   "%F %R-00:15"))
  (should (string= (t--get-datetime-format 19800 'T-colon)
                   "%FT%R+05:30"))
  (should (string= (t--get-datetime-format -16200 'T-colon)
                   "%FT%R-04:30"))
  (should (string= (t--get-datetime-format 50400 'space-none)
                   "%F %R+1400"))
  (should (string= (t--get-datetime-format -43200 'space-none)
                   "%F %R-1200"))
  (should (string= (t--get-datetime-format 37800 'T-colon-zulu)
                   "%FT%R+10:30"))
  (should-not (t--get-datetime-format 0 nil))
  (should (string= (t--get-datetime-format 'local nil t) "%F"))
  (should (string= (t--get-datetime-format 'local 'space-none)
                   "%F %R"))
  (should (string= (t--get-datetime-format 'local 'space-none-zulu)
                   "%F %R"))
  (should (string= (t--get-datetime-format 'local 'space-colon)
                   "%F %R"))
  (should (string= (t--get-datetime-format 'local 'space-colon-zulu)
                   "%F %R"))
  (should (string= (t--get-datetime-format 'local 'T-none)
                   "%FT%R"))
  (should (string= (t--get-datetime-format 'local 'T-none-zulu)
                   "%FT%R"))
  (should (string= (t--get-datetime-format 'local 'T-colon)
                   "%FT%R"))
  (should (string= (t--get-datetime-format 'local 'T-colon-zulu)
                   "%FT%R")))

(ert-deftest t--get-info-normalized-timezone ()
  (let ((info-local '(:html-timezone "local" :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-local) "")))
  ;; Test UTC cases with different formatting options
  (let ((info-utc1 '(:html-timezone 0 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-utc1) "+0000")))
  (let ((info-utc2 '(:html-timezone 0 :html-datetime-option space-none-zulu)))
    (should (string= (t--get-info-normalized-timezone info-utc2) "Z")))
  (let ((info-utc3 '(:html-timezone 0 :html-datetime-option T-colon-zulu)))
    (should (string= (t--get-info-normalized-timezone info-utc3) "Z")))
  ;; Test positive offsets with different formats
  (let ((info-pos1 '(:html-timezone 28800 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-pos1) "+0800")))
  (let ((info-pos2 '(:html-timezone 28800 :html-datetime-option space-colon)))
    (should (string= (t--get-info-normalized-timezone info-pos2) "+08:00")))
  (let ((info-pos3 '(:html-timezone 19800 :html-datetime-option T-none)))
    (should (string= (t--get-info-normalized-timezone info-pos3) "+0530")))
  (let ((info-pos4 '(:html-timezone 19800 :html-datetime-option T-colon)))
    (should (string= (t--get-info-normalized-timezone info-pos4) "+05:30")))
  (let ((info-neg1 '(:html-timezone -18000 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-neg1) "-0500")))
  (let ((info-neg2 '(:html-timezone -18000 :html-datetime-option T-colon)))
    (should (string= (t--get-info-normalized-timezone info-neg2) "-05:00")))
  (let ((info-neg3 '(:html-timezone -16200 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-neg3) "-0430")))
  (let ((info-override '( :html-timezone 28800
                          :html-export-timezone -18000
                          :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-override) "-0500")))
  ;; Test fractional timezones
  (let ((info-frac1 '(:html-timezone 5400 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-frac1) "+0130")))
  (let ((info-frac2 '(:html-timezone -5400 :html-datetime-option T-colon)))
    (should (string= (t--get-info-normalized-timezone info-frac2) "-01:30")))
  ;; Test edge cases
  (let ((info-max '(:html-timezone 50400 :html-datetime-option space-none)))
    (should (string= (t--get-info-normalized-timezone info-max) "+1400")))
  (let ((info-min '(:html-timezone -43200 :html-datetime-option space-colon)))
    (should (string= (t--get-info-normalized-timezone info-min) "-12:00"))))

(ert-deftest t--format-normalized-timestamp ()
  ;; Basic test with space separator and +HHMM timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info1 '( :html-timezone 28800
                  :html-export-timezone 28800
                  :html-datetime-option space-none)))
    (should (string= (t--format-normalized-timestamp
                      test-time info1)
                     "2023-01-01 12:00+0800")))
  ;; Test with colon in time and -HHMM timezone
  (let ((test-time (encode-time 0 30 9 15 6 2023))
        (info2 '( :html-timezone -14400
                  :html-export-timezone -14400
                  :html-datetime-option space-colon)))
    (should (string= (t--format-normalized-timestamp
                      test-time info2)
                     "2023-06-15 09:30-04:00")))
  ;; Test UTC with Zulu timezone
  (let ((test-time (encode-time 0 0 0 1 1 2023))
        (info3 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option space-none-zulu)))
    (should (string= (t--format-normalized-timestamp
                      test-time info3)
                     "2023-01-01 00:00Z")))
  ;; Test with T separator and +HH:MM timezone
  (let ((test-time (encode-time 0 45 18 31 12 2023))
        (info4 '( :html-timezone 19800
                  :html-export-timezone 19800
                  :html-datetime-option T-colon)))
    (should (string= (t--format-normalized-timestamp
                      test-time info4)
                     "2023-12-31T18:45+05:30")))

  ;; Test with T separator and Zulu timezone for UTC
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info5 '( :html-timezone 0
                  :html-export-timezone 0
                  :html-datetime-option T-colon-zulu)))
    (should (string= (t--format-normalized-timestamp
                      test-time info5)
                     "2023-01-01T12:00Z")))
  ;; Test local timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info6 '( :html-timezone "local"
                  :html-export-timezone "local"
                  :html-datetime-option space-none)))
    (should (string= "2023-01-01 12:00"
                     (t--format-normalized-timestamp
                      test-time info6))))
  ;; Test invalid timezone
  (let ((test-time (encode-time 0 0 12 1 1 2023))
        (info7 '(:html-timezone "Invalid")))
    (should-error (t--format-normalized-timestamp
                   test-time info7))))

(ert-deftest t--get-timestamp-format ()
  (let ((info '(:html-timestamp-format
                ("%Y-%m-%d" . "%Y-%m-%d %H:%M"))))
    (should (string= (t--get-timestamp-format 'active t info)
                     "<%Y-%m-%d %H:%M>"))
    (should (string= (t--get-timestamp-format 'active nil info)
                     "<%Y-%m-%d>"))
    (should (string= (t--get-timestamp-format 'inactive t info)
                     "[%Y-%m-%d %H:%M]"))
    (should (string= (t--get-timestamp-format 'inactive-range nil info)
                     "[%Y-%m-%d]")))
  (let ((info '(:html-timestamp-format
                ("%Y-%m-%d" . "%Y-%m-%d %H:%M")))
        (org-display-custom-times t)
        (org-timestamp-custom-formats
         '("%m/%d/%y %a" . "%m/%d/%y %a %H:%M")))
    (should (string= (t--get-timestamp-format 'active t info)
                     "<%m/%d/%y %a %H:%M>"))
    (should (string= (t--get-timestamp-format 'active nil info)
                     "<%m/%d/%y %a>"))))

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
    (should (string= (test nil) "utf-8"))
    (should (string= (test 'utf-8-unix) "utf-8"))
    (should (string= (test 'utf-8-dos) "utf-8"))
    (should (string= (test 'utf-8-mac) "utf-8"))
    (should (string= (test 'gbk) "gbk"))
    (should (string= (test 'chinese-gbk) "gbk"))
    (should (string= (test 'big5) "big5"))
    (should (string= (test 'utf-7) "utf-7"))
    (should (string= (test 'gb18030) "gb18030"))
    (should (string= (test 'iso-latin-2) "iso-8859-2"))
    (should (string= (test 'japanese-shift-jis) "shift_jis"))
    (should (string= (test 'japanese-iso-8bit) "euc-jp"))
    (should (string= (test 'cp936) "gbk"))
    (should (string= (test 'cp65001) "utf-8"))))

(ert-deftest t--get-info-author-raw ()
  "Tests for `org-w3ctr--get-author-raw'."
  (should-not (t--get-info-author-raw nil))
  (let ((info '(:with-author nil)))
    (should-not (t--get-info-author-raw info)))
  (let ((info '(:with-author nil :author "test")))
    (should-not (t--get-info-author-raw info)))
  (let ((info '(:with-author t :author "test")))
    (should (equal (t--get-info-author-raw info) "test"))))

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
  (should (equal (t--build-meta-entry "name" "author")
                 "<meta name=\"author\">\n"))
  (should (equal (t--build-meta-entry "property" "og:title" "My Title")
                 "<meta property=\"og:title\" content=\"My Title\">\n"))
  (should (equal (t--build-meta-entry "name" "description"
                                      "Version %s" "1.0")
                 "<meta name=\"description\" content=\"Version 1.0\">\n"))
  (should (equal (t--build-meta-entry "name" "quote" "He said \"Hello\"")
                 "<meta name=\"quote\" content=\"He said &quot;Hello&quot;\">\n"))
  (should (equal (t--build-meta-entry "name" "version" "v%s.%s" "1" "2")
                 "<meta name=\"version\" content=\"v1.2\">\n"))
  (should (equal (t--build-meta-entry "name" "version" "'%s'" "v1.2")
                 "<meta name=\"version\" content=\"&apos;v1.2&apos;\">\n")))

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
    (should (equal (t-meta-tags-default info-with-author)
                   '(("name" "author" "Alice")
                     ("name" "generator" "Org Mode"))))
    (should (equal (t-meta-tags-default info-with-desc)
                   '(("name" "description" "Test doc")
                     ("name" "generator" "Org Mode"))))
    (should (equal (t-meta-tags-default info-with-keywords)
                   '(("name" "keywords" "org, test")
                     ("name" "generator" "Org Mode"))))
    (should (equal (t-meta-tags-default info-empty)
                   '(("name" "generator" "Org Mode"))))))

(ert-deftest t--build-meta-tags ()
  "Tests for `org-w3ctr--build-meta-tags'."
  (let ((t-meta-tags '(("a" "b" "test"))))
    (should (equal (t--build-meta-tags nil)
                   "<meta a=\"b\" content=\"test\">\n")))
  (let ((t-meta-tags '(("a" "b" nil))))
    (should (equal (t--build-meta-tags nil)
                   "<meta a=\"b\">\n"))))

(ert-deftest t--build-viewport-options ()
  "Tests for `org-w3ctr--build-viewport-options'."
  (should-not (t--build-viewport-options nil))
  (cl-flet ((f (ls) (let ((info `(:html-viewport ,ls)))
                      (t--build-viewport-options info))))
    (should (equal (f '(("a" ""))) nil))
    (should (equal (f '(("a" "b")))
                   "<meta name=\"viewport\" content=\"a=b\">\n"))
    (should (equal (f '(("a" "b") ("b" "") ("c" "d")))
                   "<meta name=\"viewport\" content=\"a=b, c=d\">\n"))
    (should-not (f '(("a" nil) ("b" nil) ("c" "  ")))))
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
    (should-error (t--load-css nil)))
  (let ((t-default-style "123"))
    (should (equal (t--load-css nil)
                   "<style>\n123\n</style>\n")))
  (let ((t-default-style "")
        (t-default-style-file nil))
    (should (equal (t--load-css nil) "")))
  (cl-letf (((symbol-function 't--load-file)
             #'identity)
            (t-default-style ""))
    (should (equal (t--load-css nil)
                   (format "<style>\n%s\n</style>\n"
                           t-default-style-file)))
    (should (equal t-default-style
                   t-default-style-file))))

(ert-deftest t--build-math-config ()
  "Test `t--build-math-config' function."
  (let ((info '( :with-latex nil)))
    (should (equal (t--build-math-config info) "")))
  (let ((info '( :with-latex mathjax
                 :html-mathjax-config "mathjax")))
    (should (equal (t--build-math-config info) "mathjax\n")))
  (let ((info '( :with-latex mathml
                 :html-mathml-config "mathml")))
    (should (equal (t--build-math-config info) "mathml\n")))
  (let ((info '(:with-latex invalid)))
    (should-error (t--build-math-config info) :type 'error))
  (let ((info '( :with-latex custom
                 :html-math-custom-config-function
                 (lambda (_i) "test"))))
    (should (equal (t--build-math-config info) "test\n")))
  (let ((info '( :with-latex custom
                 :html-math-custom-config-function
                 t-math-custom-config-function-default)))
    (should (equal (t--build-math-config info) ""))))

(ert-deftest t--has-math-p ()
  "Tests for `org-w3ctr--has-math-p'."
  (cl-flet ((mkinfo (str) `( :with-latex t
                             :parse-tree
                             ,(with-temp-buffer
                                (save-excursion (insert str))
                                (org-element-parse-buffer)))))
    (should-not (t--has-math-p (mkinfo "123")))
    (should-not (t--has-math-p (mkinfo "$1+2")))
    (should (equal (t--has-math-p (mkinfo "$1+2$")) t))
    (should (equal (t--has-math-p (mkinfo "\\(1+2\\)")) t))
    (should (equal (t--has-math-p (mkinfo "\\[1+2\\]")) t))
    (should (equal (t--has-math-p (mkinfo "\\begin_equation\n123\n\\end_equation")) t))))

(ert-deftest t--use-default-style-p ()
  "Tests for `org-w3ctr--use-default-styple-p'."
  (should-not (t--use-default-style-p nil))
  (should (equal (t--use-default-style-p
                  '(:html-head-include-default-style t))
                 t)))

(ert-deftest t-legacy-format-home/up ()
  "Tests for `org-w3ctr-legacy-format-home/up'."
  (let ((info '(:html-link-up "" :html-link-home "")))
    (should-not (t-legacy-format-home/up info)))
  (let ((info `( :html-link-up "1" :html-link-home "2"
                 :html-home/up-format ,t-home/up-format)))
    (should (equal (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"1\"> UP </a>
 <a href=\"2\"> HOME </a>\n</div>"))
    (setq info (plist-put info :html-link-home ""))
    (should (equal (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"1\"> UP </a>
 <a href=\"1\"> HOME </a>\n</div>"))
    (setq info (plist-put info :html-link-up ""))
    (setf (plist-get info :html-link-home) "2")
    (should (equal (t-legacy-format-home/up info) "\
<div id=\"home-and-up\">\n <a href=\"2\"> UP </a>
 <a href=\"2\"> HOME </a>\n</div>")))
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
  (should (equal (t--format-home/up-nav "")
                 "<nav id=\"home-and-up\">\n\n</nav>\n"))
  (should (equal (t--format-home/up-nav "1")
                 "<nav id=\"home-and-up\">\n1\n</nav>\n")))

(ert-deftest t--format-home/up-vector ()
  "Tests for `org-w3ctr--format-home/up-vector'."
  (should (equal "" (t--format-home/up-vector [])))
  (should (equal (t--format-home/up-vector [("a" . "b")]) "\
<nav id=\"home-and-up\">
<a href=\"a\">b</a>
</nav>\n"))
  (should (equal (t--format-home/up-vector [("a" . "b") ("c" . "d")]) "\
<nav id=\"home-and-up\">
<a href=\"a\">b</a>
<a href=\"c\">d</a>
</nav>\n")))

(ert-deftest t--format-home/up-list ()
  "Tests for `org-w3ctr--format-home/up-list'."
  (should (t--format-home/up-list nil nil))
  (cl-letf (((symbol-function 'org-export-data)
             (lambda (x _info) x))
            ((symbol-function 't--format-home/up-nav)
             (lambda (x) x)))
    (should (equal (t--format-home/up-list '("a") nil) "a"))
    (should (equal (t--format-home/up-list '("a" "b" "c") nil)
                   "a\nb\nc"))
    (should (equal (t--format-home/up-list '("a" " " "\t" "\n" "e") nil)
                   "a\ne"))))

(ert-deftest t-format-home/up-default-function ()
  "Tests for `org-w3ctr-format-home/up-default-function'."
  (let ((info '(:html-link-home/up [("a" . "b")])))
    (should (equal (t-format-home/up-default-function info)
                   "<nav id=\"home-and-up\">\n<a href=\"a\">b</a>\n</nav>\n")))
  (let ((info '(:html-link-home/up [("a" . "b") ("c" . "d")])))
    (should (equal (t-format-home/up-default-function info)
                   "<nav id=\"home-and-up\">\n<a href=\"a\">b</a>\n<a href=\"c\">d</a>\n</nav>\n")))
  (let ((info '(:html-link-home/up [(a . "b")])))
    (should-error (t-format-home/up-default-function info)))
  (let ((info '(:html-link-home/up [("a" . b)])))
    (should-error (t-format-home/up-default-function info)))
  (let ((info '(:html-link-home/up [(a . b)])))
    (should-error (t-format-home/up-default-function info)))
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
      (should (t--load-cc-svg a)))))

(ert-deftest t--load-cc-svg-once ()
  "Tests for `org-w3ctr--load-cc-svg-once'."
  (cl-letf (((symbol-function 't--insert-file)
             (lambda (file) file))
            (t--cc-svg-hashtable (make-hash-table :test 'equal)))
    (dolist (a '("by" "cc" "nc" "nd" "sa" "zero"))
      (t--load-cc-svg-once a))
    (dolist (a '("by" "cc" "nc" "nd" "sa" "zero"))
      (should (equal (gethash a t--cc-svg-hashtable)
                     (t--load-cc-svg a))))))

(ert-deftest t-format-public-license-default-function ()
  "Tests for `org-w3ctr-format-public-license-default-function'."
  (cl-letf (((symbol-function 't--get-info-author)
             (lambda (info) (plist-get info :author))))
    (cl-flet ((test (info)
                (t-format-public-license-default-function info)))
      (let ((info (list :html-license nil)))
        (should (equal (test info) "Not Specified"))
        (setq info (plist-put info :html-license 'all-rights-reserved))
        (should (equal (test info) "All Rights Reserved"))
        (setq info (plist-put info :html-license 'all-rights-reversed))
        (should (equal (test info) "All Rights Reversed"))
        (setq info (plist-put info :html-license 'cc-by-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/4.0/\">CC BY-NC 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-nd-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/4.0/\">CC BY-NC-ND 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-sa-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">CC BY-NC-SA 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nd-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/4.0/\">CC BY-ND 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-sa-4.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by/3.0/\">CC BY 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc/3.0/\">CC BY-NC 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-nd-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-nd/3.0/\">CC BY-NC-ND 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nc-sa-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nc-sa/3.0/\">CC BY-NC-SA 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-nd-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-nd/3.0/\">CC BY-ND 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-sa-3.0))
        (should (equal (test info)
                       "This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/3.0/\">CC BY-SA 3.0</a>"))
        (setq info (plist-put info :html-license 'cc-by-4.0))
        (setq info (plist-put info :author "test"))
        (should (equal (test info)
                       "This work by test is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>"))
        (setq info (plist-put info :html-use-cc-budget t))
        (should (equal (test info)
                       (concat "This work by test is licensed under <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a>"
                               " " (t--get-cc-svgs 'cc-by-4.0))))))))

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
      (should (equal result (t--mathml-to-oneline xml))))))

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
;; read-symbol-shorthands: (("t-" . "org-w3ctr-"))
;; coding: utf-8-unix
;; End:
