;; -*- lexical-binding: t; -*-

(ert-deftest t--2str ()
  (should (eq (t--2str nil) nil))
  (should (string= (t--2str 1) "1"))
  (should (string= (t--2str 114.514) "114.514"))
  (should (string= (t--2str 'hello) "hello"))
  (should (string= (t--2str 'has\ space) "has space"))
  (should (string= (t--2str "string") "string"))
  (should-not (t--2str [1]))
  (should-not (t--2str (lambda (x) x))))

(ert-deftest t--make-attr ()
  (should (string= (t--make-attr '(a b)) " a=\"b\""))
  (should (string= (t--make-attr '(open)) " open"))
  (should (string= (t--make-attr '("disabled")) " disabled"))
  (should (string= (t--make-attr '(class "example two"))
		   " class=\"example two\""))
  (should (string= (t--make-attr '(FOO)) " foo"))
  (should-not (t--make-attr '([x])))
  (should (string= (t--make-attr '(foo [bar] baz))
		   " foo=\"baz\""))
  (should (string= (t--make-attr '(data-A "base64..."))
		   " data-a=\"base64...\"")))

(ert-deftest t--sexp2html ()
  (should (string= (t--sexp2html nil) ""))
  ;; Basic tag with no attributes
  (should (string= (t--sexp2html '(p () "123")) "<p>123</p>"))
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
  (should (string= (t--sexp2html '(hr)) "<hr>")))

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "org-w3ctr-"))
;; End:
