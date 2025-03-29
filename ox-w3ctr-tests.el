;; -*- lexical-binding: t; -*-

(defvar t-test-values nil)

(defun t-check-element-values (fn advice pairs)
  (advice-add fn :filter-return advice)
  (unwind-protect
      (dolist (test pairs)
	(let (t-test-values)
	  (ignore (org-export-string-as (car test) 'w3ctr t))
	  (should (equal t-test-values (cdr test)))))
    (advice-remove fn advice)))

(defun t-advice-return-value (str)
  (prog1 str
    (push (if (not (stringp str)) str
	      (substring-no-properties str))
	  t-test-values)))

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

(ert-deftest t-center-block ()
  (t-check-element-values
   #'t-center-block #'t-advice-return-value
   '(("#+begin_center\n#+end_center"
      "<div style=\"text-align:center;\"></div>")
     ("#+begin_center\n123\n#+end_center"
      "<div style=\"text-align:center;\">\n<p>123</p>\n</div>"))))

(ert-deftest t-drawer ()
  (t-check-element-values
   #'t-drawer #'t-advice-return-value
   '((":hello:\n:end:"
      "<details><summary>hello</summary></details>")
     ("#+caption: what can i say\n:test:\n:end:"
      "<details><summary>what can i say</summary></details>")
     ("#+name: id\n#+attr__: [example]\n:h:\n:end:"
      "<details id=\"id\" class=\"example\"><summary>h</summary></details>")
     ("#+attr__: (open)\n:h:\n:end:"
      "<details open><summary>h</summary></details>")
     (":try-this:\n=int a = 1;=\n:end:"
      "<details><summary>try-this</summary>\n<p><code>int a = 1;</code></p>\n</details>"))))

(ert-deftest t-dynamic-block ()
  (t-check-element-values
   #'t-dynamic-block #'t-advice-return-value
   '(("#+begin: hello\n123\n#+end:" "<p>123</p>\n")
     ("#+begin: nothing\n#+end:" ""))))

(ert-deftest t-checkbox ()
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-checkbox #'t-advice-return-value
     '(("- [ ] 123" "&#x2610;")
       ("- [X] 123" "&#x2611;")
       ("- [-] 123" "&#x2612;"))))
  (let ((t-checkbox-type 'ascii))
    (t-check-element-values
     #'t-checkbox #'t-advice-return-value
     '(("- [ ] 123" "<code>[&#xa0;]</code>")
       ("- [X] 123" "<code>[X]</code>")
       ("- [-] 123" "<code>[-]</code>"))))
  (let ((t-checkbox-type 'html))
    (t-check-element-values
     #'t-checkbox #'t-advice-return-value
     '(("- [ ] 123" "<input type='checkbox' disabled>")
       ("- [X] 123" "<input type='checkbox' checked disabled>")
       ("- [-] 123" "<input type='checkbox'>")))))

(ert-deftest t-item-unordered ()
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item #'t-advice-return-value
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
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item #'t-advice-return-value
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
  (let ((t-checkbox-type 'unicode))
    (t-check-element-values
     #'t-item #'t-advice-return-value
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
       ("- a :: b\n- c" "<dt>(no term)</dt><dd>c</dd>" "<dt>a</dt><dd>b</dd>")))))

(ert-deftest t-plain-list ()
  (t-check-element-values
   #'t-plain-list #'t-advice-return-value
   '(("- 123" "<ul>\n<li>123</li>\n</ul>")
     ("1. 123" "<ol>\n<li>123</li>\n</ol>")
     ("- x :: y" "<dl>\n<dt>x</dt><dd>y</dd>\n</dl>")
     ("#+name: test\n#+attr__: (data-test \"a joke\")\n- x"
      "<ul id=\"test\" data-test=\"a joke\">\n<li>x</li>\n</ul>")
     ("1. 123\n   - 2 3 4"
      "<ol>\n<li>123\n<ul>\n<li>2 3 4</li>\n</ul></li>\n</ol>"
      "<ul>\n<li>2 3 4</li>\n</ul>"))))

(ert-deftest t-quote-block ()
  (t-check-element-values
   #'t-quote-block #'t-advice-return-value
   '(("#+begin_quote\n#+end_quote" "<blockquote></blockquote>")
     ("#+begin_quote\n123\n#+end_quote"
      "<blockquote>\n<p>123</p>\n</blockquote>")
     ("#+attr__: [test]\n#+BEGIN_QUOTE\n456\n#+END_QUOTE"
      "<blockquote class=\"test\">\n<p>456</p>\n</blockquote>"))))

(ert-deftest t-example-block ()
  (t-check-element-values
   #'t-example-block #'t-advice-return-value
   '(("#+name: t\n#+begin_example\n#+end_example"
      "<div id=\"t\">\n<pre>\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n1\n2\n3\n#+end_example"
      "<div id=\"t\">\n<pre>\n1\n2\n3\n</pre>\n</div>")
     ("#+name: t\n#+attr__: [ex]\n#+BEGIN_EXAMPLE\n123\n#+END_EXAMPLE"
      "<div id=\"t\" class=\"ex\">\n<pre>\n123\n</pre>\n</div>")
     ("#+name: t\n#+begin_example\n 1\n 2\n 3\n#+end_example"
      "<div id=\"t\">\n<pre>\n1\n2\n3\n</pre>\n</div>"))))

(ert-deftest t-export-block ()
  (t-check-element-values
   #'t-export-block #'t-advice-return-value
   '(("#+begin_export html\nanythinghere\n#+end_export"
      "anythinghere\n")
     ("#+begin_export html\n#+end_export" "")
     ("#+begin_export mhtml\nanythinghere\n#+end_export"
      "anythinghere\n")
     ("#+begin_export mhtml\n#+end_export" "")
     ("#+begin_export css\np {color: red;}\n#+end_export"
      "<style>\np {color: red;}\n</style>")
     ("#+begin_export css\n#+end_export" "<style>\n</style>")
     ("#+begin_export js\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export js\n#+end_export" "<script>\n</script>")
     ("#+begin_export javascript\nlet f = x => x + 1;\n#+end_export"
      "<script>\nlet f = x => x + 1;\n</script>")
     ("#+begin_export javascript\n#+end_export" "<script>\n</script>")
     ("#+begin_export emacs-lisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export emacs-lisp\n#+end_export" "")
     ("#+begin_export elisp\n(+ 1 2)\n#+end_export" "3")
     ("#+begin_export elisp\n#+end_export" "")
     ("#+begin_export lisp-data\n (p() \"123\")\n#+end_export"
      "<p>123</p>")
     ("#+begin_export lisp-data\n (br)\n#+end_export" "<br>")
     ("#+BEGIN_EXPORT lisp-data\n (br)\n#+END_EXPORT" "<br>")
     ("#+begin_export wtf\n no exported\n#+end_export" "")
     ("#+begin_export\n not exported\n#+end_export" ""))))

(ert-deftest t-fixed-width ()
  (t-check-element-values
   #'t-fixed-width #'t-advice-return-value
   '((":           " "<pre></pre>")
     (": 1\n" "<pre>\n1\n</pre>")
     (": 1\n: 2\n" "<pre>\n1\n2\n</pre>")
     (":  1\n:  2\n:   3\n" "<pre>\n1\n2\n 3\n</pre>")
     (": 1\n: \n" "<pre>\n1\n\n</pre>")
     ("#+name: t\n#+attr__: [test]\n: 1\n : 2\n: 3"
      "<pre id=\"t\" class=\"test\">\n1\n2\n3\n</pre>"))))

(ert-deftest t-horizontal-rule ()
  (t-check-element-values
   #'t-horizontal-rule #'t-advice-return-value
   '(("-")
     ("--")
     ("---")
     ("----")
     ("-----" "<hr>")
     ("------" "<hr>")
     ("-------" "<hr>")
     ("--------" "<hr>")
     ("---------" "<hr>")
     ("----------" "<hr>"))))

(ert-deftest t-keyword ()
  (t-check-element-values
   #'t-keyword #'t-advice-return-value
   '(("#+h: <p>123</p>" "<p>123</p>")
     ("#+h: " "")
     ("#+html: <p>123</p>" "<p>123</p>")
     ("#+html: " "")
     ("#+e: (concat \"1\" nil \"2\")" "12")
     ("#+e: " "")
     ("#+d: (p((data-x \"1\"))123)" "<p data-x=\"1\">123</p>")
     ("#+d: " "")
     ("#+l: " "")
     ("#+l: (p() 123) (p() 234)" "<p>123</p><p>234</p>")
     ("#+hello: world" ""))))

(ert-deftest t-paragraph ()
  (t-check-element-values
   #'t-paragraph #'t-advice-return-value
   '(("123" "<p>123</p>")
     ("123\n 234" "<p>123\n 234</p>")
     ("    123" "<p>123</p>")
     ("123\n\t234" "<p>123\n\011234</p>")
     ("123\n\n234" "<p>234</p>" "<p>123</p>")
     ("- 123 234" "123 234")
     ("- [ ] 123" "123")
     ("- 123\n 234" "123\n234")
     ("- 123\n\n   234" "<p>234</p>" "123\n")
     ("-\n  #+attr__: [example]\n  123"
      "<span class=\"example\">123</span>")
     ("-\n  #+name: id\n  123\n\n  #+name: id2\n  456"
      "<p id=\"id2\">456</p>" "<span id=\"id\">123\n</span>")
     ("[[./1.png]]"
      "<figure>\n<img src=\"./1.png\" alt=\"1.png\"></figure>")
     ("#+name: id\n#+caption:cap\n[[./1.png]]"
      "<figure id=\"id\">\n<img src=\"./1.png\" alt=\"1.png\"><figcaption>cap</figcaption>\n</figure>")
     ("#+attr__:[sidefigure]\n[[./2.gif]]"
      "<figure class=\"sidefigure\">\n<img src=\"./2.gif\" alt=\"2.gif\"></figure>")
     ("[[https://example.com/1.jpg]]"
      "<figure>\n<img src=\"https://example.com/1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[file:1.jpg]]" "<figure>\n<img src=\"1.jpg\" alt=\"1.jpg\"></figure>")
     ("[[./1.png][name]]" "<p><a href=\"./1.png\">name</a></p>")
     ("[[https://example.com/1.jpg][file:1.jpg]]"
      "<figure>\n<a href=\"https://example.com/1.jpg\"><img src=\"1.jpg\" alt=\"1.jpg\"></a></figure>"))))

(ert-deftest t-verse-block ()
  (t-check-element-values
   #'t-verse-block #'t-advice-return-value
   '(("#+begin_verse\n#+end_verse" "<p>\n</p>")
     ("#+BEGIN_VERSE\n#+END_VERSE" "<p>\n</p>")
     ("#+begin_verse\n1  2  3\n#+end_verse" "<p>\n1  2  3<br>\n</p>")
     ("#+begin_verse\n 1\n  2\n   3\n#+end_verse"
      "<p>\n1<br>\n&#xa0;2<br>\n&#xa0;&#xa0;3<br>\n</p>")
     ("#+name: this\n#+begin_verse\n#+end_verse" "<p id=\"this\">\n</p>")
     ("#+attr__:[hi]\n#+begin_verse\n\n\n#+end_verse"
      "<p class=\"hi\">\n<br>\n<br>\n</p>"))))

(ert-deftest t-entity ()
  (t-check-element-values
   #'t-entity #'t-advice-return-value
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
  (t-check-element-values
   #'t-export-snippet #'t-advice-return-value
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

(ert-deftest t-statistics-cookie ()
  (t-check-element-values
   #'t-statistics-cookie #'t-advice-return-value
   '(("- hello [/]" "<code>[/]</code>")
     ("- hello [0/1]\n  - [ ] helllo" "<code>[0/1]</code>")
     ("- hello [33%]\n  - [X] hello" "<code>[33%]</code>")
     ("- hello :: abc [0/1]\n  - [ ] this is what"
      "<code>[0/1]</code>")
     ("1. hello [50%]\n   1. [ ] hello1\n   2. [X] hello2"
      "<code>[50%]</code>"))))

(ert-deftest t-subscript ()
  (t-check-element-values
   #'t-subscript #'t-advice-return-value
   '(("1_2" "<sub>2</sub>")
     ("x86_64" "<sub>64</sub>")
     ("f_{1}" "<sub>1</sub>"))))

(ert-deftest t-superscript ()
  (t-check-element-values
   #'t-superscript #'t-advice-return-value
   '(("1^2" "<sup>2</sup>")
     ("x86^64" "<sup>64</sup>")
     ("f^{1}" "<sup>1</sup>"))))

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
