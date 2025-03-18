;;; ox-w3ctr.el --- HTML Back-End for Org Export Engine using W3C TR CSS specification -*- lexical-binding: t; -*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 2024-03-18 04:51:00

;; Package-Version: 0.2
;; Package-Requires: ((emacs "30.1"))
;; Keywords: HTML, Org
;; URL: https://github.com/include-yy/ox-w3ctr

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a HTML back-end for Org generic exporter.
;; A parasitic implementation of ox-html.el

;; See:
;; - https://respec.org/docs/
;; - https://www.w3.org/StyleSheets/TR/2021/
;; - https://github.com/w3c/tr-design

;;; Code:

;;; Dependencies
(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'ox-publish)
(require 'table)


;;; Define Back-End

(org-export-define-backend 'w3ctr
  '(;; see https://orgmode.org/worg/org-syntax.html for details
    ;; top-level structure
    (inner-template . t-inner-template)
    (template . t-template)
    ;;@ headline section [2]
    (headline . t-headline)
    (section . t-section)
    ;;@ greater elements [11]
    ;; footnote-definition                      NO-EXIST
    ;; inlinetasks `inlinetask'                 NO-USE
    ;; property drawers `property-drawer'       NO-USE
    (center-block . t-center-block)             ; #+begin_center
    (drawer . t-drawer)                         ; :name: ... :end:
    (dynamic-block . t-dynamic-block)           ; #+begin: name para
    (item . t-item)                             ; plain list item
    (plain-list . t-plain-list)                 ; plain list
    (quote-block . t-quote-block)               ; #+begin_quote
    (special-block . t-special-block)           ; #+begin_{sth}
    (table . t-table)                           ; | | | \n | | | ...
    ;;@ lesser elements [17]
    ;; babel cell                               NO-EXIST
    ;; clock `clock'                            NO-USE
    ;; comments                                 NO-EXPORT
    ;; comment block                            NO-EXPORT
    ;; diary sexp `diary-sexp'                  NO-USE
    ;; node properties `node-property'          NO-USE
    ;; planning `planning'                      NO-USE
    (example-block . t-example-block)           ; #+BEGIN_EXAMPLE
    (export-block . t-export-block)             ; #+BEGIN_EXPORT html
    (fixed-width . t-fixed-width)               ; ^: contents
    (horizontal-rule . t-horizontal-rule)       ; -----------
    (keyword . t-keyword)                       ; #+NAME: ...
    (latex-environment . t-latex-environment)   ; \begin
    (paragraph . t-paragraph)                   ; \n ... \n
    (src-block . t-src-block)                   ; #+BEGIN_SRC lang
    (table-row . t-table-row)                   ; | |
    (verse-block . t-verse-block)               ; #+BEGIN_VERSE
    ;;@ objects [25]
    ;; citation                                 (NOUSE)
    ;; citation reference                       (NOUSE)
    ;; inline babel calls                       (NOEXIST)
    ;; macros                                   (NOEXIST)
    (entity . t-entity)                         ; \alpha, \cent
    (export-snippet . t-export-snippet)         ; @@html:something@@
    (footnote-reference . t-footnote-reference) ; [fn:]
    (inline-src-block . t-inline-src-block)     ; src_LANG{BODY}
    (latex-fragment . t-latex-fragment)         ; \(, \[
    (line-break . t-line-break)                 ; \\
    (link . t-link)                             ; [[...][...]]
    (radio-target . t-radio-target)             ; <<<CONTENTS>>>
    (statistics-cookie . t-statistics-cookie)   ; [%] [/]
    (subscript . t-subscript)                   ; a_{b}
    (superscript . t-superscript)               ; a^{b}
    (table-cell . t-table-cell)                 ; | |
    (target . t-target)                         ; <<target>>
    (timestamp . t-timestamp)                   ; [<time-spec>]
    ;; smallest objects
    (bold . t-bold)                             ; *a*
    (italic . t-italic)                         ; /a/
    (underline . t-underline)                   ; _a_
    (verbatim . t-verbatim)                     ; =a=
    (code . t-code)                             ; ~a~
    (strike-through . t-strike-through)         ; +a+
    (plain-text . t-plain-text))
  :filters-alist '((:filter-parse-tree . t-image-link-filter)
		   (:filter-paragraph . t-paragraph-filter)
		   (:filter-final-output . t-final-function))
  :menu-entry
  '(?w "Export to W3C technical reports style html"
       ((?H "As HTML buffer" t-export-as-html)
	(?h "As HTML file" t-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (t-export-to-html t s v b)
		(org-open-file (t-export-to-html nil s v b)))))))
  :options-alist
  '(;; meta information for HTML <head> --------
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:creator "CREATOR" nil t-creator-string)
    (:language "LANGUAGE" nil t-language-string)
    (:html-viewport nil nil t-viewport)
    (:html-head "HTML_HEAD" nil t-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil t-head-extra newline)
    (:subtitle "SUBTITLE" nil nil parse)
    (:html-head-include-default-style
     nil "html-style" t-head-include-default-style)
    (:html-metadata-timestamp-format nil nil t-metadata-timestamp-format)
    ;; HTML TOP place naviagtion elements --------------
    (:html-link-left  "HTML_LINK_LEFT"  nil t-link-left)
    (:html-link-lname "HTML_LINK_LNAME" nil t-link-lname)
    (:html-link-right "HTML_LINK_RIGHT" nil t-link-right)
    (:html-link-rname "HTML_LINK_RNAME" nil t-link-rname)
    ;; Latex and MathJAX options -------
    (:with-latex nil "tex" t-with-latex)
    (:latex-header "LATEX_HEADER" nil nil newline)
    (:html-equation-reference-format
     "HTML_EQUATION_REFERENCE_FORMAT" nil t-equation-reference-format t)
    (:html-mathjax "HTML_MATHJAX" nil "" space)
    (:html-mathjax-options nil nil t-mathjax-options)
    (:html-mathjax-template nil nil t-mathjax-template)
    ;; postamble and preamble ------------------------
    (:html-postamble nil "html-postamble" t-postamble)
    (:html-preamble nil "html-preamble" t-preamble)
    (:html-format-home/up-function nil nil t-format-home/up-function)
    (:html-validation-link nil nil t-validation-link)
    ;; footnote options -----------------------------
    (:html-footnote-format nil nil t-footnote-format)
    (:html-footnote-separator nil nil t-footnote-separator)
    (:html-footnotes-section nil nil t-footnotes-section)
    ;; headline options -------------------------------------
    (:html-self-link-headlines nil nil t-self-link-headlines)
    (:html-toplevel-hlevel nil nil t-toplevel-hlevel)
    ;; <yy> aux counter for unnumbered headline
    (:html-headline-cnt nil nil 0)
    ;; <yy> store zeroth section's output
    (:html-zeroth-section-output nil nil "")
    ;; <yy> zeroth section's toc title name
    (:html-zeroth-section-tocname nil "zeroth-name" t-zeroth-section-tocname)
    ;; <yy> control max headline level
    (:headline-levels nil "H" t-headline-level)
    ;; <yy> control todo, priority and tags export
    (:with-todo-keywords nil "todo" t-with-todo-keywords)
    (:with-priority nil "pri" t-with-priority)
    (:with-tags nil "tags" t-with-tags)
    ;; table options ------------------------
    (:html-table-align-individual-fields
     nil nil t-table-align-individual-fields)
    (:html-table-caption-above nil nil t-table-caption-above)
    (:html-table-data-tags nil nil t-table-data-tags)
    (:html-table-header-tags nil nil t-table-header-tags)
    (:html-table-use-header-tags-for-first-column
     nil nil t-table-use-header-tags-for-first-column)
    (:html-table-row-open-tag nil nil t-table-row-open-tag)
    (:html-table-row-close-tag nil nil t-table-row-close-tag)
    ;; misc options -----------------------------
    (:html-checkbox-type nil nil t-checkbox-type)
    (:html-extension nil nil t-extension)
    (:html-indent nil nil t-indent)
    (:html-inline-image-rules nil nil t-inline-image-rules)
    (:html-link-org-files-as-html nil nil t-link-org-files-as-html)
    (:html-text-markup-alist nil nil t-text-markup-alist)
    (:html-inline-images nil nil t-inline-images)
    ;; <yy> add back to top arrow
    (:html-back-to-top nil "back-to-top" t-back-to-top)
    ;; <yy> add timestamp format for timestamp
    (:html-timestamp-format nil nil t-timestamp-format)
    ;; <yy> add options for fixup.js's code
    (:html-fixup-js "HTML_FIXUP_JS" nil t-fixup-js newline)
    ))

;;; Customizations

(defvar t-checkbox-type 'unicode
  "The type of checkboxes to use for HTML export.

See `org-html-checkbox-types' for the values used for each option.")

;;;; LaTeX

(defcustom t-equation-reference-format "\\eqref{%s}"
  "The MathJax command to use when referencing equations.

Most common values are:
  \\eqref{%s}    Wrap the equation in parentheses
  \\ref{%s}      Do not wrap the equation in parentheses

See `org-html-equation-reference-format' for more information."
  :group 'org-export-w3ctr
  :type 'string
  :safe #'stringp)

(defcustom t-with-latex t
  "Non-nil means process LaTeX math snippets.

See `org-html-with-latex' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)


;;; Basic utilties

(defsubst t--2str (s)
  "Convert S to string.

S can be number, symbol, string."
  (cl-typecase s
    (null nil)
    (symbol (symbol-name s))
    (string s)
    (number (number-to-string s))
    (otherwise nil)))

(defsubst t--make-attr (list)
  "Convert LIST to HTML attribute."
  (when-let* ((name (t--2str (nth 0 list))))
    (if-let* ((rest (cdr list)))
	;; use lowercase prop name.
	(concat " " (downcase name) "=\""
		(mapconcat #'t--2str rest) "\"")
      (concat " " (downcase name)))))

;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element
(defconst t--void-element-regexp
  (rx string-start
      (or "area" "base" "br" "col" "embed" "hr"
	  "img" "input" "link" "meta" "param"
	  "source" "track" "wbr")
      string-end)
  "Regexp matching HTML void elements (self-closing tags).
These elements do not require a closing tag in HTML.")

(defun t--sexp2html (data)
  "Convert S-expression DATA into an HTML string.

The function only accepts symbols, strings, numbers, and lists as
input. Other data types will be ignored."
  (cl-typecase data
    (null "")
    ((or symbol string number) (t--2str data))
    (list
     ;; always use lowercase tagname.
     (let* ((tag (downcase (t--2str (nth 0 data))))
	    (attr-ls (nth 1 data))
	    (attrs (if (or (eq attr-ls t) (eq attr-ls nil)) ""
		     (mapconcat #'t--make-attr (nth 1 data)))))
       (if (string-match-p t--void-element-regexp tag)
	   (format "<%s%s>" tag attrs)
	 (let ((children (mapconcat #'t--sexp2html (cddr data))))
	   (format "<%s%s>%s</%s>"
		   tag attrs children tag)))))
    (otherwise "")))

(defun t--read-attr (attribute element)
  "Turn ATTRIBUTE property from ELEMENT into a alist."
  (when-let* ((value (org-element-property attribute element))
	      (str (mapconcat #'identity value " ")))
    (when (org-string-nw-p str)
      (read (concat "(" str ")")))))

(defun t--read-attr__ (element)
  "Like `t--read-attr', but treat vector as class sequence."
  (when-let* ((attrs (t--read-attr :attr__ element)))
    (mapcar
     (lambda (x) (if (not (vectorp x)) x
	       (list "class" (mapconcat #'t--2str x " "))))
     attrs)))

(defun t--make-attr__ (attributes)
  "Return a list of attributes, as a string.

ATTRIBUTES is a alist where values are either strings or nil. An
attribute with a nil value means a boolean attribute."
  (mapconcat
   (lambda (x) (if (atom x)
	       (and-let* ((s (t--2str x)))
		 (concat " " (downcase s)))
	     (t--make-attr x)))
   attributes))

(defun t--make-attr__id (element info &optional named-only)
  "Return ELEMENT's attribute string."
  (let* ((reference (t--reference element info named-only))
	 (attributes (t--read-attr__ element))
	 (a (t--make-attr__
	     (if (not reference) attributes
	       (cons `("id" ,reference) attributes)))))
    (if (org-string-nw-p a) a "")))

(define-inline t--nw-p (s)
  "Return S if S is a string containing a non-blank character.
Otherwise, return nil. See also `org-string-nw-p'."
  (inline-letevals (s)
    (inline-quote
     (and (stringp ,s) (string-match-p "[^ \r\t\n]" ,s) ,s))))

(defsubst t--trim (s &optional keep-lead)
  "Remove whitespace at the beginning and the end of string S.
When optional argument KEEP-LEAD is non-nil, removing blank lines
at the beginning of the string does not affect leading indentation.

See also `org-trim'."
  (replace-regexp-in-string
   (if keep-lead "\\`\\([ \t]*\n\\)+" "\\`[ \t\n\r]+") ""
   (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

;;;; Center Block
(defun t-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to HTML."
  (format "<div style=\"text-align:center;\">%s</div>"
	  (if contents (concat "\n" contents) "")))

;;;; Drawer
(defun t-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML."
  (let* ((name (org-element-property :drawer-name drawer))
	 (cap (if-let* ((cap (org-export-get-caption drawer)))
		  (org-export-data cap info) name))
	 (attrs (t--make-attr__id drawer info t)))
    (format "<details%s>\n<summary>%s</summary>%s</details>"
	    attrs cap (if contents (concat "\n" contents) ""))))

;;;; Dynamic Block
(defun t-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML."
  contents)

;;;; Item
(defconst t-checkbox-types
  '(( unicode .
      ((on . "&#x2611;") (off . "&#x2610;") (trans . "&#x2612;")))
    ( ascii .
      ((on . "<code>[X]</code>") (off . "<code>[&#xa0;]</code>")
       (trans . "<code>[-]</code>")))
    ( html .
      ((on . "<input type='checkbox' checked disabled>")
       (off . "<input type='checkbox' disabled>")
       (trans . "<input type='checkbox'>"))))
  "Alist of checkbox types.
The cdr of each entry is an alist list three checkbox types for
HTML export: `on', `off' and `trans'.

The choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes")

(defun t-checkbox (checkbox info)
  "Format CHECKBOX into HTML.
See `org-w3ctr-checkbox-types' for customization options."
  (cdr (assq checkbox
	     (cdr (assq (plist-get info :html-checkbox-type)
			t-checkbox-types)))))

(defun t-format-list-item ( contents type checkbox info
			    &optional term-counter-id
			    headline)
  "Format a list item into HTML."
  (let ((checkbox (concat (t-checkbox checkbox info)
			  (and checkbox " ")))
	(br "<br>")
	(extra-newline (if (and (t--nw-p contents) headline)
			   "\n" "")))
    (concat
     (pcase type
       (`ordered
	(let* ((counter term-counter-id)
	       (extra (if counter
			  (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s>" extra)
	   (when headline (concat headline br)))))
       (`unordered
	(concat "<li>"
		(if (not term-counter-id) ""
		  (format "[@%d] " term-counter-id))
		(when headline (concat headline br))))
       (`descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat (format "<dt>%s</dt>"
			  (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     extra-newline
     (and (t--nw-p contents) (t--trim contents))
     extra-newline
     (pcase type
       (`ordered "</li>")
       (`unordered "</li>")
       (`descriptive "</dd>")))))

(defun t-item (item contents info)
  "Transcode an ITEM element from Org to HTML."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (t-format-list-item
     contents type checkbox info (or tag counter))))

;;;; Plain List
(defun t-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML."
  (let* ((type (pcase (org-element-property :type plain-list)
		 (`ordered "ol")
		 (`unordered "ul")
		 (`descriptive "dl")
		 (_ (error "Unknown HTML list type: %s" other))))
	 (attributes (t--make-attr__id plain-list info t)))
    (format "<%s%s>\n%s</%s>"
	    type attributes contents type)))

;;;; Quote Block
(defun t-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<blockquote%s>%s</blockquote>"
	  (t--make-attr__id quote-block info t)
	  (if contents (concat "\n" contents) "")))

;;;; Special Block
;; FIXME
(defun t-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (html5-fancy (member block-type t-html5-elements))
         (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class (concat class " " block-type)
                                      block-type)))))
    (let* ((contents (or contents ""))
	   (reference (t--reference special-block info t))
	   (a (t--make-attribute-string
	       (if (or (not reference) (plist-member attributes :id))
		   attributes
		 (plist-put attributes :id reference))))
	   (str (if (org-string-nw-p a) (concat " " a) "")))
      (if html5-fancy
	  (format "<%s%s>\n%s</%s>" block-type str contents block-type)
	(format "<div%s>\n%s\n</div>" str contents)))))

;; FIXME
(defun t-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (t-table--table.el-table table info)
    ;; Standard table.
    (let* ((caption (org-export-get-caption table))
	   (attributes
	    (t--make-attribute-string
	     (org-combine-plists
	      (list :id (t--reference table info t))
	      (org-export-read-attribute :attr_html table))))
	   (alignspec "class=\"org-%s\"")
	   (table-column-specs
	    (lambda (table info)
	      (mapconcat
	       (lambda (table-cell)
		 (let ((alignment (org-export-table-cell-alignment
				   table-cell info)))
		   (concat
		    ;; Begin a colgroup?
		    (when (org-export-table-cell-starts-colgroup-p
			   table-cell info)
		      "\n<colgroup>")
		    ;; Add a column.  Also specify its alignment.
		    (format "\n%s"
			    (t-close-tag
			     "col" (concat " " (format alignspec alignment)) info))
		    ;; End a colgroup?
		    (when (org-export-table-cell-ends-colgroup-p
			   table-cell info)
		      "\n</colgroup>"))))
	       (t-table-first-row-data-cells table info) "\n"))))
      (format "<table%s>\n%s\n%s\n%s</table>"
	      (if (equal attributes "") "" (concat " " attributes))
	      (if (not caption) ""
		(format (if (plist-get info :html-table-caption-above)
			    "<caption class=\"t-above\">%s</caption>"
			  "<caption class=\"t-bottom\">%s</caption>")
			(org-export-data caption info)))
	      (funcall table-column-specs table info)
	      contents))))

;;;; Example Block
;; FIXME
(defun t-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML."
  (format "<div%s>\n<pre>%s</pre>\n</div>"
	  (t--make-attr__id example-block info)
	  (t-format-src-block-code example-block info)))

;;;; Export Block

(defun t-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to HTML."
  (let* ((type (org-element-property :type export-block))
	 (value (org-element-property :value export-block))
	 (text (org-remove-indentation value)))
    (pcase type
      ((or "HTML" "MHTML") text)
      ("CSS" (concat "<style>\n" text "</style>"))
      ((or "JS" "JAVASCRIPT") (concat "<script>\n" text
				      "</script>"))
      ;; Expression that return HTML string.
      ((or "EMACS-LISP" "ELISP") (format "%s" (eval (read value))))
      ;; SEXP-style HTML data.
      ("LISP-DATA" (t--sexp2html (read value)))
      (_ nil))))

;;;; Fixed Width

(defun t-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML."
  (format "<pre%s>%s</pre>"
	  (t--make-attr__id fixed-width info t)
	  (t-fontify-code
	   (org-remove-indentation
	    (org-element-property :value fixed-width))
	   nil)))

;;;; Horizontal Rule

(defun t-horizontal-rule (_horizontal-rule _contents info)
  "Transcode an HORIZONTAL-RULE object from Org to HTML."
  (t-close-tag "hr" nil info))

;;;; Keyword

(defun t-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to HTML."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (pcase key
      ((or "H" "HTML") value)
      ("E" (format "%s" (eval (read value))))
      ("D" (t--sexp2html (read value)))
      ("L" (mapconcat #'t--sexp2html (read (format "(%s)" value))))
      (_ nil))))

;;;; Latex Environment

(defun t-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It can
be `mathjax', `verbatim', `html', nil, t or symbols in
`org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
`imagemagick'.  See `org-w3ctr-with-latex' for more information.
INFO is a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (or (eq processing-type 'mathjax)
                (eq processing-type 'html))
      (let ((bfn (or (buffer-file-name)
		     (make-temp-name
		      (expand-file-name
		       "latex" temporary-file-directory))))
	    (latex-header
	     (if-let* ((header (plist-get info :latex-header)))
		 (concat (mapconcat
			  (lambda (line) (concat "#+LATEX_HEADER: " line))
			  (org-split-string header "\n")
			  "\n")
			 "\n"))))
	(setq cache-relpath
	      (concat (file-name-as-directory
		       org-preview-latex-image-directory)
		      (file-name-sans-extension
		       (file-name-nondirectory bfn)))
	      cache-dir (file-name-directory bfn))
	;; Re-create LaTeX environment from original buffer in
	;; temporary buffer so that dvipng/imagemagick can properly
	;; turn the fragment into an image.
	(setq latex-frag (concat latex-header latex-frag))))
    (org-export-with-buffer-copy
     :to-buffer (get-buffer-create " *Org HTML Export LaTeX*")
     :drop-visibility t :drop-narrowing t :drop-contents t
     (erase-buffer)
     (insert latex-frag)
     (org-format-latex cache-relpath nil nil cache-dir nil
		       "Creating LaTeX Image..." nil processing-type)
     (buffer-string))))

(defun t--wrap-latex-environment (contents _ &optional caption label)
  "Wrap CONTENTS string within appropriate environment for equations.
When optional arguments CAPTION and LABEL are given, use them for
caption and \"id\" attribute."
  (format "\n<div%s class=\"equation-container\">\n%s%s\n</div>"
          ;; ID.
          (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
          ;; Contents.
          (format "<span class=\"equation\">\n%s\n</span>" contents)
          ;; Caption.
          (if (not (org-string-nw-p caption)) ""
            (format "\n<span class=\"equation-label\">\n%s\n</span>"
                    caption))))

(defun t--math-environment-p (element &optional _)
  "Non-nil when ELEMENT is a LaTeX math environment.
Math environments match the regular expression defined in
`org-latex-math-environments-re'.  This function is meant to be
used as a predicate for `org-export-get-ordinal' or a value to
`t-standalone-image-predicate'."
  (string-match-p org-latex-math-environments-re
                  (org-element-property :value element)))

(defun t--latex-environment-numbered-p (element)
  "Non-nil when ELEMENT contains a numbered LaTeX math environment.
Starred and \"displaymath\" environments are not numbered."
  (not (string-match-p "\\`[ \t]*\\\\begin{\\(.*\\*\\|displaymath\\)}"
		       (org-element-property :value element))))

(defun t--unlabel-latex-environment (latex-frag)
  "Change environment in LATEX-FRAG string to an unnumbered one.
For instance, change an `equation' environment to `equation*'."
  (replace-regexp-in-string
   "\\`[ \t]*\\\\begin{\\([^*]+?\\)}"
   "\\1*"
   (replace-regexp-in-string "^[ \t]*\\\\end{\\([^*]+?\\)}[ \r\t\n]*\\'"
			     "\\1*"
			     latex-frag nil nil 1)
   nil nil 1))

(defun t-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex))
	(latex-frag (org-remove-indentation
		     (org-element-property :value latex-environment)))
        (attributes (org-export-read-attribute :attr_html latex-environment))
        (label (t--reference latex-environment info t))
        (caption (and (t--latex-environment-numbered-p latex-environment)
		      (number-to-string
		       (org-export-get-ordinal
			latex-environment info nil
			(lambda (l _)
			  (and (t--math-environment-p l)
			       (t--latex-environment-numbered-p l))))))))
    (cond
     ((memq processing-type '(t mathjax))
      (t-format-latex
       (if (org-string-nw-p label)
	   (replace-regexp-in-string "\\`.*"
				     (format "\\&\n\\\\label{%s}" label)
				     latex-frag)
	 latex-frag)
       'mathjax info))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (t-format-latex
              (t--unlabel-latex-environment latex-frag)
              processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (let ((source (org-export-file-uri (match-string 1 formula-link))))
	    (t--wrap-latex-environment
	     (t--format-image source attributes info)
	     info caption label)))))
     (t (t--wrap-latex-environment latex-frag info caption label)))))


;;;; Paragraph

(defun t-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (class (org-export-read-attribute :attr_html paragraph :class))
	 (attrs (t--make-attr__id paragraph info t)))
    (cond
     ((and (eq parent-type 'item)
	   (string= attrs "")
	   (not (org-export-get-previous-element paragraph info)))
      ;; First paragraph in an item
      contents)
     ((t-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption (org-export-data (org-export-get-caption paragraph) info))
	    (label (t--reference paragraph info t)))
	(t--wrap-image contents info caption label class)))
     ;; Regular paragraph.
     (t (format "<p%s>%s</p>" attrs (string-trim contents))))))

(defun t-paragraph-filter (value _backend _info)
  "Delete trailing newlines."
  (concat (string-trim-right value) "\n"))

;;;; Src Block

(defun t-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (t--textarea-block src-block)
    (if (not (t--has-caption-p src-block))
	(let ((code (t-format-src-block-code src-block info))
	      (id (t--reference src-block info t))
	      (cls (org-export-read-attribute :attr_html src-block :class)))
	  (format "<pre%s%s>%s</pre>"
		  (if id (format " id=\"%s\"" id) "")
		  (if cls (format " class=\"%s\"" cls) "")
		  code))
      (let* ((code (t-format-src-block-code src-block info))
	     (id (t--reference src-block info))
	     (cls (org-export-read-attribute :attr_html src-block :class))
	     (caption (let ((cap (org-export-get-caption src-block)))
			(if cap (org-trim (org-export-data cap info) nil))))
	     (class (if (org-string-nw-p cls) (concat "example " cls) "example")))
	(format "<div%s%s>\n%s\n%s\n<pre>%s</pre></div>"
		(format " id=\"%s\"" id)
		(format " class=\"%s\"" class)
		(format "<a class=\"self-link\" href=\"#%s\" %s></a>" id
			"aria-label=\"source block\"")
		(if (not caption) "" caption)
		code)))))

;;;; Table Row

(defun t-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((group (org-export-table-row-group table-row info))
	   (number (org-export-table-row-number table-row info))
	   (start-group-p
	    (org-export-table-row-starts-rowgroup-p table-row info))
	   (end-group-p
	    (org-export-table-row-ends-rowgroup-p table-row info))
	   (topp (and (equal start-group-p '(top))
		      (equal end-group-p '(below top))))
	   (bottomp (and (equal start-group-p '(above))
			 (equal end-group-p '(bottom above))))
           (row-open-tag
            (pcase (plist-get info :html-table-row-open-tag)
              ((and accessor (pred functionp))
               (funcall accessor
			number group start-group-p end-group-p topp bottomp))
	      (accessor accessor)))
           (row-close-tag
            (pcase (plist-get info :html-table-row-close-tag)
              ((and accessor (pred functionp))
               (funcall accessor
			number group start-group-p end-group-p topp bottomp))
	      (accessor accessor)))
	   (group-tags
	    (cond
	     ;; Row belongs to second or subsequent groups.
	     ((not (= 1 group)) '("<tbody>" . "\n</tbody>"))
	     ;; Row is from first group.  Table has >=1 groups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row) info)
	      '("<thead>" . "\n</thead>"))
	     ;; Row is from first and only group.
	     (t '("<tbody>" . "\n</tbody>")))))
      (concat (and start-group-p (car group-tags))
	      (concat "\n"
		      row-open-tag
		      contents
		      "\n"
		      row-close-tag)
	      (and end-group-p (cdr group-tags))))))

;;;; Verse Block

(defun t-verse-block (_verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "<p class=\"verse\">\n%s</p>"
	  ;; Replace leading white spaces with non-breaking spaces.
	  (replace-regexp-in-string
	   "^[ \t]+" (lambda (m) (t--make-string (length m) "&#xa0;"))
	   ;; Replace each newline character with line break.  Also
	   ;; remove any trailing "br" close-tag so as to avoid
	   ;; duplicates.
	   (let* ((br (t-close-tag "br" nil info))
		  (re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote br))))
	     (replace-regexp-in-string re (concat br "\n") contents)))))

;;;; Entity

(defun t-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to HTML."
  (org-element-property :html entity))

;;;; Export Snippet

(defun t-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML."
  (let* ((backend (org-export-snippet-backend export-snippet))
	 (value (org-element-property :value export-snippet)))
    (pcase backend
      ;; plain html text.
      ((or 'h 'html) value)
      ;; Read, Evaluate, Print, no Loop :p
      ('e (format "%s" (eval (read value))))
      ;; sexp-style html data.
      ('d (t--sexp2html (read value)))
      ;; sexp-style html data list.
      ('l (mapconcat #'t--sexp2html (read (format "(%s)" value))))
      (_ nil))))

;;;; Footnote Reference

(defun t-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
	  (label (org-element-property :label footnote-reference)))
      (t--anchor
       nil (format (plist-get info :html-footnote-format) (or label n))
       (format " href=\"#fn.%d\" aria-label=\"reference to %s\"" n label) info))))

;;;; Inline Src Block

(defun t-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language inline-src-block))
	 (code (t-fontify-code
		(org-element-property :value inline-src-block)
		lang))
	 (label
	  (let ((lbl (t--reference inline-src-block info t)))
	    (if (not lbl) "" (format " id=\"%s\"" lbl)))))
    (format "<code class=\"src-inline src-%s\"%s>%s</code>" lang label code)))

;;;; Latex Fragment

(defun t-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML."
  (let ((frag (org-element-property :value latex-fragment))
	(type (plist-get info :with-latex)))
    (cond
     ((memq type '(t mathjax))
      (t-format-latex frag 'mathjax info))
     ((eq type 'html)
      (t-format-latex frag 'html info))
     ((assq type org-preview-latex-process-alist)
      (when-let* ((formula-link (t-format-latex frag type info)))
	(when (string-match "file:\\([^]]*\\)" formula-link)
	  (let ((source (org-export-file-uri
			 (match-string 1 formula-link))))
	    (t--format-image source nil info)))))
     (t frag))))

;;;; Line Break

(defun t-line-break (_line-break _contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat (t-close-tag "br" nil info) "\n"))



;;; Internal Variables

(defconst t-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "noscript"
    "output" "progress" "section" "summary" "video")
  "Elements in html5.

For blocks that should contain headlines, use the HTML_CONTAINER
property on the headline itself.")

(defconst t-special-string-regexps
  '(("\\\\-" . "&#x00ad;")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defvar t--id-attr-prefix "ID-"
  "Prefix to use in ID attributes.
This affects IDs that are determined from the ID property.")

(defvar t-style-default ""
  "The default style specification for exported HTML files.
You can use `t-head' and `t-head-extra' to add to
this style.  If you don't want to include this default style,
customize `t-head-include-default-style'.")

(defvar t-fixup-js ""
  "js code that control toc's hide and show")

;; load default CSS from style.css
(defun t-update-css-js ()
  "update `t-style-default' and t-fixup-js"
  (interactive)
  (setq t-style-default
	(let ((fname (if (not load-in-progress) (expand-file-name "style.css")
		       (concat (file-name-directory load-file-name) "style.css"))))
	  (format "<style>\n%s\n</style>\n"
		  (with-temp-buffer
		    (insert-file-contents fname)
		    (buffer-string)))))
  (setq t-fixup-js
	(let ((fname (if (not load-in-progress) (expand-file-name "fixup.js")
		       (concat (file-name-directory load-file-name) "fixup.js"))))
	  (format "<script>\n%s\n</script>\n"
		  (with-temp-buffer
		    (insert-file-contents fname)
		    (buffer-string))))))
;; do update
(t-update-css-js)

(defconst t-back-to-top-arrow "\
<p role=\"navigation\" id=\"back-to-top\">\n<a href=\"#title\">\
<abbr title=\"Back to Top\">↑</abbr></a>\n</p>\n")


;;; User Configuration Variables

(defgroup org-export-w3ctr nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export W3C TR HTML"
  :group 'org-export)

;;;; Bold, etc.

(defcustom t-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>"))
  "Alist of HTML expressions to convert text markup.

See `org-html-text-markup-alist' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-w3ctr
  :type 'boolean)

;;;; Footnotes

(defcustom t-footnotes-section "<div id=\"references\">
<h2>%s</h2>
<dl>%s</dl>\n</div>\n"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-footnote-format "[%s]"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-footnote-separator ", "
  "Text used to separate footnotes."
  :group 'org-export-w3ctr
  :type 'string)

;;;; Headline

(defcustom t-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.

See `org-html-toplevel-hlevel' for more information."
  :group 'org-export-w3ctr
  :type 'integer)

(defcustom t-self-link-headlines t
  "When non-nil, the headlines contain a hyperlink to themselves."
  :group 'org-export-w3ctr
  :type 'boolean
  :safe #'booleanp)

;;;; Links :: Generic

(defcustom t-link-org-files-as-html t
  "Non-nil means make file links to \"file.org\" point to \"file.html\".
When nil, the links still point to the plain \".org\" file.

See `org-html-link-org-files-as-html' for more information."
  :group 'org-export-w3ctr
  :type 'boolean)

;;;; Links :: Inline images

(defcustom t-inline-images t
  "Non-nil means inline images into exported HTML pages.
When nil, an anchor with href is used to link to the image."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-inline-image-rules
  `(("file" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp" ".avif")))
    ("http" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp" ".avif")))
    ("https" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp" ".avif"))))
  "Rules characterizing image files that can be inlined into HTML.

See `org-html-inline-image-rules' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)

;;;; Plain Text

(defvar t-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `t-encode-plain-text'.")

;;;; Src Block

(defcustom t-fontify-method 'engrave
  "Method to fontify code
- nil means no highlighting
- engrave means use a subset of engrave-face.el for code fontify

There was a support for highlight.js, but has been abandoned."
  :group 'org-export-w3ctr
  :type '(choice (const engrave) (const nil)))

;;;; Table

(defcustom t-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening and ending tags for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `t-table-use-header-tags-for-first-column'.
See also the variable `t-table-align-individual-fields'."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-table-data-tags '("<td%s>" . "</td>")
  "The opening and ending tags for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `t-table-align-individual-fields'."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-table-row-open-tag "<tr>"
  "The opening tag for table rows.

See `org-html-table-row-open-tag' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-table-row-close-tag "</tr>"
  "The closing tag for table rows.

See `org-html-table-row-close-tag' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-w3ctr
  :type 'boolean)

;;;; Template :: Generic

(defcustom t-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-coding-system 'utf-8
  "Coding system for HTML export."
  :group 'org-export-w3ctr
  :type 'coding-system)



(defvar t-metadata-timestamp-format "%Y-%m-%d %H:%M"
  "Format used for timestamps in preamble, postamble and metadata.

See `format-time-string' for more information on its components.")

;;;; Template :: Mathjax
(defvar t-mathjax-options
  '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    (scale 1.0)
    (align "center")
    (font "mathjax-modern")
    (overflow "overflow")
    (tags "ams")
    (indent "0em")
    (multlinewidth "85%")
    (tagindent ".8em")
    (tagside "right"))
  "Options for MathJax setup.

See `org-html-mathjax-options' for details")

(defvar t-mathjax-template
  "<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '%MULTLINEWIDTH'
      },
      tags: '%TAGS',
      tagSide: '%TAGSIDE',
      tagIndent: '%TAGINDENT'
    },
    chtml: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    svg: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    output: {
      font: '%FONT',
      displayOverflow: '%OVERFLOW'
    }
  };
</script>

<script
  id=\"MathJax-script\"
  async
  src=\"%PATH\">
</script>"
  "The MathJax template.  See also `org-html-mathjax-options'.")

;;;; Template :: Postamble

(defcustom t-postamble nil
  "Non-nil means insert a postamble in HTML export.

See `org-html-postamble' for more information"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-validation-link
  "<a href=\"https://validator.w3.org/check?uri=referer\">Validate</a>"
  "Link to HTML validation service."
  :group 'org-export-w3ctr
  :type 'string)

(defvar t-creator-string
  (format "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> %s (<a href=\"https://orgmode.org\">Org</a> mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the HTML document.
See `org-html-creator-string' for more information.")

;;;; Template :: Preamble

(defcustom t-preamble "\
<details open>
<summary>More details about this document</summary>
<dl>
<dt>Create Date:</dt> <dd>%d</dd>
<dt>Publish Date:</dt> <dd>%f</dd>
<dt>Update Date:</dt> <dd>%C</dd>
<dt>Creator:</dt> <dd>%c</dd>
<dt>License:</dt> <dd>This work is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a></dd>
</dl>
</details>
<hr>"
  "Non-nil means insert a preamble in HTML export.

See `org-html-preamble' for more information"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-link-left ""
  "Where should the \"left\" link of exported HTML pages lead?"
  :group 'org-export-w3ctr
  :type '(string :tag "File or URL"))

(defcustom t-link-lname "UP"
  "The left link's name"
  :group 'org-export-w3ctr
  :type '(string))

(defcustom t-link-right ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-w3ctr
  :type '(string :tag "File or URL"))

(defcustom t-link-rname "HOME"
  "The right link's name"
  :group 'org-export-w3ctr
  :type '(string))

(defcustom t-format-home/up-function #'t-format-home/up-default-function
  "function used for home/div formatting"
  :group 'org-export-w3ctr
  :type '(symbol))

;;;; Template :: Styles

(defcustom t-meta-tags #'t-meta-tags-default
  "Form that is used to produce meta tags in the HTML head.

Can be a list where each item is a list of arguments to be passed
to `t--build-meta-entry'.  Any nil items are ignored.

Also accept a function which gives such a list when called with a
single argument (INFO, a communication plist)."
  :group 'org-export-w3ctr
  :type '(choice
	  (repeat
	   (list (string :tag "Meta label")
		 (string :tag "label value")
		 (string :tag "Content value")))
	  function))

(defcustom t-head-include-default-style t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `t-style-default' and
should not be modified.  Use `t-head' to use your own
style information."
  :group 'org-export-w3ctr
  :type 'boolean)

(defvar t-head ""
  "Org-wide head definitions for exported HTML files.

See `org-html-head' for more information.")

;;;###autoload
(put 't-head 'safe-local-variable 'stringp)

(defcustom t-head-extra ""
  "More head information to add in the HTML output.

You can set this on a per-file basis using #+HTML_HEAD_EXTRA:,
or for publication projects using the :html-head-extra property."
  :group 'org-export-w3ctr
  :type 'string)
;;;###autoload
(put 't-head-extra 'safe-local-variable 'stringp)

;;;; Template :: Viewport

(defvar t-viewport '((width "device-width")
			(initial-scale "1")
			(minimum-scale "")
			(maximum-scale "")
			(user-scalable ""))
  "Viewport options for mobile-optimized sites.

See `org-html-viewport' for more infomation.
See the following site for a reference:
https://developer.mozilla.org/zh-CN/docs/Web/HTML/Viewport_meta_tag")

;;;; Some options added by include-yy
(defcustom t-use-babel nil
  "use babel or not when exporting.

This option will override `org-export-use-babel'"
  :group 'org-export-w3ctr
  :type '(boolean))

(defcustom t-back-to-top t
  "add back-to-top arrow at the end of html file"
  :group 'org-export-w3ctr
  :type '(boolean))

(defcustom t-headline-level 5
  "max level of export headline"
  :group 'org-export-w3ctr
  :type '(natnum))

(defcustom t-with-todo-keywords nil
  "Export headline with TODO keywords"
  :group 'org-export-w3ctr
  :type '(boolean))
(defcustom t-with-priority nil
  "Export headline with [#A] priority"
  :group 'org-export-w3ctr
  :type '(boolean))
(defcustom t-with-tags nil
  "Export headline with :a: tags"
  :group 'org-export-w3ctr
  :type '(boolean))

(defcustom t-timestamp-format '("%Y-%m-%d" . "%Y-%m-%d %H:%M")
  "Format used for timestamps in
See `format-time-string' for more information on its components."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-example-default-class "example"
  "default CSS class for example block, nil means no default class"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-language-string "zh-CN"
  "default HTML lang attribtue"
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-zeroth-section-tocname "Abstract"
  "default toc name of the zeroth section"
  :group 'org-export-w3ctr
  :type 'sexp)

;;; Internal Functions

(defun t-close-tag (tag attr _info)
  "Return close-tag for string TAG.
ATTR specifies additional attributes.  INFO is a property list
containing current export state."
  (concat "<" tag
	  (org-string-nw-p (concat " " attr))
	  ">"))

(defun t--make-attribute-string (attributes)
  "Return a list of attributes, as a string.
ATTRIBUTES is a plist where values are either strings or nil.  An
attribute with a nil value will be omitted from the result."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (replace-regexp-in-string
                             "\"" "&quot;" (t-encode-plain-text item))))
                 (setcar output (format "%s=\"%s\"" key value))))))))

(defun t--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (custom-id (and (eq type 'headline)
			 (org-element-property :CUSTOM_ID datum)))
	 (user-label
	  (or custom-id
	      (and (memq type '(radio-target target))
		   (let ((val (org-element-property :value datum)))
		     (when (string-match-p "^[a-zA-Z][a-zA-Z0-9-_]*$" val) val)))
	      (org-element-property :name datum)
	      (when-let* ((id (org-element-property :ID datum)))
		(concat t--id-attr-prefix id))
	      (t--get-headline-reference datum info))))
    (cond (user-label user-label)
	  ((and named-only ; no #+NAME: and not headline
		(not (memq type '(headline radio-target target))))
	   nil)
	  (t (org-export-get-reference datum info)))))

(defun t--get-headline-reference (datum info)
  "return a reference id for headline
if DATUM's type is not headline, return nil"
  (when (eq 'headline (org-element-type datum))
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
	  (let ((newid
		 (if-let* ((numbers (org-export-get-headline-number datum info)))
		     (concat "orgnh-" (mapconcat #'number-to-string numbers "."))
		   (format "orguh-%s" (cl-incf (plist-get info :html-headline-cnt))))))
	    (push (cons newid datum) cache)
	    (plist-put info :internal-references cache)
	    newid)))))

(defun t--wrap-image (contents _info &optional caption label class)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute.

Also, include-yy allows it to contain class, we can then use selectors
to specify the inner img styles [2024-04-13]"
  (format "\n<figure%s%s>\n%s%s</figure>"
	  (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
	  (if (org-string-nw-p class) (format " class=\"%s\"" class) "")
	  ;; Contents.
	  contents
	  ;; Caption.
	  (if (not (org-string-nw-p caption)) ""
	    (format "<figcaption>%s</figcaption>\n"
		    caption))))

(defun t--format-image (source attributes info &optional caller)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (when (eq caller 'link)
    (cl-remf attributes :id)
    (cl-remf attributes :class))
  (t-close-tag
   "img"
   (t--make-attribute-string
    (org-combine-plists
     (list :src source
           :alt (if (string-match-p
                     (concat "^" org-preview-latex-image-directory) source)
                    (t-encode-plain-text
                     (org-find-text-property-in-string 'org-latex-src source))
                  (file-name-nondirectory source)))
     (if (string= "svg" (file-name-extension source))
         (org-combine-plists '(:class "org-svg") attributes '(:fallback nil))
       attributes)))
   info))

(defun t--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a source or an example block."
  (let* ((code (car (org-export-unravel-code element)))
	 (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
	    (or (plist-get attr :width) 80)
	    (or (plist-get attr :height) (org-count-lines code))
	    code)))

(defun t--has-caption-p (element &optional _info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal' or
a value to `t-standalone-image-predicate'."
  (org-element-property :caption element))

(defun t--make-string (n string)
  "Build a string by concatenating N times STRING."
  (let (out) (dotimes (_ n out) (setq out (concat string out)))))

(defun t-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info)
    (`nil nil)
    (definitions
      (format
       (plist-get info :html-footnotes-section)
       "References"
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (definition)
	   (pcase definition
	     (`(,n ,label ,def)
	      (let* ((dt (format (plist-get info :html-footnote-format)
				 (or label n)))
		     (id (format "fn.%d" n))
		     (contents (org-trim (org-export-data def info))))
		(format "<dt id=\"%s\">%s</dt>\n<dd>\n%s\n</dd>"
			id dt contents)))))
	 definitions
	 "\n"))))))


;;; Template

(defun t-meta-tags-default (info)
  "A default value for `t-meta-tags'.

Generate a list items, each of which is a list of arguments that can
be passed to `t--build-meta-entry', to generate meta tags to be
included in the HTML head.

Use document's plist INFO to derive relevant information for the tags."
  (let ((author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       ;; Return raw Org syntax.
                       (and auth (org-element-interpret-data auth))))))
    (list
     (when (org-string-nw-p author)
       (list "name" "author" author))
     (when (org-string-nw-p (plist-get info :description))
       (list "name" "description"
             (plist-get info :description)))
     (when (org-string-nw-p (plist-get info :keywords))
       (list "name" "keywords" (plist-get info :keywords)))
     '("name" "generator" "Org Mode"))))

(defun t--build-meta-entry
    (label identity &optional content-format &rest content-formatters)
  "Build a meta tag using the provided information.

Construct <meta> tag of form <meta LABEL=\"IDENTITY\" />, or when CONTENT-FORMAT
is present: <meta LABEL=\"IDENTITY\" content=\"{content}\" />

Here {content} is determined by applying any CONTENT-FORMATTERS to the
CONTENT-FORMAT and encoding the result as plain text."
  (concat "<meta "
	  (format "%s=\"%s" label identity)
	  (when content-format
	    (concat "\" content=\""
		    (replace-regexp-in-string
		     "\"" "&quot;"
		     (t-encode-plain-text
		      (if content-formatters
			  (apply #'format content-format content-formatters)
			content-format)))))
	  "\">\n"))

(defun t--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let* ((title (t-plain-text
		 (org-element-interpret-data (plist-get info :title)) info))
	 ;; Set title to an invisible character instead of leaving it
	 ;; empty, which is invalid.
	 (title (if (org-string-nw-p title) title "&lrm;"))
	 (charset (or (and t-coding-system
			   (fboundp 'coding-system-get)
			   (symbol-name
			    (coding-system-get t-coding-system
					       'mime-charset)))
		      "iso-8859-1")))
    (concat
     (when (plist-get info :time-stamp-file)
       (format-time-string
	(concat "<!-- "
		(plist-get info :html-metadata-timestamp-format)
		" -->\n")))
     (t--build-meta-entry "charset" charset)
     (let ((viewport-options
	    (cl-remove-if-not (lambda (cell) (org-string-nw-p (cadr cell)))
			      (plist-get info :html-viewport))))
       (if viewport-options
	   (t--build-meta-entry "name" "viewport"
				(mapconcat
				 (lambda (elm)
                                   (format "%s=%s" (car elm) (cadr elm)))
				 viewport-options ", "))))

     (format "<title>%s</title>\n" title)
     (mapconcat
      (lambda (args) (apply #'t--build-meta-entry args))
      (delq nil (if (functionp t-meta-tags)
		    (funcall t-meta-tags info)
		  t-meta-tags))
      ""))))

(defun t--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (plist-get info :html-head-include-default-style)
      (org-element-normalize-string t-style-default)))))

(defun t--build-mathjax-config (info)
  "Insert the user setup into the mathjax template.
INFO is a plist used as a communication channel."
  (when (and (memq (plist-get info :with-latex) '(mathjax t))
             (org-element-map (plist-get info :parse-tree)
                 '(latex-fragment latex-environment) #'identity info t nil t))
    (let ((template (plist-get info :html-mathjax-template))
          (options (let ((options (plist-get info :html-mathjax-options)))
                     ;; If the user customized some legacy option, set
                     ;; the corresponding new option to nil, so that
                     ;; the legacy user choice overrides the default.
                     ;; Otherwise, the user did not set the legacy
                     ;; option, in which case still set the legacy
                     ;; option but to no value, so that the code can
                     ;; find its in-buffer value, if set.
                     `((,(if (plist-member options 'autonumber)
                             'tags 'autonumber)
                        nil)
                       (,(if (plist-member options 'linebreaks)
                             'overflow 'linebreaks)
                        nil)
                       ,@options)))
          (in-buffer (or (plist-get info :html-mathjax) "")))
      (dolist (e options (org-element-normalize-string template))
        (let ((symbol (car e))
              (value (nth 1 e)))
          (when (string-match (concat "\\<" (symbol-name symbol) ":")
                              in-buffer)
            (setq value
                  (car (split-string (substring in-buffer
                                                (match-end 0))))))
          (when value
            (pcase symbol
              (`font
               (when-let*
                   ((value-new
                     (pcase value
                       ("TeX" "mathjax-tex")
                       ("STIX-Web" "mathjax-stix2")
                       ("Asana-Math" "mathjax-asana")
                       ("Neo-Euler" "mathjax-euler")
                       ("Gyre-Pagella" "mathjax-pagella")
                       ("Gyre-Termes" "mathjax-termes")
                       ("Latin-Modern" "mathjax-modern"))))
                 (setq value value-new)))
              (`linebreaks
               (org-display-warning
                "Converting legacy MathJax option: linebreaks")
               (setq symbol 'overflow
                     value (if (string= value "true")
                               "linebreak"
                             "overflow")))
              (`scale
               (when (stringp value)
                 (let ((value-maybe (string-to-number value)))
                   (setq value
                         (if (= value-maybe 0)
                             (progn
                               (org-display-warning
                                (format "Non-numerical MathJax scale: %s"
                                        value))
                               1.0)
                           value-maybe))))
               (when (>= value 10)
                 (setq value
                       (let ((value-new (/ (float value) 100)))
                         (org-display-warning
                          (format "Converting legacy MathJax scale: %s to %s"
                                  value
                                  value-new))
                         value-new))))
              (`autonumber
               (org-display-warning
                "Converting legacy MathJax option: autonumber")
               (setq symbol 'tags
                     value (downcase value))))
            (while (string-match (format "\\(%%%s\\)[^A-Z]"
                                         (upcase (symbol-name symbol)))
                                 template)
              (setq template
                    (replace-match (format "%s" value)
                                   t
                                   t template 1)))))))))

(defun t-get-date (info &optional boundary)
  (let ((date (plist-get info :date)))
    (cond
     ((not date) nil)
     ((not (eq (org-element-type (car date)) 'timestamp)) nil)
     (t (t-timestamp (car date) nil info boundary)))))

(defun t-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(t-get-date info 'start))
      (?f . ,(t-get-date info 'end))
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
	      (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
	      (split-string (plist-get info :email)  ",+ *")
	      ", "))
      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file))))))
      (?v . ,(or (plist-get info :html-validation-link) "")))))

(defun t--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil.
TYPE is either `preamble' or `postamble', INFO is a plist used as a
communication channel."
  (let ((section (plist-get info (intern (format ":html-%s" type))))
	(spec (t-format-spec info)))
    (if section
      (let ((section-contents
	     (cond
	      ((functionp section) (funcall section info))
	      ((stringp section) (format-spec section spec))
	      ((symbolp section)
	       (if (fboundp section) (funcall section info)
		 (if (not (boundp section))
		     (error "pre/postamble not exist: %s" section)
		   (format-spec (symbol-value section) spec))))
	      (t ""))))
	(if (org-string-nw-p section-contents)
	    (org-element-normalize-string section-contents) ""))
      "")))

(defun t-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.

See `org-html-inner-template' for more information"
    (concat
     (plist-get info :html-zeroth-section-output)
     (when-let* ((depth (plist-get info :with-toc)))
       (t-toc depth info))
     "<main>\n"
     contents
     "</main>\n"
     (t-footnote-section info)))

(defun t-format-home/up-default-function (info)
  "format the home/div element"
  (let ((link-left  (org-trim (plist-get info :html-link-left)))
	(link-right (org-trim (plist-get info :html-link-right)))
	(link-lname (org-trim (plist-get info :html-link-lname)))
	(link-rname (org-trim (plist-get info :html-link-rname))))
    (if (and (not (string= link-lname "")) (not (string= link-rname ""))
	     (not (string= link-left "")) (not (string= link-right "")))
	(format "<nav id=\"home-and-up\">\n<a href=\"%s\">%s</a><a href=\"%s\">%s</a></nav>\n"
		link-left link-lname link-right link-rname)
      (cond
       ((and (not (string= link-lname "")) (not (string= link-left "")))
	(format "<nav id=\"home-and-up\">\n<a href=\"%s\">%s</a>\n</nav>\n"
		link-left link-lname))
       ((and (not (string= link-rname "")) (not (string= link-right "")))
	(format "<nav id=\"home-and-up\">\n<a href=\"%s\">%s</a>\n</nav>\n"
		link-right link-rname))))))

(defun t--build-title (info)
  (when (plist-get info :with-title)
    (let ((title (and (plist-get info :with-title)
		      (plist-get info :title)))
	  (subtitle (plist-get info :subtitle)))
      (when title
	(format
	 "<header>\n<h1 id=\"title\">%s</h1>\n<p id=\"w3c-state\">%s</p>\n</header>\n"
	 (org-export-data title info)
	 (if subtitle (org-export-data subtitle info) ""))))))

(defun t-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "<!DOCTYPE html>\n"
   (concat "<html"
	   (format " lang=\"%s\"" (plist-get info :language))
	   ">\n")
   "<head>\n"
   (t--build-meta-info info)
   (t--build-head info)
   (t--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   ;; home and up links
   (when-let* ((fun (plist-get info :html-format-home/up-function)))
     (funcall fun info))
   ;; title and preamble
   (format "<div class=\"head\">\n%s%s\n</div>\n"
	   (t--build-title info)
	   (t--build-pre/postamble 'preamble info))
   contents
   ;; back-to-top
   (when (plist-get info :html-back-to-top)
     t-back-to-top-arrow)
   ;; Postamble.
   (t--build-pre/postamble 'postamble info)
   ;; fixup.js here
   (plist-get info :html-fixup-js)
   ;; Closing document.
   "</body>\n\n</html>"))


;;;; Anchor

(defun t--anchor (id desc attributes _info)
  "Format a HTML anchor."
  (let* ((attributes (concat (and id (format " id=\"%s\"" id))
			     attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

;;;; Todo

(defun t--todo (todo info)
  "Format TODO keywords into HTML.
TODO is the keyword, as a string.
INFO is the info plist."
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    (or (plist-get info :html-todo-kwd-class-prefix) "")
	    (org-html-fix-class-name todo)
	    todo)))

;;;; Priority

(defun t--priority (priority _info)
  "Format a priority into HTML.
PRIORITY is the character code of the priority or nil.  INFO is
a plist containing export options."
  (and priority (format "<span class=\"priority\">[%c]</span>" priority)))

;;;; Tags

(defun t--tags (tags info)
  "Format TAGS into HTML.
INFO is a plist containing export options."
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat (plist-get info :html-tag-class-prefix)
			       (org-html-fix-class-name tag))
		       tag))
	     tags "&#xa0;"))))

;;;; src-block export backend

;; engrave src-block render code is steal from engrave-faces.el
;; see https://github.com/tecosaur/engrave-faces
;; To get CSS from current or specified theme, use
;; `engrave-faces-html-gen-stylesheet'

(defun org-w3ctr-faces-buffer (&optional in-buffer out-buffer)
  "Export the current buffer to HTML and return the output buffer.
If IN-BUFFER is not nil, use it instead of current buffer.
If OUT-BUFFER is not nil, it will be the output buffer and return value.

Make sure the current buffer is already fontified with `font-lock-ensure'"
  (let ((ibuf (or in-buffer (current-buffer)))
	(obuf (or out-buffer
		  (generate-new-buffer "*html*")))
	(completed nil))
    (with-current-buffer ibuf
      (unwind-protect
	  (let (next-change text)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (setq next-change (org-w3ctr-faces--next-change (point)))
	      (setq text (buffer-substring-no-properties (point) next-change))
	      (when (> (length text) 0)
		(princ (org-w3ctr-faces-transformer
			(get-text-property (point) 'face)
			text)
		       obuf))
	      (goto-char next-change)))
	(setq completed t)))
    (if (not completed)
	(if out-buffer t (kill-buffer obuf))
      obuf)))

(defun org-w3ctr-faces--next-change (pos &optional limit)
  "Find the next face change from POS up to LIMIT.

This function is lifted from htmlize.
This function is lifted from engrave-faces [2024-04-12]"
  (unless limit
    (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (org-w3ctr-faces--overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (org-w3ctr-faces--overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    ;; Additionally, we include the entire region that specifies the
    ;; `display' property.
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defun org-w3ctr-faces--overlay-faces-at (pos)
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

(defun org-w3ctr-faces-transformer (prop text)
  "Transform text to HTML code with CSS"
  (let ((protected-content (org-w3ctr-faces--protect-string text))
	(style (org-w3ctr-faces-get-style prop)))
    (if (string-match-p "\\`[\n[:space:]]+\\'" text) protected-content
      (if (not style) protected-content
	(concat "<span class=\"ef-"
		(plist-get (cdr style) :slug) "\">"
		protected-content "</span>")))))

(defun org-w3ctr-faces--protect-string (text)
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")) text)
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t))))

(defconst org-w3ctr-faces-style-plist
  '(;; faces.el --- excluding: bold, italic, bold-italic, underline, and some others
    (default :slug "D")
    (shadow  :slug "h")
    (success :slug "sc")
    (warning :slug "w")
    (error   :slug "e")
    ;; font-lock.el
    (font-lock-comment-face :slug "c")
    (font-lock-comment-delimiter-face :slug "cd")
    (font-lock-string-face :slug "s")
    (font-lock-doc-face :slug "d")
    (font-lock-doc-markup-face :slug "m")
    (font-lock-keyword-face :slug "k")
    (font-lock-builtin-face :slug "b")
    (font-lock-function-name-face :slug "f")
    (font-lock-variable-name-face :slug "v")
    (font-lock-type-face :slug "t")
    (font-lock-constant-face :slug "o")
    (font-lock-warning-face :slug "wr")
    (font-lock-negation-char-face :slug "nc")
    (font-lock-preprocessor-face :slug "pp")
    (font-lock-regexp-grouping-construct :slug "rc")
    (font-lock-regexp-grouping-backslash :slug "rb")
    ;; font for css
    (css-property :slug "f")
    (css-selector :slug "k")
    ))

(defun org-w3ctr-faces-get-style (prop)
  (cond
   ((null prop) nil)
   ((listp prop)
    (assoc (car prop) org-w3ctr-faces-style-plist))
   (t (assoc prop org-w3ctr-faces-style-plist))))

(defun t-faces-fontify-code (code lang)
  (setq lang (or (assoc-default lang org-src-lang-modes) lang))
  (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
    (cond
     ((not (functionp lang-mode))
      (format "<code class=\"src src-%s\">%s</code>" lang (t-encode-plain-text code)))
     (t
      (setq code
	    (let ((inhibit-read-only t))
	      (with-temp-buffer
		(let ((inbuf (current-buffer)))
		(funcall lang-mode)
		(insert code)
		(font-lock-ensure)
		(set-buffer-modified-p nil)
		(with-temp-buffer
		  (org-w3ctr-faces-buffer inbuf (current-buffer))
		  (buffer-string))))))
      (format "<code class=\"src src-%s\">%s</code>" lang code)))))

;;;; Src Code

(defun t-fontify-code (code lang)
  "Color the code.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (cond
   ((or (string= code "") (not lang) (not t-fontify-method))
    (format "<code>%s</code>" (t-encode-plain-text code)))
   ((eq t-fontify-method 'engrave)
    (t-faces-fontify-code code lang))
   (t (format "<code>%s</code>" (t-encode-plain-text code)))))

(defun t-format-src-block-code (element _info)
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info)))
    (let ((code (t-fontify-code code lang)))
      code)))


;;; Tables of Contents

(defun t-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (t--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth))))
    (when toc-entries
      (let* ((toc-entries
	      (if-let* ((zeroth-tocname (plist-get info :html-zeroth-section-tocname)))
		  (cons (cons (format "<a href=\"#abstract\">%s</a>" zeroth-tocname) 1) toc-entries)
		toc-entries))
	     (toc (t--toc-text toc-entries)))
	(concat "<nav id=\"toc\">\n"
		(let ((top-level (plist-get info :html-toplevel-hlevel)))
		  (format "<h%d id=\"table-of-contents\">%s</h%d>\n"
			  top-level "Table of Contents" top-level))
		toc
		"</nav>\n")))))

(defun t--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (car entry))
	      (level (cdr entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt))))
	     (setq prev-level level)
	     (concat
	      (t--make-string
	       times (cond ((> cnt 0) "\n<ul class=\"toc\">\n<li class=\"tocline\">")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "\n<ul class=\"toc\">\n<li class=\"tocline\">" "</li>\n<li class=\"tocline\">")))
	   headline)))
      toc-entries "")
     (t--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun t--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		(org-export-toc-entry-backend 'html)
		info)))
    (format "<a href=\"#%s\">%s</a>"
	    ;; Label.
	    (t--reference headline info)
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (format "<span class=\"secno\">%s</span>"
			  (mapconcat #'number-to-string headline-number ".")))
	     text))))


;;; Transcode Functions

;;;; Bold

(defun t-bold (_bold contents info)
  "Transcode BOLD from Org to HTML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'bold (plist-get info :html-text-markup-alist))) "%s")
	  contents))

;;;; Code

(defun t-code (code _contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'code (plist-get info :html-text-markup-alist))) "%s")
	  (t-encode-plain-text (org-element-property :value code))))

;;;; Headline

(defun t-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
	   (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
	   (todo-type (and todo (org-element-property :todo-type headline)))
	   (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
	   (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
	   (full-text (t--format-headline
                       todo todo-type priority text tags info))
           (contents (or contents ""))
	   (id (t--reference headline info)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (t-format-list-item
	      contents (if numberedp 'ordered 'unordered)
	      nil info nil
	      (concat (t--anchor id nil nil info) full-text))
	     "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let* ((extra-class
		(org-element-property :HTML_CONTAINER_CLASS headline))
	       (headline-class
		(org-element-property :HTML_HEADLINE_CLASS headline))
	       (secno (if (not numberedp) ""
			(mapconcat #'number-to-string numbers ".")))
	       (hd (concat (if (equal "" secno) ""
			     (format "<span class=\"secno\">%s. </span>" secno))
			   full-text)))
          (format "<%s id=\"%s\"%s>%s%s</%s>\n"
                  (t--container headline info)
		  id
		  (if (not extra-class) ""
		    (format " class=\"%s\"" extra-class))
		  (t--format-head-wrapper
		   (format "h%s" level)
		   id
		   (if (not headline-class) ""
		     (format " class=\"%s\"" headline-class))
		   secno hd info)
                  contents
                  (t--container headline info)))))))

(defun t--container (headline info)
  (or (org-element-property :HTML_CONTAINER headline)
      (if (<= (org-export-get-relative-level headline info) 5)
	  "section" "div")))

(defun t--format-headline (todo _todo-type priority text tags info)
  (let ((todo (t--todo todo info))
	(priority (t--priority priority info))
	(tags (t--tags tags info)))
    (concat todo (and todo " ")
	    priority (and priority " ")
	    text
	    (and tags "&#xa0;&#xa0;&#xa0;") tags)))

(defun t--format-head-wrapper (tag id cls secno headline info)
  (format
   (concat "<div class=\"header-wrapper\">\n"
	   "<%s id=\"x-%s\"%s>%s</%s>\n"
	   (when (plist-get info :html-self-link-headlines)
	     "<a class=\"self-link\" href=\"#%s\" aria-label=\"Permalink for Section %s\"></a>\n")
	   "</div>\n")
   tag id cls headline tag id secno))


;;;; Italic

(defun t-italic (_italic contents info)
  "Transcode ITALIC from Org to HTML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format
   (or (cdr (assq 'italic (plist-get info :html-text-markup-alist))) "%s")
   contents))

;;;; Link

(defun t-image-link-filter (data _backend info)
  "Process image links that are inside descriptions.
DATA is the parse tree.  INFO is and info plist.
See `org-export-insert-image-links' for more details."
  (org-export-insert-image-links data info t-inline-image-rules))

(defun t-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel.  LINK is an
inline image when it has no description and targets an image
file (see `t-inline-image-rules' for more information), or
if its description is a single link targeting an image file."
  (if (not (org-element-contents link))
      (org-export-inline-image-p
       link (plist-get info :html-inline-image-rules))
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
	   (cons 'plain-text org-element-all-objects)
	 (lambda (obj)
	   (pcase (org-element-type obj)
	     (`plain-text (org-string-nw-p obj))
	     (`link (if (= link-count 1) t
		      (cl-incf link-count)
		      (not (org-export-inline-image-p
			    obj (plist-get info :html-inline-image-rules)))))
	     (_ t)))
         info t)))))

(defvar t-standalone-image-predicate)
(defun t-standalone-image-p (element info)
  "Non-nil if ELEMENT is a standalone image.

INFO is a plist holding contextual information.

An element or object is a standalone image when

  - its type is `paragraph' and its sole content, save for white
    spaces, is a link that qualifies as an inline image;

  - its type is `link' and its containing paragraph has no other
    content save white spaces.

Bind `t-standalone-image-predicate' to constrain paragraph
further.  For example, to check for only captioned standalone
images, set it to:

  (lambda (paragraph) (org-element-property :caption paragraph))"
  (let ((paragraph (pcase (org-element-type element)
		     (`paragraph element)
		     (`link (org-export-get-parent element)))))
    (and (eq (org-element-type paragraph) 'paragraph)
	 (or (not (and (boundp 't-standalone-image-predicate)
                       (fboundp t-standalone-image-predicate)))
	     (funcall t-standalone-image-predicate paragraph))
	 (catch 'exit
	   (let ((link-count 0))
	     (org-element-map (org-element-contents paragraph)
		 (cons 'plain-text org-element-all-objects)
	       (lambda (obj)
		 (when (pcase (org-element-type obj)
			 (`plain-text (org-string-nw-p obj))
			 (`link (or (> (cl-incf link-count) 1)
				    (not (t-inline-image-p obj info))))
			 (_ t))
		   (throw 'exit nil)))
	       info nil 'link)
	     (= link-count 1))))))

(defun t-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
	 (dot (when (> (length html-ext) 0) "."))
	 (link-org-files-as-html-maybe
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
	    ;; needed.  See `t-link-org-files-as-html'.
	    (cond
	     ((and (plist-get info :html-link-org-files-as-html)
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) dot html-ext))
	     (t raw-path))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((string= "file" type)
	    ;; During publishing, turn absolute file names belonging
	    ;; to base directory into relative file names.  Otherwise,
	    ;; append "file" protocol to absolute file name.
	    (setq raw-path
		  (org-export-file-uri
		   (org-publish-file-relative-name raw-path info)))
	    ;; Maybe turn ".org" into ".html".
	    (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id, a headline title, a name or
	    ;; a target.
	    (let ((option (org-element-property :search-option link)))
	      (if (not option) raw-path
		(let ((path (org-element-property :path link)))
		  (concat raw-path
			  "#"
			  (org-publish-resolve-external-link option path t))))))
	   (t (url-encode-url (concat type ":" raw-path)))))
	 (attributes-plist
	  (org-combine-plists
	   ;; Extract attributes from parent's paragraph.  HACK: Only
	   ;; do this for the first link in parent (inner image link
	   ;; for inline images).  This is needed as long as
	   ;; attributes cannot be set on a per link basis.
	   (let* ((parent (org-export-get-parent-element link))
		  (link (let ((container (org-export-get-parent link)))
			  (if (and (eq 'link (org-element-type container))
				   (t-inline-image-p link info))
			      container
			    link))))
	     (and (eq link (org-element-map parent 'link #'identity info t))
		  (org-export-read-attribute :attr_html parent)))
	   ;; Also add attributes from link itself.  Currently, those
	   ;; need to be added programmatically before `t-link'
	   ;; is invoked, for example, by backends building upon HTML
	   ;; export.
	   (org-export-read-attribute :attr_html link)))
	 (attributes
	  (let ((attr (t--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'w3ctr info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (t--format-image path attributes-plist info 'link))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (t--reference destination info)
		  attributes
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ;; ID link points to an external file.
	  (`plain-text
	   (let ((fragment (concat t--id-attr-prefix path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  (`nil
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (`headline
	   (let ((href (t--reference destination info))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat #'number-to-string
				 (org-export-get-headline-number destination info)
				 ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc
			(org-export-data
			 (org-element-property :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (eq 'latex-environment (org-element-type destination))
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
	       ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (t--reference destination info))
             (let* ((ref (t--reference destination info))
                    (t-standalone-image-predicate
                     #'t--has-caption-p)
                    (counter-predicate
                     (if (eq 'latex-environment (org-element-type destination))
                         #'t--math-environment-p
                       #'t--has-caption-p))
                    (number
		     (cond
		      (desc nil)
		      ((t-standalone-image-p destination info)
		       (org-export-get-ordinal
			(org-element-map destination 'link #'identity info t)
			info 'link 't-standalone-image-p))
		      (t (org-export-get-ordinal
			  destination info nil counter-predicate))))
                    (desc
		     (cond (desc)
			   ((not number) "No description for this link")
			   ((numberp number) (number-to-string number))
			   (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (t-encode-plain-text path))))
	(format "<a href=\"#%s\" %s%s>%s</a>"
		fragment
		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			fragment fragment)
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
	      (t-encode-plain-text path)
	      attributes
	      desc))
     ;; External link without a description part.
     (path
      (let ((path (t-encode-plain-text path)))
	(format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

;;;; Plain Text

(defun t-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (dolist (a t-special-string-regexps string)
    (let ((re (car a))
	  (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defun t-encode-plain-text (text)
  "Convert plain text characters from TEXT to HTML equivalent.
Possible conversions are set in `t-protect-char-alist'."
  (dolist (pair t-protect-char-alist text)
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t))))

(defun t-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (t-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (t-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (t-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))

;;;; Section

(defun t-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent)
	;; the zeroth section
	(prog1 ""
	  (plist-put info :html-zeroth-section-output
		     (format "<div id=\"abstract\">\n%s</div>\n" (or contents ""))))
      (or contents ""))))

;;;; Radio Target

(defun t-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (let ((ref (t--reference radio-target info)))
    (t--anchor ref text nil info)))

;;;; Statistics Cookie

(defun t-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))

;;;; Strike-Through

(defun t-strike-through (_strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format
   (or (cdr (assq 'strike-through (plist-get info :html-text-markup-alist)))
       "%s")
   contents))

;;;; Subscript

(defun t-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))

;;;; Superscript

(defun t-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))

;;;; Table Cell

(defun t-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs ""))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "col" cell-attrs)
		contents
		(cdr header-tags))))
     ((and (plist-get info :html-table-use-header-tags-for-first-column)
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "row" cell-attrs)
		contents
		(cdr header-tags))))
     (t (let ((data-tags (plist-get info :html-table-data-tags)))
	  (concat "\n" (format (car data-tags) cell-attrs)
		  contents
		  (cdr data-tags)))))))

;;;; Table

(defun t-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun t-table--table.el-table (table _info)
  "Format table.el tables into HTML.
INFO is a plist used as a communication channel."
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer) )))))

;;;; Target

(defun t-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((ref (t--reference target info)))
    (t--anchor ref nil nil info)))

;;;; Timestamp

(defun t-timestamp (timestamp _contents info &optional boundary)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((type (org-element-property :type timestamp))
	 (fmt (let ((org-time-stamp-custom-formats (plist-get info :html-timestamp-format)))
		(org-time-stamp-format (org-timestamp-has-time-p timestamp)
				       (member type '(inactive inactive-range)) 'custom)))
	 (flag (org-timestamp-has-time-p timestamp))
	 (utcfmt (if (org-timestamp-has-time-p timestamp) "%Y-%m-%dT%H:%MZ" "%Y-%m-%d")))
    (if (eq type 'diary)
	(format "<time>%s</time>" (org-element-interpret-data timestamp))
      (pcase type
	((or `active `inactive (guard boundary))
	 (let* ((time (org-timestamp-to-time timestamp (eq boundary 'end)))
		(utc0 (format-time-string utcfmt time flag)))
	   (format "<time datetime=\"%s\">%s</time>"
		   utc0 (org-format-timestamp timestamp fmt (eq boundary 'end)))))
	((or `active-range `inactive-range)
	 (let* ((time1 (org-timestamp-to-time timestamp))
		(time2 (org-timestamp-to-time timestamp t))
		(utc1 (format-time-string utcfmt time1 flag))
		(utc2 (format-time-string utcfmt time2 flag)))
	   (concat (format "<time datetime=\"%s\">%s</time>"
			   utc1 (org-format-timestamp timestamp fmt))
		   "&#x2013;"
		   (format "<time datetime=\"%s\">%s</time>"
			   utc2 (org-format-timestamp timestamp fmt t)))))
	(_ (error "ox-w3ctr: Seems not a valid time type %s" type))))))

;;;; Underline

(defun t-underline (_underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'underline (plist-get info :html-text-markup-alist)))
	      "%s")
	  contents))

;;;; Verbatim

(defun t-verbatim (verbatim _contents info)
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'verbatim (plist-get info :html-text-markup-alist))) "%s")
	  (t-encode-plain-text (org-element-property :value verbatim))))


;;; Filter Functions

(defun t-final-function (contents _backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    (when (plist-get info :html-indent)
      (indent-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; End-user functions

;;;###autoload
(defun t-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

See `org-html-export-as-html' for more information."
  (interactive)
  (let ((org-export-use-babel t-use-babel))
    (org-export-to-buffer 'w3ctr "*Org w3ctr HTML Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (set-auto-mode t)))))

;;;###autoload
(defun t-convert-region-to-html ()
  "Assume the current region has Org syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an HTML buffer and use this command
to convert it."
  (interactive)
  (let ((org-export-use-babel t-use-babel))
    (org-export-replace-region-by 'w3ctr)))

;;;###autoload
(defun t-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

See `org-html-export-to-html' for more information."
  (interactive)
  (let* ((extension (concat
		     (when (> (length t-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 t-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system t-coding-system)
	 (org-export-use-babel t-use-babel)
	 )
    (org-export-to-file 'w3ctr file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun t-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((org-export-use-babel t-use-babel))
    (org-publish-org-to 'w3ctr filename
			(concat (when (> (length t-extension) 0) ".")
				(or (plist-get plist :html-extension)
				    t-extension
				    "html"))
			plist pub-dir)))

(provide 'ox-w3ctr)

;; Local variables:
;; read-symbol-shorthands: (("t-" . "org-w3ctr-"))
;; fill-column: 72
;; coding: utf-8-unix
;; End:

;;; ox-w3ctr.el ends here
