;;; ox-w3ctr.el --- An Org export Back-End -*- lexical-binding:t;-*-

;; Copyright (C) 2024-2025 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 2024-03-18 04:51:00+0900

;; Package-Version: 0.2.10
;; Package-Requires: ((emacs "31"))
;; Keywords: tools, html
;; URL: https://github.com/include-yy/ox-w3ctr

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Ox-w3ctr is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Ox-w3ctr is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Ox-w3ctr.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: [Comments need improvements]
;; This library implements a HTML back-end for Org generic exporter.
;; A parasitic implementation of ox-html.el

;; See:
;; - https://respec.org/docs/
;; - https://www.w3.org/StyleSheets/TR/2021/
;; - https://github.com/w3c/tr-design

;;; Code:

;;;; Dependencies
(require 'cl-lib)
(require 'map)
(require 'format-spec)
(require 'xml)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)
(require 'table)

;;;; Fundamental utilities
(defconst t-version "0.2.10"
  "The current version string of the ox-w3ctr package.")

(defconst t--dir
  (if (not load-in-progress) default-directory
    (file-name-directory load-file-name))
  "The root directory of the ox-w3ctr package.")

(define-error 't-error "ox-w3ctr-error")

;; FIXME: Replace all `error' calls with `org-w3ctr-error'.
(defun t-error (string &rest args)
  "Signal an `org-w3ctr-error' error.  STRING is formatted with ARGS."
  (declare (ftype (function (string &rest t) t)))
  (signal 't-error (list (apply #'format-message string args))))

;; A PRECONDITION OF THE WHOLE BACK-END
;; INFO is shared by identity with every transcoder, while callers discard
;; `plist-put''s return value (its docstring only promises that value), so
;; `plist-put' has to modify a *non-empty* plist in place — keeping the head
;; cell (setcar for an existing key, splicing for a missing one).  An empty
;; plist would need the return value; INFO is never empty here.
(unless (let ((p (list :probe nil)))
          (and (eq p (plist-put p :probe t))   ; existing key, in place
               (eq p (plist-put p :probe-2 t)) ; missing key, in place
               (eq (plist-get p :probe-2) t))) ; and the write is visible
  (t-error "`plist-put' does not modify a non-empty plist in place"))

;;;; Define Back-End
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
    (table . t-table)                           ; | | | \n | | |
    ;;@ lesser elements [17]
    ;; babel cell                               NO-EXIST
    ;; clock `clock'                            NO-USE
    ;; comments                                 NO-EXPORT
    ;; comment block                            NO-EXPORT
    ;; diary sexp `diary-sexp'                  NO-USE
    ;; node properties `node-property'          NO-USE
    ;; planning `planning'                      NO-USE
    (example-block . t-example-block)           ; #+begin_example
    (export-block . t-export-block)             ; #+begin_export
    (fixed-width . t-fixed-width)               ; ^: contents
    (horizontal-rule . t-horizontal-rule)       ; -----------
    (keyword . t-keyword)                       ; #+name: ...
    (latex-environment . t-latex-environment)   ; \begin
    (paragraph . t-paragraph)                   ; \n ... \n
    (src-block . t-src-block)                   ; #+begin_src lang
    (table-row . t-table-row)                   ; | |
    (verse-block . t-verse-block)               ; #+begin_verse
    ;;@ objects [25]
    ;; citation                                 NO-USE
    ;; citation reference                       NO-USE
    ;; inline babel calls                       NO-EXIST
    ;; macros                                   NO-EXIST
    (entity . t-entity)                         ; \alpha, \cent
    (export-snippet . t-export-snippet)         ; @@html:something@@
    (footnote-reference . t-footnote-reference) ; [fn:]
    (inline-src-block . t-inline-src-block)     ; src_LANG{body}
    (latex-fragment . t-latex-fragment)         ; \(, \[
    (line-break . t-line-break)                 ; \\
    (link . t-link)                             ; [[...][...]]
    (radio-target . t-radio-target)             ; <<<contents>>>
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
  '(;; Headline and Section
    (:html-todo-class nil nil t-todo-class)
    (:html-todo-kwd-class-prefix nil nil t-todo-kwd-class-prefix)
    (:html-priority-class nil nil t-priority-class)
    (:html-tag-class nil nil t-tag-class)
    (:html-format-headline-function nil nil t-format-headline-function)
    (:html-toplevel-hlevel nil nil t-toplevel-hlevel)
    (:html-honor-ox-headline-levels nil nil t-honor-ox-headline-levels)
    (:html-prefer-user-labels nil nil t-prefer-user-labels)
    (:html-container nil nil t-container-element)
    (:html-self-link-headlines nil nil t-self-link-headlines)
    (:html-zeroth-section-tocname nil "zeroth-name" t-zeroth-section-tocname)
    (:headline-levels nil "H" org-export-headline-levels)
    ;; Markup texts
    (:html-text-markup-alist nil nil t-text-markup-alist)
    ;; Item and Plain Lists
    (:html-checkbox-type nil nil t-checkbox-type)
    ;; Table
    (:html-table-use-header-tags-for-first-column
     nil nil t-table-use-header-tags-for-first-column)
    ;; LaTeX and Math
    (:with-latex nil "tex" t-with-latex)
    (:html-mathjax-config nil nil t-mathjax-config)
    (:html-math-head-function nil nil t-math-head-function)
    (:html-math-custom-render-function nil nil t-math-custom-render-function)
    (:html-equation-reference-format "HTML_EQUATION_REFERENCE_FORMAT"
     nil t-equation-reference-format)
    ;; Link and Images
    (:html-link-org-files-as-html nil nil t-link-org-files-as-html)
    (:html-inline-images nil nil t-inline-images)
    (:html-inline-image-rules nil nil t-inline-image-rules)
    (:html-link-home "HTML_LINK_HOME" nil t-link-home)
    (:html-link-up "HTML_LINK_UP" nil t-link-up)
    (:html-home/up-format nil nil t-home/up-format)
    (:html-link-navbar "HTML_LINK_NAVBAR" nil t-link-navbar parse)
    (:html-format-navbar-function nil nil t-format-navbar-function)
    ;; Footnote
    (:html-footnotes-section nil nil t-footnotes-section)
    (:html-footnote-format nil nil t-footnote-format)
    (:html-footnote-separator nil nil t-footnote-separator)
    (:html-footnote-section-function nil nil t-footnote-section-function)
    ;; Timestamp
    (:html-timezone "HTML_TIMEZONE" nil t-timezone)
    (:html-export-timezone "HTML_EXPORT_TIMEZONE" nil t-export-timezone)
    (:html-datetime-option nil "dt" t-datetime-format-choice)
    (:html-timestamp-option nil "ts" t-timestamp-option)
    (:html-timestamp-wrapper nil "tsw" t-timestamp-wrapper-type)
    (:html-timestamp-formats nil "tsf" t-timestamp-formats)
    (:html-timestamp-format-function nil "tsfn" t-timestamp-format-function)
    ;; Head and Template
    (:html-viewport nil nil t-viewport)
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:subtitle "SUBTITLE" nil nil parse)
    (:html-head-include-style nil "html-style" t-head-include-style)
    (:html-head "HTML_HEAD" nil t-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil t-head-extra newline)
    (:html-file-timestamp-function nil nil t-file-timestamp-function)
    (:html-metadata-timestamp-format nil nil t-metadata-timestamp-format)
    (:creator "CREATOR" nil t-creator-string)
    (:html-validation-link nil nil t-validation-link)
    (:html-toc-element nil nil t-toc-element)
    (:html-back-to-top nil "back-to-top" t-back-to-top)
    (:html-fixup-js "HTML_FIXUP_JS" nil t-fixup-js newline)
    (:html-extension nil nil t-extension)
    ;; Preamble and License
    (:html-use-cc-budget nil "cc-budget" t-use-cc-budget)
    (:html-license nil "license" t-public-license)
    (:html-format-license-function nil nil t-format-license-function)
    (:html-preamble nil "html-preamble" t-preamble)
    (:html-postamble nil "html-postamble" t-postamble)
    ;; Misc
    (:html-indent nil nil t-indent)))

;;; User Configuration Variables.

(defgroup org-export-w3ctr nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export W3CTR HTML"
  :group 'org-export)

;;;; Headline and Section
(defcustom t-todo-class "org-todo"
  "The CSS class for the `<span>' element wrapping a TODO keyword."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-todo-kwd-class-prefix "org-status-"
  "Prefix for CSS classes applied to TODO keywords.

The final class will be this prefix followed by the status
\(e.g., \"todo\" or \"done\").  For example, if a headline is a
TODO item, its class will be \"org-status-todo\" by default."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-priority-class "org-priority"
  "The CSS class for the `<span>' element wrapping a priority marker."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-tag-class "org-tag"
  "The CSS class for the `<span>' element wrapping all tags."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-format-headline-function
  #'t-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
- TODO      the todo keyword (string or nil).
- PRIORITY  the priority of the headline (integer or nil)
- TEXT      the main headline text (string).
- TAGS      the tags (list of string).
- INFO      the export options (plist).

The function should return the formatted HTML string for the headline."
  :group 'org-export-w3ctr
  :type 'function)

;; See `org-html-toplevel-hlevel' for more information.

(defcustom t-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export."
  :group 'org-export-w3ctr
  :type 'integer)

(defcustom t-honor-ox-headline-levels nil
  "Honor `org-export-headline-levels' or not."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-container-element "section"
  "The HTML tag name for the element that contains a headline.

Common values are \"section\" or \"div\".  If nil, \"div\" is used."
  :group 'org-export-w3ctr
  :type '(choice string (const nil)))

(defcustom t-self-link-headlines t
  "When non-nil, the headlines contain a hyperlink to themselves."
  :group 'org-export-w3ctr
  :type 'boolean
  :safe #'booleanp)

(defcustom t-prefer-user-labels nil
  "When non-nil, use user-defined NAME and ID over internal references.

By default, `org-w3ctr--reference' generates internal ID values
during export.  When this variable is non-nil, the NAME keyword
or the real name of a target is used as the ID attribute instead.

Regardless of this variable, CUSTOM_ID is always used as a
reference."
  :group 'org-export-w3ctr
  :type 'boolean
  :safe #'booleanp)

(defcustom t-zeroth-section-tocname "Abstract"
  "Default toc name of the zeroth section."
  :group 'org-export-w3ctr
  :type 'sexp)

;;;; Markup texts
(defcustom t-text-markup-alist
  ;; See also `org-html-text-markup-alist'.
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<s>%s</s>")
    (underline . "<u>%s</u>")
    (verbatim . "<code>%s</code>"))
  "Association list mapping Org markup types to HTML format strings.
Each element in the alist should have the form (TYPE . FORMAT).

TYPE is a symbol representing the markup, which must be one of
symbol `bold', symbol `code', symbol `italic',
symbol `strike-through', symbol `underline', or symbol `verbatim'.

FORMAT is a string used to wrap the text, where \"%s\" is
replaced by the text content itself.

If no entry exists for a given markup type, the text is exported
without any special HTML tags."
  :group 'org-export-w3ctr
  :type '(alist :key-type (symbol :tag "Markup type")
                :value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

;;;; Item and Plain Lists
(defcustom t-checkbox-type 'unicode
  "Specify the type of checkboxes for HTML export.

Possible values are:
- `unicode': Use Unicode symbols.
- `ascii'  : Use ASCII characters.
- `html'   : Use HTML <input> elements.

See `org-w3ctr-checkbox-types' for details."
  :group 'org-export-w3ctr
  :type '(choice (const :tag "Unicode symbols" unicode)
                 (const :tag "ASCII characters" ascii)
                 (const :tag "HTML <input> elements" html)))

;;;; Table
(defcustom t-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-w3ctr
  :type 'boolean)

;;;; LaTeX and Math
(defcustom t-with-latex 'mathjax
  "Control how LaTeX math expressions are processed in HTML export.

The value specifies the rendering method:
- `verbatim'         : Keep raw fragment
- `mathjax'          : Render math using MathJax (client-side)
- `mathml-by-mathjax': Convert to MathML markup using MathJax
- `svg-by-mathjax'   : Convert to inline SVG using MathJax
- `custom'           : Use custom option and function"
  :group 'org-export-w3ctr
  :type '(choice
          (const :tag "Keep raw fragment" verbatim)
          (const :tag "Use MathJax to display math" mathjax)
          (const :tag "Use MathJax to render mathML" mathml-by-mathjax)
          (const :tag "Use MathJax to render SVG" svg-by-mathjax)
          (const :tag "Use custom method" custom)))

(defcustom t-mathjax-config "\
<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '85%'
      },
      tags: 'ams',
      tagSide: 'right',
      tagIndent: '.8em'
    },
    chtml: {
      scale: 1.0,
      displayAlign: 'center',
      displayIndent: '0em'
    },
    svg: {
      scale: 1.0,
      displayAlign: 'center',
      displayIndent: '0em'
    },
    output: {
      font: 'mathjax-modern',
      displayOverflow: 'overflow'
    }
  };
</script>

<script
  id='MathJax-script'
  async
  src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'>
</script>"
  "Configuration for MathJax rendering in HTML export.
Used for MathJax rendering (:with-latex is set to `mathjax').

For detailed configuration options, see:
https://docs.mathjax.org/en/latest/options/index.html"
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-math-head-function #'t-math-head-default-function
  "Function returning the math setup to insert into <head>.
Called with the INFO plist; return a string (or nil)."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-math-custom-render-function
  #'t-math-custom-default-render-function
  "Function rendering a LaTeX fragment for the `custom' math mode.
It is called with FRAG, a LaTeX string, and the INFO plist, and
must return the HTML/MathML/SVG string for the fragment."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-equation-reference-format "\\eqref{%s}"
  "The command to use when referencing an equation.

A format control string expecting the label as its single
argument.  It is inserted verbatim, so it only makes sense with
client-side MathJax (`mathjax' mode), where \\eqref and \\ref are
resolved in the browser.

See `org-html-equation-reference-format' for more information."
  :group 'org-export-w3ctr
  :type 'string)

;;;; Link and Images
(defcustom t-link-org-files-as-html t
  "Non-nil means make file links to \"file.org\" point to \"file.html\".
When nil, the links still point to the plain \".org\" file.

See `org-html-link-org-files-as-html' for more information."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-inline-images t
  "Non-nil means inline images into exported HTML pages.
When nil, an anchor with href is used to link to the image."
  :group 'org-export-w3ctr
  :type 'boolean)

(defconst t-inline-image-extensions
  '(".jpeg" ".jpg" ".jfif" ".png" ".apng" ".gif" ".svg"
    ".webp" ".avif" ".jxl" ".bmp" ".ico")
  "Image file extensions that can be inlined into HTML.

These are the formats modern browsers render natively in an
`<img>' element.")

(defconst t-inline-image-path-regexp
  (concat (regexp-opt t-inline-image-extensions)
          "\\(?:[?#].*\\)?\\'")
  "Regexp matching a link path that points to an inlinable image.

The extension must end the path, optionally followed by a query
string or a fragment, so that e.g. \"img.png.txt\" is not mistaken
for an image.")

(defcustom t-inline-image-rules
  `(("file" . ,t-inline-image-path-regexp)
    ("http" . ,t-inline-image-path-regexp)
    ("https" . ,t-inline-image-path-regexp))
  "Rules characterizing image files that can be inlined into HTML.

See `org-html-inline-image-rules' for more information."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-link-home ""
  "URL for the `HOME' link in the legacy navigation bar.

Used as a fallback when `org-w3ctr-link-navbar' is not set."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-up ""
  "URL for the `UP' link in the legacy navigation bar.

Used as a fallback when `org-w3ctr-link-navbar' is not set."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-home/up-format
  "<nav id=\"navbar\">\n <a href=\"%s\"> UP </a>
 <a href=\"%s\"> HOME </a>\n</nav>"
  "Formatting string for the legacy home/up navigation bar.

The first %s is for the `UP' link, and the second for `HOME'."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-navbar nil
  "Navigation bar links.  Can be:
- A vector of (URL . NAME) pairs, e.g [(\"../index.html\" . \"Up\")]
- A list of Org elements (from HTML_LINK_NAVBAR)
- nil to use the legacy home/up behavior"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-format-navbar-function #'t-format-navbar-default-function
  "The function used to generate the HTML for the navbar.

This function is called with one argument: INFO plist.  It should
return a string containing the complete HTML for the navigation bar
\(e.g., inside `<nav>` tags).

See `org-w3ctr-format-navbar-default-function' for an example."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Footnote
(defcustom t-footnotes-section "<div id=\"references\">
<h2>%s</h2>
<dl>%s</dl>\n</div>\n"
  "Format for the footnotes section.
Should contain two instances of %s.  The first will be replaced with the
section heading (e.g. \"References\"), the second one with the footnote
definitions themselves."
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

(defcustom t-footnote-section-function #'t-footnote-section-default-function
  "Function used to build the footnotes section.

It is called with the list of footnote definitions, as returned by
`org-export-collect-footnote-definitions', and INFO; it should return
the complete HTML for the section.  See
`org-w3ctr-footnote-section-default-function' for an example."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Src Block
(defcustom t-fontify-method 'engrave
  "Method to fontify code.
- nil means no highlighting
- engrave means use a subset of engrave-face.el for code fontify

There was a support for highlight.js, but has been abandoned."
  :group 'org-export-w3ctr
  :type '(choice (const engrave) (const nil)))

;; FIXME: Unused — t-example-block hardcodes the "example" class and
;; never consults this defcustom.  Decide whether to wire it in or drop
;; it.
(defcustom t-example-default-class "example"
  "Default CSS class for example block, nil means no default class."
  :group 'org-export-w3ctr
  :type 'sexp)

;;;; Timestamp
(defconst t-timezone-regex
  (rx string-start
      (or "local"
          (seq
           (or "UTC" "GMT")
           (group
            (seq (or "+" "-")
                 (or (seq (? "0") num)
                     (seq "1" (any (?0 . ?2)))))))
          (group
           (seq
            (or "+" "-")
            (or (seq "0" num)
                (seq "1" (any (?0 . ?3))))
            (any (?0 . ?5))
            (any (?0 . ?9)))))
      string-end)
  "Regular expression for matching supported time zone designators.")

(defcustom t-timezone "local"
  "Specify the assumed time zone for timestamps in the source Org file.

This value is used as the base time zone when interpreting timestamps
from the file and generating datetime metadata for the export.  It
must be a string in one of the following formats:

- The keyword \"local\" for the system's local time zone.
- A four-digit offset from UTC, for example, \"+0800\" or \"-0500\".
- A UTC/GMT-based designator, for example, \"UTC+8\" or \"GMT-5\".

For more details on time zone formats, see IETF RFC 2822
\(URL `https://datatracker.ietf.org/doc/html/rfc2822') or
RFC 3339 (URL `https://datatracker.ietf.org/doc/html/rfc3339')."
  :group 'org-export-w3ctr
  :set (lambda (symbol value)
         (let ((case-fold-search t))
           (if (not (string-match-p t-timezone-regex value))
               (error "Invalid time zone designator: %s" value)
             (set symbol value))))
  :type 'string)

(defcustom t-export-timezone nil
  "Specify the target time zone for timestamps in the exported file.

If this is nil (the default), no time zone conversion occurs.
Timestamps are formatted based on the source time zone defined in
the option `org-w3ctr-timezone'.

When set to a string, this option enables time zone conversion.
The exporter uses the option `org-w3ctr-timezone' to correctly
interpret the source timestamp, then converts it to the time zone
specified by this variable for the final output.  The string must
follow the same format rules as the option `org-w3ctr-timezone'."
  :group 'org-export-w3ctr
  :set (lambda (symbol value)
         (when value
           (let ((case-fold-search t))
             (if (not (string-match-p t-timezone-regex value))
                 (error "Invalid time zone designator: %s" value))))
         (set symbol value))
  :type '(choice (const nil) string))

(defcustom t-datetime-format-choice 'T-none-zulu
  "Option for datetime attribute's format.

This option controls how timestamps are formatted when exporting
datetime attributes, with variations in:

Separator : Use `\s' or `T' between date and time.
Timezone  : Use `:' in zone offset or not (`+08:00' and `+0800').
UTC-Zulu  : Use a trailing `Z' when the timezone is UTC+0, or omit it."
  :group 'org-export-w3ctr
  :type '(radio (const s-none) (const s-none-zulu)
                (const s-colon) (const s-colon-zulu)
                (const T-none) (const T-none-zulu)
                (const T-colon) (const T-colon-zulu)))

(defcustom t-timestamp-option 'org
  "Option for ox-w3ctr timestamp export.

Possible values:

- raw: Use the timestamp's `:raw-value' property directly.
- int: Use `org-element-timestamp-interpreter' to format the timestamp.
- fmt: Like `int', but dynamically bind `org-timestamp-formats' to
       `org-w3ctr-timestamp-formats'.
- org: Behave like `org-html-timestamp', respecting both
       `org-display-custom-times' and `org-timestamp-custom-formats'.
- cus: Like `org', but behave as if `org-display-custom-times' is always
       non-nil and use `org-w3ctr-timestamp-formats' instead of
       `org-timestamp-custom-formats' for custom string output.
- fun: Use a user-supplied function to handle timestamp formatting."
  :group 'org-export-w3ctr
  :type '(choice (const raw) (const int) (const fmt)
                 (const org) (const cus) (const fun)))

(defcustom t-timestamp-wrapper-type 'span
  "The way to wrap timestamps with HTML tags during export.

Possible values:
- none: Export the plain timestamp string.
- span: Wrap the timestamp like `org-html-timestamp'.
- time: Wrap the timestamp inside a <time> element."
  :group 'org-export-w3ctr
  :type '(choice (const none) (const span) (const time)))

(defcustom t-timestamp-formats '("%F" . "%F %R")
  "Format specification used for exporting timestamps.

This option accepts a cons cell (DATE . DATE-TIME), where:
- DATE: format string for year/month/day (e.g., \"%Y-%m-%d\")
- DATE-TIME: date plus hours and minutes (e.g., \"%F %H:%M\")

These format strings follow the conventions of `format-time-string'.

*Note*: This option only takes effect when
`org-w3ctr-timestamp-option' is set to `fmt' or `cus'."
  :group 'org-export-w3ctr
  :type '(cons string string))

(defcustom t-timestamp-format-function #'t-ts-default-format-function
  "Custom timestamp format function."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Head and Template
(defcustom t-coding-system 'utf-8-unix
  "Coding system for HTML export.

UTF-8 is the de facto standard for modern web content.  The default
value `utf-8-unix' is strongly recommended and should not be changed
unless you have specific legacy system requirements."
  :group 'org-export-w3ctr
  :type '(radio (const utf-8-unix)
                (const utf-8-dos)
                (const utf-8-mac)))

(defcustom t-viewport '((width "device-width")
                        (initial-scale "1")
                        (minimum-scale "")
                        (maximum-scale "")
                        (user-scalable ""))
  "Viewport options for mobile-optimized sites.

The following values are recognized

width          Size of the viewport.
initial-scale  Zoom level when the page is first loaded.
minimum-scale  Minimum allowed zoom level.
maximum-scale  Maximum allowed zoom level.
user-scalable  Whether zoom can be changed.

The viewport meta tag is inserted if this variable is non-nil.

See the following site for a reference:
https://developer.mozilla.org/en-US/docs/Mozilla/Mobile/Viewport_meta_tag"
  :group 'org-export-w3ctr
  :type
  '(choice
    (const :tag "Disable" nil)
    (list :tag "Enable"
          (list :tag "Width of viewport"
                (const :format "             " width)
                (choice (const :tag "unset" "")
                        (string)))
          (list :tag "Initial scale"
                (const :format "             " initial-scale)
                (choice (const :tag "unset" "")
                        (string)))
          (list :tag "Minimum scale/zoom"
                (const :format "             " minimum-scale)
                (choice (const :tag "unset" "")
                        (string)))
          (list :tag "Maximum scale/zoom"
                (const :format "             " maximum-scale)
                (choice (const :tag "unset" "")
                        (string)))
          (list :tag "User scalable/zoomable"
                (const :format "             " user-scalable)
                (choice (const :tag "unset" "")
                        (const "true")
                        (const "false"))))))

(defcustom t-meta-tags #'t-meta-tags-default
  "Form that is used to produce <meta> tags in the HTML head.

This can be either:
- A list where each item is a list with the form of (NAME VALUE CONTENT)
  to be passed to `org-w3ctr--build-meta-entry'.  Any nil items are
  ignored.
- A function that takes the INFO plist as single argument and returns
  such a list of items."
  :group 'org-export-w3ctr
  :type '(choice
          (repeat (list (string :tag "Meta label")
                        (string :tag "label value")
                        (string :tag "Content value")))
          function))

(defcustom t-head-include-style t
  "Control whether to include CSS styles in the exported HTML.

When non-nil, the styles defined by `t-style' or loaded from
`t-style-file' will be embedded within a <style> tag in the HTML <head>."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-style nil
  "CSS rules to be embedded directly into the exported HTML.

When this string is not empty, it *takes precedence* over
`org-w3ctr-style-file'.

This variable is also used as a *cache* for styles loaded from
`org-w3ctr-style-file'.  If you modify the source file, you must clear
this cache (e.g., via the `org-w3ctr-clear-css' command) to see your
changes."
  :group 'org-export-w3ctr
  :type '(choice string (const nil)))

(defcustom t-style-file (file-name-concat t--dir "assets" "style.css")
  "Path to a CSS file to load styles from.

This path must be *absolute*.  This option is used as a fallback when
`org-w3ctr-style' is empty.

When you set a new file path here, the `org-w3ctr-style' cache is
automatically cleared to ensure the new file is loaded on the next
export.

The default value points to a `style.css' file inside the package's
`assets' directory."
  :group 'org-export-w3ctr
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (unless (and (stringp value)
                      (file-exists-p value)
                      (file-name-absolute-p value))
           (error "Not a valid default CSS file: %s" value))
         (set symbol value)
         ;; Refresh cached CSS string.
         (setq t-style ""))
  :type '(choice (const nil) file))

(defcustom t-head ""
  "Raw HTML content to insert into the <head> section.

This variable can contain the full HTML structure to provide a style,
including the surrounding HTML tags.  As the value of this option
simply gets inserted into the HTML <head> header, you can use it to
add any arbitrary text to the header.

You can set this on a per-file basis using #+HTML_HEAD:,
or for publication projects using the :html-head property."
  :group 'org-export-w3ctr
  :type 'string)
;;;###autoload
(put 't-head 'safe-local-variable 'stringp)

(defcustom t-head-extra ""
  "More head information to add in the <head> section.

You can set this on a per-file basis using #+HTML_HEAD_EXTRA:,
or for publication projects using the :html-head-extra property."
  :group 'org-export-w3ctr
  :type 'string)
;;;###autoload
(put 't-head-extra 'safe-local-variable 'stringp)

(defcustom t-file-timestamp-function #'t-file-timestamp-default-function
  "Function to generate timestamp for exported files at top place.

This function should take INFO as the only argument and return a
string representing the timestamp.

The default value is `org-w3ctr-file-timestamp-default', which generates
timestamps in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-metadata-timestamp-format "%Y-%m-%d %H:%M"
  "Formatting string used for timestamps in preamble and postamble.
See `format-time-string' for more information on its components."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-creator-string
  (format "<a href=\"https://www.gnu.org/software/emacs/\">\
Emacs</a> %s (<a href=\"https://orgmode.org\">Org</a> mode %s) \
<a href=\"https://github.com/include-yy/ox-w3ctr\">ox-w3ctr</a> %s"
          emacs-version
          (if (fboundp 'org-version) (org-version)
            "unknown version")
          t-version)
  ;; See also `org-html-creator-string'.
  "Information about the creator of the HTML document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-validation-link
  "<a href=\"https://validator.w3.org/check?uri=referer\">\
Validate</a>"
  "Link to HTML validation service."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-toc-element 'ul
  "List element of table of contents."
  :group 'org-export-w3ctr
  :type '(choice (const ul) (const ol)))

(defcustom t-back-to-top t
  "Add back-to-top arrow at the end of html file."
  :group 'org-export-w3ctr
  :type '(boolean))

(defcustom t-back-to-top-arrow
  "<p role=\"navigation\" id=\"back-to-top\">\
<a href=\"#title\"><abbr title=\"Back to Top\">↑\
</abbr></a></p>\n"
  "Add comments here."
  :group 'org-export-w3ctr
  :type 'string)

(defvar t-fixup-js ""
  "Js code that control toc's hide and show.")

(defcustom t-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-w3ctr
  :type 'string)

;;;; Preamble and License
(defcustom t-use-cc-budget t
  "Use CC budget or not."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-public-license nil
  "Default license for exported content.
Value should be one of the supported Creative Commons licenses
or variants."
  :group 'org-export-w3ctr
  :type '(choice
          (const nil) (const cc0)
          (const all-rights-reserved)
          (const all-rights-reversed)
          (const cc-by-4.0) (const cc-by-nc-4.0)
          (const cc-by-nc-nd-4.0) (const cc-by-nc-sa-4.0)
          (const cc-by-nd-4.0) (const cc-by-sa-4.0)
          (const cc-by-3.0) (const cc-by-nc-3.0)
          (const cc-by-nc-nd-3.0) (const cc-by-nc-sa-3.0)
          (const cc-by-nd-3.0) (const cc-by-sa-3.0)))

(defcustom t-format-license-function #'t-format-license-default-function
  "Default function to build license string."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-preamble #'t-preamble-default-function
  "Controls the insertion of a preamble in the exported HTML.

It can be one of the following types:
- string: The string will be formatted using `format-spec' and
  inserted.  See `org-w3ctr--pre/postamble-format-spec' for available
  format codes (e.g., %d, %c).
- function: The function is called with the INFO plist, and its return
  value is inserted.
- symbol: If the symbol is a function, it is called as above.
  Otherwise, its string value is retrieved and formatted.
- nil: No preamble is inserted."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

(defcustom t-postamble nil
  "Controls the insertion of a postamble in the exported HTML.

See `org-w3ctr-preamble' for more information."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

;;;; Misc
(defcustom t-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-w3ctr
  :type 'boolean)

(defcustom t-use-babel nil
  "Use babel or not when exporting.

This option will override `org-export-use-babel'"
  :group 'org-export-w3ctr
  :type '(boolean))
;;; Internal Variables

(defvar t--id-attr-prefix "ID-"
  "Prefix to use in ID attributes.
This affects IDs that are determined from the ID property.")

;; load default CSS from style.css
(defun t-update-css-js ()
  "Update ??? and `t-fixup-js'."
  (interactive)
  (setq t-fixup-js
        (let ((fname (file-name-concat t--dir "assets/fixup.js")))
          (format "<script>\n%s\n</script>\n"
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8))
                      (insert-file-contents fname))
                    (buffer-string))))))
;; do update
(t-update-css-js)

;;; Simple JSON based sync RPC, not JSONRPC
;; FIXME: Hand-rolled RPC from way back.  Consider migrating to the
;; built-in `jsonrpc.el', which provides the same JSON framing over
;; the same process transport.
(defvar t--rpc-timeout 1.0
  "Timeout for a rpc, in seconds.")
(defvar t--rpc-id 0
  "JSON-rpc ID for request.")

;; https://www.jsonrpc.org/specification
(defun t--rpc-make-json (func args)
  (json-serialize
   `( :jsonrpc "2.0" :method ,(format "%s" func)
      :params ,args  :id ,(incf t--rpc-id))))

(defun t--rpc-send (proc jstr)
  (process-send-string proc (concat jstr "\n")))

(defun t--rpc-call (proc func args)
  (t--rpc-send proc (t--rpc-make-json func args)))

(defun t--rpc-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; insert string
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      ;; find json data
      (when-let* ((curr (point))
                  (end (search-forward "\n" nil t)))
        (goto-char curr)
        (let* ((hash (json-parse-buffer)))
          (if (not (process-get proc 'debug))
              (delete-region curr end)
            (goto-char (point-max)))
          (set-marker (process-mark proc) (point))
          (throw 't--rpc hash))))))

(defun t--rpc-sentinel (proc _change)
  (when (not (process-live-p proc))
    (unless (process-get proc 'debug)
      (let* ((re " \\*ox-w3ctr-proc-\\[")
             (buf (process-buffer proc))
             (bufname (buffer-name buf)))
        (when (string-match-p re bufname)
          (kill-buffer buf))))))

(defun t--rpc-start (name cmd-list &optional debug)
  (let* ((buf (get-buffer-create
               (concat " *ox-w3ctr-proc-[" name "]*")))
         (proc (make-process
                :name name :buffer buf
                :command cmd-list :coding 'utf-8
                :noquery t :filter #'t--rpc-filter
                :sentinel #'t--rpc-sentinel)))
    (with-current-buffer buf
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (when debug (process-put proc 'debug t))
    proc))

(defun t--rpc-request-sync (proc fun args)
  (catch 't--rpc
    (t--rpc-call proc fun args)
    (let ((curr-time (float-time)))
      (while (< (- (float-time) curr-time) t--rpc-timeout)
        (accept-process-output nil 1))
      (error "ox-w3ctr RPC timeout: (%s %s)" fun args))))

(defun t--rpc-request! (proc fun args)
  (let* ((data (t--rpc-request-sync proc fun args)))
    (if-let* ((err (gethash "error" data)))
        (error "ox-w3ctr RPC error: %s %s %s"
               (gethash "code" err)
               (gethash "message" err)
               (gethash "data" err))
      (gethash "result" data))))

(defvar t--jstools-proc nil
  "js-tools process object.")
(defvar t-jstools-debug nil)

(defvar t--jstools-timeout 30000
  "default server side timeout, in milliseconds.")

(defun t-toggle-jstools-debug ()
  (interactive)
  (if t-jstools-debug
      (progn
        (setq t-jstools-debug nil)
        (message "ox-w3ctr: jstools debug disabled."))
    (setq t-jstools-debug t)
    (message "ox-w3ctr: jstools debug enabled.")))

(defun t--start-jstools ()
  (unless (process-live-p t--jstools-proc)
    (setq t--jstools-proc
          (t--rpc-start
           "jstools"
           `("node" ,(file-name-concat t--dir "jstools/index.js")
             "--timeout" ,(number-to-string t--jstools-timeout))
           t-jstools-debug))))

(defun t--restart-jstools ()
  (when (process-live-p t--jstools-proc)
    (delete-process t--jstools-proc))
  (t--start-jstools))

(defun t-launch-jstools ()
  (interactive)
  (t--restart-jstools))

(defun t--jstools-call (fun args)
  (t--start-jstools)
  (t--rpc-request! t--jstools-proc fun args))

;;; Basic utilities

;;;; OINFO oclosure
;; A lightweight caching system for property lookups within the INFO
;; plist used during Org export.

;; Each property marked for caching associates a dedicated oclosure,
;; which remembers the last INFO object and the corresponding property
;; value.  If a subsequent lookup uses the same INFO object, the cached
;; value is returned immediately, avoiding redundant `plist-get' calls.

;; An oclosure's `pid' and `val' are cleared at the end of a full export
;; (`org-w3ctr--oinfo-cleanup'); `org-w3ctr-oinfo-cleanup-before-export'
;; does the same at the start of an export, for those who add it as a hook.

;; OINFO ships on: `org-w3ctr-oinfo-enabled' (see there) is a build-time
;; switch for measuring and for checking the cache against the plain path,
;; not a release knob.  It is not part of the export semantics: both
;; builds must produce the same output.
;;
;; Reading order: `org-w3ctr--oinfo-cache-props' first — it lists the keys
;; the cache knows and the read/write discipline they require — then
;; `org-w3ctr--pget' / `org-w3ctr--pput', and finally
;; `org-w3ctr--oinfo-cache-alist', `org-w3ctr--oinfo-oclosure' and
;; `org-w3ctr--make-cache-oclosure'.
;; Statistics helpers (`org-w3ctr-collect-oinfo-statistics',
;; `org-w3ctr-clear-oinfo-statistics') at the end.

(eval-and-compile
  ;; The switch is read at definition time — when the file is compiled,
  ;; or on every evaluation of the buffer if it is interpreted — so change
  ;; it and recompile, or re-evaluate the whole buffer.
  (defvar t-oinfo-enabled t
    "Non-nil means use the OINFO cache for property lookups.

Nil makes `org-w3ctr--pget' and `org-w3ctr--pput' plain `plist-get'
and `plist-put' calls, with no cache and no oclosures; either way a
read returns the same value, only a write to a cached key differs.

It is t by default; the value this build actually uses is
`org-w3ctr--oinfo-cache-p'.  It stays on in releases too: the switch is
for measuring, and for comparing the cache against the plain path, not a
release knob.")

  ;; Decided once, when the file is compiled or evaluated; it cannot
  ;; change at run time.
  (defconst t--oinfo-cache-p (eval-when-compile (and t-oinfo-enabled t))
    "Non-nil if this build of ox-w3ctr uses the OINFO cache.")

  (oclosure-define t--oinfo
    "Caching oclosure for one property of an export INFO plist.

PID - The INFO plist the slots were filled from (compared with `eq').
KEY - The property keyword this oclosure caches.
VAL - The value of KEY in PID, or nil if it is absent.
CNT - How many times the oclosure has been called, hits and misses alike."
    (pid :mutable t :type list)
    (key :type symbol)
    (val :mutable t)
    (cnt :mutable t :type integer))

  (defun t--make-cache-oclosure (keyword)
    "Return a fresh caching oclosure for the property KEYWORD.

Call the oclosure with an INFO plist to get the value of KEYWORD in it;
it remembers the plist it last saw, so further calls with the same plist
skip the lookup.  It is only used while `org-w3ctr--oinfo-cache-p' is
non-nil: `org-w3ctr--pput' fills it in and `org-w3ctr--oinfo-cleanup'
empties it."
    (declare (ftype (function (symbol) function))
             (important-return-value t))
    (oclosure-lambda (t--oinfo (pid nil) (key keyword)
                               (val nil) (cnt 0))
        (info)
      (incf cnt)
      (if (eq pid info) val
        (setq pid info val (plist-get info key)))))

  (defun t--oinfo-oclosure (key)
    "Return the symbol whose function cell holds KEY's caching oclosure.

The name is `org-w3ctr--oinfo' followed by the keyword, as in
`org-w3ctr--oinfo:title'.  `org-w3ctr--oinfo-cache-alist' pairs each
cached property with the name this function returns for it, so that
`org-w3ctr--pget' — which is inlined, and may run compiled — reaches the
oclosure through that symbol.  KEY is a property keyword."
    (declare (ftype (function (symbol) symbol))
             (important-return-value t))
    (intern (concat "org-w3ctr--oinfo" (symbol-name key))))

  (defconst t--oinfo-cache-props
    '( :html-checkbox-type :html-text-markup-alist
       :with-smart-quotes :with-special-strings :preserve-breaks
       :html-timezone :html-export-timezone :html-datetime-option
       :html-timestamp-option :html-timestamp-wrapper
       :html-timestamp-formats :html-timestamp-format-function
       ;; headline and section
       :html-todo-kwd-class-prefix :html-todo-class :with-todo-keywords
       :html-priority-class :with-priority
       :with-tags :html-tag-class
       :html-format-headline-function :html-toplevel-hlevel
       :html-honor-ox-headline-levels
       ;; inner-template and template
       :with-author :author :title
       :time-stamp-file :html-file-timestamp-function :html-viewport
       :with-latex :html-mathjax-config
       :html-math-head-function :html-math-custom-render-function
       :html-use-cc-budget :html-license
       :html-format-license-function
       ;; link
       :html-extension :html-link-org-files-as-html
       :html-inline-images :html-inline-image-rules
       :html-equation-reference-format
       )
    "List of property keys the OINFO cache keeps an oclosure for.

Read and write every one of them through `org-w3ctr--pget' and
`org-w3ctr--pput', never `plist-get' or `plist-put': `plist-put' keeps
the plist object identical, so a write that bypasses the cache is
invisible to it.  A key that Org reads with `plist-get'
(`:with-latex', `:time-stamp-file', `:with-tags') must in particular
never be written with `org-w3ctr--pput'.  The test suite checks that every
key here is read through `org-w3ctr--pget', and that neither `plist-get'
nor `plist-put' reaches one of them by a literal key.

The cache notices a different plist object, not a change inside one: a
`plist-put' that keeps the plist's identity cannot invalidate it.")

  (defconst t--oinfo-cache-alist
    (static-when t--oinfo-cache-p
      (let (alist)
        (dolist (a t--oinfo-cache-props alist)
          (let ((fname (t--oinfo-oclosure a)))
            (fset fname (t--make-cache-oclosure a))
            (push (cons a fname) alist)))))
    "Alist of cached property keys to the names of their oclosures.

The cdr is a symbol, not the oclosure: `org-w3ctr--pget' inlines that
symbol into compiled code and reaches the oclosure through its function
cell.  Built at load time from `org-w3ctr--oinfo-cache-props', with the
names that `org-w3ctr--oinfo-oclosure' returns.

Nil when the cache is off, which makes every OINFO helper a no-op.")

  (define-inline t--pget (info prop)
    "Return the value of property PROP in the export INFO plist.

Like `plist-get', except that the keys listed in
`org-w3ctr--oinfo-cache-props' are read through a cache when
`org-w3ctr-oinfo-enabled' says there is one.  Nil means PROP is absent.

A key that `org-w3ctr--pput' wrote while the cache is on is read back from
the cache, so it can differ from what `plist-get' returns for that key."
    (static-if t--oinfo-cache-p
        (if-let* ((f (alist-get (inline-const-val prop)
                                t--oinfo-cache-alist)))
            (inline-quote (funcall #',f ,info))
          (inline-quote (plist-get ,info ,prop)))
      (inline-quote (plist-get ,info ,prop))))

  (define-inline t--pput (info prop value)
    "Set property PROP to VALUE in the export INFO plist and return VALUE.

For a key in `org-w3ctr--oinfo-cache-props' with the cache on, the
value goes into that key's oclosure, together with INFO, and the plist is
left untouched: later `org-w3ctr--pget' calls with the same INFO plist
return it, while Org and `plist-get' still see the old value.  Any other
PROP is written with `plist-put'.

Unlike `plist-put', return VALUE rather than the plist."
    (static-if t--oinfo-cache-p
        (if-let* ((f (alist-get (inline-const-val prop)
                                t--oinfo-cache-alist)))
            (inline-quote
             (let ((o (symbol-function #',f)))
               (setf (t--oinfo--pid o) ,info (t--oinfo--val o) ,value)))
          (inline-letevals (value)
            (inline-quote (prog1 ,value (plist-put ,info ,prop ,value)))))
      (inline-letevals (value)
        (inline-quote (prog1 ,value (plist-put ,info ,prop ,value)))))))

(defun t--oinfo-cleanup ()
  "Clear the cached value of every OINFO oclosure, releasing its INFO plist.

A finished export should not stay reachable through the oclosures that
cached it.  This is a memory measure, not an invalidation: an oclosure
compares the plist it is handed with the one it cached, so correctness
does not depend on being called.

Called at the end of a full export from `org-w3ctr-template'; the lookup
counters are left alone (see `org-w3ctr-clear-oinfo-statistics'), and with
the cache off this does nothing."
  (declare (ftype (function () null)))
  (map-do
   (lambda (_k v)
     (let ((o (symbol-function v)))
       (setf (t--oinfo--pid o) nil (t--oinfo--val o) nil)))
   t--oinfo-cache-alist))

(defun t-oinfo-cleanup-before-export (&rest _)
  "Clear the OINFO caches at the start of an export.

Add this function to `org-export-before-processing-functions' to have
every export start clean; it is not installed by default: a full export
clears the caches at its end, and it is the aborted or body-only export
that would leave them populated.  `org-export-as' runs that hook before it
transcodes anything, so the caches are already empty when the first
`org-w3ctr--pget' runs.  Any arguments it is called with (the backend
symbol) are ignored.

`org-w3ctr--oinfo-cleanup' runs only after a full transcode, so an export
aborted by an error — or a body-only export, which never reaches
`org-w3ctr-template' — would otherwise leave every oclosure holding the
dead INFO plist and its parse tree."
  (declare (ftype (function (&rest t) null)))
  (t--oinfo-cleanup))

(defun t-collect-oinfo-statistics ()
  "Display how often each cached OINFO key has been looked up.

Read the `cnt' slot of every oclosure in `org-w3ctr--oinfo-cache-alist',
sort the keys by lookup count, print them as (KEY CNT) in the buffer
*ox-w3ctr-oinfo* and display that buffer.

The counts begin when the file is loaded and keep growing across
exports; `org-w3ctr-clear-oinfo-statistics' zeroes them.

Interactive; useful for judging which keys are worth caching at all."
  (interactive)
  (let* ((buf (get-buffer-create "*ox-w3ctr-oinfo*"))
         (ls (mapcar
              (lambda (x) (let ((key (car x))
                                (o (symbol-function (cdr x))))
                            (cons key (t--oinfo--cnt o))))
              t--oinfo-cache-alist))
         (sorted (sort ls :key #'cdr :reverse t)))
    (with-current-buffer buf (erase-buffer))
    (pp sorted buf)
    (switch-to-buffer-other-window buf)))

(defun t-clear-oinfo-statistics ()
  "Clear the OINFO caches and reset their lookup counters.

Like `org-w3ctr--oinfo-cleanup', and additionally zero the
`cnt' slot of every oclosure, so that the figures from
`org-w3ctr-collect-oinfo-statistics' start again from zero.

Interactive; useful before a benchmark or a test run."
  (interactive)
  (map-do
   (lambda (_k v)
     (let ((o (symbol-function v)))
       (setf (t--oinfo--pid o) nil)
       (setf (t--oinfo--val o) nil)
       (setf (t--oinfo--cnt o) 0)))
   t--oinfo-cache-alist))

;;;; String helpers

(defsubst t--nw-p (s)
  "Return S if it is a string that has non-whitespace characters.
Otherwise, return nil."
  (declare (ftype (function (t) (or null string)))
           (pure t) (important-return-value t))
  ;; See `string-blank-p'
  (and (stringp s) (string-match-p "[^ \r\t\n]" s) s))

(defsubst t--2str (s)
  "Return S as a string, or nil if S is not a symbol, number, or string."
  (declare (ftype (function (t) (or null string)))
           (pure t) (important-return-value t))
  (cl-typecase s
    (null nil) (symbol (symbol-name s))
    (string s) (number (number-to-string s))
    (otherwise nil)))

(defsubst t--trim (s &optional keep-lead)
  "Remove whitespace from the beginning and end of string S.

This is a local, inlined copy of `org-trim'.

When the optional argument KEEP-LEAD is non-nil, removing blank
lines from the beginning of S will not affect the leading
indentation of the first line of content."
  (replace-regexp-in-string
   (if keep-lead "\\`\\([ \t]*\n\\)+" "\\`[ \t\n\r]+") ""
   (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

(defsubst t--nw-trim (s)
  "Trim S only if it is a non-empty, non-whitespace string.
Return nil otherwise."
  (and (t--nw-p s) (t--trim s)))

(defun t--prepend-newline (contents)
  "Prepend a newline to CONTENTS if it is a string.
Otherwise, return an empty string."
  (declare (ftype (function (t) string))
           (pure t) (important-return-value t))
  (if (stringp contents) (concat "\n" contents) ""))

(defun t--make-string (n string)
  "Return a new string by repeating STRING N times."
  (declare (ftype (function (integer string) string))
           (pure t) (important-return-value t))
  (if (and (> n 0) (not (string= string "")))
      (mapconcat #'identity (make-list n string)) ""))

;;;; HTML escaping

(defconst t--protect-char-alist
  '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))
  "Alist mapping HTML special characters to their entity strings.
Used by `org-w3ctr--encode-plain-text'.")

(defun t--encode-plain-text (text)
  "Escape `&', `<', `>' in TEXT for safe embedding in HTML content."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (dolist (pair t--protect-char-alist text)
    (setq text (replace-regexp-in-string
                (car pair) (cdr pair) text t t))))

(defconst t--protect-char-alist*
  '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
    ;; https://stackoverflow.com/a/2428595
    ("'" . "&apos;") ("\"" . "&quot;"))
  "Alist mapping HTML special characters to their entity strings,
including single and double quotes.

Used by `org-w3ctr--encode-plain-text*'.")

(defun t--encode-plain-text* (text)
  "Escape `&', `<', `>', `\=', `\"' in TEXT for safe use in HTML attributes."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (dolist (pair t--protect-char-alist* text)
    (setq text (replace-regexp-in-string
                (car pair) (cdr pair) text t t))))

;;;; HTML attributes

(defun t--read-attr (attribute element)
  "Read the property ATTRIBUTE from ELEMENT as a list of Lisp objects.
Return nil if the property does not exist, is empty, or whitespace-only.
Signal `org-w3ctr-error' if the value is not a valid Lisp s-expression."
  (declare (ftype (function (symbol t) list))
           (important-return-value t))
  (when-let* ((value (org-element-property attribute element))
              (str (t--nw-p (mapconcat #'identity value " "))))
    (let ((sstr (concat "(" str ")")))
      (condition-case nil (read sstr)
        (error (t-error "Invalid attribute #+%s: %s" attribute str))))))

(defun t--read-attr__ (element)
  "Parse the `:attr__' (#+attr__:) property from ELEMENT.
A vector such as [class1 class2] becomes (\"class\" \"class1 class2\");
an empty vector [] becomes nil.  Return nil if the property is absent."
  (declare (ftype (function (t) list))
           (important-return-value t))
  (when-let* ((attrs (t--read-attr :attr__ element)))
    (mapcar (lambda (x)
              ;; [] means "no class" — skip rather than emit class=""
              (cond ((not (vectorp x)) x)
                    ((equal x []) nil)
                    (t (list "class" (mapconcat #'t--2str x " ")))))
            attrs)))

(defun t--make-attr (list)
  "Format a single Lisp LIST into an HTML attribute string.

This low-level helper function converts a single list, LIST, into
its corresponding HTML attribute string.  It handles two formats:

- A boolean attribute: (ATTR) becomes \" ATTR\".
- An attribute with values: (ATTR VAL1 VAL2) becomes
  \" attr=\"VAL1VAL2...\".

The attribute name is lowercased, and its values are concatenated
without spaces.  All values are escaped for safety using
`org-w3ctr--encode-plain-text*'."
  (declare (ftype (function (list) (or string null)))
           (pure t) (important-return-value t))
  ;; (car nil) => nil
  (when-let* (((not (null list)))
              (name (t--2str (car list))))
    (if-let* ((rest (cdr list)))
        ;; use lowercase prop name; leading space for HTML tag separator.
        (concat
         " " (downcase name) "=\""
         (t--encode-plain-text* (mapconcat #'t--2str rest)) "\"")
      (concat " " (downcase name)))))

(defun t--make-attr__ (attributes)
  "Convert a list of attribute specifications into a single string.

This function takes a list, ATTRIBUTES, where each element
specifies one HTML attribute.  It calls `org-w3ctr--make-attr'
on each element and concatenates the results.

Each element in ATTRIBUTES can be an atom for a boolean attribute
(for example, `disabled') or a list for an attribute with a
value (for example, (id \"foo\") )."
  (declare (ftype (function (list) string))
           (pure t) (important-return-value t))
  (mapconcat (lambda (x) (t--make-attr (if (atom x) (list x) x)))
             attributes))

(defun t--make-attribute-string (attributes)
  "Format a property list into an HTML attribute string.

This function is a local copy of `org-html--make-attribute-string'.
It converts a property list, ATTRIBUTES, into a single string of
HTML attributes (for example, \\='id=\"foo\" class=\"bar\"\\=').

ATTRIBUTES should be a plist where keys are attribute names (as
keywords) and values are strings.  A key with a nil value will be
omitted from the result."
  (declare (ftype (function (list) string))
           (pure t) (important-return-value t))
  (let (output)
    (dolist ( item attributes
              (mapconcat 'identity (nreverse output) " "))
      (cond
       ((null item) (pop output))
       ((symbolp item) (push (substring (symbol-name item) 1) output))
       (t (let ((key (car output))
                (value (t--encode-plain-text* item)))
            (setcar output (format "%s=\"%s\"" key value))))))))

(defun t--make-attr__id (element info &optional named-only)
  "Format `:attr__' attributes, adding an `id' attribute if needed.

This function first reads and parses the `:attr__' property from
an ELEMENT.  Its main purpose is to then automatically add an `id'
attribute based on the element's reference, unless an `id' is
already explicitly defined in the property.

The final, combined list of attributes is then formatted into a
single string by `org-w3ctr--make-attr__'."
  (declare (ftype (function (t list &optional boolean) string))
           (important-return-value t))
  (let* ((reference (t--reference element info named-only))
         (attributes (t--read-attr__ element))
         (a (t--make-attr__
             (if (or (not reference)
                     (cl-find 'id attributes :key #'car-safe))
                 attributes
               (cons `("id" ,reference) attributes)))))
    (if (t--nw-p a) a "")))

(defun t--make-attr_html (element info &optional named-only)
  "Format attributes from `:attr_html', adding an `id' if needed.

This function processes the standard Org `:attr_html' property from
an ELEMENT.  Its main purpose is to automatically add an `id'
attribute based on the element's reference, unless an `id' is
already present in the property list.

The final property list is then formatted into a single string by
`org-w3ctr--make-attribute-string'."
  (declare (ftype (function (t list &optional boolean) string))
           (important-return-value t))
  (let* ((attrs (org-export-read-attribute :attr_html element))
         (reference (t--reference element info named-only))
         (a (t--make-attribute-string
             (if (or (not reference) (plist-member attrs :id))
                 attrs (plist-put attrs :id reference)))))
    (if (t--nw-p a) (concat " " a) "")))

(defun t--make-attr__id* (element info &optional named-only)
  "Format attributes, using `:attr__' with a fallback to `:attr_html'.

This is the main function for generating an element's complete
attribute string.  It first checks for the custom `:attr__'
property and processes it with `org-w3ctr--make-attr__id'.

If `:attr__' is not found, it falls back to processing the
standard `:attr_html' property using `org-w3ctr--make-attr_html'."
  (declare (ftype (function (t list &optional boolean) string))
           (important-return-value t))
  ;; `#+attr__:' takes priority even when empty — its presence alone
  ;; means "use ox-w3ctr syntax", so `#+attr_html:' is ignored.
  (if (org-element-property :attr__ element)
      (t--make-attr__id element info named-only)
    (t--make-attr_html element info named-only)))

;;;; File and regexp

(defun t--load-file (file)
  "Read the entire contents of FILE into a string.

This function returns the full content of the file at path FILE
as a single string.  It signals a `org-w3ctr-error' if FILE does not
exist or is a directory.

FILE is decoded as UTF-8 regardless of the locale coding system,
so the same file reads identically on every machine."
  (declare (ftype (function (string) string)))
  (unless (and (file-exists-p file) (not (file-directory-p file)))
    (t-error "Bad File: %s" file))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun t--insert-file (file)
  "Insert the contents of FILE at point in the current buffer.

This function uses `insert-file-contents-literally' to place
the full contents of FILE into the current buffer.  It signals
a `org-w3ctr-error' if FILE does not exist or is a directory."
  (declare (ftype (function (string) t)))
  (unless (and (file-exists-p file) (not (file-directory-p file)))
    (t-error "Bad File: %s" file))
  (insert-file-contents-literally file))

(defun t--find-all (regexp str &optional start)
  "Return a list of all non-overlapping matches for REGEXP in STR.

The search begins at position START, which defaults to the
beginning of the string.  This function returns a list of all
substrings that completely match REGEXP.

For example:
  (org-w3ctr--find-all \"[a-z]+\" \"1a-b2-cde\")
  => (\"a\" \"b\" \"cde\")

If no matches are found, or if REGEXP is an empty string, this
function returns nil."
  (declare (ftype (function (string string &optional (or null fixnum))
                            list))
           (pure t) (important-return-value t))
  (if (string= regexp "") nil
    (let ((pos (max (or start 0) 0))
          (matches))
      (while (string-match regexp str pos)
        (push (match-string 0 str) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

;;;; S-exp rendering

;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element
(defconst t--void-element-regexp
  (rx string-start
      (or "area" "base" "br" "col" "embed" "hr"
          "img" "input" "link" "meta" "param"
          "source" "track" "wbr")
      string-end)
  "A regular expression that matches HTML void elements.

Void elements, also known as self-closing or empty tags, are
elements in HTML that cannot have any child nodes.  Therefore,
they do not require a closing tag.  This regexp is used to
identify such tags during HTML generation.")

(defun t--void-element (tag attrs)
  "Return a void element string for TAG with ATTRS.

TAG is the element name, as a string.  ATTRS is a string of
pre-formatted attributes, with or without surrounding whitespace,
or nil.  Void elements have no closing tag, so the result has the
form \"<TAG ...>\"."
  (declare (ftype (function (string (or null string)) string))
           (pure t) (important-return-value t))
  (let ((attrs (t--trim (or attrs ""))))
    (format "<%s%s>" tag (if (t--nw-p attrs) (concat " " attrs) ""))))

(defun t--sexp2html (data)
  "Recursively convert an S-expression, DATA, into an HTML string.

This function translates a Lisp S-expression into its HTML
representation.  The expected format is:

  (TAG-SYMBOL ATTRIBUTE-LIST ...CHILDREN)

- TAG-SYMBOL: A symbol for the HTML tag (for example, `p', `div').
  It is automatically converted to lowercase.
- ATTRIBUTE-LIST: A list of attribute specifications suitable for
  `org-w3ctr--make-attr__'.  Use nil or an empty list for no attributes.
- CHILDREN: Zero or more child elements, which are recursively
  converted.  Children can be other S-expressions, strings, or numbers.

For example, the expression (p ((class \"foo\")) \"Hello\") is
converted to \"<p class=\\\"foo\\\">Hello</p>\".

The function correctly handles void elements (like `br') and
sanitizes string content using `org-w3ctr--encode-plain-text'."
  (declare (ftype (function (t) string))
           (pure t) (important-return-value t))
  (cl-typecase data
    (null "")
    ((or symbol string number)
     (t--encode-plain-text (t--2str data)))
    (list
     ;; always use lowercase tagname.
     (let* ((tag (downcase (t--2str (nth 0 data))))
            (attr-ls (nth 1 data))
            (attrs (if (booleanp attr-ls) ""
                     (mapconcat #'t--make-attr attr-ls))))
       (if (string-match-p t--void-element-regexp tag)
           (t--void-element tag attrs)
         (let ((children (mapconcat #'t--sexp2html (cddr data))))
           (format "<%s%s>%s</%s>"
                   tag attrs children tag)))))
    (otherwise "")))

;;;; References

(defun t--target-reference (datum)
  "Return the value of a target or radio-target as a reference string.
Return nil if DATUM is not a target type, or if the value does
not look like a valid HTML identifier."
  (declare (ftype (function (t) (or null string)))
           (pure t) (important-return-value t))
  (when (memq (org-element-type datum) '(radio-target target))
    (let ((val (org-element-property :value datum)))
      (when (string-match-p "^[a-zA-Z][a-zA-Z0-9-_]*$" val)
        val))))

(defun t--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to radio targets and targets."
  (let ((type (org-element-type datum)))
    (cond
     ;; CUSTOM_ID always wins.
     ((and (eq type 'headline)
           (org-element-property :CUSTOM_ID datum)))
     ;; Radio/target value (if it looks like a valid identifier).
     ((t--target-reference datum))
     ;; NAME keyword — only when prefer-user-labels is on.
     ((and (t--pget info :html-prefer-user-labels)
           (org-element-property :name datum)))
     ;; ID property — only when prefer-user-labels is on.
     ;; In practice `:ID' comes from `org-id' on headlines.
     ((and (t--pget info :html-prefer-user-labels)
           (when-let* ((id (org-element-property :ID datum)))
             (concat t--id-attr-prefix id))))
     ;; No #+NAME: and not a target → skip.
     ((and named-only
           (not (memq type '(radio-target target))))
      nil)
     ;; Fallback: random orgXXXXXXX.
     (t (org-export-get-reference datum info)))))

;;; Greater elements
;; special-block and table are not here.

;;;; Center Block

;; See (info "(org)Paragraphs")
;; `<center>' was deprecated in HTML5; use `<div>' with inline style.
;; When the user provides `#+attr__:' or `#+attr_html:', the block
;; becomes a generic `<div>' -- the centering style is dropped and
;; the user takes full control of attributes.
(defun t-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.

CONTENTS holds the contents of the block.  INFO is the info plist.
Without user attributes, center the contents with an inline style.
With user attributes, drop the centering style and let the user
control all attributes.  Return the formatted <div> element as a
string."
  (declare (ftype (function (t (or null string) t) string))
           (important-return-value t))
  (let* ((has-user-attrs (or (org-element-property :attr__ center-block)
                             (org-element-property :attr_html center-block)))
         (attrs (t--make-attr__id* center-block info t)))
    (format "<div%s%s>%s</div>"
            (if (t--nw-p attrs) attrs "")
            (if has-user-attrs "" " style=\"text-align:center;\"")
            (t--prepend-newline contents))))

;;;; Drawer

;; See (info "(org)Drawers")
;; `<details>' is the semantic HTML5 element for collapsible content.
;; Supports `#+attr__:' / `#+attr_html:' for custom attributes.
;; Caption becomes the `<summary>' text; falls back to drawer name.
(defun t-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.

CONTENTS holds the contents of the block.  INFO is the info plist.
Return the formatted <details> element as a string.  The caption
becomes the <summary> text; falls back to the drawer name."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((name (org-element-property :drawer-name drawer))
         (cap (if-let* ((cap (org-export-get-caption drawer))
                        (exp (t--nw-p (org-export-data cap info))))
                  exp name))
         (attrs (t--make-attr__id* drawer info t)))
    (format "<details%s><summary>%s</summary>%s</details>"
            attrs cap (t--prepend-newline contents))))

;;;; Dynamic Block

;; See (info "(org)Dynamic Blocks")
;; Org-internal extension mechanism; exported as-is.
(defun t-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block."
  (declare (ftype (function (t (or null string) t) string))
           (pure t) (important-return-value t))
  (or contents ""))

;;;; Item and Plain Lists helper functions

;; See (info "(org)Plain lists")
;; Options:
;; - :html-checkbox-type (`org-w3ctr-checkbox-type')
(defconst t-checkbox-types
  '(( unicode .
      ((on . "&#x2611;")
       (off . "&#x2610;")
       (trans . "&#x2612;")))
    ( ascii .
      ((on . "<code>[X]</code>")
       (off . "<code>[&#xa0;]</code>")
       (trans . "<code>[-]</code>")))
    ( html .
      ((on . "<input type=\"checkbox\" checked>")
       (off . "<input type=\"checkbox\">")
       (trans . "<input type=\"checkbox\">"))))
  "Alist of checkbox types.
The cdr of each entry is an alist of three checkbox states for
HTML export: `on', `off' and `trans'.

Choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes")

;; See (info "(org)Checkboxes")
(defun t--checkbox (checkbox info)
  "Format CHECKBOX into HTML.

CHECKBOX is nil or one of the symbols `on', `off', or `trans'.
INFO is the info plist.  See `org-w3ctr-checkbox-types' for the
customization options.  Return nil when CHECKBOX does not match one
of those three."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (cdr (assq checkbox
             (cdr (assq (t--pget info :html-checkbox-type)
                        t-checkbox-types)))))

(defsubst t--format-checkbox (checkbox info)
  "Format CHECKBOX into HTML, followed by a space.

CHECKBOX is nil or one of the symbols `on', `off', or `trans'.
INFO is the info plist.  Return an empty string when CHECKBOX does
not match one of those three; otherwise return the checkbox HTML
with a trailing space."
  (let ((a (t--checkbox checkbox info)))
    (concat a (and a " "))))

(defun t--format-ordered-item (contents checkbox info cnt)
  "Format an ordered list item into HTML.

CONTENTS is the item contents, nil or a string.  CHECKBOX is nil or
one of the symbols `on', `off', or `trans'.  INFO is the info plist.
CNT is the list item counter, an integer or nil; when non-nil, the
<li> element carries a value attribute with that number.  Return the
formatted <li> element as a string."
  (declare (ftype (function ((or null string) t list (or null integer)) string))
           (important-return-value t))
  (let ((checkbox (t--format-checkbox checkbox info))
        (counter (if (not cnt) "" (format " value=\"%s\"" cnt))))
    (concat (format "<li%s>" counter) checkbox
            (t--nw-trim contents) "</li>")))

(defun t--format-unordered-item (contents checkbox info)
  "Format an unordered list item into HTML.

CONTENTS is the item contents, nil or a string.  CHECKBOX is nil or
one of the symbols `on', `off', or `trans'.  INFO is the info plist.
Return the formatted <li> element as a string."
  (declare (ftype (function ((or null string) t list) string))
           (important-return-value t))
  (let ((checkbox (t--format-checkbox checkbox info)))
    (concat "<li>" checkbox (t--nw-trim contents) "</li>")))

(defun t--format-descriptive-item (contents checkbox info term)
  "Format a descriptive list item into HTML.

CONTENTS is the item contents, nil or a string.  CHECKBOX is nil or
one of the symbols `on', `off', or `trans'.  INFO is the info plist.
TERM is the item tag, nil or an exported string; it becomes the
<dt> content, possibly prefixed by the checkbox.  Return the
formatted <dt>...</dt><dd>...</dd> pair as a string."
  (declare (ftype (function ((or null string) t list (or null string)) string))
           (important-return-value t))
  (let ((checkbox (t--format-checkbox checkbox info))
        (term (or term "")))
    (concat (format "<dt>%s</dt>" (concat checkbox term))
            "<dd>" (t--nw-trim contents) "</dd>")))

;;;; Item

;; See (info "(org)Plain Lists")
(defun t-item (item contents info)
  "Transcode an ITEM element from Org to HTML.

CONTENTS holds the contents of the item, nil or a string.  INFO is
the info plist.  Return the formatted item as a string."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((plain-list (org-element-parent item))
         (type (org-element-property :type plain-list))
         (checkbox (org-element-property :checkbox item)))
    (pcase type
      ('ordered
       (let ((counter (org-element-property :counter item)))
         (t--format-ordered-item contents checkbox info counter)))
      ('unordered
       (t--format-unordered-item contents checkbox info))
      ('descriptive
       (let ((term (when-let* ((a (org-element-property :tag item)))
                     (org-export-data a info))))
         (t--format-descriptive-item contents checkbox info term)))
      (_ (t-error "Unrecognized list item type: %s" type)))))

;;;; Plain List

;; See (info "(org)Plain Lists")
(defun t-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.

CONTENTS is the contents of the list.  INFO is the info plist.
Return the formatted <ol>, <ul>, or <dl> element as a string."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol") (`unordered "ul") (`descriptive "dl")
                 (other (t-error "Unknown HTML list type: %s" other))))
         (attributes (t--make-attr__id* plain-list info t)))
    (format "<%s%s>\n%s</%s>" type attributes contents type)))

;;;; Quote Block

;; See (info "(org)Paragraphs")
(defun t-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.

CONTENTS holds the contents of the block.  INFO is the info plist.
Return the formatted <blockquote> element as a string."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (format "<blockquote%s>%s</blockquote>"
          (t--make-attr__id* quote-block info t)
          (t--prepend-newline contents)))

;;; Lesser elements
;; latex-environment, src-block, and table-row are not here.

;;;; Example Block

;; See (info "(org)Literal Examples")
;; The W3C stylesheet uses `.example' for numbered example boxes with
;; `::before' pseudo-elements; the class is added automatically unless
;; the user provides `#+attr__:' or `#+attr_html:', in which case the
;; user controls all attributes.  A bare `<pre>' without a wrapper is
;; available via `org-w3ctr-fixed-width'.
(defun t-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the formatted
<div><pre>...</pre></div> element as a string.  Without user
attributes, the <div> carries class=\"example\"; with user
attributes, the user controls all attributes on the <div>."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let* ((has-user-attrs (or (org-element-property :attr__ example-block)
                             (org-element-property :attr_html example-block)))
         (attrs (t--make-attr__id* example-block info t))
         (content (org-remove-indentation
                   (org-element-property :value example-block))))
    (format "<div%s%s>\n<pre>\n%s</pre>\n</div>"
            (if (t--nw-p attrs) attrs "")
            (if has-user-attrs "" " class=\"example\"")
            content)))

;;;; Export Block

(defun t--eval-lisp (element value type default context)
  "Read VALUE as Lisp and return the result as a string.

TYPE is \\='eval to read, eval, and convert via `org-w3ctr--2str',
or \\='sexp to read and convert via `org-w3ctr--sexp2html'.  When
VALUE is empty or whitespace-only, DEFAULT is used instead.
Signal `org-w3ctr-error' with the line number from ELEMENT on
read or eval failure; CONTEXT labels the error message."
  (declare (ftype (function (t string symbol string string) string))
           (important-return-value t))
  (if (not (t--nw-p value)) ""
    (let ((proc (pcase type ('eval #'eval) ('sexp nil)))
          (s (or (t--nw-p value) default))
          (line (line-number-at-pos
                 (org-element-property :begin element))))
      (or (handler-bind
              ((error (lambda (err)
                        (t-error "%s at line %d: %s"
                                 context line
                                 (error-message-string err)))))
            (let ((data (read s)))
              (if proc (t--2str (funcall proc data))
                (t--sexp2html data))))
          ""))))

;; See (info "(org) Quoting HTML tags")
(defun t-export-block (export-block _contents _info)
  "Transcode an EXPORT-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the exported
content as a string, or an empty string for unsupported types."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let* ((type (org-element-property :type export-block))
         (value (or (org-element-property :value export-block) "")))
    (pcase type
      ("HTML" value)
      ("CSS" (format "<style>%s</style>" (t--prepend-newline value)))
      ((or "JS" "JAVASCRIPT")
       (format "<script>%s</script>" (t--prepend-newline value)))
      ((or "EMACS-LISP" "ELISP")
       (t--eval-lisp export-block value 'eval "\"\""
                     "EMACS-LISP block"))
      ("LISP-DATA"
       (t--eval-lisp export-block value 'sexp "()"
                     "LISP-DATA block"))
      (_ ""))))

;;;; Fixed Width

;; See (info "(org) Literal Examples")
(defun t-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the formatted
<pre> element as a string."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (format "<pre%s>%s</pre>"
          (t--make-attr__id* fixed-width info t)
          (let ((value (org-remove-indentation
                        (org-element-property :value fixed-width))))
            (if (not (t--nw-p value)) value
              (concat "\n" value "\n")))))

;;;; Horizontal Rule

;; See (info "(org) Horizontal Rules")
(defun t-horizontal-rule (horizontal-rule _contents info)
  "Transcode a HORIZONTAL-RULE element from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the formatted
<hr> element as a string."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (t--void-element "hr" (t--make-attr__id* horizontal-rule info t)))

;;;; Keyword

(defun t-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the keyword
value as a string, or nil for unsupported keywords."
  (declare (ftype (function (t t list) (or null string)))
           (important-return-value t))
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (pcase key
      ((or "H" "HTML") value)
      ("E" (t--eval-lisp keyword value 'eval "\"\"" "#+E keyword"))
      ("D" (t--eval-lisp keyword value 'sexp "()" "#+D keyword"))
      ("TOC" (t--keyword-toc keyword value info))
      (_ nil))))

;;;; Paragraph

(defsubst t--wrap-image (contents _info caption attrs)
  "Wrap CONTENTS in a <figure> element for standalone images.

CONTENTS is the image HTML.  _INFO is unused.  CAPTION is the
caption string (may be empty).  ATTRS is a pre-formatted attribute
string for the <figure> tag.  Return the formatted <figure> element
as a string."
  (declare (ftype (function (string t string string) string))
           (pure t) (important-return-value t))
  (format "<figure%s>\n%s%s</figure>"
          attrs contents
          (if-let* ((c (t--nw-trim caption)))
              (format "<figcaption>%s</figcaption>\n" c) "")))

;; See (info "(org)Paragraphs")
(defun t-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph, as a string.  INFO is
the info plist.  Return the formatted paragraph as a string, or
an empty string for empty paragraphs.  The first paragraph in a
list item is rendered without a <p> tag; a standalone image is
wrapped in <figure>."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (let* ((parent (org-element-parent paragraph))
         (parent-type (org-element-type parent))
         (attrs (t--make-attr__id* paragraph info t)))
    (cond
     (;; Item's first line.
      (and (eq parent-type 'item)
           ;; In a <dd> list item, the text immediately following "::"
           ;; is not enclosed in a <p> tag.  If this part of the export
           ;; lacks HTML elements, the next text block will become the
           ;; first-child of the dd element, which has a margin-top of
           ;; 0 by default CSS.  Not wrapping in <p> avoids that.
           (not (org-export-get-previous-element paragraph info)))
      (let ((c (t--trim contents)))
        (if (string= attrs "") c
          (format "<span%s>%s</span>" attrs c))))
     (;; Standalone image.  Only `:attr__' applies to the figure here;
      ;; `:attr_html' is reserved for the image element itself (see
      ;; `org-w3ctr--link-attributes').
      (t-standalone-image-p paragraph info)
      (let* ((caption (org-export-get-caption paragraph))
             (cap (or (and caption (org-export-data caption info)) "")))
        (t--wrap-image contents info cap
                       (t--make-attr__id paragraph info t))))
     ;; Regular paragraph.
     (t (let ((c (t--trim contents)))
          (if (string= c "") ""
            (format "<p%s>%s</p>" attrs c)))))))

;;;; Verse Block

;; See (info "(org)Paragraphs")
(defun t-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.

CONTENTS is the verse block contents.  INFO is the info plist.
Return the formatted <p> element as a string.  Leading whitespace
is converted to non-breaking spaces; newlines become <br>."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (format
   "<p%s>\n%s</p>"
   (t--make-attr__id* verse-block info t)
   ;; Replace leading white spaces with non-breaking spaces.
   (replace-regexp-in-string
    "^[ \t]+" (lambda (m) (t--make-string (length m) "&#xa0;"))
    ;; Replace each newline character with line break. Also
    ;; remove any trailing "br" close-tag so as to avoid
    ;; duplicates.
    (let* ((re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote "<br>"))))
      (replace-regexp-in-string re "<br>\n" (or contents ""))))))

;;; Objects
;; footnote-reference, inline-src-block are not here.
;; latex-fragment, link and table-cell are not here.
;; timestamp is not here.
;; smallest objects are not here.

;;;; Entity

;; See (info "(org)Special Symbols")
(defun t-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to HTML.

CONTENTS and INFO are unused.  Return the HTML entity string
for ENTITY (e.g. `&alpha;')."
  (declare (ftype (function (t t t) string))
           (pure t) (important-return-value t))
  (org-element-property :html entity))

;;;; Export Snippet

;; See (info "(org)Quoting HTML tags")
(defun t-export-snippet (export-snippet _contents info)
  "Transcode an EXPORT-SNIPPET object from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return the snippet
value as a string, or an empty string for unsupported backends."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let* ((backend (org-export-snippet-backend export-snippet))
         (value (org-element-property :value export-snippet)))
    (pcase backend
      ((or 'h 'html) value)
      ('e (t--eval-lisp export-snippet value 'eval "\"\""
                        "@@e snippet"))
      ('d (t--eval-lisp export-snippet value 'sexp "()"
                        "@@d snippet"))
      (_ ""))))

;;;; Line Break

;; See (info "(org)Paragraphs")
(defun t-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to HTML.

CONTENTS and INFO are unused.  Return the HTML line break string."
  (declare (ftype (function (t t t) string))
           (pure t) (important-return-value t))
  "<br>\n")

;;;; Target

;; See (info "(org)Internal Links")
(defun t-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.

CONTENTS is nil.  INFO is the info plist.  Return a <span> element
with the target's reference as its id."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (format "<span id=\"%s\"></span>" (t--reference target info)))

;;;; Radio Target

;; See (info "(org)Radio Targets")
(defun t-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.

TEXT is the target text, nil or a string.  INFO is the info plist.
Return a <span> element with the target's reference as its id and
TEXT as its content."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (format "<span id=\"%s\">%s</span>"
          (t--reference radio-target info) (or text "")))

;;;; Statistics Cookie

;; See (info "(org)Checkboxes")
(defun t-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS and INFO are unused.  Return the cookie value wrapped in <code>."
  (declare (ftype (function (t t t) string))
           (pure t) (important-return-value t))
  (format "<code>%s</code>"
          (org-element-property :value statistics-cookie)))

;;;; Subscript

;; See (info "(org)Subscripts and Superscripts")
(defun t-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the subscript content.  INFO is unused.

Return a <sub> element."
  (declare (ftype (function (t string t) string))
           (pure t) (important-return-value t))
  (format "<sub>%s</sub>" contents))

;;;; Superscript

;; See (info "(org)Subscripts and Superscripts")
(defun t-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the superscript content.  INFO is unused.

Return a <sup> element."
  (declare (ftype (function (t string t) string))
           (pure t) (important-return-value t))
  (format "<sup>%s</sup>" contents))

;;; Smallest objects
;; See (info "(org) Emphasis and Monospace")
;; Options:
;; - :html-text-markup-alist (`org-w3ctr-text-markup-alist')

(defun t--get-markup-format (name info)
  "Get markup format string for NAME from INFO plist.
Returns \"%s\" if not found.

NAME is a symbol (like \\='bold), INFO is Org export info plist."
  (declare (ftype (function (symbol list) string))
           (important-return-value t))
  (if-let* ((alist (t--pget info :html-text-markup-alist))
            (str (cdr (assq name alist))))
      str "%s"))

;;;; Bold

(defun t-bold (_bold contents info)
  "Transcode BOLD from Org to HTML.

CONTENTS is the bold text.  INFO is the info plist.  Return the
formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'bold info) contents))

;;;; Italic

(defun t-italic (_italic contents info)
  "Transcode ITALIC from Org to HTML.

CONTENTS is the italic text.  INFO is the info plist.  Return the
formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'italic info) contents))

;;;; Underline

(defun t-underline (_underline contents info)
  "Transcode UNDERLINE from Org to HTML.

CONTENTS is the underlined text.  INFO is the info plist.  Return
the formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'underline info) contents))

;;;; Verbatim

(defun t-verbatim (verbatim _contents info)
  "Transcode VERBATIM from Org to HTML.

CONTENTS is unused; the value comes from the element's `:value'
property.  INFO is the info plist.  Return the formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'verbatim info)
          (t--encode-plain-text
           (org-element-property :value verbatim))))

;;;; Code

(defun t-code (code _contents info)
  "Transcode CODE from Org to HTML.

CONTENTS is unused; the value comes from the element's `:value'
property.  INFO is the info plist.  Return the formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'code info)
          (t--encode-plain-text
           (org-element-property :value code))))

;;;; Strike-Through

(defun t-strike-through (_strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.

CONTENTS is the struck-through text.  INFO is the info plist.
Return the formatted text."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (format (t--get-markup-format 'strike-through info) contents))

;;;; Plain Text

;; Options:
;; :with-smart-quotes    (`org-export-with-smart-quotes')
;; :with-special-strings (`org-export-with-special-strings')
;; :preserve-breaks      (`org-export-preserve-breaks')
(defconst t-special-string-regexps
  '(("\\\\-" . "&#x00ad;"); shy
    ("---\\([^-]\\)" . "&#x2014;\\1"); mdash
    ("--\\([^-]\\)" . "&#x2013;\\1"); ndash
    ("\\.\\.\\." . "&#x2026;")); hellip
  "Regular expressions for special string conversion.")

(defun t--convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (dolist (a t-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defun t-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.

TEXT is the plain text content.  INFO is the info plist.  Encode
HTML entities, activate smart quotes when enabled, convert special
strings when enabled, and preserve line breaks when enabled.
Return the transcoded string."
  (declare (ftype (function (string list) string))
           (important-return-value t))
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (t--encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original
    ;; string since OUTPUT may have been modified.
    (when (t--pget info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes
                    output :html info text)))
    ;; Handle special strings.
    (when (t--pget info :with-special-strings)
      (setq output (t--convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (t--pget info :preserve-breaks)
      (setq output
            (replace-regexp-in-string
             "\\(\\\\\\\\\\)?[ \t]*\n"
             "<br>\n" output)))
    ;; Return value.
    output))

;;;; Timestamp

;; See (info "(org)Timestamps")
;; Options:
;; - :html-timezone          (`org-w3ctr-timezone')
;; - :html-export-timezone   (`org-w3ctr-export-timezone')
;; - :html-datetime-option   (`org-w3ctr-datetime-format-choice')
;; - :html-timestamp-option  (`org-w3ctr-timestamp-option')
;; - :html-timestamp-wrapper (`org-w3ctr-timestamp-wrapper-type')
;; - :html-timestamp-formats (`org-w3ctr-timestamp-formats')

(defun t--timezone-to-offset (zone)
  "Convert timezone string ZONE to offset in seconds.

Valid formats are UTC/GMT[+-]XX (e.g., UTC+8), [+-]HHMM (e.g., -0500)
or \"local\", which means use zero offset.  Return nil if ZONE doesn't
match `org-w3ctr-timezone-regex'."
  (declare (ftype (function (string) (or fixnum symbol)))
           (pure t) (important-return-value t))
  (let ((case-fold-search t)
        (zone (t--trim zone)))
    (when (string-match t-timezone-regex zone)
      (if (string-equal-ignore-case zone "local") 'local
        (let* ((time (or (match-string 1 zone)
                         (match-string 2 zone)))
               (len (length time))
               (number (string-to-number time)))
          (cond
           ;; UTC/GMT[+-]xx
           ((<= 2 len 3) (* number 3600))
           ;; [+-]MMMM
           ((= len 5)
            (let ((hour (/ number 100))
                  (minute (% number 100)))
              (+ (* hour 3600) (* minute 60))))))))))

(defun t--get-info-timezone-offset (info)
  "Return timezone offset from INFO plist.

If it is a fixnum, return it directly; if it is the symbol \\='local,
return \\='local; if it is a string, attempt to parse it as a timezone
offset using `org-w3ctr--timezone-to-offset'.

On successful parsing, the numeric offset will be stored back into INFO
to avoid repeated parsing.  If timezone is `nil' or timezone format is
invalid, signal an error."
  (declare (ftype (function (list) (or fixnum symbol)))
           (important-return-value t))
  (if-let* ((zone (t--pget info :html-timezone)))
      (cond
       ((fixnump zone) zone)
       ((eq zone 'local) 'local)
       (t (if-let* ((time (t--timezone-to-offset zone)))
              (t--pput info :html-timezone time)
            (t-error "Invalid timezone format: %s" zone))))
    (t-error ":html-timezone is nil")))

(defun t--get-info-export-timezone-offset (info &optional zone1-offset)
  "Return export timezone offset from INFO plist.

The export timezone is determined by:
- If `:html-export-timezone' is nil, use `:html-timezone' value.
- If `:html-timezone' is \\='local, always use \\='local.
- Otherwise use `:html-export-timezone' value.

If optional argument ZONE1-OFFSET is non-nil, use it as the default
timezone offset instead of querying `:html-timezone' via
`org-w3ctr--get-info-timezone-offset'.  This avoids redundant lookups
when the caller already knows the default timezone offset."
  (declare (ftype (function (list &optional (or fixnum symbol))
                            (or fixnum symbol)))
           (important-return-value t))
  (let ((zone1 (or zone1-offset (t--get-info-timezone-offset info)))
        (zone2 (t--pget info :html-export-timezone)))
    (cond
     ((not zone2) zone1)
     ((eq zone1 'local) 'local)
     ((eq zone2 'local) 'local)
     ((fixnump zone2) zone2)
     (t (if-let* ((time (t--timezone-to-offset zone2)))
            (t--pput info :html-export-timezone time)
          (t-error "Invalid export timezone format: %s" zone2))))))

(defun t--get-info-timezone-delta (info &optional z1 z2)
  "Return the offset difference of export timezone(Z2) and timezone(Z1).

The returned value is (Z2 - Z1), in seconds.  If either timezone is
\\='local or both offsets are equal, returns 0.

If optional argument Z1 or Z2 is provided, use directly; otherwise,
their values are retrieved from INFO using
`org-w3ctr--get-info-timezone-offset' and
`org-w3ctr--get-info-export-timezone-offset'.

This value can be used to convert timestamps between timezones:
1. Subtract the base timezone offset from a local timestamp to obtain
   the corresponding UTC time.
2. Then add the export timezone offset to the UTC time to get the
   timestamp in the export timezone."
  (declare (ftype (function (list &optional t t) fixnum))
           (important-return-value t))
  (let* ((offset1 (or z1 (t--get-info-timezone-offset info)))
         (offset2 (or z2 (t--get-info-export-timezone-offset
                          info offset1))))
    (cond
     ((or (eq offset1 'local) (eq offset2 'local)) 0)
     ((= offset1 offset2) 0)
     (t (- offset2 offset1)))))

(defconst t--timestamp-datetime-options
  '((s-none       . (" " ""  "+0000"))
    (s-none-zulu  . (" " ""  "Z"))
    (s-colon      . (" " ":" "+00:00"))
    (s-colon-zulu . (" " ":" "Z"))
    (T-none       . ("T" ""  "+0000"))
    (T-none-zulu  . ("T" ""  "Z"))
    (T-colon      . ("T" ":" "+00:00"))
    (T-colon-zulu . ("T" ":" "Z")))
  "HTML <time>'s datetime format options.

  See `org-w3ctr-datetime-format-choice' for more details.")

(defun t--get-datetime-format (offset option &optional notime)
  "Return a datetime format string for HTML <time> tags.

OFFSET is the timezone offset in seconds.  OPTION is a symbol specifying
the format style, as defined in `org-w3ctr--timestamp-datetime-options'.

If NOTIME is non-nil, only the date format (\"%F\") will be returned;
If NOTIME is nil, this function looks up the formatting option and
builds the timezone string based on OFFSET and the selected formatting
rule, and returns a full datetime format string suitable for use in HTML
<time> tag's `datetime' attributes."
  (declare (ftype (function ((or fixnum symbol) t &optional boolean)
                            (or string null)))
           (pure t) (important-return-value t))
  (if notime "%F"
    (when-let* (((symbolp option))
                (ls (alist-get option t--timestamp-datetime-options)))
      (if (eq offset 'local)
          (format "%%F%s%%R" (nth 0 ls))
        (let* ((hours (/ (abs offset) 3600))
               (minutes (/ (- (abs offset) (* hours 3600)) 60))
               (zone (if (= offset 0) (nth 2 ls)
                       (format "%s%02d%s%02d"
                               (if (plusp offset) "+" "-")
                               hours (nth 1 ls) minutes))))
          (format "%%F%s%%R%s" (nth 0 ls) zone))))))

(defun t--format-datetime (time info &optional notime)
  "Format TIME into a datetime string.

TIME is an Emacs internal time value.  INFO is the info plist.
NOTIME, when non-nil, returns only the date format.  Return the
formatted datetime string."
  (declare (ftype (function (list list &optional boolean) string))
           (important-return-value t))
  (let* ((offset0 (t--get-info-timezone-offset info))
         (offset1 (t--get-info-export-timezone-offset info offset0))
         (delta (t--get-info-timezone-delta info offset0 offset1))
         (option (t--pget info :html-datetime-option)))
    (if-let* (option
              (fmt (t--get-datetime-format offset1 option notime))
              (time (if notime time (time-add time delta))))
        (condition-case nil
            (format-time-string fmt time)
          (error (t-error "Time may be out of range: %s" time)))
      (t-error ":html-datetime-option is invalid: %s" option))))

(defun t--call-with-invalid-time-spec-handler (fn timestamp &rest args)
  "Wrap FN call with clearer error messages for invalid timestamps.

Call FN with TIMESTAMP and ARGS.  If FN signals an error with the
message \"Invalid time specification\", re-signal as `org-w3ctr-error'
with the raw value of TIMESTAMP.  Other errors are re-signaled as-is."
  (declare (ftype (function (function t &rest t) t))
           (important-return-value t))
  (condition-case e
      (apply fn timestamp args)
    (error
     (if (equal e '(error "Invalid time specification"))
         (t-error "Timestamp %s encode failed"
                  (org-element-property :raw-value timestamp))
       (signal e)))))

(defun t--format-ts-datetime (timestamp info &optional end)
  "Format TIMESTAMP to its datetime attribute string.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
END, when non-nil, format the end of a range.  Return a string
suitable for an HTML <time> datetime attribute."
  (declare (ftype (function (t list &optional t) string))
           (important-return-value t))
  (format " datetime=\"%s\""
          (t--format-datetime
           (t--call-with-invalid-time-spec-handler
            #'org-timestamp-to-time timestamp end)
           info (not (org-timestamp-has-time-p timestamp)))))

(defun t--interpret-timestamp (timestamp)
  "Interpret an Org TIMESTAMP object with improved error reporting.

This function calls `org-element-timestamp-interpreter' on TIMESTAMP,
and provides a clearer error message if the timestamp is invalid or out
of range.

It is also possible to use `org-element-interpret-data' directly, but it
inserts trailing spaces when the timestamp is followed by space."
  (declare (ftype (function (t) string))
           (important-return-value t))
  (or (t--call-with-invalid-time-spec-handler
       #'org-element-timestamp-interpreter timestamp :nothing)
      (t-error "Bad start date: %s" timestamp)))

(defun t--format-timestamp-diary (timestamp info)
  "Format a diary TIMESTAMP object.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
If `:html-timestamp-option' is `raw', use the `:raw-value'
property.  Otherwise, interpret TIMESTAMP via
`org-w3ctr--interpret-timestamp'.  Return the formatted string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let* ((option (t--pget info :html-timestamp-option))
         (text (pcase option
                 (`raw (org-element-property :raw-value timestamp))
                 (_ (t--interpret-timestamp timestamp)))))
    (t-plain-text text info)))

(defun t--format-ts-span-time (str info &optional time)
  "Format timestamp string STR using <span> or <time>.

STR is the timestamp text.  INFO is the info plist.  TIME, when
non-nil, use <time> tag and return a template string with a `%s'
placeholder for the datetime attribute (caller fills it via
`format'); otherwise use <span> and return a complete string."
  (declare (ftype (function (string list &optional boolean) string))
           (important-return-value t))
  (if (not time)
      ;; taken from `org-html-timestamp'.
      (concat "<span class=\"timestamp-wrapper\">"
              "<span class=\"timestamp\">"
              (t-plain-text str info) "</span></span>")
    (concat "<time%s>" (t-plain-text str info) "</time>")))

(defun t--format-timestamp-raw-1 (timestamp raw info)
  "Format TIMESTAMP with its RAW string.

TIMESTAMP is an Org timestamp object.  RAW is a string matching
`org-ts-regexp-both'.  INFO is the info plist.  Return the
formatted timestamp string."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (pcase (t--pget info :html-timestamp-wrapper)
    (`none (t-plain-text raw info))
    (`span (t--format-ts-span-time raw info))
    (`time
     (let* ((tss (t--find-all org-ts-regexp-both raw))
            (len (length tss))
            (str (mapconcat
                  (lambda (s) (t--format-ts-span-time s info t))
                  tss (if (t--pget info :with-special-strings)
                          "&#x2013;" "--"))))
       (pcase len
         (1 (format str (t--format-ts-datetime timestamp info)))
         (2 (format str (t--format-ts-datetime timestamp info)
                    (t--format-ts-datetime timestamp info t)))
         (_ (t-error "Abnormal timestamp: %s" raw)))))
    (w (t-error "Unknown timestamp wrapper: %s" w))))

(defun t--format-timestamp-raw (timestamp info)
  "Format TIMESTAMP without altering its string content.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
Return the formatted timestamp string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((raw (org-element-property :raw-value timestamp)))
    (t--format-timestamp-raw-1 timestamp raw info)))

(defun t--format-timestamp-int (timestamp info)
  "Format TIMESTAMP with `org-timestamp-formats'.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
Return the formatted timestamp string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((raw (t--interpret-timestamp timestamp)))
    (t--format-timestamp-raw-1 timestamp raw info)))

(defun t--format-timestamp-fmt (timestamp info)
  "Format TIMESTAMP with `org-w3ctr-timestamp-formats'.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
Return the formatted timestamp string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if-let* ((fmt (t--pget info :html-timestamp-formats))
            (org-timestamp-formats fmt)
            (raw (t--interpret-timestamp timestamp)))
      (t--format-timestamp-raw-1 timestamp raw info)
    (error ":html-timestamp-formats not valid: %s"
           (t--pget info :html-timestamp-formats))))

(defun t--format-timestamp-fix (timestamp fmt info)
  "Format TIMESTAMP with a fixed format string FMT.

TIMESTAMP is an Org timestamp object.  FMT is a format string.
INFO is the info plist.  Used internally for `org' and `cus'
options.  Return the formatted timestamp string."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (let* ((wrap (t--pget info :html-timestamp-wrapper))
         (type (org-element-property :type timestamp)))
    (pcase type
      ((or `active `inactive)
       (let ((time (org-format-timestamp timestamp fmt)))
         (pcase wrap
           (`none (t-plain-text time info))
           (`span (t--format-ts-span-time time info))
           (`time
            (format (t--format-ts-span-time time info t)
                    (t--format-ts-datetime timestamp info)))
           (_ (error "Unknown timestamp wrap: %s" wrap)))))
      ((or `active-range `inactive-range)
       (let* ((t1 (org-format-timestamp timestamp fmt))
              (t2 (org-format-timestamp timestamp fmt t)))
         (pcase wrap
           (`none (t-plain-text (concat t1 "--" t2) info))
           (`span (t--format-ts-span-time (concat t1 "--" t2) info))
           (`time
            (let* ((de (if (t--pget info :with-special-strings)
                           "&#x2013;" "--"))
                   (tt (concat (t--format-ts-span-time t1 info t) de
                               (t--format-ts-span-time t2 info t))))
              (format tt (t--format-ts-datetime timestamp info)
                      (t--format-ts-datetime timestamp info t))))
           (_ (error "Unknown timestamp wrap: %s" wrap)))))
      (_ (error "Unknown timestamp type: %s" type)))))

(defun t--format-timestamp-org (timestamp info)
  "Format TIMESTAMP like `org-timestamp-translate'.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
When `org-display-custom-times' is nil, fall back to `int'
formatting.  Return the formatted timestamp string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if (not org-display-custom-times)
      (t--format-timestamp-int timestamp info)
    (let ((fmt (org-time-stamp-format
                (org-timestamp-has-time-p timestamp)
                nil 'custom)))
      (t--format-timestamp-fix timestamp fmt info))))

(defun t--format-timestamp-cus (timestamp info)
  "Format TIMESTAMP according to custom formats.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
The format string must be enclosed in [], <>, or {}; curly
braces indicate no enclosing brackets.  Return the formatted
timestamp string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let* ((re (rx string-start
                 (or (seq "[" (*? anything) "]")
                     (seq "{" (*? anything) "}")
                     (seq "<" (*? anything) ">"))
                 string-end))
         (fmts (t--pget info :html-timestamp-formats))
         (fmt (if (org-timestamp-has-time-p timestamp)
                  (cdr fmts) (car fmts))))
    (unless (and (stringp fmt) (string-match-p re fmt))
      (error "FMT not fit in `cus': %s" fmts))
    (let ((fmt (if (/= (aref fmt 0) ?\{) fmt (substring fmt 1 -1))))
      (t--format-timestamp-fix timestamp fmt info))))

(defun t-ts-default-format-function (timestamp _info)
  "The default custom TIMESTAMP format function.

TIMESTAMP is an Org timestamp object.  _INFO is unused.  Return
the raw value of TIMESTAMP."
  (declare (ftype (function (t list) string))
           (pure t) (important-return-value t))
  (org-element-property :raw-value timestamp))

(defun t--format-timestamp-fun (timestamp info)
  "Format TIMESTAMP using a user-specified function from INFO."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if-let* ((fun (t--pget info :html-timestamp-format-function)))
      (funcall fun timestamp info)
    (error ":html-timestamp-format-function is nil")))

(defun t-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML.

TIMESTAMP is an Org timestamp object.  _CONTENTS is unused.  INFO
is the info plist.  Return the formatted timestamp string."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let ((type (org-element-property :type timestamp)))
    (if (eq type 'diary)
        (t--format-timestamp-diary timestamp info)
      (let* ((option (t--pget info :html-timestamp-option))
             (fun (pcase option
                    (`raw #'t--format-timestamp-raw)
                    (`int #'t--format-timestamp-int)
                    (`fmt #'t--format-timestamp-fmt)
                    (`cus #'t--format-timestamp-cus)
                    (`org #'t--format-timestamp-org)
                    (`fun #'t--format-timestamp-fun)
                    (o (error "Unknown timestamp option: %s" o)))))
        (funcall fun timestamp info)))))

;;; Headline and Section

;;;; Section

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Fixed export. Not customizable.

(defvar t--zeroth-section-output nil
  "Internal variable storing zeroth section's HTML output.

This is used to override the default ox-html behavior where TOC comes
first, allowing zeroth section's content to appear before the TOC while
the TOC remains near the beginning of the document.")

;; FIXME: consider consider malformed headline(e.g., ** before first *)
(defun t-section (section contents _info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (declare (ftype (function (t t t) string))
           (important-return-value t))
  ;; normal section
  (if (org-element-lineage section 'headline) contents
    (prog1 nil (setq t--zeroth-section-output contents))))

;;;; Todo

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - `org-done-keywords'
;; - :with-todo-keywords (`org-export-with-todo-keywords')
;; - :html-todo-class (`org-w3ctr-todo-class')
;; - :html-todo-kwd-class-prefix (`org-w3ctr-todo-kwd-class-prefix')

(defun t--todo (todo info)
  "Format TODO keywords into HTML."
  (declare (ftype (function ((or null string) list) (or null string)))
           (important-return-value t))
  (when todo
    (let* ((prefix (t--pget info :html-todo-kwd-class-prefix))
           (common (t--pget info :html-todo-class))
           (status (if (member todo (cons "DONE" org-done-keywords))
                       "done" "todo")))
      (format "<span class=\"%s%s\">%s</span>"
              (concat prefix status)
              (if-let* ((c (t--nw-trim common))) (concat " " c) "")
              todo))))

;;;; Priority

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :with-priority (`org-export-with-priority')
;; - :html-priority-class (`org-w3ctr-priority-class')
;; - `org-priority-highest'(65)
;; - `org-priority-default'(66)
;; - `org-priority-lowest' (67)

(defun t--priority (priority info)
  "Format a priority into HTML."
  (declare (ftype (function ((or null fixnum) list) (or null string)))
           (important-return-value t))
  (when priority
    (let ((class (t--pget info :html-priority-class)))
      (format "<span%s>[%s]</span>"
              (if-let* ((c (t--nw-trim class)))
                  (format " class=\"%s\"" c) "")
              (if (< 0 priority 65) priority
                (string priority))))))

;;;; Tags

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :with-tags (`org-export-with-tags')
;; - :html-tag-class (`org-w3ctr-tag-class')

(defun t--tags (tags info)
  "Format TAGS into HTML."
  (declare (ftype (function (list list) (or null string)))
           (important-return-value t))
  (when-let* ((f (lambda (tag) (format "<span>%s</span>" tag)))
              (spans (t--nw-p (mapconcat f tags "&#xa0;"))))
    (if-let* ((class (t--nw-trim (t--pget info :html-tag-class))))
        (format "<span class=\"%s\">%s</span>"
                class spans)
      (format "<span>%s</span>" spans))))

;;;; Headline

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-format-headline-function (`org-w3ctr-format-headline-function')
;; - :html-toplevel-hlevel (`org-w3ctr-toplevel-hlevel')
;; - :html-honor-ox-headline-levels (`org-w3ctr-honor-ox-headline-levels')
;; - :html-container (`org-w3ctr-container-element')
;; - :html-self-link-headlines (`org-w3ctr-self-link-headlines')
;; - :headline-levels (`org-export-headline-levels')
;; - :headline-offset (internal)
;; - :section-numbers (`org-export-with-section-numbers')
;; - `org-footnote-section'

(defun t--headline-todo (headline info)
  "Format and return the TODO keyword for HEADLINE.

Returns the exported keyword string only if `:with-todo-keywords' is
enabled in INFO and a TODO keyword exists on the HEADLINE.  Returns nil
otherwise."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (and-let* (((t--pget info :with-todo-keywords))
             (todo (org-element-property :todo-keyword headline)))
    (org-export-data todo info)))

(defun t--headline-priority (headline info)
  "Return the numerical priority of a headline.

This function returns the priority number (e.g., 65 for [#A]) only if
the export option `:with-priority` is non-nil in INFO and the HEADLINE
element has a priority cookie.  Returns nil otherwise."
  (declare (ftype (function (t list) (or null fixnum)))
           (important-return-value t))
  (and (t--pget info :with-priority)
       (org-element-property :priority headline)))

(defun t--headline-tags (headline info)
  "Return the list of tags for a headline.

This function returns a list of tags associated with the HEADLINE
element, but only if the export option `:with-tags` is enabled in the
INFO plist. The tags are processed for export.  Returns nil if tags are
disabled or not present."
  (declare (ftype (function (t list) list))
           (important-return-value t))
  (and (t--pget info :with-tags)
       (org-export-get-tags headline info)))

(defun t-format-headline-default-function (todo priority text tags info)
  "Default format function for a headline.
See `org-w3ctr-format-headline-function' for details and the
description of TODO, PRIORITY, TEXT, TAGS, and INFO arguments."
  (declare (ftype (function ((or null string) (or null fixnum)
                             (or null string) list list)
                            string)))
  (let ((todo (t--todo todo info))
        (priority (t--priority priority info))
        (tags (t--tags tags info)))
    (concat todo (and todo " ")
            priority (and priority " ")
            text (and tags "&#xa0;&#xa0;&#xa0;") tags)))

;; FIXME: Add tests
(defun t--build-bare-headline (headline text info)
  "Build the inner HTML content of a headline.

This function extracts all components of a HEADLINE element (like TODO
keyword, priority and tags) from the parse tree. It respects export
options like `:with-todo-keywords' and `:with-tags'.

Then, it passes these extracted components as arguments to the
user-defined formatting function (from `:html-format-headline-function')
to construct the final string."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (let* ((todo (t--headline-todo headline info))
         (priority (t--headline-priority headline info))
         (tags (t--headline-tags headline info))
         ;; FIXME: Check headline-function if valid
         (f (t--pget info :html-format-headline-function)))
    (funcall f todo priority text tags info)))

;; FIXME: Adjust tests
(defun t--build-base-headline (headline info)
  "Build a standard headline string for the document body.

This function extracts the main title from the HEADLINE element, formats
it for export, and then passes it to `org-w3ctr--build-bare-headline' to
be combined with other components like TODO keywords and tags."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((text (org-export-data
               (org-element-property :title headline) info)))
    (t--build-bare-headline headline text info)))

;; FIXME: Add tests
(defun t--build-toc-headline (headline info)
  "Build a headline string for the Table of Contents.

This function retrieves the headline's alternative title, which is used
for TOC entries. It ensures the title is formatted with the correct
backend before passing it to `org-w3ctr--build-bare-headline' for final
assembly."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((text (org-export-data-with-backend
               (org-export-get-alt-title headline info)
               (org-export-toc-entry-backend 'w3ctr)
               info)))
    (t--build-bare-headline headline text info)))

(defun t--get-headline-hlevel (headline info)
  "Calculate the absolute HTML heading level for a headline.

This function computes the final HTML heading level based on the
headline's relative level within the Org document and the value
of `:html-toplevel-hlevel'. The formula used is:
  (relative-level + top-level - 1).

It also validates that `:html-toplevel-hlevel' is an integer
between 2 and 6, signaling an error if it is not."
  (declare (ftype (function (t list) fixnum))
           (important-return-value t))
  (let ((top-level (t--pget info :html-toplevel-hlevel))
        (level (org-export-get-relative-level headline info)))
    (unless (and (fixnump top-level) (<= 2 top-level 6))
      (t-error "Invalid HTML top level: %s" top-level))
    (+ level top-level -1)))

(defun t--low-level-headline-p (headline info)
  "Check if HEADLINE should be rendered as a low-level list item.

This predicate determines if a headline's level exceeds the
standard HTML heading range (i.e., <h6>).

Its behavior depends on `:html-honor-ox-headline-levels':
- If non-nil, it uses the default `org-export-low-level-p'.
- If nil, it uses a custom check based on the calculated h-level
  from `org-w3ctr--get-headline-hlevel'."
  (declare (ftype (function (t list) boolean))
           (important-return-value t))
  (if-let* ((honor (t--pget info :html-honor-ox-headline-levels)))
      (org-export-low-level-p headline info)
    (let ((level (t--get-headline-hlevel headline info)))
      (> level 6))))

(defun t--build-low-level-headline (headline contents info)
  "Transcode a low-level headline into an HTML list item (`<li>').

This function renders headlines that are too deep to become standard
<hN> tags. It creates a list structure where a group of sibling
low-level headlines becomes a single `<ol>' or `<ul>'.

The list type (`<ol>' vs. `<ul>') is determined by whether section
numbering is active."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
         (tag (if numberedp "ol" "ul"))
         (text (t--build-base-headline headline info))
         (id (t--reference headline info)))
    (concat
     (and (org-export-first-sibling-p headline info)
          (format "<%s>\n" tag))
     "<li>" (format "<span id=\"%s\"></span>" id) text
     (when-let* ((c (t--nw-p contents))) (concat "<br>\n" c))
     "</li>\n"
     (and (org-export-last-sibling-p headline info)
          (format "</%s>\n" tag)))))

;; FIXME: Add container checker here.
(defun t--headline-container (headline info)
  "Return HTML container name for HEADLINE as a string."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (or (org-element-property :HTML_CONTAINER headline)
      (t--pget info :html-container)
      "div"))

(defun t--headline-self-link (headline id info)
  "Build a self-link for a headline."
  (declare (ftype (function (t string list) (or null string)))
           (important-return-value t))
  (let ((opt (org-element-property :HTML_SELF_LINK headline))
        (global-opt (t--pget info :html-self-link-headlines)))
    (when (or (and (null opt) global-opt)
              (and (stringp opt) (not (string= opt "noref"))))
      (format (concat "<a class=\"self-link\" href=\"#%s\""
                      " aria-label=\"Link to this section\"></a>\n")
              id))))

(defun t--headline-secno (headline info)
  "Return section number for HEADLINE as an HTML span."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (when-let* (((org-export-numbered-headline-p headline info))
              (numbers (org-export-get-headline-number headline info)))
    (format "<span class=\"secno\">%s. </span>"
            (mapconcat #'number-to-string numbers "."))))

(defun t--headline-hN (headline info)
  "Return the HTML heading tag name (e.g., \"h2\") for HEADLINE.

The level is capped at 6, so this function always returns a
string from \"h1\" to \"h6\"."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let* ((level (min 6 (t--get-headline-hlevel headline info))))
    (format "h%s" level)))

(defun t--build-normal-headline (headline contents info)
  "Build HTML for a standard headline and its section.

This function formats a regular headline, which is not a footnote
or a low-level headline treated as a list item."
  (let* ((secno (t--headline-secno headline info))
         (h (t--headline-hN headline info))
         (text (t--build-base-headline headline info))
         (full-text (concat secno text))
         (id (t--reference headline info))
         (c (t--headline-container headline info))
         (c-cls (org-element-property :HTML_CONTAINER_CLASS headline))
         (h-cls (org-element-property :HTML_HEADLINE_CLASS headline)))
    ;; <C>, id, class, header, contents, </C>
    (format "<%s id=\"%s\"%s>\n%s%s</%s>\n"
            c id (or (and c-cls (format " class=\"%s\"" c-cls)) "")
            (format
             ;; <H>, id, class, headline, </H>
             ;; FIXME: is x-id necessary?
             (concat "<div class=\"header-wrapper\">\n"
                     "<%s id=\"x-%s\"%s>%s</%s>\n"
                     (t--headline-self-link headline id info)
                     "</div>\n")
             h id (or (and h-cls (format " class=\"%s\"" h-cls)) "")
             full-text h)
            (or contents "") c)))

(defun t-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (if (t--low-level-headline-p headline info)
        ;; This is a deep sub-tree: export it as a list item.
        (t--build-low-level-headline headline contents info)
      ;; Normal headline.  Export it as a section.
      (t--build-normal-headline headline contents info))))

;;; Template and Inner Template

;;;; <head> tags export.

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :time-stamp-file (`org-export-timestamp-file')
;; - :html-file-timestamp-function (`org-w3ctr-file-timestamp-function')
;; - `org-w3ctr-coding-system'
;; - :html-viewport (`org-w3ctr-viewport')
;; - :author #+AUTHOR: (`user-full-name')
;; - :with-author (`org-export-with-author')
;; - :title #+TITLE:
;; - :with-title (`org-export-with-title')
;; - `org-w3ctr-meta-tags'

(defun t--build-meta-entry ( label identity
                             &optional content-format
                             &rest content-formatters)
  "Build a meta tag using the provided information.

Construct <meta> tag of form <meta LABEL=\"IDENTITY\">,
or when CONTENT-FORMAT is present:
<meta LABEL=\"IDENTITY\" content=\"{content}\">

Here {content} is determined by applying any CONTENT-FORMATTERS
to the CONTENT-FORMAT and encoding the result as plain text."
  (declare (ftype (function ( string string
                              &optional string &rest t)
                            string))
           (pure t) (important-return-value t))
  (concat
   "<meta " (format "%s=\"%s\"" label identity)
   (when content-format
     (format " content=\"%s\""
             (t--encode-plain-text*
              (if (not content-formatters) content-format
                (apply #'format content-format content-formatters)))))
   ">\n"))

(defun t-file-timestamp-default-function (_info)
  "Return current timestamp in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  (declare (ftype (function (t) string))
           (side-effect-free t) (important-return-value t))
  (format-time-string "%FT%RZ" nil t))

(defun t--get-info-file-timestamp (info)
  "Get file timestamp from INFO plist."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (when (t--pget info :time-stamp-file)
    (if-let* ((fun (t--pget info :html-file-timestamp-function))
              ((functionp fun)))
        (funcall fun info)
      (t-error "Invalid :html-file-timestamp-function: %s"
               (t--pget info :html-file-timestamp-function)))))

(defun t--ensure-charset-utf8 ()
  "Validate `org-w3ctr-coding-system' and ensure its MIME is UTF-8.
Signals an error if `org-w3ctr-coding-system' is invalid or not UTF-8."
  (declare (ftype (function () string))
           (important-return-value t))
  (let* ((c t-coding-system)
         (h (lambda (_) (t-error "Invalid coding system: %s" c))))
    (unless (symbolp c) (funcall h c))
    (handler-bind ((coding-system-error h))
      (let* ((uc (coding-system-get c :mime-charset)))
        (if (eq uc 'utf-8) "utf-8" (funcall h c))))))

(defun t--build-viewport-options (info)
  "Build <meta> viewport tags."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* ((opts (cl-remove-if-not
                     #'t--nw-p (t--pget info :html-viewport)
                     :key #'cadr)))
    (t--build-meta-entry
     "name" "viewport"
     (mapconcat (pcase-lambda (`(,k ,v)) (format "%s=%s" k v))
                opts ", "))))

(defun t--get-info-title-raw (info)
  "Extract title from INFO plist and return as plain text.

If title exists, is non-whitespace, and can be converted to plain text,
return the text.  Otherwise return a left-to-right mark (invisible)."
  (declare (ftype (function (list) string))
           (important-return-value t))
  ;; HTML always need <title>, so just ignore :with-title.
  (if-let* ((title (t--pget info :title))
            (str0 (org-element-interpret-data title))
            (str (t--nw-trim str0))
            (text (t-plain-text str info)))
      ;; Set title to an invisible character instead of
      ;; leaving it empty, which is invalid.
      text "&lrm;"))

(defun t--get-info-author-raw (info)
  "Get author from INFO if :with-author is non-nil."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* (((t--pget info :with-author))
              (a (t--pget info :author)))
    ;; Return raw Org syntax.
    ;; #+author is parsed as Org object.
    (t--nw-trim (org-element-interpret-data a))))

(defun t-meta-tags-default (info)
  "A default value for `org-w3ctr-meta-tags'.

Generate a list items, each of which is a list of arguments
that can be passed to `org-w3ctr--build-meta-entry', to generate meta
tags to be included in the HTML head."
  (declare (ftype (function (list) list))
           (important-return-value t))
  (list
   (when-let* ((author (t--get-info-author-raw info)))
     (list "name" "author" author))
   (when-let* ((desc (t--nw-trim (t--pget info :description))))
     (list "name" "description" desc))
   (when-let* ((keyw (t--nw-trim (t--pget info :keywords))))
     (list "name" "keywords" keyw))
   '("name" "generator" "Org Mode")))

(defun t--build-meta-tags (info)
  "Build HTML <meta> tags get from `org-w3ctr-meta-tags'."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (mapconcat
   (lambda (args) (apply #'t--build-meta-entry args))
   (remq nil (if (not (functionp t-meta-tags)) t-meta-tags
               (funcall t-meta-tags info)))))

(defun t--build-meta-info (info)
  "Return meta tags for exported document."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (concat
   ;; timestamp
   (when-let* ((ts (t--get-info-file-timestamp info)))
     (format "<!-- %s -->\n" ts))
   ;; charset
   (t--build-meta-entry "charset" (t--ensure-charset-utf8))
   ;; viewport
   (t--build-viewport-options info)
   ;; title
   (format "<title>%s</title>\n" (t--get-info-title-raw info))
   ;; meta tags
   (t--build-meta-tags info)))

;;;; Default CSS export.

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-head-include-style (`org-w3ctr-head-include-style')
;; - `org-w3ctr-style'
;; - `org-w3ctr-style-file'

(defun t--load-css (_info)
  "Load CSS for HTML export from configured sources.

This function handles CSS loading in the following priority:
  If `org-w3ctr-style' is non-empty string, use it directly.
  If `org-w3ctr-style-file' is non-nil, load CSS from that file.
  If both are empty/nil, return empty string (no styles).

The loaded CSS will be wrapped in HTML <style> tags when non-empty."
  (declare (ftype (function (t) (or null string)))
           (important-return-value t))
  (or (t--nw-p t-style)
      (when t-style-file
        (let* ((str (t--load-file t-style-file))
               (str* (org-element-normalize-string str))
               (css (format "<style>\n%s</style>\n" str*)))
          (setq t-style css)))))

(defun t-clear-css ()
  "Set `org-w3ctr-style' to empty string \"\".

When CSS is loaded from `org-w3ctr-style-file', its content is cached in
`org-w3ctr-style' to improve performance.  If you modify the external
CSS file and want the changes to take effect on the next export, run
this command to clear the cache.  This forces the exporter to re-read
the file."
  (interactive)
  (setq t-style ""))

;;;; Mathjax config

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :with-latex (`org-w3ctr-with-latex')
;; - :html-mathjax-config (`org-w3ctr-mathjax-config')
;; - :html-math-head-function (`org-w3ctr-math-head-function')
;; - :html-math-custom-render-function
;; (`org-w3ctr-math-custom-default-render-function')

(defconst t-svg-math-style "\
<style>
.math-display { display: block; text-align: center; margin: 1em 0; }
</style>
"
  "Style for display math produced by `svg-by-mathjax'.")

(defun t-math-head-default-function (info)
  "Default value for `org-w3ctr-math-head-function'.
Return the MathJax script for `mathjax' mode, the display-math style
for `svg-by-mathjax' mode, nothing otherwise."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (pcase (t--pget info :with-latex)
    (`mathjax (t--pget info :html-mathjax-config))
    (`svg-by-mathjax t-svg-math-style)
    (_ "")))

(defun t--build-math-config (info)
  "Return the math setup to insert into <head>."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (funcall (t--pget info :html-math-head-function) info))

;;;; Rest of <head>

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-head (`org-w3ctr-head')
;; - :html-head-extra (`org-w3ctr-head-extra')
;; - :html-head-include-style (`org-w3ctr-head-include-style')

(defun t--use-default-style-p (info)
  "Test if org export use default CSS style."
  (declare (ftype (function (list) boolean))
           (important-return-value t))
  (t--pget info :html-head-include-style))

(defun t--has-math-p (info)
  "Test if org doc has latex fragment or latex environment."
  (declare (ftype (function (list) boolean))
           (important-return-value t))
  (and (t--pget info :with-latex)
       (org-element-map (t--pget info :parse-tree)
           '(latex-fragment latex-environment)
         #'identity info t nil t)))

;; FIXME: Consider add code hightlight (such as highlight.js) codes.
(defun t--build-head (info)
  "Return information for the <head>...</head> of the HTML output."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (concat
   "<head>\n"
   ;; <meta>
   (t--build-meta-info info)
   ;; <style>
   (when (t--use-default-style-p info) (t--load-css info))
   ;; Mathjax or MathML config.
   (when (t--has-math-p info) (t--build-math-config info))
   ;; User defined <head> contents
   (org-element-normalize-string (t--pget info :html-head))
   (org-element-normalize-string (t--pget info :html-head-extra))
   "</head>\n"))

;;;; Legacy home and up

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-link-up (`org-w3ctr-link-up')
;; - :html-link-home (`org-w3ctr-link-home')
;; - :html-home/up-format (`org-w3ctr-home/up-format')

(defun t--format-legacy-navbar (info)
  "Format the legacy Home/Up navigation bar.

Generates HTML navigation bar using either :html-link-up or
:html-link-home from the INFO plist, falling back to each other when
empty. Returns nil if both links are empty strings."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (let ((link-up (t--nw-trim (t--pget info :html-link-up)))
        (link-home (t--nw-trim (t--pget info :html-link-home))))
    (unless (and (null link-up) (null link-home))
      (format (t--pget info :html-home/up-format)
              (or link-up link-home) (or link-home link-up)))))

;;;; Navbar

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-link-navbar (`org-w3ctr-link-navbar')
;; - :html-format-navbar-function (`org-w3ctr-format-navbar-function')

(defun t--format-navbar-nav (s)
  "Format navbar <nav> element."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (format "<nav id=\"navbar\">\n%s\n</nav>\n" s))

(defun t--format-navbar-vector (v)
  "Submodule of `t-format-navbar-default-function'."
  (declare (ftype (function (vector) string))
           (pure t) (important-return-value t))
  (if (equal v []) ""
    (t--format-navbar-nav
     (mapconcat
      (pcase-lambda (`(,link . ,name))
        (format "<a href=\"%s\">%s</a>" link name))
      v "\n"))))

(defun t--format-navbar-list (ll info)
  "Submodule of `t-format-navbar-default-function'."
  (declare (ftype (function (list list) string))
           (important-return-value t))
  (if (null ll) ""
    (let* ((elems (mapcar (lambda (x) (org-export-data x info)) ll))
           (links (cl-remove-if-not #'t--nw-p elems))
           (as (mapcar #'t--trim links)))
      (t--format-navbar-nav (string-join as "\n")))))

(defun t-format-navbar-default-function (info)
  "Generate HTML navigation links from the export INFO plist. This
function processes the :html-link-navbar property to create a
navigation section in the exported document.

When :html-link-navbar is a vector, it should contain cons cells in
the form (URL . LABEL) where URL is the target location and LABEL is
the display text.

When :html-link-navbar is a list, it is treated as containing Org
link elements. These links will be processed through `org-export-data'
to generate the final HTML output.

The output is always wrapped in a <nav> HTML element with
id=\"navbar\" for consistent styling and semantic markup.
Each link is separated by newlines for readability in the output HTML."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let* ((links (t--pget info :html-link-navbar))
         (p (lambda (x) (and (stringp (car-safe x))
                             (stringp (cdr-safe x))))))
    (pcase links
      ((pred vectorp)
       (or (and (cl-every p links) (t--format-navbar-vector links))
           (t-error "Invalid navbar vector: %s" links)))
      ((pred listp)
       (let ((res (t--format-navbar-list links info)))
         (if (not (string-empty-p res)) res
           (or (t--format-legacy-navbar info) ""))))
      (other (t-error "Invalid navbar type: %s" other)))))

;;;; CC license budget

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-use-cc-budget (`org-w3ctr-use-cc-budget')
;; - :html-license (`org-w3ctr-public-license')
;; - :html-format-license-function (`org-w3ctr-format-license-function')

(defconst t-public-license-alist
  '((nil "Not Specified")
    (all-rights-reserved "All Rights Reserved")
    (all-rights-reversed "All Rights Reversed")
    (cc0 "CC0" "https://creativecommons.org/public-domain/cc0/")
    ;; 4.0
    ( cc-by-4.0 "CC BY 4.0"
      "https://creativecommons.org/licenses/by/4.0/")
    ( cc-by-nc-4.0 "CC BY-NC 4.0"
      "https://creativecommons.org/licenses/by-nc/4.0/")
    ( cc-by-nc-nd-4.0 "CC BY-NC-ND 4.0"
      "https://creativecommons.org/licenses/by-nc-nd/4.0/")
    ( cc-by-nc-sa-4.0 "CC BY-NC-SA 4.0"
      "https://creativecommons.org/licenses/by-nc-sa/4.0/")
    ( cc-by-nd-4.0 "CC BY-ND 4.0"
      "https://creativecommons.org/licenses/by-nd/4.0/")
    ( cc-by-sa-4.0 "CC BY-SA 4.0"
      "https://creativecommons.org/licenses/by-sa/4.0/")
    ;; 3.0 (not recommended by Creative Commons)
    ( cc-by-3.0 "CC BY 3.0"
      "https://creativecommons.org/licenses/by/3.0/")
    ( cc-by-nc-3.0 "CC BY-NC 3.0"
      "https://creativecommons.org/licenses/by-nc/3.0/")
    ( cc-by-nc-nd-3.0 "CC BY-NC-ND 3.0"
      "https://creativecommons.org/licenses/by-nc-nd/3.0/")
    ( cc-by-nc-sa-3.0 "CC BY-NC-SA 3.0"
      "https://creativecommons.org/licenses/by-nc-sa/3.0/")
    ( cc-by-nd-3.0 "CC BY-ND 3.0"
      "https://creativecommons.org/licenses/by-nd/3.0/")
    ( cc-by-sa-3.0 "CC BY-SA 3.0"
      "https://creativecommons.org/licenses/by-sa/3.0/"))
  "Alist mapping license symbols to their display names and URLs.
Each element is of form (SYMBOL DISPLAY-NAME &optional URL).")

(defvar t--cc-svg-hashtable (make-hash-table :test 'equal)
  "Hash table stores base64 encoded svg file contents.

Include cc, by, sa, nc, nd, and zero.")

(defun t--load-cc-svg (name)
  "Load SVG file with given NAME from assets directory, return as
base64 encoded string. If the file does not exist, raise an error."
  (declare (ftype (function (string) string))
           (important-return-value t))
  (let ((file (file-name-concat t--dir "assets" (concat name ".svg"))))
    (if (not (file-exists-p file))
        (t-error "Svg budget not exists: %s" file)
      (with-temp-buffer
        (t--insert-file file)
        (base64-encode-region (point-min) (point-max) t)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun t--load-cc-svg-once (name)
  "Load SVG file with given NAME once and cache it in a hash table.
If the SVG is already cached, return the cached base64 string."
  (declare (ftype (function (string) string))
           (important-return-value t))
  (with-memoization (gethash name t--cc-svg-hashtable)
    (t--load-cc-svg name)))

(defun t--build-cc-img (base64)
  "Create HTML img tag with embedded BASE64 encoded SVG.

See https://chooser-beta.creativecommons.org/"
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (format "<img style=\"height:1.4em!important;margin-left:0.2em;\
vertical-align:text-bottom;\" src=\"data:image/svg+xml;base64,%s\" \
alt=\"\">" base64))

(defun t--get-cc-svgs (license)
  "Get HTML img tags for Creative Commons LICENSE icons.

For CC0 license, returns both `cc' and `zero' icons. For other licenses,
splits the license name to get individual component icons."
  (declare (ftype (function (symbol) string))
           (important-return-value t))
  (let ((names (if (eq license 'cc0) '("cc" "zero")
                 (split-string (symbol-name license) "[0-9.-]" t)))
        (f (lambda (x) (t--build-cc-img (t--load-cc-svg-once x)))))
    (mapconcat f names)))

(defun t--get-info-author (info)
  "Get exported author string from INFO if :with-author is non-nil."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* (((t--pget info :with-author))
              (a (t--pget info :author)))
    (t--nw-trim (org-export-data a info))))

(defun t-format-license-default-function (info)
  "Generate HTML string describing the public license for a work.

Extracts license information from INFO plist and formats it with author
attribution and appropriate Creative Commons icons when applicable."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let* ((license (t--pget info :html-license))
         (details (assq license t-public-license-alist))
         (is-cc (string-match-p "^cc" (symbol-name license)))
         (use-budget (t--pget info :html-use-cc-budget))
         (author (t--get-info-author info)))
    (unless details
      (t-error "Unknown license: %s" license))
    (pcase (cdr details)
      (`(,name) name)
      (`(,name ,link)
       (concat
        "This work"
        (when (and author (not (eq license 'cc0)))
          (concat " by " author))
        " is licensed under "
        (if (null link) name
          (format "<a href=\"%s\">%s</a>" link name))
        (when (and is-cc use-budget)
          (concat " " (t--get-cc-svgs license)))))
      (_ (t-error "Internal error")))))

(defun t-format-public-license (info)
  "Generate HTML string describing the public license for a work."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (funcall (t--pget info :html-format-license-function) info))

;;;; Preamble and Postamble

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-metadata-timestamp-format (`org-w3ctr-metadata-timestamp-format')
;; - :email (`user-mail-address')
;; - :with-email (`org-export-with-email')
;; - `org-export-date-timestamp-format'
;; - :creator (`org-w3ctr-creator-string')
;; - :html-validation-link (`org-w3ctr-validation-link')
;; - :html-preamble (`org-w3ctr-preamble')
;; - :html-postamble (`org-w3ctr-postamble')
;; - :with-date (`org-export-with-date')
;; - :with-creator (`org-export-with-creator')

;; Compared with `org-html-format-spec', rename to make the name more
;; specific, and add some helpful docstring.
(defun t--pre/postamble-format-spec (info)
  "Return format specification for preamble and postamble.

Supported format specifiers:
- %t means produce title.
- %s means produce subtitle.
- %d means produce (start)date.
- %T means produce current time formatted with pre/postamble format.
- %a means produce author.
- %e means produce mailto link.
- %c means produce creator string.
- %C means produce file modification time (if exists).
- %v means produce W3C HTML validation link."
  (declare (ftype (function (list) list))
           (important-return-value t))
  (let ((fmt (t--pget info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (t--pget info :title) info))
      (?s . ,(org-export-data (t--pget info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info fmt) info))
      (?T . ,(format-time-string fmt))
      (?a . ,(org-export-data (t--pget info :author) info))
      (?e . ,(mapconcat
              (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
              (split-string (t--pget info :email)  ",+ *")
              ", "))
      (?c . ,(t--pget info :creator))
      (?C . ,(let ((file (t--pget info :input-file)))
               (format-time-string
                fmt (and file (file-attribute-modification-time
                               (file-attributes file))))))
      (?v . ,(or (t--pget info :html-validation-link) "")))))

;; Modified preamble/postamble handling compared to ox-html:
;; - Remove `org-html-preamble-format' / `org-html-postamble-format'
;;   mechanism; values are now set directly through `org-w3ctr-preamble'
;;   and `org-w3ctr-postamble'.
;; - Drop the 'auto option for postamble; when value is a symbol:
;;   - Calls the symbol if it's a function.
;;   - Otherwise formats the symbol's string value if present.
(defun t--build-pre/postamble (type info)
  "Build the preamble or postamble string.

This function reads the configuration from `:html-preamble' or
`:html-postamble' based on TYPE.  TYPE should be the symbol `preamble'
or `postamble'."
  (declare (ftype (function (symbol list) string))
           (important-return-value t))
  (let ((section (t--pget info (intern (format ":html-%s" type))))
        (spec (t--pre/postamble-format-spec info))
        it)
    (cond
     ((null section) (setq it ""))
     ;; string formatted with `format-spec'.
     ((stringp section) (setq it (format-spec section spec)))
     ;; function.
     ((functionp section) (setq it (funcall section info)))
     ;; symbol's function cell is nil or not a function.
     ((symbolp section)
      (if-let* ((value (symbol-value section))
                ((t--nw-p value)))
          (setq it (format-spec value spec))
        ;; When pre/postamble's value type is symbol and symbol's
        ;; function cell is nil, its value cell must be string type.
        (t-error "Invalid %s symbol value: %s"
                 type (symbol-value section))))
     ;; not nil, string or symbol
     (t (t-error "Invalid %s: %s" type section)))
    (or (and (t--nw-p it) (org-element-normalize-string it)) "")))

;; Copied from `org-export-get-date'.
(defun t--get-info-date (info)
  "Extract and format the document's date from the INFO plist.

This function looks for a `:date' property in INFO that contains a
single Org timestamp. It returns the formatted timestamp as a string,
or nil if no valid date is found."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* ((date (t--pget info :date))
              ((and date (proper-list-p date) (null (cdr date))))
              ((org-element-type-p (car date) 'timestamp)))
    (t--format-timestamp-int (car date) info)))

;; Copied from `org-html-format-spec'.
(defun t--get-info-mtime (info)
  "Return the modification time of the input file as a formatted string.

If :input-file is not found, use current time."

  (declare (ftype (function (list) string))
           (important-return-value t))
  (format-time-string
   "%FT%RZ" (and-let* ((file (t--pget info :input-file))
                       (time (file-attribute-modification-time
                              (file-attributes file)))))
   t))

(defun t-preamble-default-function (info)
  "Return a default HTML preamble string with document metadata.

The generated HTML uses a <details> element to display the document's
publication date, modification date, creator tools, and license.
It takes the export options plist INFO as its argument."
  (concat
   "<details open>\n"
   "<summary>More details about this document</summary>\n"
   "<dl>\n"
   ;; Create or finish time.
   "<dt>Drafting to Completion / Publication:</dt> <dd>"
   (or (t--get-info-date info) "[Not Specified]")
   "</dd>\n"
   ;; Modification time.
   "<dt>Date of last modification:</dt> <dd>"
   (t--get-info-mtime info)
   "</dd>\n"
   ;; Creation tools.
   "<dt>Creation Tools:</dt> <dd>"
   (or (t--pget info :creator) "[Not Specified]")
   "</dd>\n"
   ;; License.
   "<dt>Public License:</dt> <dd>"
   (t-format-public-license info)
   "</dd>\n"
   "</dl>\n"
   "</details>\n"
   "<hr>"))

(defconst t-preamble-example "\
<details open>
  <summary>More details about this document</summary>
  <dl>
    <dt>Date:</dt> <dd>%d</dd>
    <dt>Creator:</dt> <dd>%c</dd>
    <dt>License:</dt> <dd>This work is licensed under CC BY-SA 4.0</dd>
  </dl>
</details>
<hr>"
  "Default HTML template for document preamble metadata section.

Note: This variable is provided as an example only and may need
adaptation for actual project use.")

;;;; Table of Contents

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; :html-toc-element (`org-w3ctr-toc-element')
;; :with-toc (`org-export-with-toc')

(defun t--toc-headline-secno (headline info)
  "Return section number for HEADLINE as an HTML span."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (when-let* (((org-export-numbered-headline-p headline info))
              (numbers (org-export-get-headline-number headline info)))
    (format "<span class=\"secno\">%s</span>"
            (mapconcat #'number-to-string numbers "."))))

(defun t--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (format "<a href=\"#%s\">%s</a>" (t--reference headline info)
          (concat (and (not (t--low-level-headline-p headline info))
                       (t--toc-headline-secno headline info))
                  (t--build-toc-headline headline info))))

(defun t--get-info-toc-element (info)
  "Return the HTML tag (`ul' or `ol') for the TOC list from INFO.

This function retrieves the value of the :html-toc-element
property from the INFO plist. It ensures the value is a valid
tag, either \\='ul or \\='ol, and returns the corresponding string.
It signals an error for any other value."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let ((tag (t--pget info :html-toc-element)))
    (pcase tag
      (`ul "ul") (`ol "ol")
      (_ (t-error "Invalid TOC list tag: %s" tag)))))

(defun t--toc-alist-to-text (toc-entries info &optional top)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (declare (ftype (function (list list &optional boolean) string))
           (important-return-value t))
  (let* ((prev-level (or (and top 0) (1- (cdar toc-entries))))
         (start-level prev-level)
         (tag (t--get-info-toc-element info))
         (open (format "\n<%s class=\"toc\">\n<li>" tag))
         (close (format "</li>\n</%s>\n" tag)))
    (concat
     (mapconcat
      (pcase-lambda (`(,headline . ,level))
        (let* ((cnt (- level prev-level))
               (times (if (> cnt 0) (1- cnt) (- cnt))))
          (setq prev-level level)
          (concat
           (t--make-string
            times (cond ((> cnt 0) open) ((< cnt 0) close)))
           (if (> cnt 0) open "</li>\n<li>")
           headline)))
      toc-entries "")
     (t--make-string (- prev-level start-level) close))))

;; FIXME: Improve doc.
;; Compose with headline, interesting www
(defun t--build-table-of-contents (info)
  "Build top-level table of contents."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (let ((fn (lambda (h) (cons (t--format-toc-headline h info)
                              (org-export-get-relative-level h info)))))
    (when-let* ((depth (t--pget info :with-toc))
                (headlines (org-export-collect-headlines info depth))
                (entries (mapcar fn headlines)))
      (concat
       "<nav id=\"toc\">\n"
       (let ((top-level (t--pget info :html-toplevel-hlevel)))
         (format "<h%d>%s</h%d>"
                 top-level "Table of Contents" top-level))
       (t--toc-alist-to-text entries info t)
       "</nav>\n"))))

(defun t--build-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((fn (lambda (h) (cons (t--format-toc-headline h info)
                              (org-export-get-relative-level h info)))))
    (when-let* ((hs (org-export-collect-headlines info depth scope))
                (entries (mapcar fn hs)))
      (t--toc-alist-to-text entries info (not scope)))))

;; FIXME: Add index class to ul
;; FIXME: Add named-only argument (MAYBE)
(defun t--list-of-elements (collect-fn info)
  "Return an HTML list of elements collected by COLLECT-FN.

COLLECT-FN is a function that takes INFO and returns a list of Org
elements. INFO is the export state plist.

The function generates a `<ul>' list where each list item corresponds to
an element, displaying its caption. If the element has a reference
label, the item is hyperlinked to it."
  (declare (ftype (function (function list) (or null string)))
           (important-return-value t))
  (when-let* ((entries (funcall collect-fn info)))
    (concat
     "<ul class=\"index\">\n"
     (thread-first
       (lambda (entry)
         (let* ((label (t--reference entry info t))
                (caption (or (org-export-get-caption entry t)
                             (org-export-get-caption entry)))
                (title (t--trim (org-export-data caption info))))
           (format "<li>%s</li>"
                   (if (not label) title
                     (format "<a href=\"#%s\">%s</a>" label title)))))
       (mapconcat entries "\n"))
     "\n</ul>")))

(defun t--list-of-listings (info)
  "Return a formatted HTML list of source code listings."
  (declare (ftype (function (list) (or null string))))
  (t--list-of-elements #'org-export-collect-listings info))

(defun t--list-of-tables (info)
  "Return a formatted HTML list of tables."
  (declare (ftype (function (list) (or null string))))
  (t--list-of-elements #'org-export-collect-tables info))

;; copied from `org-html-keyword'.
(defun t--keyword-toc (keyword value info)
  "Transcode a table of contents keyword VALUE.

VALUE determines the type of list to generate:
- \"tables\": A list of tables.
- \"listings\": A list of source code listings.
- \"headlines\": A table of contents for headlines. It can be
  followed by a number for depth and keywords like \":target\" or
  \"local\" for scope."
  (declare (ftype (function (t string list) (or (null string))))
           (important-return-value t))
  (let ((case-fold-search t))
    (cond
     ((string= "listings" value) (t--list-of-listings info))
     ((string= "tables" value) (t--list-of-tables info))
     ((string-match "\\<headlines\\>" value)
      (let ((depth (and (string-match "\\<[0-9]+\\>" value)
                        (string-to-number (match-string 0 value))))
            (scope
             (cond
              ;; link
              ((string-match ":target +\\(\".+?\"\\|\\S-+\\)" value)
               (org-export-resolve-link
                (org-strip-quotes (match-string 1 value)) info))
              ;; local headline
              ((string-match-p "\\<local\\>" value) keyword))))
        (t--build-toc depth info scope))))))

;;;; Template

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; :language (`org-export-default-language')
;; :html-back-to-top (`org-w3ctr-back-to-top')
;; :html-fixup-js (`org-w3ctr-fixup-js')

;; FIXME: Consider use :with-title
;; Maybe I have use it in above or below codes.
(defun t-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string."
  (declare (ftype (function ((or null string) list) string))
           (important-return-value t))
  ;; See also `org-html-inner-template'.
  (concat
   t--zeroth-section-output
   (t--build-table-of-contents info)
   "<main>\n"
   contents
   "</main>\n"
   (t-footnote-section info)))

(defun t--build-title (info)
  "Build the HTML for the document title and subtitle.

This function generates the `<h1>' title and an associated
paragraph for the subtitle. It only produces output if
:with-title is non-nil in the INFO plist."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (when (plist-get info :with-title)
    (let ((title (t--pget info :title))
          (subtitle (plist-get info :subtitle)))
      (concat
       "<h1 id=\"title\">"
       (let ((tit (org-export-data title info)))
         (or (t--nw-p tit)  "&lrm;"))
       "</h1>\n"
       ;; FIXME: Consider use subtitle, not w3c-state
       (let ((sub (org-export-data subtitle info)))
         (format "<p id=\"w3c-state\">%s</p>\n" sub))))))

(defun t-template-1 (contents info)
  "Assemble the full HTML document structure around CONTENTS.

This function generates the complete HTML page, including the `<html>',
`<head>', and `<body>' tags. It orchestrates the inclusion of the
navbar, title, preamble, postamble, and other standard page elements."
  (declare (ftype (function (string list) string))
           (important-return-value t))
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   (t--build-head info)
   "<body>\n"
   ;; home and up links
   (when-let* ((fun (plist-get info :html-format-navbar-function)))
     (funcall fun info))
   ;; title and preamble
   (format "<div class=\"head\">\n%s%s</div>\n"
           (t--build-title info)
           (t--build-pre/postamble 'preamble info))
   contents
   ;; back-to-top
   (when (plist-get info :html-back-to-top)
     t-back-to-top-arrow)
   ;; Postamble.
   (t--build-pre/postamble 'postamble info)
   ;; fixup.js here
   (when-let* ((js (t--nw-p (plist-get info :html-fixup-js))))
     (org-element-normalize-string js))
   ;; Closing document.
   "</body>\n</html>"))

(defun t-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (declare (ftype (function (string list) string))
           (important-return-value t))
  (prog1 (t-template-1 contents info)
    (static-when t--oinfo-cache-p (t--oinfo-cleanup))))

;;; Complex elements
;;;; Table

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-table-use-header-tags-for-first-column
;;   (`org-w3ctr-table-use-header-tags-for-first-column')

(defun t--table-column-cookie (table column info)
  "Return the explicit alignment cookie for COLUMN in TABLE, or nil.

The value is taken from the last special row providing an `<l>',
`<c>', or `<r>' cookie for COLUMN.  A number in the cookie denotes
a column width and is ignored; a width-only cookie (e.g. `<5>') is
not an alignment."
  (declare (ftype (function (t fixnum list) (or null symbol)))
           (important-return-value t))
  (let (align)
    (dolist (row (org-element-contents table) align)
      (when (org-export-table-row-is-special-p row info)
        (let* ((cells (org-element-contents row))
               (value (and (< column (length cells))
                           (org-element-contents (nth column cells)))))
          (when (and value (null (cdr value)) (stringp (car value))
                     (string-match "\\`<\\([lrc]\\)?\\([0-9]+\\)?>\\'"
                                   (car value))
                     (match-string 1 (car value)))
            (setq align (pcase (match-string 1 (car value))
                          ("l" 'left) ("c" 'center) ("r" 'right)))))))))

(defun t--table-cell-align (cell info)
  "Return the explicit alignment for CELL's column, or nil.

The alignment is taken from the last `<l>', `<c>', or `<r>' cookie
in the column.  When the column has no explicit cookie, return nil
so that the CSS decides; Org's number-fraction heuristic is not
used.  Results are memoized per table in INFO under
`:html-table-align-cache' (the symbol `none' marks a column that
was computed and has no cookie)."
  (declare (ftype (function (t list) (or null symbol)))
           (important-return-value t))
  (let* ((row (org-element-parent cell))
         (table (org-export-get-parent-table cell))
         (cells (org-element-contents row))
         (column (- (length cells) (length (memq cell cells))))
         (cache (or (t--pget info :html-table-align-cache)
                    (let ((h (make-hash-table :test #'eq)))
                      (t--pput info :html-table-align-cache h)
                      h)))
         (vector (or (gethash table cache)
                     (puthash table (make-vector (length cells) nil) cache))))
    (when (>= column (length vector))
      (setq vector (vconcat vector
                            (make-list (- (1+ column) (length vector)) nil)))
      (puthash table vector cache))
    (let ((cached (aref vector column)))
      (if cached
          (if (eq cached 'none) nil cached)
        (let ((align (t--table-column-cookie table column info)))
          (aset vector column (or align 'none))
          align)))))

(defun t--table-cell-attrs (cell info)
  "Return CELL's inline alignment attribute, or the empty string.

Only an explicit Org alignment cookie produces an attribute; a
column without a cookie is left to the CSS."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if-let* ((align (t--table-cell-align cell info)))
      (format " style=\"text-align:%s\"" align) ""))

(defun t--table-column-specs (table info)
  "Return the <colgroup> markup describing TABLE's column groups.

Each column group is emitted as a single <colgroup span=\"N\">
element.  `<col>' children are omitted: alignment now lives on the
cells, and no other per-column attribute is expressible in Org."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((n 0) out)
    (dolist (cell (t--table-first-row-data-cells table info))
      (setq n (1+ n))
      (when (org-export-table-cell-ends-colgroup-p cell info)
        (push (format "\n<colgroup span=\"%d\">" n) out)
        (setq n 0)))
    (mapconcat #'identity (nreverse out) "")))

(defun t--table-caption (table info)
  "Return TABLE's <caption> element, or the empty string.

The caption is emitted as the table's first child; its visual
position is left to CSS (`caption-side')."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if-let* ((caption (org-export-get-caption table)))
      (format "<caption>%s</caption>" (org-export-data caption info))
    ""))

(defun t--table-first-row-data-cells (table info)
  "Return the cells of TABLE's first non-rule row.
When TABLE has a special column, its first cell is dropped."
  (declare (ftype (function (t list) list))
           (important-return-value t))
  (let ((row (org-element-map table 'table-row
               (lambda (r)
                 (unless (eq (org-element-property :type r) 'rule) r))
               info 'first-match)))
    (if (not (org-export-table-has-special-column-p table))
        (org-element-contents row)
      (cdr (org-element-contents row)))))

(defun t-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is the cell's contents.  INFO is a plist used as a
communication channel."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((row (org-element-parent table-cell))
         (table (org-export-get-parent-table table-cell))
         (attrs (t--table-cell-attrs table-cell info))
         (contents (if (or (not contents) (string= "" (org-trim contents)))
                       "&#xa0;" contents)))
    (cond
     ((and (org-export-table-has-header-p table info)
           (= 1 (org-export-table-row-group row info)))
      (format "\n<th scope=\"col\"%s>%s</th>" attrs contents))
     ((and (t--pget info :html-table-use-header-tags-for-first-column)
           (zerop (cdr (org-export-table-cell-address table-cell info))))
      (format "\n<th scope=\"row\"%s>%s</th>" attrs contents))
     (t
      (format "\n<td%s>%s</td>" attrs contents)))))

(defun t-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (declare (ftype (function (t (or null string) list) (or null string)))
           (important-return-value t))
  ;; Rules are ignored since table separators are deduced from the
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((group (org-export-table-row-group table-row info))
           (start (org-export-table-row-starts-rowgroup-p table-row info))
           (end (org-export-table-row-ends-rowgroup-p table-row info))
           (group-tags
            (cond
             ((not (= 1 group)) '("<tbody>" . "\n</tbody>"))
             ((org-export-table-has-header-p
               (org-export-get-parent-table table-row) info)
              '("<thead>" . "\n</thead>"))
             (t '("<tbody>" . "\n</tbody>")))))
      (concat (and start (car group-tags))
              (concat "\n<tr>" contents "\n</tr>")
              (and end (cdr group-tags))))))

(defun t-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      ;; (Modern-HTML reimplementation pending.)
      (t--table.el-table table info)
    ;; Standard table.
    (format "<table%s>\n%s\n%s\n%s</table>"
            (t--make-attr__id* table info t)
            (t--table-caption table info)
            (t--table-column-specs table info)
            contents)))

(defun t--table.el-table (table _info)
  "Format a table.el TABLE into HTML.
INFO is a plist used as a communication channel.
Output is delegated to `table-generate-source' for now; a
modern-HTML reimplementation is planned."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
                      (get-buffer-create "*org-export-table*")
                    (erase-buffer) (current-buffer))))
      (with-temp-buffer
        (insert (org-element-property :value table))
        (goto-char (point-min))
        (re-search-forward "^[ \t]*|[^|]" nil t)
        (table-generate-source 'html outbuf))
      (with-current-buffer outbuf
        (prog1 (org-trim (buffer-string))
          (kill-buffer))))))

;;;; LaTeX

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-math-custom-render-function
;;   (`org-w3ctr-math-custom-render-function')

(defun t-math-custom-default-render-function (frag _info)
  "Default value for `org-w3ctr-math-custom-render-function'."
  (declare (ftype (function (string t) string))
           (pure t) (important-return-value t))
  frag)

(defun t--normalize-latex (frag)
  "Normalize the delimiters of LaTeX FRAG for client-side MathJax.

Inline `$...$' becomes `\\(...\\)' and display `$$...$$' becomes
`\\[...\\]'; anything else is returned unchanged."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (cond
   ((string-prefix-p "$$" frag) (concat "\\[" (substring frag 2 -2) "\\]"))
   ((string-prefix-p "$" frag) (concat "\\(" (substring frag 1 -1) "\\)"))
   (t frag)))

(defun t--format-latex (frag mode info)
  "Return the HTML for LaTeX fragment FRAG under MODE.
MODE is the value of `:with-latex'; INFO is the export state."
  (declare (ftype (function (string t list) string))
           (important-return-value t))
  (pcase mode
    ((or `nil `verbatim) frag)
    (`mathjax (t--normalize-latex frag))
    (`mathml-by-mathjax
     (t--jstools-call 'tex2mml (t--normalize-latex frag)))
    (`svg-by-mathjax
     (t--jstools-call 'tex2svg (t--normalize-latex frag)))
    (`custom
     (funcall (t--pget info :html-math-custom-render-function) frag info))
    (o (error "Unknown LaTeX mode: %s" o))))

(defun t-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (t--format-latex
   (org-element-property :value latex-fragment)
   (t--pget info :with-latex) info))

(defun t-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (t--format-latex
   (org-remove-indentation (org-element-property :value latex-environment))
   (t--pget info :with-latex) info))

;;;; Link

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-link-org-files-as-html (`org-w3ctr-link-org-files-as-html')
;; - :html-inline-images (`org-w3ctr-inline-images')
;; - :html-inline-image-rules (`org-w3ctr-inline-image-rules')
;; - :html-extension (`org-w3ctr-extension')
;; - :html-equation-reference-format
;;   (`org-w3ctr-equation-reference-format')

(defun t-image-link-filter (data _backend info)
  "Process image links that are inside descriptions.
DATA is the parse tree.  INFO is an info plist.
See `org-export-insert-image-links' for more details."
  (declare (ftype (function (t t list) t))
           (important-return-value t))
  (org-export-insert-image-links data info t-inline-image-rules))

;; The "exactly one link" rule below fixes an upstream Org bug found
;; while refactoring `ox-html.el'; see
;; https://lists.gnu.org/archive/html/emacs-orgmode/2026-09/msg00073.html
(defun t--sole-image-link-p (objects link-image-p)
  "Non-nil when OBJECTS hold white space and exactly one image link.

OBJECTS is a list of elements or objects (a link's description, or a
paragraph's contents).  LINK-IMAGE-P is called with each link object
and must return non-nil when that link counts as an image.

Links nested inside another link's description are not visited, so
only the top-level links are counted."
  (declare (ftype (function (list function) boolean))
           (pure t) (important-return-value t))
  (let ((link-count 0)
        (clean t))
    (org-element-map objects
        (cons 'plain-text org-element-all-objects)
      (lambda (obj)
        (when (pcase (org-element-type obj)
                (`plain-text (org-string-nw-p obj))
                (`link (or (> (incf link-count) 1)
                           (not (funcall link-image-p obj))))
                (_ t))
          (setq clean nil)))
      nil nil 'link)
    (and clean (= link-count 1))))

(defun t--link-description-image-p (link rules)
  "Non-nil when LINK's description is a single link to an image.

The description may only hold white space and one link, and that
link must target an image (see `t-inline-image-rules')."
  (declare (ftype (function (t list) boolean))
           (pure t) (important-return-value t))
  (t--sole-image-link-p
   (org-element-contents link)
   (lambda (obj) (org-export-inline-image-p obj rules))))

(defun t-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
LINK is an inline image in either of two cases:
  - it has no description and targets an image file (see
    `org-w3ctr-inline-image-rules'); or
  - its description is a single link to an image file.

The second case is the \"clickable image\" construct.  Org does not
parse nested links, so `[[URL][file:img.png]]' starts out as a link
whose description is the plain text \"file:img.png\".  The
`org-w3ctr-image-link-filter' parse-tree filter turns that text into a
real nested link (see `org-export-insert-image-links'), after which the
outer link is recognized here as an image.

The description has to be link-like (\"file:...\", \"https://...\"); a
bare file name such as `[[URL][img.png]]' is not enough."
  (declare (ftype (function (t list) t))
           (important-return-value t))
  (let ((rules (t--pget info :html-inline-image-rules)))
    (if (not (org-element-contents link))
        (org-export-inline-image-p link rules)
      (t--link-description-image-p link rules))))

(defun t-standalone-image-p (element info &optional predicate)
  "Non-nil if ELEMENT is a standalone image.
INFO is a plist holding contextual information.

PREDICATE, when non-nil, is called with the containing paragraph and
must return non-nil for ELEMENT to count as a standalone image.

An element or object is a standalone image when
  - its type is `paragraph' and its sole content, save for white
    spaces, is a link that qualifies as an inline image;
  - its type is `link' and its containing paragraph has no other
    content save white spaces."
  (declare (ftype (function (t list &optional t) t))
           (important-return-value t))
  (let ((paragraph (pcase (org-element-type element)
                     (`paragraph element)
                     (`link (org-element-parent element)))))
    (and (eq (org-element-type paragraph) 'paragraph)
         (or (not predicate) (funcall predicate paragraph))
         (t--sole-image-link-p
          (org-element-contents paragraph)
          (lambda (obj) (t-inline-image-p obj info))))))

(defun t--link-image-p (link info)
  "Non-nil when LINK should be exported as an inline image.
Inline images must be enabled and LINK itself must target an image
file (see `t-inline-image-p')."
  (declare (ftype (function (t list) t))
           (important-return-value t))
  (and (t--pget info :html-inline-images)
       (org-export-inline-image-p
        link (t--pget info :html-inline-image-rules))))

(defun t--format-image (source attributes)
  "Return an \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by `org-export-read-attribute'."
  (declare (ftype (function (string list) string))
           (important-return-value t))
  (t--void-element
   "img"
   (t--make-attribute-string
    (org-combine-plists
     (list :src source :alt (file-name-nondirectory source))
     attributes))))

(defun t--link-org-files-as-html (raw-path info)
  "Return RAW-PATH with a \".org\" extension turned into the
configured `:html-extension' (usually \".html\").

Only do so when `:html-link-org-files-as-html' is non-nil and
RAW-PATH actually ends in \".org\"; otherwise return RAW-PATH
unchanged."
  (declare (ftype (function (string list) string))
           (pure t) (important-return-value t))
  (if-let* ((_ (t--pget info :html-link-org-files-as-html))
            (src-ext (downcase (file-name-extension raw-path ".")))
            (_ (string= ".org" src-ext)))
      (let* ((ext (t--pget info :html-extension))
             (dot (if (string-empty-p ext) "" ".")))
        (concat (file-name-sans-extension raw-path) dot ext))
    raw-path))

(defun t--link-path (link info)
  "Return the URL to use for LINK.

A file link becomes a \"file:\" URI (relative during publishing),
with its \".org\" extension rewritten to `:html-extension' (usually
\".html\") when requested, and any search option appended.  Other
link types get their \"TYPE:\" prefix and URL encoding."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((type (org-element-property :type link))
        (raw-path (org-element-property :path link)))
    (if (not (string= "file" type))
        (url-encode-url (concat type ":" raw-path))
      ;; FIXME: Replace `org-publish-file-relative-name' with a local
      ;; implementation (see the ox-publish non-goal in AGENTS.md).
      (let* ((raw-path (org-publish-file-relative-name raw-path info))
             ;; FIXME: Do we really need `:html-link-home' and
             ;; `:html-link-use-abs-url'?  ox-html prepends the home
             ;; directory to relative file paths here; ox-w3ctr has
             ;; never implemented this.
             (path (t--link-org-files-as-html
                    (org-export-file-uri raw-path) info))
             (option (org-element-property :search-option link)))
        (if (not option) path
          (concat path "#"
                  ;; FIXME: Replace `org-publish-resolve-external-link'
                  ;; with a local implementation (see the ox-publish
                  ;; non-goal in AGENTS.md).
                  (org-publish-resolve-external-link
                   option (org-element-property :path link) t)))))))

(defun t--link-attributes (link info)
  "Return the HTML attribute plist for LINK.

Attributes are read from LINK itself and, when LINK is the image of
a standalone-image paragraph (i.e., it becomes the <img> element of
a figure), from that paragraph as well, so that `#+attr_html' on
the paragraph applies to the image.  In any other paragraph the
attribute stays on the paragraph and is not duplicated onto every
image it contains."
  (declare (ftype (function (t list) list))
           (important-return-value t))
  (let ((parent (org-element-parent-element link)))
    (org-combine-plists
     ;; `org-element-parent-element' returns the nearest *element*
     ;; (paragraph, item, ...), skipping intermediate objects, so the
     ;; enclosing link of a clickable image is never consulted here.
     ;; The parent's attribute is inherited only in a standalone-image
     ;; paragraph; otherwise it belongs to the paragraph itself.
     (and (org-export-inline-image-p
           link (t--pget info :html-inline-image-rules))
          (t-standalone-image-p parent info)
          (org-export-read-attribute :attr_html parent))
     ;; A link is an object, not an element, and `:attr_html' is an
     ;; affiliated keyword, so this is almost always nil; it only has a
     ;; value when a backend sets it programmatically.
     (org-export-read-attribute :attr_html link))))

(defun t--link-radio (link desc info attributes)
  "Transcode a radio target LINK.
DESC and ATTRIBUTES are as computed in `t-link'.  INFO is the
export state."
  (declare (ftype (function (t (or null string) list string) string))
           (important-return-value t))
  (let ((destination (org-export-resolve-radio-link link info)))
    (if (not destination) desc
      (format "<a href=\"#%s\"%s>%s</a>"
              (t--reference destination info)
              attributes
              desc))))

(defun t--link-to-file (destination path desc attributes info)
  "Transcode an ID link pointing to an external file.
DESTINATION is the target file name and PATH is the link's path.
DESC and ATTRIBUTES are as computed in `t-link'.  INFO is the
export state."
  (declare (ftype (function (t string (or null string) string list) string))
           (important-return-value t))
  ;; FIXME: PATH is `t--link-path''s output, e.g. "id:xyz", but the
  ;; fragment must match the anchor emitted for the ID ("ID-xyz").
  ;; ox-html uses the raw path here, so this looks broken for
  ;; `[[id:...]]' links pointing at another file.
  (format "<a href=\"%s#%s\"%s>%s</a>"
          (t--link-org-files-as-html destination info)
          (concat t--id-attr-prefix path)
          attributes
          (or desc destination)))

(defun t--link-broken (link desc info)
  "Transcode LINK that points nowhere.
DESC is the link's description.  INFO is the export state."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (format "<i>%s</i>"
          (or desc
              (org-export-data
               (org-element-property :raw-link link) info))))

(defun t--link-to-headline (destination desc attributes info)
  "Transcode a link to headline DESTINATION.
DESC and ATTRIBUTES are as computed in `t-link'.  INFO is the
export state."
  (declare (ftype (function (t (or null string) string list) string))
           (important-return-value t))
  ;; FIXME: Rethink the auto-generated section number below: links
  ;; should carry an explicit description rather than a
  ;; system-generated one (see the same decision for `t--link-target').
  (let* ((href (t--reference destination info))
         (desc (if (and (org-export-numbered-headline-p destination info)
                        (not desc))
                   (mapconcat #'number-to-string
                              (org-export-get-headline-number destination info)
                              ".")
                 (or desc
                     (org-export-data
                      (org-element-property :title destination) info)))))
    (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))

(defun t--link-equation (destination info)
  "Transcode a reference to a math latex-environment DESTINATION.
The label lives inside the LaTeX, so client-side MathJax resolves
the reference.  INFO is the export state."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  ;; FIXME: LaTeX handling is weak here.  This just emits the raw
  ;; `:html-equation-reference-format' (default "\\eqref{%s}") and
  ;; trusts MathJax to resolve it; nothing ensures the environment
  ;; actually carries a matching \label.
  (format (t--pget info :html-equation-reference-format)
          (t--reference destination info)))

(defun t--link-target (desc info destination attributes)
  "Transcode a link whose fuzzy DESTINATION is a target or element.
The description is the link's own DESC; no number is generated for
it, so a link without a description falls back to a fixed string.
DESC and ATTRIBUTES are as in `org-w3ctr-link'."
  (declare (ftype (function ((or null string) list t string) string))
           (important-return-value t))
  ;; FIXME: Rethink whether to use information from the destination
  ;; (image alt/caption, target value, ...) to build the description
  ;; instead of the fixed string below.
  (format "<a href=\"#%s\"%s>%s</a>"
          (t--reference destination info)
          attributes
          (or desc "No description for this link")))

(defun t--link-dispatch (link desc info path attributes)
  "Transcode LINK resolved through its ID, custom ID or fuzzy target.
PATH, DESC and ATTRIBUTES are as computed in `t-link'."
  (declare (ftype (function (t (or null string) list string string) string))
           (important-return-value t))
  (let* ((type (org-element-property :type link))
         (destination (if (string= type "fuzzy")
                          (org-export-resolve-fuzzy-link link info)
                        (org-export-resolve-id-link link info))))
    (pcase (org-element-type destination)
      (`plain-text (t--link-to-file destination path desc attributes info))
      (`nil (t--link-broken link desc info))
      (`headline (t--link-to-headline destination desc attributes info))
      (_
       ;; FIXME: LaTeX handling is weak here: only math
       ;; `latex-environment's under the `mathjax' (and legacy `t')
       ;; `:with-latex' mode reach `t--link-equation'.  The
       ;; `mathml-by-mathjax' / `svg-by-mathjax' / `custom' modes and
       ;; non-math environments fall through to `t--link-target'.
       (if (and destination
                (memq (t--pget info :with-latex) '(mathjax t))
                (eq 'latex-environment (org-element-type destination))
                (eq 'math (org-latex--environment-type destination)))
           ;; The label lives inside the LaTeX; let client-side MathJax
           ;; resolve the reference.
           (t--link-equation destination info)
         (t--link-target desc info destination attributes))))))

(defun t--link-coderef (path desc info attributes)
  "Transcode a coderef link with PATH, DESC and ATTRIBUTES."
  (declare (ftype (function (string (or null string) list string) string))
           (important-return-value t))
  ;; FIXME: Consider removing coderef support altogether; it is kept
  ;; for now only for compatibility with ox-html.el.
  (let ((fragment (concat "coderef-" (t--encode-plain-text path))))
    (format "<a href=\"#%s\" %s%s>%s</a>"
            fragment
            (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                    fragment fragment)
            attributes
            (format (org-export-get-coderef-format path desc)
                    (org-export-resolve-coderef path info)))))

(defun t--link-external (path desc attributes)
  "Transcode a plain external link with PATH, DESC and ATTRIBUTES.
When DESC is nil, PATH doubles as the description."
  (declare (ftype (function (string (or null string) string) string))
           (important-return-value t))
  (let ((path (t--encode-plain-text path)))
    (format "<a href=\"%s\"%s>%s</a>" path attributes (or desc path))))

(defun t-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((type (org-element-property :type link))
         (path (t--link-path link info))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (attributes-plist (t--link-attributes link info))
         (attributes (let ((attr (t--make-attribute-string attributes-plist)))
                       (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'w3ctr info))
     ;; Image file.
     ((t--link-image-p link info)
      (t--format-image path attributes-plist))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (t--link-radio link desc info attributes))
     ;; Links pointing to a headline, a target or an element.
     ((member type '("custom-id" "fuzzy" "id"))
      (t--link-dispatch link desc info path attributes))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (t--link-coderef path desc info attributes))
     ;; External link.
     (path
      (t--link-external path desc attributes))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

;;;; Footnote

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-footnotes-section (`org-w3ctr-footnotes-section')
;; - :html-footnote-format (`org-w3ctr-footnote-format')
;; - :html-footnote-separator (`org-w3ctr-footnote-separator')
;; - :html-footnote-section-function
;;   (`org-w3ctr-footnote-section-function')

(defun t--footnote-key (label n)
  "Return the key of a footnote with LABEL and number N.
A nil or purely numeric LABEL is ignored, so that `[fn:1]' and an
anonymous footnote do not share a key."
  (declare (ftype (function ((or null string) integer) (or string integer)))
           (pure t) (important-return-value t))
  (if (and label (not (string-match-p "\\`[0-9]+\\'" label)))
      label n))

(defun t--footnote-id (label n)
  "Return the HTML id for a footnote with LABEL and number N."
  (declare (ftype (function ((or null string) integer) string))
           (pure t) (important-return-value t))
  (format "fn-%s" (t--footnote-key label n)))

(defun t-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (org-element-type-p prev 'footnote-reference)
       (t--pget info :html-footnote-separator)))
   (let* ((label (org-element-property :label footnote-reference))
          (n (org-export-get-footnote-number footnote-reference info)))
     (format (t--pget info :html-footnote-format)
             (format "<a href=\"#%s\">%s</a>"
                     (t--footnote-id label n)
                     (t--footnote-key label n))))))

(defun t--footnote-definition (definition info)
  "Format a footnote DEFINITION.
DEFINITION is a (NUMBER LABEL DEF) tuple, as returned by
`org-export-collect-footnote-definitions'.  INFO is the export
state."
  (declare (ftype (function (list list) string))
           (important-return-value t))
  (pcase-let ((`(,n ,label ,def) definition))
    (format "<dt id=\"%s\">%s</dt>\n<dd>\n%s\n</dd>"
            (t--footnote-id label n)
            (format (t--pget info :html-footnote-format)
                    (t--footnote-key label n))
            (t--trim (org-export-data def info)))))

(defun t-footnote-section-default-function (definitions info)
  "Default function to build the footnotes section.
DEFINITIONS is the list returned by
`org-export-collect-footnote-definitions'.  INFO is the export
state."
  (declare (ftype (function (list list) string))
           (important-return-value t))
  (format (t--pget info :html-footnotes-section)
          "References"
          (format "\n%s\n"
                  (mapconcat (lambda (d) (t--footnote-definition d info))
                             definitions "\n"))))

(defun t-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (declare (ftype (function (list) t))
           (important-return-value t))
  (when-let* ((definitions (org-export-collect-footnote-definitions info)))
    (funcall (t--pget info :html-footnote-section-function) definitions info)))

;;;; Engrave-faces subset

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).

;; A self-contained subset of engrave-faces.el's HTML backend.
;; See https://github.com/tecosaur/engrave-faces (v0.3.1,
;; engrave-faces-html.el + engrave-faces.el).  The CSS for the slugs
;; below lives in assets/style.css (".ef-*").
;;
;; Unlike engrave-faces, there is no inline-style fallback: unknown or
;; unspecified faces are emitted as plain escaped text; the slug →
;; colour mapping is the stylesheet's job.

(defun t--engrave-buffer (&optional in-buffer out-buffer)
  "Engrave the fontified text of IN-BUFFER into OUT-BUFFER.
IN-BUFFER defaults to the current buffer; it must already be
fontified (see `font-lock-ensure').  OUT-BUFFER defaults to a fresh
\"*html*\" buffer and is returned."
  (declare (ftype (function (&optional buffer buffer) buffer))
           (important-return-value t))
  (let ((ibuf (or in-buffer (current-buffer)))
        (obuf (or out-buffer (generate-new-buffer "*html*")))
        (completed nil))
    (with-current-buffer ibuf
      (unwind-protect
          (let (next-change text)
            (goto-char (point-min))
            (while (not (eobp))
              (setq next-change (t--engrave-next-face-change (point)))
              (setq text (buffer-substring-no-properties (point) next-change))
              (when (> (length text) 0)
                (princ (t--engrave-face-transformer
                        (get-text-property (point) 'face)
                        text)
                       obuf))
              (goto-char next-change)))
        (setq completed t)))
    (if (not completed)
        (if out-buffer t (kill-buffer obuf))
      obuf)))

(defun t--engrave-next-face-change (pos &optional limit)
  "Return the position of the next face change after POS, up to LIMIT.
Merges text-property and overlay faces, and extends over `display'
properties.  Lifted from htmlize, via engrave-faces [2024-04-12]."
  (declare (ftype (function (integer &optional integer) integer))
           (important-return-value t))
  (unless limit (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (t--engrave-overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (t--engrave-overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defun t--engrave-overlay-faces-at (pos)
  "Return the non-nil `face' values of overlays at POS."
  (declare (ftype (function (integer) list))
           (important-return-value t))
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

(defun t--engrave-face-transformer (prop text)
  "Transform TEXT with face property PROP into an HTML span.
Whitespace-only runs and text without a known face are returned
escaped but unwrapped."
  (declare (ftype (function (t string) string))
           (pure t) (important-return-value t))
  (let ((escaped (t--encode-plain-text text))
        (style (t--engrave-get-style prop)))
    (if (or (string-match-p "\\`[\n[:space:]]+\\'" text)
            (not style))
        escaped
      (concat "<span class=\"ef-" (plist-get (cdr style) :slug) "\">"
              escaped "</span>"))))

(defconst t--engrave-style-plist
  '(;; faces.el --- excluding bold, italic, bold-italic, underline, …
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
    ;; css-mode: reuse the function-name / keyword colours.
    (css-property :slug "f")
    (css-selector :slug "k"))
  "Face → slug alist used by the engraving engine.
A slug is the compact CSS class emitted by `t--engrave-face-transformer';
the colours live in assets/style.css under \".ef-SLUG\".  `default' is
deliberately absent: bare (unfaced) text carries a nil face and is
emitted as plain text, as engrave-faces does.")

(defun t--engrave-get-style (prop)
  "Return the style entry for face property PROP, or nil.
PROP is a face name, a list of faces, or nil (no face).  A nil PROP
returns nil, so bare text gets no span."
  (declare (ftype (function (t) (or null cons)))
           (pure t) (important-return-value t))
  (cond
   ((null prop) nil)
   ((listp prop) (assoc (car prop) t--engrave-style-plist))
   (t (assoc prop t--engrave-style-plist))))

;;;; Source block

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).

(defun t--engrave-fontify-code (code lang)
  "Fontify CODE (a string) in LANG, returning bare fontified HTML.
The result contains `<span>' runs only, with no wrapper element:
callers wrap it in `<code>' as appropriate.  When LANG has no
associated major mode, CODE is returned escaped but uncoloured."
  (declare (ftype (function (string (or null string)) string))
           (important-return-value t))
  (let ((lang-mode (and lang (org-src-get-lang-mode lang))))
    (if (not (functionp lang-mode))
        (t--encode-plain-text code)
      (let ((inhibit-read-only t))
        (with-temp-buffer
          (let ((inbuf (current-buffer)))
            (funcall lang-mode)
            (insert code)
            (font-lock-ensure)
            (set-buffer-modified-p nil)
            (with-temp-buffer
              (ignore (t--engrave-buffer inbuf (current-buffer)))
              (buffer-string))))))))

(defun t--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a source or an example block."
  (let* ((code (car (org-export-unravel-code element)))
         (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
            (or (plist-get attr :width) 80)
            (or (plist-get attr :height) (org-count-lines code))
            code)))

(defun t-fontify-code (code lang)
  "Colorize CODE (a string) for LANG, returning bare fontified HTML.
The result contains `<span>' runs only, with no wrapper element:
callers wrap it in `<code>' as appropriate (block vs inline).  LANG
is a language name as a string, or nil.  Uses `t-fontify-method';
returns escaped plain text when it is nil or CODE is empty."
  (declare (ftype (function (string (or null string)) string))
           (important-return-value t))
  (if (and (not (string-empty-p code)) lang (eq t-fontify-method 'engrave))
      (t--engrave-fontify-code code lang)
    (t--encode-plain-text code)))

(defun t--src-code (src-block lang)
  "Return bare fontified HTML for SRC-BLOCK's code in LANG.
`org-export-unravel-code' also returns a coderef alist, which
ox-w3ctr does not support; drop it (the car)."
  (declare (ftype (function (t (or null string)) string))
           (important-return-value t))
  (t-fontify-code (car (org-export-unravel-code src-block)) lang))

(defun t--src-code-tag (lang code)
  "Wrap bare CODE in a `<code>' tag carrying LANG's class.
A nil LANG yields a plain `<code>' without a language class."
  (declare (ftype (function ((or null string) string) string))
           (pure t) (important-return-value t))
  (if lang
      (format "<code class=\"src src-%s\">%s</code>" lang code)
    (format "<code>%s</code>" code)))

(defun t--src-block-attrs (src-block info captioned)
  "Return the attribute string for SRC-BLOCK, as \" ATTRS\" or \"\".
Attributes come from `:attr__' (`#+attr__:').  An `id' is added
from the element reference unless the attributes already carry one.
CAPTIONED non-nil prepends the default `example' class to the class
list (class is written as a vector, e.g. `#+attr__: [foo]')."
  (declare (ftype (function (t list boolean) string))
           (important-return-value t))
  (let* ((reference (t--reference src-block info (not captioned)))
         (attributes (t--read-attr__ src-block)))
    (when captioned
      (let ((entry (or (assoc "class" attributes)
                       (assoc 'class attributes))))
        (if entry
            (setcdr entry (list (concat "example " (cadr entry))))
          (push (list "class" "example") attributes))))
    (let ((a (t--make-attr__
              (if (or (not reference)
                      (cl-find 'id attributes :key #'car-safe))
                  attributes
                (cons `("id" ,reference) attributes)))))
      (if (t--nw-p a) a ""))))

(defun t-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.

A `:textarea' attribute yields a `<textarea>'; otherwise the code is
wrapped in `<pre>', and a captioned block gets a `.example' `<div>'
wrapper with a self-link."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (cond
   ((org-export-read-attribute :attr_html src-block :textarea)
    (t--textarea-block src-block))
   ((org-element-property :caption src-block)
    (let* ((lang (org-element-property :language src-block))
           (code (t--src-code src-block lang))
           (id (t--reference src-block info))
           (caption (when-let* ((cap (org-export-get-caption src-block)))
                      (org-trim (org-export-data cap info)))))
      (format (concat "<div%s>\n"
                      "<a class=\"self-link\" href=\"#%s\""
                      " aria-label=\"source block\"></a>\n"
                      "%s\n<pre>\n%s</pre></div>")
              (t--src-block-attrs src-block info t)
              id (or caption "") (t--src-code-tag lang code))))
   (t
    (let* ((lang (org-element-property :language src-block))
           (code (t--src-code src-block lang)))
      (format "<pre%s>\n%s</pre>"
              (t--src-block-attrs src-block info nil)
              (t--src-code-tag lang code))))))

(defun t-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.
The code is fontified bare by `t-fontify-code' and wrapped here in a
single `<code class=\"src-inline src-LANG\">' (no nesting)."
  (declare (ftype (function (t t list) string))
           (important-return-value t))
  (let* ((lang (org-element-property :language inline-src-block))
         (code (t-fontify-code
                (org-element-property :value inline-src-block)
                lang))
         (label (if-let* ((lbl (t--reference inline-src-block info t)))
                    (format " id=\"%s\"" lbl)
                  "")))
    (format "<code class=\"src-inline src-%s\"%s>%s</code>" lang label code)))

;;;; Special Block

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; FIXME
;; See (info "(org)HTML doctypes")
(defconst t-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "noscript"
    "output" "progress" "section" "summary" "video")
  "Elements in html5.

For blocks that should contain headlines, use the HTML_CONTAINER
property on the headline itself.")

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

;;; Filter Functions

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
(defun t-final-function (contents _backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    (when (plist-get info :html-indent)
      (indent-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; End-user functions

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
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
;; coding: utf-8-unix
;; End:
