;;; ox-w3ctr.el --- An Org export Back-End -*- lexical-binding:t;-*-

;; Copyright (C) 2024-2025 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 2024-03-18 04:51:00+0900

;; Package-Version: 0.2.14
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
(require 'jsonrpc)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)
(require 'table)

;;;; Fundamental utilities
(defconst t-version "0.2.14"
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
    ;; The pairs follow the order of their implementations below; the
    ;; annotations keep Org's taxonomy, so family members that the
    ;; source keeps with their element sit beside it here too.
    ;;@ greater elements [11]
    ;; footnote-definition                      NO-EXIST
    ;; inlinetasks `inlinetask'                 NO-USE
    ;; property drawers `property-drawer'       NO-USE
    (center-block . t-center-block)             ; #+begin_center
    (drawer . t-drawer)                         ; :name: ... :end:
    (dynamic-block . t-dynamic-block)           ; #+begin: name para
    (footnote-reference . t-footnote-reference) ; [fn:] (an object)
    (item . t-item)                             ; plain list item
    (plain-list . t-plain-list)                 ; plain list
    (quote-block . t-quote-block)               ; #+begin_quote
    (special-block . t-special-block)           ; #+begin_{sth}
    (table-cell . t-table-cell)                 ; | | (an object)
    (table-row . t-table-row)                   ; | | (a lesser element)
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
    (latex-fragment . t-latex-fragment)         ; \(, \[ (an object)
    (latex-environment . t-latex-environment)   ; \begin
    (paragraph . t-paragraph)                   ; \n ... \n
    (verse-block . t-verse-block)               ; #+begin_verse
    (src-block . t-src-block)                   ; #+begin_src lang
    (inline-src-block . t-inline-src-block)     ; src_LANG{body} (an object)
    ;;@ objects [25]
    ;; citation                                 NO-USE
    ;; citation reference                       NO-USE
    ;; inline babel calls                       NO-EXIST
    ;; macros                                   NO-EXIST
    (entity . t-entity)                         ; \alpha, \cent
    (export-snippet . t-export-snippet)         ; @@html:something@@
    (line-break . t-line-break)                 ; \
    (target . t-target)                         ; <<target>>
    (radio-target . t-radio-target)             ; <<<contents>>>
    (statistics-cookie . t-statistics-cookie)   ; [%] [/]
    (subscript . t-subscript)                   ; a_{b}
    (superscript . t-superscript)               ; a^{b}
    (timestamp . t-timestamp)                   ; [<time-spec>]
    (link . t-link)                             ; [[...][...]]
    ;; smallest objects
    (bold . t-bold)                             ; *a*
    (italic . t-italic)                         ; /a/
    (underline . t-underline)                   ; _a/
    (verbatim . t-verbatim)                     ; =a=
    (code . t-code)                             ; ~a~
    (strike-through . t-strike-through)         ; +a+
    (plain-text . t-plain-text)
    ;;@ headline section [2]
    (section . t-section)
    (headline . t-headline)
    ;; top-level structure
    (inner-template . t-inner-template)
    (template . t-template))
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
  '(;; Reference
    (:html-prefer-user-labels nil nil t-prefer-user-labels)
    ;; Drawer
    (:html-format-drawer-function nil nil t-drawer-format-function)
    ;; Footnote
    (:html-footnotes-section nil nil t-footnotes-section)
    (:html-footnote-format nil nil t-footnote-format)
    (:html-footnote-separator nil nil t-footnote-separator)
    (:html-footnote-section-function nil nil t-footnote-section-function)
    ;; Item and Plain Lists
    (:html-checkbox-type nil nil t-checkbox-type)
    ;; Table
    (:html-table-use-header-tags-for-first-column
     nil nil t-table-use-header-tags-for-first-column)
    ;; LaTeX
    (:html-math-custom-render-function nil nil t-math-custom-render-function)
    ;; Timestamp
    (:html-timezone "HTML_TIMEZONE" nil t-timezone)
    (:html-export-timezone "HTML_EXPORT_TIMEZONE" nil t-export-timezone)
    (:html-datetime-option nil "dt" t-datetime-format-choice)
    (:html-timestamp-option nil "ts" t-timestamp-option)
    (:html-timestamp-wrapper nil "tsw" t-timestamp-wrapper-type)
    (:html-timestamp-formats nil "tsf" t-timestamp-formats)
    (:html-timestamp-format-function nil "tsfn" t-timestamp-format-function)
    ;; Link and Images
    (:html-link-org-files-as-html nil nil t-link-org-files-as-html)
    (:html-inline-images nil nil t-inline-images)
    (:html-inline-image-rules nil nil t-inline-image-rules)
    (:html-extension nil nil t-extension)
    (:html-equation-reference-format
     "HTML_EQUATION_REFERENCE_FORMAT" nil t-equation-reference-format)
    ;; Markup texts
    (:html-text-markup-alist nil nil t-text-markup-alist)
    ;; Todo
    (:html-todo-kwd-class-prefix nil nil t-todo-kwd-class-prefix)
    (:html-todo-format-function nil nil t-todo-format-function)
    ;; Priority
    (:html-priority-format-function nil nil t-priority-format-function)
    ;; Tags
    (:html-tags-format-function nil nil t-tags-format-function)
    (:html-tag-class-prefix nil nil t-tag-class-prefix)
    ;; Headline
    (:html-format-headline-function nil nil t-format-headline-function)
    (:html-toplevel-hlevel nil nil t-toplevel-hlevel)
    (:html-honor-ox-headline-levels nil nil t-honor-ox-headline-levels)
    (:html-container nil nil t-container-element)
    (:html-self-link-headlines nil nil t-self-link-headlines)
    (:html-heading-format-function nil nil t-heading-format-function)
    (:headline-levels nil "H" org-export-headline-levels)
    ;; <meta>
    (:html-file-timestamp-function nil nil t-file-timestamp-function)
    (:html-viewport nil nil t-viewport)
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    ;; Math
    (:with-latex nil "tex" t-with-latex)
    (:html-mathjax-config nil nil t-mathjax-config)
    (:html-math-head-function nil nil t-math-head-function)
    ;; <head>
    (:html-head "HTML_HEAD" nil t-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil t-head-extra newline)
    (:html-head-include-style nil "html-style" t-head-include-style)
    ;; Navbar
    (:html-link-home "HTML_LINK_HOME" nil t-link-home)
    (:html-link-up "HTML_LINK_UP" nil t-link-up)
    (:html-home/up-format "HTML_HOME/UP_FORMAT" nil t-home/up-format newline)
    (:html-link-navbar "HTML_LINK_NAVBAR" nil t-link-navbar parse)
    (:html-navbar-format-function nil nil t-navbar-format-function)
    ;; CC badges
    (:html-use-cc-badges nil "cc-badges" t-use-cc-badges)
    (:html-license nil "license" t-public-license)
    (:html-license-format-function nil nil t-license-format-function)
    (:html-cc-badges-format-function nil nil t-cc-badges-format-function)
    ;; Pre/Postamble
    (:html-metadata-timestamp-format nil nil t-metadata-timestamp-format)
    (:html-validation-link nil nil t-validation-link)
    (:html-preamble nil "html-preamble" t-preamble)
    (:html-postamble nil "html-postamble" t-postamble)
    (:creator "CREATOR" nil t-creator-string)
    ;; Table of Contents
    (:html-toc-element nil nil t-toc-element)
    (:html-toc-title nil nil t-toc-title)
    (:html-toc-headline-format-function nil nil t-toc-headline-format-function)
    ;; Template
    (:html-include-fixup-js nil "fixup-js" t-include-fixup-js)
    (:html-fixup-js "HTML_FIXUP_JS" nil t-fixup-js newline)
    (:subtitle "SUBTITLE" nil nil parse)
    ;; Misc
    (:html-indent nil nil t-indent)))

;;; User Configuration Variables.

(defgroup org-export-w3ctr nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export W3CTR HTML"
  :group 'org-export)

;;;; Reference
(defcustom t-prefer-user-labels nil
  "When non-nil, use user-defined NAME and ID over internal references.

By default, `org-w3ctr--reference' generates internal ID values
during export.  When this variable is non-nil, the NAME keyword,
the ID property, or the real name of a target is used as the ID
attribute instead.

Regardless of this variable, CUSTOM_ID is always used as a
reference."
  :group 'org-export-w3ctr
  :type 'boolean
  :safe #'booleanp)

(defvar t--id-attr-prefix "ID-"
  "Prefix to use in ID attributes.
This affects IDs that are determined from the ID property.")

;;;; Drawer
(defcustom t-drawer-format-function #'t-drawer-default-format-function
  "Function to format a drawer in HTML.

It is called with five arguments:
- NAME     the drawer name (a string).
- SUMMARY  the <summary> text (a string): the caption when one is
           present, NAME otherwise.
- ATTRS    the HTML attribute string (a string, possibly empty),
           including the leading space when non-empty.
- CONTENTS the transcoded drawer contents (a string or nil).
- INFO     the export options (a plist).

It should return the complete HTML for the drawer.  The default is
`org-w3ctr-drawer-default-format-function', which emits a
`<details>' element with a `<summary>'."
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

;;;; LaTeX
(defcustom t-math-custom-render-function
  #'t-math-custom-default-render-function
  "Function rendering a LaTeX fragment for the `custom' math mode.
It is called with FRAG, a LaTeX string, and the INFO plist, and
must return the HTML/MathML/SVG string for the fragment."
  :group 'org-export-w3ctr
  :type 'function)

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
  "Control the format of datetime attributes for <time> elements.

This option controls how timestamps are formatted when exporting
datetime attributes, with variations in:

Separator : Use a space or `T' between date and time.
Timezone  : Use `:' in the zone offset or not (`+08:00' and `+0800').
UTC-Zulu  : Use a trailing `Z' when the timezone is UTC+0, or omit it."
  :group 'org-export-w3ctr
  :type '(radio (const s-none) (const s-none-zulu)
                (const s-colon) (const s-colon-zulu)
                (const T-none) (const T-none-zulu)
                (const T-colon) (const T-colon-zulu)))

(defcustom t-timestamp-option 'org
  "Control how timestamps are exported.

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
- DATE: format string for year/month/day (for example, \"%Y-%m-%d\")
- DATE-TIME: date plus hours and minutes (for example, \"%F %H:%M\")

These format strings follow the conventions of `format-time-string'.

Note: This option only takes effect when
`org-w3ctr-timestamp-option' is set to `fmt' or `cus'."
  :group 'org-export-w3ctr
  :type '(cons string string))

(defcustom t-timestamp-format-function #'t-ts-default-format-function
  "Custom function for formatting timestamps.

The function must accept two arguments: a TIMESTAMP object and an
INFO plist.  It must return a string.  The default is
`org-w3ctr-ts-default-format-function', which returns the raw
value of TIMESTAMP.

This option only takes effect when `org-w3ctr-timestamp-option'
is set to `fun'."
  :group 'org-export-w3ctr
  :type 'function)

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

(defcustom t-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-equation-reference-format "\\eqref{%s}"
  "The command to use when referencing an equation.

A format control string expecting the label as its single
argument.  It is inserted verbatim, so it only makes sense with
client-side MathJax (`mathjax' mode), where \\eqref and \\ref are
resolved in the browser.

See `org-html-equation-reference-format' for more information."
  :group 'org-export-w3ctr
  :type 'string)

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

;;;; Todo
(defcustom t-todo-kwd-class-prefix ""
  "Prefix for CSS classes applied to TODO keywords.

The class of a TODO keyword is its status (\"todo\" or \"done\"),
followed by a space, this prefix, and the fixed-up keyword name.
With the default empty prefix, a TODO item therefore has the class
\"todo TODO\"."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-todo-format-function #'t-todo-default-format-function
  "Custom function for formatting TODO keywords.

The function must accept two arguments: a TODO keyword string and
an INFO plist.  It must return a string (HTML) or nil.  The
default is `org-w3ctr-todo-default-format-function', which wraps
the keyword in a <span> with status-based CSS classes."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Priority
(defcustom t-priority-format-function #'t-priority-default-format-function
  "Custom function for formatting priority markers.

The function must accept two arguments: a PRIORITY value (number
or character) and an INFO plist.  It must return a string (HTML)
or nil.  The default is `org-w3ctr-priority-default-format-function'."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Tags
(defcustom t-tags-format-function #'t-tags-default-format-function
  "Custom function for formatting tags.

The function must accept two arguments: a TAGS list (list of
strings) and an INFO plist.  It must return a string (HTML) or
nil.  The default is `org-w3ctr-tags-default-format-function',
which wraps each tag in a <span> with a class based on
`:html-tag-class-prefix' and the tag name."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-tag-class-prefix ""
  "Prefix for CSS classes applied to individual tags.

The final class will be this prefix followed by the fixed-up tag
name.  For example, if a tag is \"a\", its class will be
\"tag-a\" when the prefix is \"tag-\"."
  :group 'org-export-w3ctr
  :type 'string)

;;;; Headline
(defcustom t-format-headline-function
  #'t-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
- TODO      the todo keyword (string or nil).
- TODO-TYPE the type of todo (symbol: `todo', `done', nil).
- PRIORITY  the priority of the headline (integer or nil)
- TEXT      the main headline text (string).
- TAGS      the tags (list of string).
- INFO      the export options (plist).

The function should return the formatted HTML string for the headline.
The default is `org-w3ctr-format-headline-default-function'."
  :group 'org-export-w3ctr
  :type 'function)

;; See `org-html-toplevel-hlevel' for more information.
(defcustom t-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.

Must be an integer between 2 and 6, inclusive; other values signal
`org-w3ctr-error' during export."
  :group 'org-export-w3ctr
  :type '(integer 2 6))

(defcustom t-honor-ox-headline-levels nil
  "Non-nil means honor `org-export-headline-levels' when exporting.

When non-nil, a headline whose relative level exceeds
`org-export-headline-levels' is exported as a low-level list item,
as ox-html does.  When nil, only headlines deeper than <h6> are
exported as list items."
  :group 'org-export-w3ctr
  :type 'boolean
  :safe #'booleanp)

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

(defcustom t-heading-format-function
  #'t-heading-default-format-function
  "Function to format the heading block.

The function is called with six arguments:
- HEADLINE the headline element.
- TITLE    the headline title HTML (string).
- H        the heading tag name (for example, \"h2\").
- ID       the reference id (string).
- CLASS    the `:HTML_HEADLINE_CLASS:' value (string or nil).
- INFO     the export options (plist).

It returns the HTML for the heading block (for example, the `.header-wrapper'
div).  The default builds the section number and self-link from
`org-w3ctr--headline-secno' and `org-w3ctr--headline-self-link'.
The default is `org-w3ctr-heading-default-format-function'."
  :group 'org-export-w3ctr
  :type 'function)

;;;; <meta>
(defcustom t-file-timestamp-function #'t-file-timestamp-default-function
  "Function to generate timestamp for exported files at top place.

This function should take INFO as the only argument and return a
string representing the timestamp.

The default value is `org-w3ctr-file-timestamp-default', which generates
timestamps in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  :group 'org-export-w3ctr
  :type 'function)

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

;;;; Math
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

;;;; <head>
(defcustom t-head ""
  "Raw HTML content to insert into the <head> section.

This variable can contain the full HTML structure to provide a style,
including the surrounding HTML tags.  It can be a string, or a function
that accepts the INFO plist and returns a string.  As the value of this
option simply gets inserted into the HTML <head> header, you can use it
to add any arbitrary text to the header.

You can set this on a per-file basis using #+HTML_HEAD:,
or for publication projects using the :html-head property."
  :group 'org-export-w3ctr
  :type '(choice string function))
;;;###autoload
(put 't-head 'safe-local-variable 'stringp)

(defcustom t-head-extra ""
  "More head information to add in the <head> section.

It can be a string, or a function that accepts the INFO plist and
returns a string.

You can set this on a per-file basis using #+HTML_HEAD_EXTRA:,
or for publication projects using the :html-head-extra property."
  :group 'org-export-w3ctr
  :type '(choice string function))
;;;###autoload
(put 't-head-extra 'safe-local-variable 'stringp)

(defcustom t-head-include-style t
  "Control whether to include CSS styles in the exported HTML.

When non-nil, the styles defined by `t-style' or loaded from
`t-style-file' will be embedded within a <style> tag in the HTML <head>."
  :group 'org-export-w3ctr
  :type 'boolean)

(defvar t--style-cache nil
  "Cached CSS loaded from `org-w3ctr-style-file'.

`org-w3ctr--load-css' stores the wrapped file contents here so that
repeated exports do not re-read the file; `org-w3ctr-clear-css' resets
it.")

(defcustom t-style nil
  "CSS rules to be embedded directly into the exported HTML.

When this string is not empty, it *takes precedence* over
`org-w3ctr-style-file'."
  :group 'org-export-w3ctr
  :type '(choice string (const nil)))

(defcustom t-style-file (file-name-concat t--dir "assets" "style.css")
  "Path to a CSS file to load styles from.

This path must be *absolute*.  This option is used as a fallback when
`org-w3ctr-style' is empty.

When you set a new file path here, the cached CSS is automatically
cleared to ensure the new file is loaded on the next export.

The default value points to a `style.css' file inside the package's
`assets' directory."
  :group 'org-export-w3ctr
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (unless (and (stringp value)
                      (file-exists-p value)
                      (file-name-absolute-p value))
           (error "Invalid style file: %s" value))
         (set symbol value)
         ;; Refresh the cached CSS.
         (setq t--style-cache nil))
  :type '(choice (const nil) file))

;;;; Navbar
(defcustom t-link-home ""
  "URL for the `HOME' link in the legacy navigation bar.

The legacy bar appears only when `org-w3ctr-link-navbar' yields
no links; see `org-w3ctr-home/up-format' for its format.  When
this option is empty or blank, the `HOME' anchor falls back to
`org-w3ctr-link-up', and the reverse also holds."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-up ""
  "URL for the `UP' link in the legacy navigation bar.

The legacy bar appears only when `org-w3ctr-link-navbar' yields
no links; see `org-w3ctr-home/up-format' for its format.  When
this option is empty or blank, the `UP' anchor falls back to
`org-w3ctr-link-home', and the reverse also holds."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-home/up-format
  "<nav id=\"navbar\">\n <a href=\"%s\"> UP </a>
 <a href=\"%s\"> HOME </a>\n</nav>"
  "Format string for the legacy home/up navigation bar.

The default bar shares id \"navbar\" with the navbar of
`org-w3ctr-navbar-default-format-function', so one CSS rule
styles both.

The first %s receives the `UP' link and the second the `HOME'
link.  Both go in verbatim, without HTML escaping.  Set the
option per file with the HTML_HOME/UP_FORMAT keyword: as in
ox-html, multiple keyword lines are joined with newlines.  The
bar is omitted entirely when `org-w3ctr-link-up' and
`org-w3ctr-link-home' are both empty or blank.  The transcoder
normalizes the result to end in a newline."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-navbar nil
  "Navigation bar links.  Can be:
- A vector of (URL . NAME) pairs, for example
  [(\"../index.html\" . \"Up\")],
- A list of Org elements (from the HTML_LINK_NAVBAR keyword),
- nil for the legacy home/up behavior.

A value that yields no links (nil, an empty vector, or a list
that transcodes to nothing) falls back to the legacy bar; see
`org-w3ctr--format-legacy-navbar'.  To suppress the navbar
entirely, set `org-w3ctr-navbar-format-function' to nil."
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-navbar-format-function #'t-navbar-default-format-function
  "The function used to generate the HTML for the navbar.

This function is called with one argument: INFO plist.  It should
return a string containing the complete HTML for the navigation bar
\(e.g., inside `<nav>' tags).

See `org-w3ctr-navbar-default-format-function' for an example."
  :group 'org-export-w3ctr
  :type 'function)

;;;; CC badges
(defcustom t-use-cc-badges t
  "Non-nil means append the CC badge icons to the license line.

`org-w3ctr-license-default-format-function' reads this option;
the icons for a license come from `org-w3ctr--cc-icon-names'."
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

(defcustom t-license-format-function #'t-license-default-format-function
  "Default function to build license string."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-cc-badges-format-function #'t-cc-badges-default-format-function
  "The function used to render the CC badge icons for a license.

This function is called with a LICENSE symbol and an INFO plist.
It should return an HTML string with the license's badge icons, or
the empty string when there are none.  The default,
`org-w3ctr-cc-badges-default-format-function', embeds the icons
as base64 images; replace it for other markup, for example a
shared SVG sprite or inline <svg>."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Pre/Postamble

(defcustom t-metadata-timestamp-format "%Y-%m-%d %H:%M"
  "Format string for the %d, %T, and %C pre/postamble format codes.

See `format-time-string' for its components.  The default omits
ox-html's %a weekday abbreviation on purpose."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-validation-link
  "<a href=\"https://validator.w3.org/check?uri=referer\">\
Validate</a>"
  "Link to the HTML validation service, for the %v format code.

The link is inserted verbatim, like the other format-code
replacements."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-preamble #'t-preamble-default-function
  "Control the preamble inserted into the exported HTML.

The value is one of:
- nil: no preamble.
- string: formatted with `format-spec' against the codes in
  `org-w3ctr--pre/postamble-format-spec' (for example %d, %c) and
  inserted.
- function: called with the export options plist, its return
  value inserted.
- symbol: called like a function if it has one, else its value
  cell is formatted as a string.

The default is `org-w3ctr-preamble-default-function'.  The result
is normalized to end in a newline; see
`org-w3ctr--build-pre/postamble' for the exact contract."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

(defcustom t-postamble
  "<p role=\"navigation\" id=\"back-to-top\"><a href=\"#title\"><abbr title=\"Back to Top\">↑</abbr></a></p>
"
  "Control the postamble inserted into the exported HTML.

The value takes the same kinds as `org-w3ctr-preamble'.  The
default is the back-to-top arrow; nil inserts nothing."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

(defcustom t-creator-string
  (format "<a href=\"https://www.gnu.org/software/emacs/\">\
Emacs</a> %s (<a href=\"https://orgmode.org\">Org</a> mode %s) \
<a href=\"https://github.com/include-yy/ox-w3ctr\">ox-w3ctr</a> %s"
          emacs-version
          (if (fboundp 'org-version) (org-version)
            "unknown version")
          t-version)
  "Information about the creator of the HTML document, for the %c
format code.

This option can also be set with the CREATOR keyword.  See also
`org-html-creator-string'."
  :group 'org-export-w3ctr
  :type 'string)

;;;; Table of Contents
(defcustom t-toc-element 'ul
  "List element of the table of contents.

The default `ul' keeps the extreme no-CSS display clean: `ol'
would add browser numbering on top of the inlined section
numbers.  With the stylesheet applied both render alike (it drops
the markers), so `ol' is the better choice for list semantics
wherever a stylesheet is guaranteed."
  :group 'org-export-w3ctr
  :type '(choice (const ul) (const ol)))

(defcustom t-toc-title "Table of Contents"
  "Heading text of the table of contents.

W3C technical reports use \"Table of Contents\".  Unlike ox-html,
which derives its heading from the translation machinery
(`org-export-translate'), this is a plain string to set per
installation."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-toc-headline-format-function
  #'t-toc-headline-default-format-function
  "The function used to format a table of contents entry.

This function is called with a HEADLINE element and an INFO plist.
It should return the entry HTML: an anchor to the headline's
reference.  The default,
`org-w3ctr-toc-headline-default-format-function', pairs the section
number with the headline text; replace the function to change the
entry markup."
  :group 'org-export-w3ctr
  :type 'function)

;;;; Template
(defcustom t-include-fixup-js t
  "Control whether to include the fixup JavaScript in the exported HTML.

When non-nil, the fixup script is embedded before the closing </body>
tag: the document's own `:html-fixup-js' when set, otherwise the
`assets/fixup.js' shipped with the package.  When nil, no script is
emitted."
  :group 'org-export-w3ctr
  :type 'boolean)

(defvar t-fixup-js ""
  "JavaScript to inject before the closing </body> tag.

When this is a non-blank string it overrides the `assets/fixup.js'
shipped with the package; when it is empty, the exporter loads that
file instead.  See `org-w3ctr--load-fixup-js'.")

(defcustom t-coding-system 'utf-8-unix
  "Coding system for HTML export.

UTF-8 is the de facto standard for modern web content.  The default
value `utf-8-unix' is strongly recommended and should not be changed
unless you have specific legacy system requirements."
  :group 'org-export-w3ctr
  :type '(radio (const utf-8-unix)
                (const utf-8-dos)
                (const utf-8-mac)))

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

;;;; Src Block
(defcustom t-fontify-method 'engrave
  "Method to fontify code.
- nil means no highlighting
- engrave means use a subset of engrave-face.el for code fontify

There was a support for highlight.js, but has been abandoned."
  :group 'org-export-w3ctr
  :type '(choice (const engrave) (const nil)))

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
       :html-todo-kwd-class-prefix :html-todo-format-function
       :with-todo-keywords
       :html-priority-format-function :with-priority
       :with-tags :html-tag-class-prefix :html-tags-format-function
       :html-format-headline-function :html-heading-format-function
       :html-toplevel-hlevel :html-honor-ox-headline-levels
       ;; inner-template and template
       :with-author :author :title
       :time-stamp-file :html-file-timestamp-function :html-viewport
       :with-latex :html-mathjax-config
       :html-math-head-function :html-math-custom-render-function
       :html-use-cc-badges :html-license
       :html-license-format-function
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
\(`:with-latex', `:time-stamp-file', `:with-tags') must in particular
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
`org-w3ctr--oinfo-cache-props' are read through a cache when this build
has one (see `org-w3ctr--oinfo-cache-p').  Nil means PROP is absent.

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
  "Clear every OINFO oclosure's cached value, freeing its INFO plist.

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
  "Return S as a string, or nil if S is not a symbol, number, or string.
A symbol contributes its `symbol-name' (a keyword keeps its colon); nil
returns nil."
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
  (declare (ftype (function (string &optional boolean) string))
           (pure t) (important-return-value t))
  (replace-regexp-in-string
   (if keep-lead "\\`\\([ \t]*\n\\)+" "\\`[ \t\n\r]+") ""
   (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

(defsubst t--nw-trim (s)
  "Trim S only if it is a non-empty, non-whitespace string.
Return nil otherwise."
  (declare (ftype (function (t) (or null string)))
           (pure t) (important-return-value t))
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
    ;; Single and double quotes also need escaping inside attribute
    ;; values; see https://stackoverflow.com/a/2428595.
    ("'" . "&apos;") ("\"" . "&quot;"))
  "Alist mapping HTML special characters to their entity strings.

Single and double quotes are escaped too.  Used by
`org-w3ctr--encode-plain-text*'.")

(defun t--encode-plain-text* (text)
  "Escape `&', `<', `>', and both quote characters in TEXT.

The result is safe to use inside an HTML attribute value."
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

It handles two formats:

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
\(for example, `disabled') or a list for an attribute with a
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
keywords or plain symbols) and values are strings.  A key with a nil
value is omitted; values are escaped for an attribute with
`org-w3ctr--encode-plain-text*'."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond
       ((null item) (pop output))
       ((keywordp item) (push (substring (symbol-name item) 1) output))
       ((symbolp item) (push (symbol-name item) output))
       (t (let ((key (car output))
                (value (t--encode-plain-text* item)))
            (setcar output (format "%s=\"%s\"" key value))))))))

(defun t--make-attr__id (element info &optional named-only)
  "Format `:attr__' attributes, adding an `id' attribute if needed.

ELEMENT is the element, INFO the info plist, and NAMED-ONLY, when
non-nil, restricts the id to elements with an explicit name.  Read and
parse the `:attr__' property from ELEMENT, then add an `id' attribute
based on the element's reference, unless an `id' is already explicitly
defined in the property.

`org-w3ctr--make-attr__' formats the final, combined list of
attributes into a single string."
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

ELEMENT is the element, INFO the info plist, and NAMED-ONLY, when
non-nil, restricts the id to elements with an explicit name.  Process
the standard Org `:attr_html' property from ELEMENT, then add an `id'
attribute based on the element's reference, unless an `id' is already
present in the property list.

`org-w3ctr--make-attribute-string' formats the final property list
into a single string."
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

ELEMENT is the element, INFO the info plist, and NAMED-ONLY, when
non-nil, restricts the id to elements with an explicit name.  This is
the main function for generating an element's complete attribute
string.  It first checks for the custom `:attr__' property and
processes it with `org-w3ctr--make-attr__id'.

If `:attr__' is absent, it falls back to processing the standard
`:attr_html' property using `org-w3ctr--make-attr_html'.  A present
`#+attr__:', even empty, wins: its presence alone selects the
`:attr__' syntax, so `#+attr_html:' is ignored."
  (declare (ftype (function (t list &optional boolean) string))
           (important-return-value t))
  ;; `#+attr__:' takes priority even when empty — its presence alone
  ;; means "use ox-w3ctr syntax", so `#+attr_html:' is ignored.
  (if (org-element-property :attr__ element)
      (t--make-attr__id element info named-only)
    (t--make-attr_html element info named-only)))

;;;; File and regexp

(defun t--load-file (file)
  "Read the entire contents of FILE into a string, verbatim.

Signal `org-w3ctr-error' if FILE does not exist or is a directory.
FILE is decoded as UTF-8 regardless of the locale coding system, so
the same file reads identically on every machine; nothing is added or
removed."
  (declare (ftype (function (string) string))
           (important-return-value t))
  (unless (and (file-exists-p file) (not (file-directory-p file)))
    (t-error "Invalid file: %s" file))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun t--find-all (regexp str &optional start)
  "Return a list of all non-overlapping matches for REGEXP in STR.

The search begins at character position START, which defaults to
the beginning of the string.

For example:
  (org-w3ctr--find-all \"[a-z]+\" \"1a-b2-cde\")
  => (\"a\" \"b\" \"cde\")

If no matches are found, or if REGEXP is an empty string, this
function returns nil.  A match of length zero is skipped."
  (declare (ftype (function (string string &optional (or null fixnum))
                            list))
           (pure t) (important-return-value t))
  (if (string= regexp "") nil
    (let ((pos (max (or start 0) 0))
          (matches))
      (while (and (< pos (length str))
                  (string-match regexp str pos))
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (if (= beg end)
              ;; Zero-width match: skip it and move on, so a regexp
              ;; that can match the empty string does not loop forever.
              (setq pos (1+ pos))
            (push (match-string 0 str) matches)
            (setq pos end))))
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
form \"<TAG ...>\", or \"<TAG>\" when ATTRS is blank."
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
sanitizes string content using `org-w3ctr--encode-plain-text'.
Signal `org-w3ctr-error' when a list's first element is not a symbol."
  (declare (ftype (function (t) string))
           (important-return-value t))
  (cl-typecase data
    (null "")
    ((or symbol string number)
     (t--encode-plain-text (t--2str data)))
    (list
     (let ((tag (nth 0 data)))
       (unless (and tag (symbolp tag))
         (t-error "Invalid S-expression tag: %S" tag))
       ;; always use lowercase tagname.
       (let* ((tag (downcase (symbol-name tag)))
              (attr-ls (nth 1 data))
              (attrs (if (booleanp attr-ls) ""
                       (t--make-attr__ attr-ls))))
         (if (string-match-p t--void-element-regexp tag)
             (t--void-element tag attrs)
           (let ((children (mapconcat #'t--sexp2html (cddr data))))
             (format "<%s%s>%s</%s>"
                     tag attrs children tag))))))
    (otherwise "")))

;;;; References

;; Options:
;; - :html-prefer-user-labels (`org-w3ctr-prefer-user-labels')

(defun t--target-reference (datum)
  "Return the value of a target or radio-target as a reference string.
Return nil if DATUM is not a target type, or if the value is not a
letter followed by letters, digits, hyphens or underscores."
  (declare (ftype (function (t) (or null string)))
           (pure t) (important-return-value t))
  (when (memq (org-element-type datum) '(radio-target target))
    (when-let* ((val (org-element-property :value datum))
                (_ (string-match-p "^[a-zA-Z][a-zA-Z0-9-_]*$" val)))
      val)))

(defun t--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to radio targets and targets."
  (declare (ftype (function (t list &optional boolean) (or null string)))
           (important-return-value t))
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

;;;; Filter Functions

(defun t-image-link-filter (data _backend info)
  "Filter to insert image links inside link descriptions.

This is the backend's `:filter-parse-tree' filter.  DATA is the parse
tree, BACKEND the backend symbol (unused), and INFO the export options
plist.  Return DATA with any image that is a link's description turned
into a proper nested link; `org-w3ctr-inline-image-rules' decides which
links count as images.  See `org-export-insert-image-links'."
  (declare (ftype (function (t t list) t))
           (important-return-value t))
  (org-export-insert-image-links data info t-inline-image-rules))

(defun t-final-function (contents _backend info)
  "Indent the HTML when `:html-indent' is non-nil, and return it.

This is the backend's `:filter-final-output' filter.  CONTENTS is the
exported HTML string and INFO the export plist.  The major mode is set
only when indenting, so that the HTML indentation rules apply; its hooks
are delayed, as in `org-html-final-function'."
  (declare (ftype (function (string t list) string))
           (important-return-value t))
  (with-temp-buffer
    (insert contents)
    (when (t--pget info :html-indent)
      (delay-mode-hooks (set-auto-mode t))
      (indent-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; JSON-RPC

;; A JSON-RPC 2.0 client built on `jsonrpc.el'.  `org-w3ctr--jrpc' is the
;; generic layer; `org-w3ctr--jstools' is the one instance used here, whose
;; COMMAND starts the node MathJax helper.  The helper speaks Content-Length
;; framing (see jstools/index.js), which is what `jsonrpc.el' reads and
;; writes too.

(oclosure-define t--jrpc
  "Callable JSON-RPC client.

NAME - the client and connection name.
CONN - the live `jsonrpc-process-connection', or nil before first use.
TIMEOUT - the default per-request timeout, in seconds.
COMMAND - the argv used to (re)start the server.
METHODS - the method names this side exposes; nil disables the check."
  (name :type string)
  (conn :mutable t)
  (timeout :type number)
  (command :type list)
  (methods :type list))

(defun t--jrpc-make (name command &optional timeout methods)
  "Return a callable JSON-RPC client that starts COMMAND on demand.

NAME is the client and connection name.  COMMAND is the argv of the
server process.  TIMEOUT is the default per-request timeout in seconds,
10.0 when nil.  METHODS is the list of method names the client accepts,
or nil to accept any.  The client is called as
`(METHOD PARAMS &optional TIMEOUT)'; `org-w3ctr--jcall' wraps that with
the connection check."
  (declare (ftype (function (string list &optional number list) function))
           (important-return-value t))
  (oclosure-lambda (t--jrpc (name name)
                            (conn nil)
                            (timeout (or timeout 10.0))
                            (command command)
                            (methods methods))
      (method params &optional timeout-override)
    (jsonrpc-request conn method params
                     :timeout (or timeout-override timeout))))

(defun t--jrpc-connect (name command)
  "Return a new `jsonrpc-process-connection' running COMMAND.

NAME names the connection and its process.  `jsonrpc.el' creates a
stderr buffer (`*NAME stderr*') during initialization and then calls the
`:process' function, so the process is created there with that buffer
wired as its stderr.  Passing an already-made process would leave the
child's stderr merged into stdout and corrupt the protocol."
  (declare (ftype (function (string list) t))
           (important-return-value t))
  (make-instance
   'jsonrpc-process-connection
   :name name
   :process (lambda (_conn)
              (make-process
               :name name
               :command command
               :stderr (get-buffer (format "*%s stderr*" name))
               :noquery t :coding 'binary))))

(defun t--jrpc-shutdown (client)
  "Shut down CLIENT's connection and clear its `conn' slot.

CLIENT is a `org-w3ctr--jrpc' object.  Return nil."
  (declare (ftype (function (t) null)))
  (let ((conn (t--jrpc--conn client)))
    (when conn (ignore-errors (jsonrpc-shutdown conn t)))
    (setf (t--jrpc--conn client) nil)))

(defun t--jrpc-ensure (client)
  "Return CLIENT's live connection, starting one if needed.

CLIENT is a `org-w3ctr--jrpc' object.  A dead or absent connection is
shut down and rebuilt from CLIENT's `command', and the result is stored
back in CLIENT's `conn' slot."
  (declare (ftype (function (t) t)))
  (let ((conn (t--jrpc--conn client)))
    (unless (and conn (jsonrpc-running-p conn))
      (t--jrpc-shutdown client)
      (setf (t--jrpc--conn client)
            (t--jrpc-connect (t--jrpc--name client)
                             (t--jrpc--command client))))
    (t--jrpc--conn client)))

(defun t--jrpc-restart (client)
  "Restart CLIENT's server process and return the new connection.

CLIENT is a `org-w3ctr--jrpc' object."
  (declare (ftype (function (t) t)))
  (t--jrpc-shutdown client)
  (t--jrpc-ensure client))

(defun t--jcall (client method params &optional timeout)
  "Call METHOD on CLIENT, restarting its connection if needed.

CLIENT is a `org-w3ctr--jrpc' object.  METHOD is a method name and
PARAMS the JSON-RPC params value.  TIMEOUT overrides CLIENT's default.
Return the decoded `result' of the response.  Signal `org-w3ctr-error'
when METHOD is outside CLIENT's METHODS; a remote error or a timeout
surfaces as `jsonrpc-error'."
  (declare (ftype (function (t symbol t &optional number) t))
           (important-return-value t))
  (let ((allowed (t--jrpc--methods client)))
    (unless (or (null allowed) (memq method allowed))
      (t-error "Unknown jstools method: %s" method)))
  (t--jrpc-ensure client)
  (funcall client method params timeout))

(defconst t--jstools-methods '(tex2mml tex2svg)
  "The RPC methods ox-w3ctr exposes from the jstools helper.

The node helper also answers the test methods echo and add; they stay
unexposed.")

(defvar t--jstools
  (t--jrpc-make "ox-w3ctr-jstools"
                (list "node" (file-name-concat t--dir "jstools/index.js")
                      "--timeout" "30000")
                nil t--jstools-methods)
  "The JSON-RPC client for the node MathJax helper.")

(defun t-show-jstools-events ()
  "Show the JSON-RPC event log for the jstools connection."
  (interactive)
  (pop-to-buffer (jsonrpc-events-buffer (t--jrpc-ensure t--jstools))))

(defun t-launch-jstools ()
  "Restart the jstools helper process."
  (interactive)
  (t--jrpc-restart t--jstools))

;;; Greater elements

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
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((has-user-attrs (or (org-element-property :attr__ center-block)
                             (org-element-property :attr_html center-block)))
         (attrs (t--make-attr__id* center-block info t)))
    (format "<div%s%s>%s</div>" attrs
            (if has-user-attrs "" " style=\"text-align:center;\"")
            (t--prepend-newline contents))))

;;;; Drawer

;; See (info "(org)Drawers")
;; `<details>' is the semantic HTML5 element for collapsible content.
;; Supports `#+attr__:' / `#+attr_html:' for custom attributes.
;; Caption becomes the `<summary>' text; falls back to drawer name.
;; Options:
;; - :html-format-drawer-function (`org-w3ctr-drawer-format-function')
(defun t-drawer-default-format-function (_name summary attrs contents _info)
  "Return the <details> element holding the drawer summary and contents.

See `org-w3ctr-drawer-format-function' for the descriptions of
NAME, SUMMARY, ATTRS, CONTENTS, and INFO."
  (declare (ftype (function (string string string (or null string) list)
                            string))
           (pure t) (important-return-value t))
  (format "<details%s><summary>%s</summary>%s</details>"
          attrs summary (t--prepend-newline contents)))

(defun t-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.

CONTENTS holds the contents of the drawer.  INFO is the info plist.
The <summary> text is the caption when one is present, and the
drawer name otherwise.  The markup is built by the function in
`:html-format-drawer-function'.  Return the formatted <details>
element as a string."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((name (org-element-property :drawer-name drawer))
         (caption (org-export-get-caption drawer))
         (summary (or (and caption (t--nw-p (org-export-data caption info)))
                      name))
         (attrs (t--make-attr__id* drawer info t)))
    (funcall (or (t--pget info :html-format-drawer-function)
                 #'t-drawer-default-format-function)
             name summary attrs contents info)))

;;;; Dynamic Block

;; See (info "(org)Dynamic Blocks")
;; Org-internal extension mechanism; exported as-is.
(defun t-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block."
  (declare (ftype (function (t (or null string) t) string))
           (pure t) (important-return-value t))
  (or contents ""))

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
  (declare (ftype (function (t list) string))
           (important-return-value t))
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
      (_ (t-error "Unknown list item type: %s" type)))))

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

;;; Lesser elements

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
                                 ;; A nested `org-w3ctr-error' has a clean
                                 ;; message already; do not re-render it with
                                 ;; its type name and quotes.
                                 (if (eq (car err) 't-error)
                                     (cadr err)
                                   (error-message-string err))))))
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
     (t--jcall t--jstools 'tex2mml (list :fragment (t--normalize-latex frag))))
    (`svg-by-mathjax
     (t--jcall t--jstools 'tex2svg (list :fragment (t--normalize-latex frag))))
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

;;;; Paragraph

(defsubst t--wrap-image (contents _info caption attrs)
  "Wrap CONTENTS in a <figure> element for standalone images.

CONTENTS is the image HTML.  INFO is unused.  CAPTION is the
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
    ;; Replace each newline character with line break.  Also
    ;; remove any trailing "br" close-tag so as to avoid
    ;; duplicates.
    (let ((re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote "<br>"))))
      (replace-regexp-in-string re "<br>\n" (or contents ""))))))

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

;;; Objects

;;;; Entity

;; See (info "(org)Special Symbols")
(defun t-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to HTML.

CONTENTS and INFO are unused.  Return the HTML entity string
for ENTITY (for example, `&alpha;')."
  (declare (ftype (function (t t t) string))
           (pure t) (important-return-value t))
  (org-element-property :html entity))

;;;; Export Snippet

;; See (info "(org)Quoting HTML tags")
(defun t-export-snippet (export-snippet _contents _info)
  "Transcode an EXPORT-SNIPPET object from Org to HTML.

CONTENTS and INFO are unused.  Return the snippet value as a
string, or an empty string for unsupported backends."
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

CONTENTS is the subscript content.  INFO is unused.  Return a <sub> element."
  (declare (ftype (function (t string t) string))
           (pure t) (important-return-value t))
  (format "<sub>%s</sub>" contents))

;;;; Superscript

;; See (info "(org)Subscripts and Superscripts")
(defun t-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML.

CONTENTS is the superscript content.  INFO is unused.  Return a <sup> element."
  (declare (ftype (function (t string t) string))
           (pure t) (important-return-value t))
  (format "<sup>%s</sup>" contents))

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

Valid formats are UTC/GMT[+-]XX (for example, UTC+8), [+-]HHMM (for
example, -0500) or \"local\", which means use zero offset.  Return nil
if ZONE doesn't match `org-w3ctr-timezone-regex'."
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
to avoid repeated parsing.  If timezone is nil or timezone format is
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
    (t-error "Invalid timezone: nil")))

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
  (declare (ftype (function ( list &optional
                              (or fixnum symbol)
                              (or fixnum symbol))
                            fixnum))
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
          (error (t-error "Invalid time value: %s" time)))
      (t-error "Invalid datetime option: %s" option))))

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
         (t-error "Invalid timestamp: %s"
                  (org-element-property :raw-value timestamp))
       (signal e)))))

(defun t--format-ts-datetime (timestamp info &optional end)
  "Format TIMESTAMP to its datetime attribute string.

TIMESTAMP is an Org timestamp object.  INFO is the info plist.
END, when non-nil, format the end of a range.  Return a string
suitable for an HTML <time> datetime attribute."
  (declare (ftype (function (t list &optional boolean) string))
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
      (t-error "Invalid timestamp start: %s" timestamp)))

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

(defun t--format-ts-time (timestamp parts info)
  "Format TIMESTAMP's PARTS as <time> elements.

PARTS is a list of one or two timestamp strings: the start and,
for a range, the end.  INFO is the info plist.  Each part is
wrapped in a <time> element whose datetime attribute comes from
TIMESTAMP.  Parts are joined with \"--\", or with an en dash when
`:with-special-strings' is non-nil.  Return the formatted string."
  (declare (ftype (function (t list list) string))
           (important-return-value t))
  (let* ((sep (if (t--pget info :with-special-strings) "&#x2013;" "--"))
         (tt (mapconcat (lambda (s) (t--format-ts-span-time s info t))
                        parts sep)))
    (pcase (length parts)
      (1 (format tt (t--format-ts-datetime timestamp info)))
      (2 (format tt (t--format-ts-datetime timestamp info)
                 (t--format-ts-datetime timestamp info t)))
      (_ (t-error "Invalid timestamp range: %s" parts)))))

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
     (t--format-ts-time
      timestamp (t--find-all org-ts-regexp-both raw) info))
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
    (t-error "Invalid timestamp formats: %s"
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
           (`time (t--format-ts-time timestamp (list time) info))
           (_ (t-error "Unknown timestamp wrapper: %s" wrap)))))
      ((or `active-range `inactive-range)
       (let* ((t1 (org-format-timestamp timestamp fmt))
              (t2 (org-format-timestamp timestamp fmt t)))
         (pcase wrap
           (`none (t-plain-text (concat t1 "--" t2) info))
           (`span (t--format-ts-span-time (concat t1 "--" t2) info))
           (`time (t--format-ts-time timestamp (list t1 t2) info))
           (_ (t-error "Unknown timestamp wrapper: %s" wrap)))))
      (_ (t-error "Unknown timestamp type: %s" type)))))

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
      (t-error "Invalid custom timestamp format: %s" fmts))
    (let ((fmt (if (/= (aref fmt 0) ?\{) fmt (substring fmt 1 -1))))
      (t--format-timestamp-fix timestamp fmt info))))

(defun t-ts-default-format-function (timestamp _info)
  "Return the raw value of TIMESTAMP.

TIMESTAMP is an Org timestamp object.  INFO is unused.  This is the
default `org-w3ctr-timestamp-format-function'."
  (declare (ftype (function (t list) string))
           (pure t) (important-return-value t))
  (org-element-property :raw-value timestamp))

(defun t--format-timestamp-fun (timestamp info)
  "Format TIMESTAMP using a user-specified function from INFO."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (if-let* ((fun (t--pget info :html-timestamp-format-function)))
      (funcall fun timestamp info)
    (t-error "Invalid timestamp format function: nil")))

(defun t-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML.

TIMESTAMP is an Org timestamp object.  CONTENTS is unused.  INFO
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
                    (o (t-error "Unknown timestamp option: %s" o)))))
        (funcall fun timestamp info)))))

;;;; Link

;; REFINE: this section is pending the mainline fine pass (see AGENTS.md).
;; Options:
;; - :html-link-org-files-as-html (`org-w3ctr-link-org-files-as-html')
;; - :html-inline-images (`org-w3ctr-inline-images')
;; - :html-inline-image-rules (`org-w3ctr-inline-image-rules')
;; - :html-extension (`org-w3ctr-extension')
;; - :html-equation-reference-format
;;   (`org-w3ctr-equation-reference-format')

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
           (important-return-value t))
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

;;; Smallest objects
;; See (info "(org) Emphasis and Monospace")
;; Options:
;; - :html-text-markup-alist (`org-w3ctr-text-markup-alist')

(defun t--get-markup-format (name info)
  "Return the markup format string for NAME from the INFO plist.

NAME is a symbol (like \\='bold) and INFO is the Org export info
plist.  Return \"%s\" if NAME is not found."
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

;;; Headline and Section

;;;; Section

;; Malformed headlines (for example, ** before *) are exported as-is:
;; the heading level and section numbering reflect the source, not
;; a normalized hierarchy.  Both ox-html and ox-w3ctr behave the
;; same way — this is a feature, not a bug.
(defun t-section (section contents info)
  "Transcode a SECTION element from Org to HTML.

CONTENTS holds the contents of the section and INFO is the
export options plist.

A section inside a headline returns CONTENTS as-is.  The zeroth
section, the one outside any headline, returns nil and stores
CONTENTS in the `:zeroth-section-output' key of INFO, so the
template can place it before the table of contents."
  (declare (ftype (function (t (or null string) list) (or null string)))
           (important-return-value t))
  ;; normal section
  (if (org-element-lineage section 'headline) contents
    ;; FIXME: Make use of the zeroth section's property drawer, for
    ;; example `:HTML_CONTAINER:' when wrapping this output.
    ;; Facts so far: the drawer is the file-level one (`org-entry-get'
    ;; at the buffer start sees its properties; the parse tree keeps
    ;; it under the zeroth section), and it is dropped from the output.
    (prog1 nil (t--pput info :zeroth-section-output contents))))

;;;; Todo

;; Options:
;; - `org-done-keywords'
;; - :with-todo-keywords (`org-export-with-todo-keywords')
;; - :html-todo-kwd-class-prefix (`org-w3ctr-todo-kwd-class-prefix')
;; - :html-todo-format-function (`org-w3ctr-todo-format-function')

(defun t-todo-default-format-function (todo info)
  "Format TODO keyword as a <span> with status-based CSS class.

TODO is the keyword string.  INFO is the info plist.  Return a
<span> element matching `org-html--todo' output format:
class=\"status prefix+keyword\"."
  (declare (ftype (function (string list) string))
           (important-return-value t))
  (format "<span class=\"%s %s%s\">%s</span>"
          (if (member todo org-done-keywords) "done" "todo")
          (or (t--pget info :html-todo-kwd-class-prefix) "")
          (org-html-fix-class-name todo)
          todo))

(defun t--todo (todo info)
  "Format TODO keyword into HTML.

TODO is the keyword string, or nil.  INFO is the info plist.
Return the formatted HTML string, or nil when TODO is nil."
  (declare (ftype (function ((or null string) list) (or null string)))
           (important-return-value t))
  (when todo
    (funcall (or (t--pget info :html-todo-format-function)
                 #'t-todo-default-format-function)
             todo info)))

;;;; Priority

;; Options:
;; - `org-priority-highest'(65)
;; - `org-priority-default'(66)
;; - `org-priority-lowest' (67)
;; - :with-priority (`org-export-with-priority')
;; - :html-priority-format-function
;;   (`org-w3ctr-priority-default-format-function')

(defun t-priority-default-format-function (priority _info)
  "Format PRIORITY as a <span> matching `org-html--priority' output.

PRIORITY is the priority number or character, or nil.  INFO is the
info plist (unused).  Return a <span> element with class=\"priority\"."
  (declare (ftype (function ((or null fixnum) list) (or null string)))
           (important-return-value t))
  (and priority
       (format "<span class=\"priority\">[%s]</span>"
               (org-priority-to-string priority))))

(defun t--priority (priority info)
  "Format PRIORITY into HTML.

PRIORITY is the priority number or character, or nil.  INFO is the
info plist.  Return the formatted HTML string, or nil when PRIORITY
is nil."
  (declare (ftype (function ((or null fixnum) list) (or null string)))
           (important-return-value t))
  (when priority
    (funcall (or (t--pget info :html-priority-format-function)
                 #'t-priority-default-format-function)
             priority info)))

;;;; Tags

;; Options:
;; - :with-tags (`org-export-with-tags')
;; - :html-tags-format-function (`org-w3ctr-tags-format-function')
;; - :html-tag-class-prefix (`org-w3ctr-tag-class-prefix')

(defun t-tags-default-format-function (tags info)
  "Format TAGS matching `org-html--tags' output.

TAGS is a list of tag strings.  INFO is the info plist.  Return a
<span> element with class=\"tag\", wrapping each tag in a <span>
with a class based on `:html-tag-class-prefix' and the tag name."
  (declare (ftype (function (list list) (or null string)))
           (important-return-value t))
  (when tags
    (let ((prefix (t--pget info :html-tag-class-prefix)))
      (format "<span class=\"tag\">%s</span>"
              (mapconcat
               (lambda (tag)
                 (format "<span class=\"%s\">%s</span>"
                         (concat prefix (org-html-fix-class-name tag)) tag))
               tags "&#xa0;")))))

(defun t--tags (tags info)
  "Format TAGS into HTML.

TAGS is a list of tag strings.  INFO is the info plist.
Return the formatted HTML string, or nil when TAGS is empty."
  (declare (ftype (function (list list) (or null string)))
           (important-return-value t))
  (when tags
    (funcall (or (t--pget info :html-tags-format-function)
                 #'t-tags-default-format-function)
             tags info)))

;;;; Headline

;; Options:
;; - :with-todo-keywords (`org-export-with-todo-keywords')
;; - :with-priority (`org-export-with-priority')
;; - :with-tags (`org-export-with-tags')
;; - :html-format-headline-function (`org-w3ctr-format-headline-function')
;; - :html-toplevel-hlevel (`org-w3ctr-toplevel-hlevel')
;; - :html-honor-ox-headline-levels (`org-w3ctr-honor-ox-headline-levels')
;; - :html-container (`org-w3ctr-container-element')
;; - :html-self-link-headlines (`org-w3ctr-self-link-headlines')
;; - :html-heading-format-function (`org-w3ctr-heading-format-function')
;; - :headline-levels (`org-export-headline-levels')
;; - :headline-offset (internal)
;; - :section-numbers (`org-export-with-section-numbers')
;; - `org-footnote-section'

(defun t--headline-todo (headline info)
  "Format and return the TODO keyword for HEADLINE.

Return the exported keyword string only if `:with-todo-keywords' is
enabled in INFO and a TODO keyword exists on the HEADLINE.  Return nil
otherwise."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (and-let* (((t--pget info :with-todo-keywords))
             (todo (org-element-property :todo-keyword headline)))
    (org-export-data todo info)))

(defun t--headline-priority (headline info)
  "Return the numerical priority of a headline.

Return the priority number (for example, 65 for [#A]) only if the
export option `:with-priority' is non-nil in INFO and the HEADLINE
element has a priority cookie.  Return nil otherwise."
  (declare (ftype (function (t list) (or null fixnum)))
           (important-return-value t))
  (and (t--pget info :with-priority)
       (org-element-property :priority headline)))

(defun t--headline-tags (headline info)
  "Return the list of tags for a headline.

Return a list of tags associated with the HEADLINE element, but only if
the export option `:with-tags' is enabled in the INFO plist.  The tags
are processed for export.  Return nil if tags are disabled or not
present."
  (declare (ftype (function (t list) (or null list)))
           (important-return-value t))
  (and (t--pget info :with-tags)
       (org-export-get-tags headline info)))

(defun t-format-headline-default-function (todo _todo-type priority text tags info)
  "Format a headline from its todo, priority, text, and tags.

See `org-w3ctr-format-headline-function' for details and the
description of TODO, TODO-TYPE, PRIORITY, TEXT, TAGS, and INFO
arguments."
  (declare (ftype (function ((or null string) (or null symbol)
                             (or null fixnum) (or null string) list list)
                            string))
           (important-return-value t))
  (let ((todo (t--todo todo info))
        (priority (t--priority priority info))
        (tags (t--tags tags info)))
    (concat todo (and todo " ")
            priority (and priority " ")
            text (and tags "&#xa0;&#xa0;&#xa0;") tags)))

(defun t--build-bare-headline (headline text info)
  "Build the inner HTML content of HEADLINE from TEXT and INFO.

HEADLINE is the headline element and TEXT its already-exported title.
Extract the TODO keyword, its type, priority and tags from HEADLINE,
then call the function in `:html-format-headline-function' with those
values followed by TEXT and INFO: (TODO TODO-TYPE PRIORITY TEXT TAGS
INFO).  Return the formatted HTML string that function returns."
  (declare (ftype (function (t string list) string))
           (important-return-value t))
  (let* ((todo (t--headline-todo headline info))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (priority (t--headline-priority headline info))
         (tags (t--headline-tags headline info)))
    (funcall (or (t--pget info :html-format-headline-function)
                 #'t-format-headline-default-function)
             todo todo-type priority text tags info)))

(defun t--build-base-headline (headline info)
  "Build a standard headline string for the document body.

HEADLINE is the headline element and INFO is the info plist.  Extract
the main title from HEADLINE, format it for export, and pass it to
`org-w3ctr--build-bare-headline' to be combined with other components
like TODO keywords and tags."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((text (org-export-data
               (org-element-property :title headline) info)))
    (t--build-bare-headline headline text info)))

(defun t--get-headline-hlevel (headline info)
  "Calculate the absolute HTML heading level for a headline.

HEADLINE is the headline element and INFO is the info plist.  Compute
the final HTML heading level based on HEADLINE's relative level within
the Org document and the value of `:html-toplevel-hlevel'.  The formula
used is:
  (relative + top-level - 1).

Signal `org-w3ctr-error' when `:html-toplevel-hlevel' is not an
integer between 2 and 6."
  (declare (ftype (function (t list) fixnum))
           (important-return-value t))
  (let ((top-level (t--pget info :html-toplevel-hlevel))
        (relative (org-export-get-relative-level headline info)))
    (unless (and (integerp top-level) (<= 2 top-level 6))
      (t-error "Invalid HTML top level: %s" top-level))
    (+ relative top-level -1)))

(defun t--low-level-headline-p (headline info)
  "Check if HEADLINE should be rendered as a low-level list item.

HEADLINE is the headline element and INFO is the info plist.  A
headline is low-level when its h-level exceeds 6, keeping the output
within <h2>-<h6>.  When `:html-honor-ox-headline-levels' is non-nil,
`org-export-low-level-p' also applies, so a headline whose relative
level exceeds `:headline-levels' is low-level too: that option can move
the cutoff earlier but never past h6.  Return t when HEADLINE is
low-level."
  (declare (ftype (function (t list) boolean))
           (important-return-value t))
  (let ((hlevel (t--get-headline-hlevel headline info)))
    (if (or (> hlevel 6)
            (and (t--pget info :html-honor-ox-headline-levels)
                 (org-export-low-level-p headline info)))
        t)))

(defun t--build-low-level-headline (headline contents info)
  "Transcode a low-level headline into an HTML list item (`<li>').

HEADLINE is the headline element, CONTENTS its transcoded contents,
and INFO the info plist.  Render headlines that are too deep to become
standard <hN> tags.  Create a list structure where a group of sibling
low-level headlines becomes a single `<ol>' or `<ul>'.

The list type (`<ol>' vs. `<ul>') is determined by whether section
numbering is active.  The reference id and `:HTML_CONTAINER_CLASS:'
become attributes of the `<li>'; when `:HTML_HEADLINE_CLASS:' is set,
the headline text is wrapped in a `<span>' with that class."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
         (tag (if numberedp "ol" "ul"))
         (text (t--build-base-headline headline info))
         (id (t--reference headline info))
         (c-cls (org-element-property :HTML_CONTAINER_CLASS headline))
         (h-cls (org-element-property :HTML_HEADLINE_CLASS headline)))
    (concat
     (and (org-export-first-sibling-p headline info)
          (format "<%s>\n" tag))
     (format "<li id=\"%s\"%s>" id
             (or (and c-cls (format " class=\"%s\"" c-cls)) ""))
     (if h-cls (format "<span class=\"%s\">%s</span>" h-cls text) text)
     (when-let* ((c (t--nw-trim contents))) (concat "<br>\n" c "\n"))
     "</li>\n"
     (and (org-export-last-sibling-p headline info)
          (format "</%s>\n" tag)))))

(defun t--headline-container (headline info)
  "Return the HTML container tag name for HEADLINE.

HEADLINE is the headline element, INFO the export plist.  Return
HEADLINE's `:HTML_CONTAINER' property when set, else the
`:html-container' option from INFO, else \"div\".  The result names
the element (for example \"section\" or \"div\") wrapping the headline
and its contents."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (or (org-element-property :HTML_CONTAINER headline)
      (t--pget info :html-container)
      "div"))

(defun t--headline-self-link (id info)
  "Return the self-link for the headline identified by ID.

INFO is the export plist.  Return an `<a class=\"self-link\">' element
pointing to ID when the `:html-self-link-headlines' option in INFO is
non-nil, else nil."
  (declare (ftype (function (string list) (or null string)))
           (important-return-value t))
  (when (t--pget info :html-self-link-headlines)
    ;; The <a> is empty, so aria-label gives it an accessible
    ;; name for screen readers (WCAG 2.4.4/4.1.2).
    (format (concat "<a class=\"self-link\" href=\"#%s\""
                    " aria-label=\"Link to this section\"></a>\n")
            id)))

(defun t--headline-secno (headline info)
  "Return the section number for HEADLINE as an HTML span.

HEADLINE is the headline element, INFO the export plist.  When the
headline is numbered, return `<span class=\"secno\">' holding its
dotted section number (for example, \"1.1. \"), else nil."
  (declare (ftype (function (t list) (or null string)))
           (pure t) (important-return-value t))
  (when-let* (((org-export-numbered-headline-p headline info))
              (numbers (org-export-get-headline-number headline info)))
    (format "<span class=\"secno\">%s. </span>"
            (mapconcat #'number-to-string numbers "."))))

(defun t--headline-hN (headline info)
  "Return the HTML heading tag name (for example, \"h2\") for HEADLINE.

HEADLINE is the headline element, INFO the export plist.  The
h-level is capped at 6, so the tag is always at most \"h6\"."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let* ((level (min 6 (t--get-headline-hlevel headline info))))
    (format "h%s" level)))

(defun t-heading-default-format-function (headline title h id class info)
  "Return the `.header-wrapper' div holding the heading and its self-link.

See `org-w3ctr-heading-format-function' for the descriptions of
HEADLINE, TITLE, H, ID, CLASS, and INFO."
  (declare (ftype (function (t string string string (or null string) list)
                            string))
           (important-return-value t))
  (let ((secno (t--headline-secno headline info))
        (self-link (t--headline-self-link id info)))
    (format (concat "<div class=\"header-wrapper\">\n"
                    "<%s%s>%s</%s>\n"
                    "%s</div>\n")
            h (or (and class (format " class=\"%s\"" class)) "")
            (concat secno title) h (or self-link ""))))

(defun t--build-normal-headline (headline contents info)
  "Build HTML for a standard headline and its section.

HEADLINE is the headline element, CONTENTS its transcoded contents,
and INFO the info plist.  Format a regular headline, which is not a
footnote or a low-level headline treated as a list item.  The heading
block is built by the function in `:html-heading-format-function'."
  (declare (ftype (function (t (or null string) list) string))
           (important-return-value t))
  (let* ((h (t--headline-hN headline info))
         (text (t--build-base-headline headline info))
         (id (t--reference headline info))
         (c (t--headline-container headline info))
         (c-cls (org-element-property :HTML_CONTAINER_CLASS headline))
         (h-cls (org-element-property :HTML_HEADLINE_CLASS headline))
         (heading (funcall (or (t--pget info :html-heading-format-function)
                               #'t-heading-default-format-function)
                           headline text h id h-cls info)))
    (format "<%s id=\"%s\"%s>\n%s%s</%s>\n"
            c id (or (and c-cls (format " class=\"%s\"" c-cls)) "")
            heading
            (or contents "") c)))

(defun t-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (declare (ftype (function (t (or null string) list) (or null string)))
           (important-return-value t))
  (unless (org-element-property :footnote-section-p headline)
    (if (t--low-level-headline-p headline info)
        ;; This is a deep sub-tree: export it as a list item.
        (t--build-low-level-headline headline contents info)
      ;; Normal headline.  Export it as a section.
      (t--build-normal-headline headline contents info))))

;;; Template and Inner Template

;;;; <meta> tags export.

;; Options:
;; - :time-stamp-file (`org-export-timestamp-file')
;; - :html-file-timestamp-function (`org-w3ctr-file-timestamp-function')
;; - `org-w3ctr-coding-system'
;; - :html-viewport (`org-w3ctr-viewport')
;; - :author #+AUTHOR: (`user-full-name')
;; - :with-author (`org-export-with-author')
;; - :title #+TITLE:
;; - :with-title (`org-export-with-title')
;; - :description #+DESCRIPTION:
;; - :keywords #+KEYWORDS:
;; - `org-w3ctr-meta-tags'

(defun t--build-meta-entry ( label identity
                             &optional content-format
                             &rest content-formatters)
  "Build a <meta> tag from LABEL and IDENTITY.

Construct a tag of the form <meta LABEL=\"IDENTITY\">, or, when
CONTENT-FORMAT is present, <meta LABEL=\"IDENTITY\"
content=\"{content}\">.

{content} is CONTENT-FORMAT, after any CONTENT-FORMATTERS are
applied to it, encoded as plain text.  LABEL and IDENTITY are not
escaped; callers pass literal names."
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
  "Return the current timestamp in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  (declare (ftype (function (t) string))
           (side-effect-free t) (important-return-value t))
  (format-time-string "%FT%RZ" nil t))

(defun t--get-info-file-timestamp (info)
  "Return the file timestamp string from the INFO plist.

INFO is the info plist.  Return nil when `:time-stamp-file' is nil;
otherwise call the function in `:html-file-timestamp-function' with
INFO and return its result.  Signal `org-w3ctr-error' if that option
is not a function."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when (t--pget info :time-stamp-file)
    (if-let* ((fun (t--pget info :html-file-timestamp-function))
              ((functionp fun)))
        (funcall fun info)
      (t-error "Invalid file timestamp function: %s"
               (t--pget info :html-file-timestamp-function)))))

(defun t--ensure-charset-utf8 ()
  "Return \"utf-8\" after validating `org-w3ctr-coding-system'.

Signal an error when `org-w3ctr-coding-system' is not a symbol, names
no coding system, or names one whose MIME charset is not UTF-8."
  (declare (ftype (function () string))
           (side-effect-free t) (important-return-value t))
  (let* ((c t-coding-system)
         (h (lambda (_) (t-error "Invalid coding system: %s" c))))
    (unless (symbolp c) (funcall h c))
    (handler-bind ((coding-system-error h))
      (let ((uc (coding-system-get c :mime-charset)))
        (if (eq uc 'utf-8) "utf-8" (funcall h c))))))

(defun t--build-viewport-options (info)
  "Build the viewport <meta> tag from `:html-viewport'.

INFO is the info plist.  Keep the option's entries whose value is
non-whitespace, format them as key=value pairs separated by commas,
and return nil when nothing remains."
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
  "Return the title from the INFO plist as plain text.

INFO is the info plist.  Return `:title' interpreted, trimmed, and
escaped as plain text; return a left-to-right mark (invisible) when
`:title' is absent, empty, or whitespace."
  (declare (ftype (function (list) string))
           (important-return-value t))
  ;; HTML always needs <title>, so just ignore :with-title.
  (if-let* ((title (t--pget info :title))
            (str0 (org-element-interpret-data title))
            (str (t--nw-trim str0))
            (text (t-plain-text str info)))
      ;; Set title to an invisible character instead of
      ;; leaving it empty, which is invalid.
      text "&lrm;"))

(defun t--get-info-author-raw (info)
  "Return the author from the INFO plist, or nil.

INFO is the info plist.  Return nil when `:with-author' or `:author'
is nil; otherwise interpret `:author' as raw Org syntax and trim it."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* (((t--pget info :with-author))
              (a (t--pget info :author)))
    ;; Return raw Org syntax.
    ;; #+author is parsed as Org object.
    (t--nw-trim (org-element-interpret-data a))))

(defun t-meta-tags-default (info)
  "Return the default value for `org-w3ctr-meta-tags'.

INFO is the info plist.  Return a list of items, each a list of
arguments suitable for `org-w3ctr--build-meta-entry', describing the
author, description, keywords, and generator meta tags."
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
  "Build the HTML <meta> tags from `org-w3ctr-meta-tags'.

INFO is the info plist.  Evaluate the option (calling it with INFO
when it is a function) and build one <meta> tag per entry."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (mapconcat
   (lambda (args) (apply #'t--build-meta-entry args))
   (remq nil (if (not (functionp t-meta-tags)) t-meta-tags
               (funcall t-meta-tags info)))))

(defun t--build-meta-info (info)
  "Return the head meta block for the exported document.

INFO is the info plist.  Return the export-timestamp comment (when
`:time-stamp-file' is set), the charset, the viewport (when
`:html-viewport' is set), the title, and the tags from
`org-w3ctr-meta-tags'."
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

;; Options:
;; - `org-w3ctr-style'
;; - `org-w3ctr-style-file'

(defun t--load-css (_info)
  "Return the CSS for HTML export.

INFO is unused.  Return `org-w3ctr-style' when it is a non-whitespace
string, else the cached `org-w3ctr--style-cache'.  Otherwise, when
`org-w3ctr-style-file' is non-nil, load it, wrap its contents in a
<style> element, store the result in `org-w3ctr--style-cache', and
return it.  Return nil when neither is set."
  (declare (ftype (function (t) (or null string)))
           (important-return-value t))
  (or (t--nw-p t-style)
      (t--nw-p t--style-cache)
      (when t-style-file
        (let* ((str (t--load-file t-style-file))
               (str* (org-element-normalize-string str))
               (css (format "<style>\n%s</style>\n" str*)))
          (setq t--style-cache css)))))

(defun t-clear-css ()
  "Clear the cached CSS loaded from `org-w3ctr-style-file'.

When CSS is loaded from `org-w3ctr-style-file', its content is cached
to improve performance.  If you modify the external CSS file and want
the changes to take effect on the next export, run this command to
clear the cache.  This forces the exporter to re-read the file."
  (interactive)
  (setq t--style-cache nil))

;;;; Math config

;; FIXME: Consider adding a `mathml-by-mathjax' case to
;; `org-w3ctr-math-head-default-function', which today returns "" for
;; it while `svg-by-mathjax' gets its display-math style.
;; Options:
;; - :with-latex (`org-w3ctr-with-latex')
;; - :html-mathjax-config (`org-w3ctr-mathjax-config')
;; - :html-math-head-function (`org-w3ctr-math-head-function')

(defconst t-svg-math-style "\
<style>
.math-display { display: block; text-align: center; margin: 1em 0; }
</style>
"
  "Style for display math produced by `svg-by-mathjax'.")

(defun t-math-head-default-function (info)
  "Return the math setup for the <head>, by default.

INFO is the info plist.  Return the MathJax configuration for
`mathjax' mode, the display-math style for `svg-by-mathjax' mode, or an
empty string otherwise."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (pcase (t--pget info :with-latex)
    (`mathjax (t--pget info :html-mathjax-config))
    (`svg-by-mathjax t-svg-math-style)
    (_ "")))

(defun t--build-math-config (info)
  "Return the math setup to insert into <head>.

INFO is the info plist.  Call the function in `:html-math-head-function',
or `org-w3ctr-math-head-default-function' when it is nil."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (funcall (or (t--pget info :html-math-head-function)
               #'t-math-head-default-function)
           info))

;;;; Rest of <head>

;; Options:
;; - :html-head (`org-w3ctr-head')
;; - :html-head-extra (`org-w3ctr-head-extra')
;; - :html-head-include-style (`org-w3ctr-head-include-style')

(defun t--use-default-style-p (info)
  "Return non-nil if the export includes the default CSS style.

INFO is the info plist."
  (declare (ftype (function (list) boolean))
           (important-return-value t))
  (t--pget info :html-head-include-style))

(defun t--has-math-p (info)
  "Return non-nil if the Org document has a LaTeX fragment or environment.

INFO is the info plist."
  (declare (ftype (function (list) boolean))
           (important-return-value t))
  (and (t--pget info :with-latex)
       (org-element-map (t--pget info :parse-tree)
           '(latex-fragment latex-environment)
         (lambda (_) t) info t nil t)))

(defun t--normalize-string-or-function (input &rest args)
  "Normalize INPUT, whether a string or a function.

INPUT is a string, or a function called with ARGS.  Apply INPUT when it
is a function, then normalize the result with
`org-element-normalize-string'.  Return nil when the function returns
nil."
  (declare (ftype (function (t &rest t) (or null string)))
           (important-return-value t))
  (let ((s (if (functionp input)
               (let ((r (apply input args)))
                 (and r (format "%s" r)))
             input)))
    (org-element-normalize-string s)))

;; FIXME: Consider adding code highlighting (such as highlight.js).
(defun t--build-head (info)
  "Return the <head>...</head> block of the HTML output.

INFO is the info plist.  Return the <meta> block, the default style, the
math configuration, and the user's `:html-head' and `:html-head-extra'
contents, wrapped in a <head> element."
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
   (t--normalize-string-or-function (t--pget info :html-head) info)
   (t--normalize-string-or-function (t--pget info :html-head-extra) info)
   "</head>\n"))

;;;; Navbar

;; Options:
;; - :html-link-navbar (`org-w3ctr-link-navbar')
;; - :html-navbar-format-function (`org-w3ctr-navbar-format-function')
;; - :html-link-up (`org-w3ctr-link-up')
;; - :html-link-home (`org-w3ctr-link-home')
;; - :html-home/up-format (`org-w3ctr-home/up-format')

;; The legacy home/up bar fills in when the navbar yields no links.
;; FIXME: It is only half of ox-html's home/up feature: ox-html also
;; prepends `:html-link-home' to relative file links when
;; `:html-link-use-abs-url' is set (see `org-html-link-file-path').
;; ox-w3ctr has never implemented that half; see the FIXME in
;; `org-w3ctr--link-path'.

(defun t--format-home/up (fmt up home)
  "Apply the home/up format string FMT to the link URLs UP and HOME.

FMT is a `format' control string as in `org-w3ctr-home/up-format':
its first %s receives UP and its second HOME.  Insert both URLs
verbatim, without HTML escaping.  Return the filled string,
exactly what `format' produces: `org-w3ctr--format-legacy-navbar'
normalizes the trailing newline.

Signal `org-w3ctr-error' when FMT is not a string, or when `format'
rejects it, for example for a literal %, an invalid format
operation, or more %s specifications than the two links can fill."
  (declare (ftype (function (t string string) string))
           (important-return-value t))
  (unless (stringp fmt)
    (t-error "Invalid :html-home/up-format: %S" fmt))
  (condition-case err
      (format fmt up home)
    (error (t-error "Invalid :html-home/up-format: %s"
                    (error-message-string err)))))

(defun t--format-legacy-navbar (info)
  "Format the legacy Home/Up navigation bar from the export INFO.

INFO is the export options plist.  Read the link targets from the
`:html-link-up' and `:html-link-home' options, and the format
string from `:html-home/up-format'; its first %s receives UP and
its second HOME.  When only one of the two links is set, both
anchors use it.  The links go into the format string verbatim,
without HTML escaping.

Return the bar as a string, normalized to end in a newline.
Return nil when both links are empty, whitespace-only, or
missing.  Signal `org-w3ctr-error' when `:html-home/up-format' is
not a string or `format' rejects it; the format string is checked
only when the bar is built, so with both links absent a bad one
goes unnoticed."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (let ((link-up (t--nw-trim (t--pget info :html-link-up)))
        (link-home (t--nw-trim (t--pget info :html-link-home))))
    (when (or link-up link-home)
      (org-element-normalize-string
       (t--format-home/up (t--pget info :html-home/up-format)
                          (or link-up link-home)
                          (or link-home link-up))))))

(defun t--wrap-navbar (links)
  "Wrap the navbar links HTML in the <nav> element.

LINKS is the rendered anchor HTML.  Surrounding whitespace and
newlines are trimmed before wrapping, so the block shape does not
depend on how the caller formats its input; internal newlines are
kept.  Return the navbar block, a <nav> element with id
\"navbar\", ending in a newline.  A blank LINKS gives an empty
<nav> shell."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (concat "<nav id=\"navbar\">" (t--prepend-newline (t--trim links))
          "\n</nav>\n"))

(defun t--format-navbar-vector (pairs)
  "Build the navbar from the (URL . NAME) pairs in vector PAIRS.

PAIRS is a vector of conses as in `org-w3ctr-link-navbar'.  Return
the navbar block, one anchor per pair: link and name go in
verbatim, without HTML escaping.  Return \"\" for an empty PAIRS,
which `org-w3ctr-navbar-default-format-function' reads as \"no links\"
and answers with the legacy home/up bar.  Signal `org-w3ctr-error'
when an entry is not a (URL . NAME) cons of strings."
  (declare (ftype (function (vector) string))
           (important-return-value t))
  (if (equal pairs []) ""
    (let ((valid-p (lambda (x) (and (stringp (car-safe x))
                                    (stringp (cdr-safe x))))))
      (unless (cl-every valid-p pairs)
        (t-error "Invalid navbar vector: %s" pairs))
      (t--wrap-navbar
       (mapconcat
        (pcase-lambda (`(,link . ,name))
          (format "<a href=\"%s\">%s</a>" link name))
        pairs "\n")))))

(defun t--format-navbar-list (elements info)
  "Build the navbar from the Org link elements ELEMENTS.

ELEMENTS is a list of Org elements as parsed from the
HTML_LINK_NAVBAR keyword, and INFO is the export options plist
used to transcode them with `org-export-data'.  Return the navbar
block, one line per element that transcodes to a non-blank string;
the strings go in verbatim, without HTML escaping.  Return \"\"
when ELEMENTS is nil or nothing survives, which
`org-w3ctr-navbar-default-format-function' reads as \"no links\"
and answers with the legacy home/up bar."
  (declare (ftype (function (list list) string))
           (important-return-value t))
  (if (null elements) ""
    (let* ((rendered (mapcar (lambda (x) (org-export-data x info)) elements))
           (kept (delq nil (mapcar #'t--nw-trim rendered))))
      (if (null kept) "" (t--wrap-navbar (string-join kept "\n"))))))

(defun t-navbar-default-format-function (info)
  "Generate the navbar HTML from the export options INFO.

INFO is the export options plist.  Read the links from
`:html-link-navbar' and render them: a vector of (URL . NAME)
conses becomes one anchor per entry, and a list of Org elements
from the HTML_LINK_NAVBAR keyword is transcoded with
`org-export-data'.  The anchors are wrapped in a <nav> element
with id \"navbar\".

When the option yields no links at all (nil, an empty vector, or
a list that transcoded to nothing), fall back to the legacy
home/up bar, `org-w3ctr--format-legacy-navbar'.  Return the
navbar HTML as a string: \"\" when neither the navbar option nor
the legacy home/up bar yields any links.  Signal `org-w3ctr-error'
when the option is neither a vector nor a list, and when a vector
entry is not a (URL . NAME) cons of strings (checked in
`org-w3ctr--format-navbar-vector')."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let* ((links (t--pget info :html-link-navbar))
         (nav (pcase links
                ((pred vectorp) (t--format-navbar-vector links))
                ((pred listp) (t--format-navbar-list links info))
                (other (t-error "Invalid navbar type: %s" other)))))
    ;; Empty result, whatever the reason: the legacy home/up bar.
    (if (string-empty-p nav) (or (t--format-legacy-navbar info) "") nav)))

;;;; CC license badges

;; Options:
;; - :html-use-cc-badges (`org-w3ctr-use-cc-badges')
;; - :html-license (`org-w3ctr-public-license')
;; - :html-license-format-function (`org-w3ctr-license-format-function')
;; - :html-cc-badges-format-function (`org-w3ctr-cc-badges-format-function')

(defconst t-public-license-alist
  '((nil "Not Specified")
    (all-rights-reserved "All Rights Reserved")
    (all-rights-reversed "All Rights Reversed")
    (cc0 "CC0 1.0 Universal"
     "https://creativecommons.org/publicdomain/zero/1.0/")
    (public-domain-mark "Public Domain Mark 1.0"
     "https://creativecommons.org/publicdomain/mark/1.0/")
    ;; 4.0 (the whole suite: six licenses).
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
    ;; 3.0 Unported (the whole suite; not recommended by Creative
    ;; Commons).  Jurisdiction ports (US, IGO, and friends), the
    ;; retired Sampling / Sampling Plus / Developing Nations licenses,
    ;; and the 2.x series are deliberately out of scope.
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
  "Alist mapping license symbols to display names and deed URLs.

Each element has the form (SYMBOL DISPLAY-NAME &optional URL);
an entry without a URL renders as a plain name.  The catalogue is
complete for the CC 3.0 and 4.0 suites and the public-domain
tools: it lists what Creative Commons publishes and recommends
none.  CC license symbols read `cc-<components>-<version>', from
which `org-w3ctr--cc-icon-names' derives the icon names.  See
`org-w3ctr-license-default-format-function' for how entries are
rendered.")

(defvar t--cc-svg-cache (make-hash-table :test 'equal)
  "Cache of base64-encoded SVG icons, keyed by icon name.

Keys are icon name strings (the shipped set is cc, by, nc, nd,
sa, zero, and pdm); values are the base64-encoded contents of
assets/<name>.svg.  `org-w3ctr--load-cc-svg-once' fills entries
in on demand.")

(defun t--load-cc-svg (name)
  "Load the icon SVG file NAME and return it base64-encoded.

NAME is an icon name; the file is assets/<NAME>.svg under the
package directory.  Return its contents as a base64 string with
no line breaks.  Signal `org-w3ctr-error' when the file does not
exist."
  (declare (ftype (function (string) string))
           (important-return-value t))
  ;; Base64 needs bytes, and `org-w3ctr--load-file' decodes UTF-8, so
  ;; encode the string back to UTF-8 to get a unibyte string.
  (base64-encode-string
   (encode-coding-string
    (t--load-file (file-name-concat t--dir "assets" (concat name ".svg")))
    'utf-8)
   t))

(defun t--load-cc-svg-once (name)
  "Return the base64 SVG of the icon NAME, reading it at most once.

Like `org-w3ctr--load-cc-svg', but consult `org-w3ctr--cc-svg-cache'
first: NAME is read from disk on the first call and served from
the cache afterwards."
  (declare (ftype (function (string) string))
           (important-return-value t))
  (with-memoization (gethash name t--cc-svg-cache)
    (t--load-cc-svg name)))

(defun t--cc-icon-alt (name)
  "Return the alt text for the badge icon NAME.

NAME is an icon name; the alt text is its standard abbreviation:
most names simply uppercase (by becomes BY), while zero becomes
CC0."
  (declare (ftype (function (string) string))
           (pure t) (important-return-value t))
  (if (equal name "zero") "CC0" (upcase name)))

(defun t--build-cc-img (name base64)
  "Build an HTML img tag for the badge icon NAME with BASE64 SVG.

NAME is an icon name as in `org-w3ctr--cc-icon-names': the alt
attribute carries its standard abbreviation from
`org-w3ctr--cc-icon-alt' (BY, NC, SA, and CC0 for zero).  BASE64
is the icon's base64 SVG, as returned by
`org-w3ctr--load-cc-svg-once'.  Return the tag as a string.

The inline sizing style is the CC license chooser's (see the
chooser at https://chooser-beta.creativecommons.org/) minus its
!important: an exported document has no hostile host stylesheet
to guard against, and the !important would block user styling."
  (declare (ftype (function (string string) string))
           (pure t) (important-return-value t))
  (format "<img style=\"height:1.4em;margin-left:0.2em;\
vertical-align:text-bottom;\" src=\"data:image/svg+xml;base64,%s\" \
alt=\"%s\">" base64 (t--cc-icon-alt name)))

(defun t--cc-icon-names (license)
  "Return the icon file names for LICENSE, or nil when it has none.

LICENSE is a license symbol as in `org-w3ctr-public-license-alist'.
A CC license icon set is named after its components: for example
`cc-by-nc-sa-4.0' takes the icons cc, by, nc, and sa.  The two
public-domain tools carry their own icons; other entries, and
non-symbol values, have none."
  (declare (ftype (function (t) list))
           (pure t) (important-return-value t))
  (and (symbolp license)
       (pcase license
         ('cc0 '("cc" "zero"))
         ('public-domain-mark '("pdm"))
         ((pred (lambda (s) (string-prefix-p "cc" (symbol-name s))))
          (split-string (symbol-name license) "[0-9.-]" t))
         (_ nil))))

(defun t-cc-badges-default-format-function (license _info)
  "Build the HTML img tags for the icons of LICENSE.

LICENSE is a license symbol; the file names come from
`org-w3ctr--cc-icon-names'.  INFO is the export options plist,
unused here.  Return the img tags concatenated, or the empty
string when LICENSE has no icons."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (let ((f (lambda (x) (t--build-cc-img x (t--load-cc-svg-once x)))))
    (mapconcat f (t--cc-icon-names license))))

(defun t--get-info-author (info)
  "Return the exported author string from INFO, or nil.

INFO is the export options plist.  Return nil when :with-author or
:author is nil, or when the author transcodes to a blank string.
Unlike `org-w3ctr--get-info-author-raw', the author goes through
`org-export-data': markup in the author name becomes HTML."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* (((t--pget info :with-author))
              (a (t--pget info :author)))
    (t--nw-trim (org-export-data a info))))

(defun t-license-default-format-function (info)
  "Generate the license line from the export options INFO.

INFO is the export options plist.  Read the license from
`:html-license' and describe it as its kind demands: a CC license
is \"licensed under\", CC0 is dedicated to the public domain, and
the Public Domain Mark marks a work as being in the public domain.

The author comes from `org-w3ctr--get-info-author' and the badge icons
from the `:html-cc-badges-format-function' hook when
`:html-use-cc-badges' is non-nil.  Return the line as a string.
Signal `org-w3ctr-error' for an unknown license."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let* ((license (t--pget info :html-license))
         (details (assq license t-public-license-alist))
         (use-badges (t--pget info :html-use-cc-badges))
         (author (t--get-info-author info)))
    (unless details
      (t-error "Unknown license: %s" license))
    (pcase (cdr details)
      (`(,name) name)
      (`(,name ,link)
       (let ((tag (if (null link) name
                    (format "<a href=\"%s\">%s</a>" link name))))
         (concat
          "This work"
          (when author (concat " by " author))
          (pcase license
            ('cc0 (concat " is dedicated to the public domain under " tag))
            ('public-domain-mark
             (concat " is marked as being in the public domain (" tag ")"))
            (_ (concat " is licensed under " tag)))
          (when-let* ((_ use-badges)
                      (fn (or (t--pget info :html-cc-badges-format-function)
                              #'t-cc-badges-default-format-function))
                      (badges (funcall fn license info))
                      (_ (not (string-empty-p badges))))
            (concat " " badges)))))
      (_ (t-error "Internal error")))))

(defun t-format-public-license (info)
  "Generate the license line from the export options INFO.

INFO is the export options plist.  This is the stable entry for
the license line: it calls the `:html-license-format-function'
hook, whose default is `org-w3ctr-license-default-format-function'
and which receives INFO.  Return its result as a string."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (funcall (or (t--pget info :html-license-format-function)
               #'t-license-default-format-function)
           info))

;;;; Preamble and Postamble

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
  "Return the `format-spec' alist for preamble and postamble.

INFO is the export options plist.  The entries are precomputed;
each maps a format character to its replacement string:

- %t: the document title.
- %s: the document subtitle.
- %d: the document date, formatted with
  `org-w3ctr-metadata-timestamp-format'.
- %T: the current time, same format.
- %a: the author.
- %e: the author's email as mailto links.
- %c: the creator string.
- %C: the modification time of the input file.
- %v: the `org-w3ctr-validation-link' HTML."
  (declare (ftype (function (list) list))
           (important-return-value t))
  (let ((fmt (t--pget info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (t--pget info :title) info))
      (?s . ,(org-export-data (t--pget info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info fmt) info))
      (?T . ,(format-time-string fmt))
      (?a . ,(org-export-data (t--pget info :author) info))
      (?e . ,(if-let* ((email (t--pget info :email))
                       ((t--nw-p email)))
                 (mapconcat
                  (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
                  (split-string email ",+ *" t)
                  ", ")
               ""))
      (?c . ,(or (t--pget info :creator) ""))
      (?C . ,(or (when-let* ((file (t--pget info :input-file))
                             (attrs (file-attributes file)))
                   (format-time-string
                    fmt (file-attribute-modification-time attrs)))
                 ""))
      (?v . ,(or (t--pget info :html-validation-link) "")))))

;; Modified preamble/postamble handling compared to ox-html:
;; - Remove the `org-html-preamble-format' / `org-html-postamble-format'
;;   mechanism; values go directly through `org-w3ctr-preamble' and
;;   `org-w3ctr-postamble'.
;; - Drop the 'auto option for postamble.
(defun t--build-pre/postamble (type info)
  "Build the preamble or postamble string for export INFO.

TYPE is the symbol `preamble' or `postamble', selecting the
`:html-preamble' or `:html-postamble' option.  Both accept the
same kinds of value: nil gives the empty string, a string is
formatted with `format-spec' against
`org-w3ctr--pre/postamble-format-spec', a function is called with
INFO, and a symbol with a function is called like a function, or
else its value cell is formatted as a string.  Return the result
as a string, normalized to end in a newline, or the empty string
when it is blank.  Signal `org-w3ctr-error' when a symbol has no
usable string value, or when the value is of any other type."
  (declare (ftype (function (symbol list) string))
           (important-return-value t))
  (let* ((section (t--pget info (intern (format ":html-%s" type))))
         (spec (t--pre/postamble-format-spec info))
         (it (cond
              ((null section) "")
              ;; string formatted with `format-spec'.
              ((stringp section) (format-spec section spec))
              ;; function.
              ((functionp section) (funcall section info))
              ;; symbol: call it if it has a function, or else format
              ;; the string in its value cell.
              ((symbolp section)
               (unless (boundp section)
                 (t-error "Invalid %s symbol: %s" type section))
               (if-let* ((value (symbol-value section))
                         ((t--nw-p value)))
                   (format-spec value spec)
                 (t-error "Invalid %s symbol value: %s"
                          type (symbol-value section))))
              (t (t-error "Invalid %s: %s" type section)))))
    (or (and (t--nw-p it) (org-element-normalize-string it)) "")))

(defun t--get-info-date (info)
  "Return the document date from INFO, rendered by the back end.

INFO is the export options plist.  The date is the single Org timestamp
in the `:date' option, rendered by `org-w3ctr--format-timestamp-int';
return nil when `:date' is missing, malformed, or holds more than one
timestamp.

Unlike the %d format code, which renders only the start of a range with
`org-w3ctr-metadata-timestamp-format', this renders the whole timestamp
in the back-end's own style."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* ((date (t--pget info :date))
              (_ (and (proper-list-p date) (null (cdr date))))
              (_ (org-element-type-p (car date) 'timestamp)))
    (t--format-timestamp-int (car date) info)))

(defun t--get-info-mtime (info)
  "Return the input file's modification time as an ISO UTC string.

INFO is the export options plist.  Return the modification time
of the `:input-file', formatted as %FT%RZ (ISO 8601, UTC), or nil
when there is no input file or it cannot be stat'ed."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (and-let* ((file (t--pget info :input-file))
             (time (file-attribute-modification-time
                    (file-attributes file))))
    (format-time-string "%FT%RZ" time t)))

(defun t-preamble-default-function (info)
  "Return the default HTML preamble for export INFO.

INFO is the export options plist.  The preamble is a <details>
element listing the document metadata: the publication date from
`org-w3ctr--get-info-date', the last modification time from
`org-w3ctr--get-info-mtime', the creator string, and the license
line from `org-w3ctr-format-public-license'.  A row shows
\"[Not Specified]\" when its value is missing or blank."
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
   (or (t--get-info-mtime info) "[Not Specified]")
   "</dd>\n"
   ;; Creation tools.
   "<dt>Creation Tools:</dt> <dd>"
   (or (t--nw-trim (t--pget info :creator)) "[Not Specified]")
   "</dd>\n"
   ;; License.
   "<dt>Public License:</dt> <dd>"
   (t-format-public-license info)
   "</dd>\n"
   "</dl>\n"
   "</details>\n"
   "<hr>"))

;;;; Table of Contents

;; Options:
;; :html-toc-element (`org-w3ctr-toc-element')
;; :html-toc-title (`org-w3ctr-toc-title')
;; :html-toc-headline-format-function
;;   (`org-w3ctr-toc-headline-format-function')
;; :with-toc (`org-export-with-toc')

(defun t--toc-headline-secno (headline info)
  "Return the section number of HEADLINE as an HTML span, or nil.

HEADLINE is a headline element and INFO the export options plist.
The span holds the headline number, dotted between levels.  Return
nil when HEADLINE is unnumbered."
  (declare (ftype (function (t list) (or null string)))
           (important-return-value t))
  (when-let* (((org-export-numbered-headline-p headline info))
              (numbers (org-export-get-headline-number headline info)))
    (format "<span class=\"secno\">%s</span>"
            (mapconcat #'number-to-string numbers "."))))

(defun t--build-toc-headline (headline info)
  "Build a headline string for the Table of Contents.

HEADLINE is the headline element and INFO is the info plist.  Retrieve
HEADLINE's alternative title, falling back to the regular title when
none is set, format it for export with the TOC entry backend, and pass
it to `org-w3ctr--build-bare-headline' for final assembly."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  ;; FIXME: The default TOC entry backend turns links into text, so an
  ;; inline image in a headline title becomes its file name in the TOC.
  ;; Upstream `org-html--format-toc-headline' (3ea1682731, "Generate
  ;; images in TOC for HTML export") overrides the link transcoder to
  ;; render such images with `org-html-link'.  Decide whether to follow
  ;; it; for W3C TR output the text is likely preferable.
  (let ((text (org-export-data-with-backend
               (org-export-get-alt-title headline info)
               (org-export-toc-entry-backend 'w3ctr)
               info)))
    (t--build-bare-headline headline text info)))

(defun t-toc-headline-default-format-function (headline info)
  "Format the table of contents entry for HEADLINE.

This is the default of `org-w3ctr-toc-headline-format-function',
the counterpart of ox-html's `org-html--format-toc-headline'.

HEADLINE is a headline element and INFO the export options plist.
The entry is an anchor to HEADLINE's reference, holding the
section number from `org-w3ctr--toc-headline-secno' and the
headline text from `org-w3ctr--build-toc-headline'.  Low-level
headlines get no section number."
  (declare (ftype (function (t list) string))
           (important-return-value t))
  (format "<a href=\"#%s\">%s</a>" (t--reference headline info)
          (concat (and (not (t--low-level-headline-p headline info))
                       (t--toc-headline-secno headline info))
                  (t--build-toc-headline headline info))))

(defun t--get-info-toc-element (info)
  "Return the TOC list tag from INFO as a string.

INFO is the export options plist.  The `:html-toc-element' option
is the symbol `ul' or `ol'; return its name.  Signal
`org-w3ctr-error' for any other value."
  (declare (ftype (function (list) string))
           (important-return-value t))
  (let ((tag (t--pget info :html-toc-element)))
    (pcase tag
      (`ul "ul") (`ol "ol")
      (_ (t-error "Invalid TOC list tag: %s" tag)))))

(defun t--toc-alist-to-text (toc-entries info &optional top)
  "Return the innards of a table of contents as a string.

TOC-ENTRIES is a non-empty alist of (TITLE . LEVEL) pairs in
document order, TITLE a string and LEVEL the headline's relative
level.  INFO is the export options plist; its `:html-toc-element'
chooses the list tag.  With TOP non-nil, nesting starts at level
zero; otherwise it starts one below the first entry's level.  Lists
open and close with the level changes; the result carries no
wrapper."
  (declare (ftype (function (list list &optional boolean) string))
           (important-return-value t))
  (let* ((tag (t--get-info-toc-element info))
         (open (format "\n<%s class=\"toc\">\n<li>" tag))
         (close (format "</li>\n</%s>\n" tag))
         (base (if top 0 (1- (cdar toc-entries))))
         (levels (mapcar #'cdr toc-entries))
         ;; Each entry deepens or climbs from its predecessor; the
         ;; first compares against the base level.
         (deltas (cl-mapcar #'- levels (cons base levels)))
         (step (pcase-lambda (`(,title . ,delta))
                 (if (> delta 0)
                     (concat (t--make-string delta open) title)
                   (concat (t--make-string (- delta) close)
                           "</li>\n<li>" title)))))
    (concat
     (mapconcat step (cl-mapcar #'cons (mapcar #'car toc-entries) deltas))
     (t--make-string (- (car (last levels)) base) close))))

(defun t--build-toc (depth info &optional scope)
  "Build the innards of a table of contents.

DEPTH is the headline depth `org-export-collect-headlines' takes;
nil collects every level up to `org-export-headline-levels'.  INFO
is the export options plist.  Optional argument SCOPE is an
element that limits the collection to its own subtree; it also
shifts the nesting start, a scoped table beginning one level below
its first entry while a full one begins at level zero.  Each entry
is rendered by the `:html-toc-headline-format-function' hook.
Return the nested list as a string, or nil when no headline falls
within DEPTH."
  (declare (ftype (function ((or null integer) list &optional t)
                            (or null string)))
           (important-return-value t))
  (let* ((fmt (or (t--pget info :html-toc-headline-format-function)
                  #'t-toc-headline-default-format-function))
         (entry (lambda (h)
                  (cons (funcall fmt h info)
                        (org-export-get-relative-level h info)))))
    (when-let* ((hs (org-export-collect-headlines info depth scope)))
      (t--toc-alist-to-text (mapcar entry hs) info (not scope)))))

(defun t--build-table-of-contents (info)
  "Build the document table of contents for export INFO.

INFO is the export options plist.  Return the <nav id=\"toc\">
block, holding a heading with the `:html-toc-title' text at the
`:html-toplevel-hlevel' level and the entries from
`org-w3ctr--build-toc', or nil when `:with-toc' is nil or no
headline falls within its depth.  A nil `:with-toc' means no
table at all, distinct from the unlimited depth
`org-w3ctr--build-toc' gives that value."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when-let* ((depth (t--pget info :with-toc))
              (toc (t--build-toc depth info)))
    (concat
     "<nav id=\"toc\">\n"
     (let ((top-level (t--pget info :html-toplevel-hlevel)))
       (format "<h%d>%s</h%d>"
               top-level (or (t--pget info :html-toc-title) t-toc-title)
               top-level))
     toc
     "</nav>\n")))

(defun t--list-of-elements (collect-fn info)
  "Return an HTML list of elements collected by COLLECT-FN.

COLLECT-FN is a function that takes INFO and returns a list of Org
elements; each must carry a caption, as
`org-export-collect-listings' and `org-export-collect-tables'
guarantee.  INFO is the export options plist.

Each list item displays the element's caption -- the short one
when it has one, else the full one -- and links to the element
when it has a reference label.  Only named elements are linked:
unnamed ones carry fresh random ids on every export."
  (declare (ftype (function (function list) (or null string)))
           (important-return-value t))
  (when-let* ((entries (funcall collect-fn info)))
    (concat
     "<ul class=\"index\">\n"
     (mapconcat
      (lambda (entry)
        (let* ((label (t--reference entry info t))
               (caption (or (org-export-get-caption entry t)
                            (org-export-get-caption entry)))
               (title (t--trim (org-export-data caption info))))
          (format "<li>%s</li>"
                  (if (not label) title
                    (format "<a href=\"#%s\">%s</a>" label title)))))
      entries "\n")
     "\n</ul>")))

(defun t--list-of-listings (info)
  "Return an HTML list of source code listings, or nil.

INFO is the export options plist.  Delegate to
`org-w3ctr--list-of-elements' over `org-export-collect-listings';
return nil when there is no listing."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (t--list-of-elements #'org-export-collect-listings info))

(defun t--list-of-tables (info)
  "Return an HTML list of tables, or nil.

INFO is the export options plist.  Delegate to
`org-w3ctr--list-of-elements' over `org-export-collect-tables';
return nil when there is no table."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (t--list-of-elements #'org-export-collect-tables info))

;; Adapted from `org-html-keyword', with its quirks fixed: the list
;; kinds and "headlines" match regardless of case and surrounding
;; blanks, and the depth is the number right after "headlines", not
;; the first number anywhere in the value.
(defun t--keyword-toc (keyword value info)
  "Transcode the TOC keyword VALUE.

KEYWORD is the keyword element holding VALUE; it serves as the
scope when VALUE asks for a \"local\" table.  VALUE, matched
without regard to case, determines the list to generate:
- \"tables\": a list of tables.
- \"listings\": a list of source code listings.
- \"headlines\": a table of contents, optionally followed by a
  depth number and by \":target LINK\" or \"local\" for scope.  The
  depth is the number right after \"headlines\"; a \":target\"
  takes precedence over \"local\".

INFO is the export options plist.  Return the list as a string,
or nil when VALUE selects none of the three."
  (declare (ftype (function (t string list) (or null string)))
           (important-return-value t))
  (let ((case-fold-search t))
    (cond
     ((string-match-p "\\`\\s-*listings\\s-*\\'" value)
      (t--list-of-listings info))
     ((string-match-p "\\`\\s-*tables\\s-*\\'" value)
      (t--list-of-tables info))
     ((string-match "\\`\\s-*headlines\\(?:\\s-+\\([0-9]+\\)\\)?" value)
      (let ((depth (and (match-string 1 value)
                        (string-to-number (match-string 1 value))))
            (scope
             (cond
              ;; link
              ((string-match ":target\\s-+\\(\".+?\"\\|\\S-+\\)" value)
               (org-export-resolve-link
                (org-strip-quotes (match-string 1 value)) info))
              ;; local headline
              ((string-match-p "\\<local\\>" value) keyword))))
        (t--build-toc depth info scope))))))

;;;; Template

;; Options:
;; :language (`org-export-default-language')
;; :html-include-fixup-js (`org-w3ctr-include-fixup-js')
;; :html-fixup-js (`org-w3ctr-fixup-js')

(defun t-inner-template (contents info)
  "Build the document body from the transcoded CONTENTS and INFO.

INFO is the export options plist.  The body is the zeroth section
\(the content before the first headline, stored by
`org-w3ctr-section' in the `:zeroth-section-output' key of INFO),
then the table of contents, the CONTENTS wrapped in a <main>
element, and the footnote section."
  (declare (ftype (function ((or null string) list) string))
           (important-return-value t))
  ;; See also `org-html-inner-template'.
  (concat
   (t--pget info :zeroth-section-output)
   (t--build-table-of-contents info)
   "<main>\n"
   contents
   "</main>\n"
   (t-footnote-section info)))

(defun t--build-title (info)
  "Build the HTML for the document title and subtitle.

INFO is the export options plist.  Return an <h1 id=\"title\">
holding the title and, when the subtitle is non-blank, a
<p id=\"w3c-state\"> holding it; return nil when `:with-title' is
nil.  An absent or blank title renders as a left-to-right mark:
invisible, but it keeps the heading anchor alive."
  (declare (ftype (function (list) (or null string)))
           (important-return-value t))
  (when (t--pget info :with-title)
    (let ((title (t--pget info :title))
          (subtitle (t--pget info :subtitle)))
      (concat
       "<h1 id=\"title\">"
       (let ((tit (org-export-data title info)))
         (or (t--nw-p tit) "&lrm;"))
       "</h1>\n"
       ;; The subtitle rides in the W3C state line (id=\"w3c-state\"): a
       ;; real TR idiom, styled by the stylesheet's #w3c-state rule.
       (let ((sub (org-export-data subtitle info)))
         (when (t--nw-p sub)
           (format "<p id=\"w3c-state\">%s</p>\n" sub)))))))

(defvar t--fixup-js-cache nil
  "Cached fixup JavaScript loaded from `assets/fixup.js'.

`org-w3ctr--load-fixup-js' stores the wrapped file contents here so
that repeated exports do not re-read the file; `org-w3ctr-clear-js'
resets it.")

(defun t--load-fixup-js ()
  "Return the fixup JavaScript for HTML export.

Return the cached `org-w3ctr--fixup-js-cache' when it is a
non-whitespace string.  Otherwise read `assets/fixup.js' from the
package directory with `org-w3ctr--load-file', wrap its contents in a
<script> element, store the result in the cache, and return it."
  (declare (ftype (function () string))
           (important-return-value t))
  (or (t--nw-p t--fixup-js-cache)
      (setq t--fixup-js-cache
            (format "<script>\n%s\n</script>\n"
                    (t--load-file
                     (file-name-concat t--dir "assets" "fixup.js"))))))

(defun t-clear-js ()
  "Clear the cached fixup JavaScript.

The fixup script is cached after the first export reads it from
`assets/fixup.js'.  After editing that file, run this command so the
next export re-reads it."
  (interactive)
  (setq t--fixup-js-cache nil))

(defun t-template-1 (contents info)
  "Assemble the full HTML document around CONTENTS.

INFO is the export options plist.  Return the document as a
string: the doctype, an <html> element in the `:language'
language, the <head> from `org-w3ctr--build-head', and a <body>
holding, in order, the navbar from `:html-navbar-format-function',
a <div class=\"head\"> with the title and the preamble, CONTENTS,
the postamble, and the `:html-fixup-js' script."
  (declare (ftype (function ((or null string) list) string))
           (important-return-value t))
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (t--pget info :language))
   (t--build-head info)
   "<body>\n"
   ;; A nil `:html-navbar-format-function' suppresses the navbar: the
   ;; option doubles as the switch, unlike the other format hooks,
   ;; which fall back to their defaults.
   (when-let* ((fun (t--pget info :html-navbar-format-function)))
     (funcall fun info))
   ;; Title and preamble in the head block.
   (format "<div class=\"head\">\n%s%s</div>\n"
           (t--build-title info)
           (t--build-pre/postamble 'preamble info))
   contents
   ;; Postamble.
   (t--build-pre/postamble 'postamble info)
   ;; The fixup script, unless switched off: the document's own, else
   ;; the shipped default.
   (when (t--pget info :html-include-fixup-js)
     (org-element-normalize-string
      (or (t--nw-p (t--pget info :html-fixup-js))
          (t--load-fixup-js))))
   ;; Closing document.
   "</body>
</html>"))

(defun t-template (contents info)
  "Assemble the complete document for CONTENTS and INFO.

This is the outer template Org calls: the document from
`org-w3ctr-template-1', after which the OINFO caches drop whatever
the export left in them.  CONTENTS is the transcoded body string
and INFO the export options plist."
  (declare (ftype (function ((or null string) list) string))
           (important-return-value t))
  (prog1 (t-template-1 contents info)
    (static-when t--oinfo-cache-p (t--oinfo-cleanup))))

;;; End-user functions

;;;###autoload
(defun t-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer to an HTML buffer.

Like `org-html-export-as-html', with `org-export-use-babel' bound
to `org-w3ctr-use-babel'."
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

(defun t--file-extension (plist)
  "Return the HTML file extension to export to, dot included.

PLIST is the caller's override plist (the export ext-plist or a
publish project plist), not the export INFO; its `:html-extension'
wins over `org-w3ctr-extension', and \"html\" is the last resort."
  (declare (ftype (function (t) string))
           (important-return-value t))
  (concat (when (> (length t-extension) 0) ".")
          (or (plist-get plist :html-extension)
              t-extension
              "html")))

;;;###autoload
(defun t-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer to an HTML file.

Like `org-html-export-to-html', with `org-export-use-babel' bound
to `org-w3ctr-use-babel' and the file extension from
`org-w3ctr--file-extension'."
  (interactive)
  (let* ((extension (t--file-extension ext-plist))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system t-coding-system)
         (org-export-use-babel t-use-babel))
    (org-export-to-file 'w3ctr file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun t-publish-to-html (plist filename pub-dir)
  "Publish an Org file to HTML.

Like `org-html-publish-to-html', with `org-export-use-babel' bound
to `org-w3ctr-use-babel'.  FILENAME is the Org file to publish,
PLIST the project's property list, and PUB-DIR the publishing
directory.  Return the output file name."
  (let ((org-export-use-babel t-use-babel))
    (org-publish-org-to 'w3ctr filename
                        (t--file-extension plist)
                        plist pub-dir)))

(provide 'ox-w3ctr)

;; Local variables:
;; read-symbol-shorthands: (("t-" . "org-w3ctr-"))
;; coding: utf-8-unix
;; End:
