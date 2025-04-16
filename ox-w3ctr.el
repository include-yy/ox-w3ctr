;;; ox-w3ctr.el --- An Org export Back-End -*- lexical-binding:t;-*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 2024-03-18 04:51:00

;; Package-Version: 0.2.1
;; Package-Requires: ((emacs "30.1"))
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

;;; Dependencies
(require 'cl-lib)
(require 'format-spec)
(require 'xml)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)
(require 'table)

(defvar t--dir
  (if load-in-progress
      (file-name-directory load-file-name)
    default-directory)
  "Directory of ox-w3ctr package.")

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
    (table . t-table)                           ; | | | \n | | |
    ;;@ lesser elements [17]
    ;; babel cell                               NO-EXIST
    ;; clock `clock'                            NO-USE
    ;; comments                                 NO-EXPORT
    ;; comment block                            NO-EXPORT
    ;; diary sexp `diary-sexp'                  NO-USE
    ;; node properties `node-property'          NO-USE
    ;; planning `planning'                      NO-USE
    (example-block . t-example-block)           ; #+BEGIN_EXAMPLE
    (export-block . t-export-block)             ; #+BEGIN_EXPORT
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
    (:html-pre/post-timestamp-format
     nil nil t-pre/post-timestamp-format)
    ;; HTML TOP place naviagtion elements -------------------------
    (:html-link-home/up "HTML_LINK_HOMEUP" nil t-link-homeup parse)
    (:html-format-home/up-function nil nil t-format-home/up-function)
    (:html-home/up-format nil nil t-home/up-format)
    (:html-link-up "HTML_LINK_UP" nil t-link-up)
    (:html-link-home "HTML_LINK_HOME" nil t-link-home)
    ;; Latex and MathJAX options -------
    (:with-latex nil "tex" t-with-latex)
    (:html-mathjax-config nil nil t-mathjax-config)
    (:html-mathjax-options nil nil org-html-mathjax-options)
    (:html-mathjax-template nil nil org-html-mathjax-template)
    ;; postamble and preamble ------------------------
    (:html-postamble nil "html-postamble" t-postamble)
    (:html-preamble nil "html-preamble" t-preamble)
    (:html-validation-link nil nil t-validation-link)
    ;; footnote options -----------------------------
    (:html-footnote-format nil nil t-footnote-format)
    (:html-footnote-separator nil nil t-footnote-separator)
    (:html-footnotes-section nil nil t-footnotes-section)
    ;; headline options -------------------------------------
    (:html-format-headline-function
     nil nil t-format-headline-function)
    (:html-self-link-headlines nil nil t-self-link-headlines)
    (:html-toplevel-hlevel nil nil t-toplevel-hlevel)
    ;; <yy> aux counter for unnumbered headline
    (:html-headline-cnt nil nil 0)
    ;; <yy> zeroth section's toc title name
    (:html-zeroth-section-tocname
     nil "zeroth-name" t-zeroth-section-tocname)
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
    ;; <yy> time zone suffix
    (:html-timezone "HTML_TIMEZONE" nil t-timezone)
    (:html-export-timezone "HTML_EXPORT_TIMEZONE" nil t-export-timezone)
    (:html-datetime-option nil "datetime" t-datetime-format-choice)
    (:html-file-timestamp nil nil t-file-timestamp-function)
    ;; public license
    (:html-license nil "license" t-public-license)
    ;; toc tag name
    (:html-toc-tagname nil "toctag" t-toc-tagname)
    ))

;;; User Configuration Variables.

(defgroup org-export-w3ctr nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export W3CTR HTML"
  :group 'org-export)

(defcustom t-checkbox-type 'unicode
  "The type of checkboxes to use for HTML export.

Possible values:
  `unicode': Use Unicode symbols
  `ascii'  : Use ASCII characters
  `html'   : Use HTML input elements

See `org-w3ctr-checkbox-types' for the exact characters used."
  :group 'org-export-w3ctr
  :type '(choice (const :tag "Unicode symbols" unicode)
                 (const :tag "ASCII characters" ascii)
                 (const :tag "HTML <input> elements" html)))

(defcustom t-text-markup-alist
  ;; See also `org-html-text-markup-alist'.
  '((bold . "<strong>%s</strong>")
    (code . "<code>%s</code>")
    (italic . "<em>%s</em>")
    (strike-through . "<s>%s</s>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>"))
  "Alist of HTML expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-w3ctr
  :type '(alist :key-type (symbol :tag "Markup type")
                :value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defcustom t-meta-tags #'t-meta-tags-default
  "Form that is used to produce <meta> tags in the HTML head.

This can be either:
. A list where each item is a list with the form of (NAME VALUE CONTENT)
  to be passed as arguments to `org-w3ctr--build-meta-entry'.  Any nil
  items are ignored.
. A function that takes the INFO plist as single argument and returns
  such a list of items."
  :group 'org-export-w3ctr
  :type '(choice
          (repeat
           (list (string :tag "Meta label")
                 (string :tag "label value")
                 (string :tag "Content value")))
          function))

(defcustom t-file-timestamp-function #'t-file-timestamp-default
  "Function to generate timestamp for exported files at top place.
This function should take INFO as the only argument and return a
string representing the timestamp.

Default value is `org-w3ctr-file-timestamp-default', which generates
timestamps in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-coding-system 'utf-8-unix
  "Coding system for HTML export."
  :group 'org-export-w3ctr
  :type 'coding-system)

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

(defcustom t-head-include-default-style t
  "Non-nil means include the default style in exported HTML files."
  :group 'org-export-w3ctr
  :type 'boolean)
;;;###autoload
(put 't-head-include-default-style 'safe-local-variable 'booleanp)

(defcustom t-default-style-file (file-name-concat
                                 t--dir "assets" "style.css")
  "Default CSS stylesheet file for HTML export.

This should be either nil (no default stylesheet) or an absolute
path to a CSS file.  When set to a path, if `:html-style' is non-nil
and `org-w3ctr-default-style' is an empty string, the CSS code will be
loaded from the specified file and set as the value of this option.

The default value points to \"assets/style.css\" relative to the
package's installation directory `org-w3ctr--dir'."
  :group 'org-export-w3ctr
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (unless (and (stringp value)
                      (file-exists-p value)
                      (file-name-absolute-p value))
           (error "Not a valid default CSS file: %s" value))
         (set symbol value)
         ;; Refresh cached CSS string.
         (setq t-default-style ""))
  :type '(choice (const nil) file))

(defcustom t-default-style ""
  "Default CSS style content for exported HTML documents.

When non-empty, this setting takes precedence over and will override
`org-w3ctr-default-style-file' behavior (external CSS file loading).

This string contains raw CSS rules that will be embedded in a <style>
tag in the exported HTML's <head> section. Example:
  \"body { font-family: sans-serif; margin: 2em; }\""
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-with-latex 'mathjax
  "Control how LaTeX math expressions are processed in HTML export.

When non-nil, enables processing of LaTeX math snippets.  The value
specifies the rendering method:
. `mathjax': Render math using MathJax (client-side)
. `mathml' : Convert to MathML markup using MathJax  (server-side)"
  :group 'org-export-w3ctr
  :type '(choice
          (const :tag "Disable math processing" nil)
          (const :tag "Use MathJax to display math" mathjax)
          (const :tag "Use MathJax to render mathML" mathml)))

(defcustom t-mathjax-config '("" . "")
  "Configuration for MathJax rendering in HTML export.

This cons cell contains two configuration strings:
. car: MathJax JavaScript configuration
. cdr: MathML fallback configuration

The first config will be used for standard MathJax rendering,
the second will be used when `org-w3ctr-with-latex' is set to `mathml'.

For detailed configuration options, see:
https://docs.mathjax.org/en/latest/options/index.html"
  :group 'org-export-w3ctr
  :type '(cons (string :tag "mathjax config")
               (string :tag "mathml  config")))

(defcustom t-pre/post-timestamp-format "%Y-%m-%d %H:%M"
  "Formatting string used for timestamps in preamble and postamble.
See `format-time-string' for more information on its components."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-creator-string
  (format "<a href=\"https://www.gnu.org/software/emacs/\">\
Emacs</a> %s (<a href=\"https://orgmode.org\">Org</a> mode %s)"
          emacs-version
          (if (fboundp 'org-version) (org-version)
            "unknown version"))
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

(defcustom t-preamble #'t-preamble-default-function
  "Non-nil means insert a preamble in HTML export.

When set to a string, use this formatted string.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

When set to a non-nil symbol and symbol's function cell is nil,
insert formatted symbol's value string.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

(defcustom t-postamble nil
  "Non-nil means insert a postamble in HTML export.

See `org-w3ctr-preamble' for more information."
  :group 'org-export-w3ctr
  :type '(choice string function symbol))

(defcustom t-public-license 'cc-by-sa-4.0
  "Default license for exported content. Value should be one of the
supported Creative Commons licenses or variants."
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

(defconst t-public-license-alist
  '((nil "Not Specified")
    (all-rights-reserved "All Rights Reserved")
    (all-rights-reversed "All Rights Reversed")
    (cc0 "CC0" "https://creativecommons.org/public-domain/cc0/")
    ;; 4.0
    ( cc-by-4.0 "CC BY-SA 4.0"
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
    ( cc-by-3.0 "CC BY-SA 3.0"
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

(defcustom t-link-home ""
  "Default value for :html-link-home in org export.

Used as fallback navigation link when :html-link-home/up is not
specified in document.  Should be a URL pointing to the home page."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-up ""
  "Default value for :html-link-up in org export.

Used as fallback navigation link when :html-link-home/up is not
specified in document.  Should be a URL pointing to the parent page."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-home/up-format
  "<div id=\"home-and-up\">\n <a href=\"%s\"> UP </a>
 <a href=\"%s\"> HOME </a>\n</div>"
  "Formatting string for legacy home/up navigation links.

Used when :html-link-home/up is not specified. The first %s is
replaced with the up link, the second with home link."
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-link-homeup nil
  "Default value for :html-link-home/up navigation links. Can be:
. A vector of (LINK . NAME) cons pairs for multiple links
. A list of Org elements (when set through #+HTML_LINK_HOMEUP)
. nil to fall back to legacy home/up behavior

Example: [(\"../index.html\" . \"UP\")
          (\"../../index.html\" . \"HOME\")]"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-format-home/up-function
  #'t-format-home/up-default-function
  "Function used to generate home/up navigation links."
  :group 'org-export-w3ctr
  :type 'symbol)

(defcustom t-format-headline-function
  #'t-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist).

The function result will be used in the section format string."
  :group 'org-export-w3ctr
  :type 'function)

(defcustom t-toc-list-tag 'ul
  "Ordered list or unordered list."
  :group 'org-export-w3ctr
  :type '(choice (const ol) (const ul)))

(defcustom t-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export."
;; See `org-html-toplevel-hlevel' for more information.
  :group 'org-export-w3ctr
  :type 'integer)

(defcustom t-language-string "zh-CN"
  "default HTML lang attribtue"
  :group 'org-export-w3ctr
  :type 'string)

(defcustom t-back-to-top t
  "add back-to-top arrow at the end of html file"
  :group 'org-export-w3ctr
  :type '(boolean))

(defcustom t-back-to-top-arrow
  "<p role=\"navigation\" id=\"back-to-top\">\
<a href=\"#title\"><abbr title=\"Back to Top\">↑\
</abbr></a></p>\n"
  "add comments here"
  :group 'org-export-w3ctr
  :type 'string)

(defvar t-fixup-js ""
  "js code that control toc's hide and show")

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

;;;; Some options added by include-yy
(defcustom t-use-babel nil
  "use babel or not when exporting.

This option will override `org-export-use-babel'"
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

(defcustom t-example-default-class "example"
  "default CSS class for example block, nil means no default class"
  :group 'org-export-w3ctr
  :type 'sexp)

(defcustom t-zeroth-section-tocname "Abstract"
  "default toc name of the zeroth section"
  :group 'org-export-w3ctr
  :type 'sexp)

;;;; LaTeX

;;; Internal Variables

(defconst t-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "noscript"
    "output" "progress" "section" "summary" "video")
  "Elements in html5.

For blocks that should contain headlines, use the HTML_CONTAINER
property on the headline itself.")

(defvar t--id-attr-prefix "ID-"
  "Prefix to use in ID attributes.
This affects IDs that are determined from the ID property.")

;; load default CSS from style.css
(defun t-update-css-js ()
  "update `t-default-style' and t-fixup-js"
  (interactive)
  (setq t-fixup-js
        (let ((fname (file-name-concat t--dir "assets/fixup.js")))
          (format "<script>\n%s\n</script>\n"
                  (with-temp-buffer
                    (insert-file-contents fname)
                    (buffer-string))))))
;; do update
(t-update-css-js)

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
  "Regular expression for matching UTC/GMT time zone designators
and time zone offsets. Also supports \"local\" for system timezone.

This expression does not allow for missing `+' or `-' signs,
and strictly validates both UTC/GMT and ±HHMM formats.")

(defcustom t-timezone "local"
  "Time zone string for W3C TR export.

Designator should be in either:
. ±HHMM format      (e.g., \"+0800\", \"-0500\")
. GMT/UTC±XX format (e.g., \"UTC+8\", \"GMT-5\")
. local

This value is used when generating datetime metadata.
Examples of valid values:
  \"+0800\"    ; Beijing time
  \"-0500\"    ; Eastern Time
  \"UTC+8\"    ; Alternative format
  \"GMT-5\"    ; Eastern Time alternative
  \"local\"    ; System local time

See ISO 8601 and RFC 2822 for timezone formatting standards."
  :group 'org-export-w3ctr
  :set (lambda (symbol value)
         (let ((case-fold-search t))
           (if (not (string-match-p t-timezone-regex value))
               (error "Not a valid time zone designator: %s" value)
             (set symbol value))))
  :type 'string)

(defcustom t-export-timezone nil
  "Time zone for export timestamps.

`org-w3ctr-timezone' specifies the timezone for timestamps when
writing the file, export timezone specifies the timezone used for
datetime attributes during export.

Accepted formats:
  nil        ; Same as specified timezone
  \"+0800\"    ; Beijing time
  \"-0500\"    ; Eastern Time
  \"UTC+8\"    ; Alternative format
  \"GMT-5\"    ; Eastern Time alternative
  \"local\"    ; System local time"
  :group 'org-export-w3ctr
  :set (lambda (symbol value)
         (when value
           (let ((case-fold-search t))
             (if (not (string-match-p t-timezone-regex value))
                 (error "Not a valid time zone designator: %s" value))))
         (set symbol value))
  :type 'string)

(defcustom t-timestamp-format '("%F" . "%F %R")
  "Format specification used for timestamps export.

Accepts A two-element list (DATE DATE-TIME) where:
  DATE: Format string for year/month/day (e.g. \"%Y-%m-%d\")
  DATE-TIME: DATE plus hours:minutes (e.g. \"%H:%M\")

See `format-time-string' for more information on its components."
  :group 'org-export-w3ctr
  :type '(cons
          (string :tag "Year, Month, Day ")
          (string :tag "Plus Hour, Minute")))

(defcustom t-datetime-format-choice 'T-none-zulu
  "Format choice used for datetime property export. This option
controls how timestamps are formatted, with variations in:

  Separator: Space (`\s') or `T' (ISO 8601 style)
  Timezone : Offset with (`+08:00') or without (`+0800') colon
  Timezone : Use Zulu (`Z') or not when timezone is UTC+0"
  :group 'org-export-w3ctr
  :type '(radio (const space-none) (const space-none-zulu)
                (const space-colon) (const space-colon-zulu)
                (const T-none) (const T-none-zulu)
                (const T-colon) (const T-colon-zulu)))

;;; Internal Functions
(defun t--make-string (n string)
  "Build a string by concatenating N times STRING."
  (let (out) (dotimes (_ n out) (setq out (concat string out)))))

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

(defsubst t--normalize-string (s)
  "Ensure string S ends with a single newline character.

If S isn't a string return it unchanged.  If S is the empty
string, return it.  Otherwise, return a new string with a single
newline character at its end."
  (cond
   ((not (stringp s)) s)
   ((string= "" s) "")
   (t (and (string-match "\\(\n[ \t]*\\)*\\'" s)
           (replace-match "\n" nil nil s)))))

(defsubst t--maybe-contents (contents)
  "If CONTENTS is non-nil, prepend a newline and return it;
otherwise, return an empty string."
  (if contents (concat "\n" contents) ""))

(defun t-fix-class-name (kwd)
  ;; audit callers of this function
  "Turn todo keyword KWD into a valid class name.
Replaces invalid characters with \"_\"."
  (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" kwd nil t))

(defun t--has-caption-p (element &optional _info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal' or
a value to `t-standalone-image-predicate'."
  (org-element-property :caption element))

;; FIXME: Consider remove it
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

;;; Simple JSON based sync RPC, not JSONRPC
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

;;; Greater elements (11 - 3 - 2 = 6).
;;; special-block and table are not here.

;;;; Center Block
;; See (info "(org)Paragraphs")
;; Fixed export. Not customizable.
(defun t-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block."
  (format "<div style=\"text-align:center;\">%s</div>"
          (t--maybe-contents contents)))

;;;; Drawer
;; See (info "(org)Drawers")
;; Fixed export. Not customizable.
(defun t-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block."
  (let* ((name (org-element-property :drawer-name drawer))
         (cap (if-let* ((cap (org-export-get-caption drawer))
                        (exp (t--nw-p (org-export-data cap info))))
                  exp name))
         (attrs (t--make-attr__id drawer info t)))
    (format "<details%s><summary>%s</summary>%s</details>"
            attrs cap (t--maybe-contents contents))))

;;;; Dynamic Block
;; See (info "(org)Dynamic Blocks")
;; Fixed export. Not customizable.
(defun t-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block."
  (or contents ""))

;;; Item and Plain Lists
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

;; See (info "(org)Checkboxes")
;; To modify checkbox style, set `org-w3ctr-checkbox-type'.
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
               (extra (if (not counter) ""
                        (format " value=\"%s\"" counter))))
          (concat
           (format "<li%s>" extra)
           (when headline (concat headline br)))))
       (`unordered
        ;; Ignore term-counter-id
        ;; To prevent it from being parsed, try add <wbr> after '['
        (concat "<li>" (when headline (concat headline br))))
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
       (`ordered "</li>") (`unordered "</li>")
       (`descriptive "</dd>")))))

;;;; Item
;; See (info "(org)Plain Lists")
;; Fixed export. Not customizable.
(defun t-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (t-format-list-item
     contents type checkbox info (or tag counter))))

;;;; Plain List
;; See (info "(org)Plain Lists")
;; Fixed export. Not customizable.
(defun t-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list."
  (let* ((type
          (pcase (org-element-property :type plain-list)
            (`ordered "ol") (`unordered "ul") (`descriptive "dl")
            (other (error "Unknown HTML list type: %s" other))))
         (attributes (t--make-attr__id plain-list info t)))
    (format "<%s%s>\n%s</%s>"
            type attributes contents type)))

;;;; Quote Block
;; See (info "(org)Paragraphs")
;; Fixed export. Not customizable.
(defun t-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block."
  (format "<blockquote%s>%s</blockquote>"
          (t--make-attr__id quote-block info t)
          (t--maybe-contents contents)))


;;; Lesser elements (17 - 7 - 3 = 7)
;;; latex-environment, src-block, and table-row are not here.

;;;; Example Block
;; See (info "(org)Literal Examples")
;; Fixed export. Not customizable.
(defun t-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil."
  (format "<div%s>\n<pre>\n%s</pre>\n</div>"
          (t--make-attr__id example-block info)
          (org-remove-indentation
           (org-element-property :value example-block))))

;;;; Export Block
;; See (info "(org) Quoting HTML tags")
;; Fixed export. Not customizable.
(defun t-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil."
  (let* ((type (org-element-property :type export-block))
         (value (org-element-property :value export-block))
         (text (org-remove-indentation value)))
    (pcase type
      ;; Add mhtml-mode also.
      ((or "HTML" "MHTML") text)
      ("CSS" (format "<style>%s</style>"
                     (t--maybe-contents value)))
      ((or "JS" "JAVASCRIPT")
       (concat "<script>\n" text "</script>"))
      ;; Expression that return HTML string.
      ((or "EMACS-LISP" "ELISP")
       (format "%s" (eval (read (or (t--nw-p value) "\"\"")))))
      ;; SEXP-style HTML data.
      ("LISP-DATA"
       (t--sexp2html (read (or (t--nw-p value) "\"\""))))
      (_ ""))))

;;;; Fixed Width
;; See (info "(org) Literal Examples")
;; Fixed export. Not customizable.
(defun t-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil."
  (format "<pre%s>%s</pre>"
          (t--make-attr__id fixed-width info t)
          (let ((value
                 (org-remove-indentation
                  (org-element-property
                   :value fixed-width))))
            (if (not (t--nw-p value)) value
              (concat "\n" value "\n")))))

;;;; Horizontal Rule
;; See (info "(org) Horizontal Rules")
;; Fixed export. Not customizable.
(defun t-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE object from Org to HTML.
CONTENTS is nil."
  "<hr>")

;;;; Keyword
;; See (info "(org) Quoting HTML tags")
;; Fixed export. Not customizable.
(defun t-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (pcase key
      ((or "H" "HTML") value)
      ("E" (format "%s" (eval (read (or (t--nw-p value) "\"\"")))))
      ("D" (t--sexp2html (read (or (t--nw-p value) "\"\""))))
      ("L" (mapconcat #'t--sexp2html
                      (read (format "(%s)" value))))
      (_ ""))))

;;;; Paragraph
;; See (info "(org)Paragraphs")
;; Fixed export. Not customizable.
(defun t-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string."
  (let* ((parent (org-export-get-parent paragraph))
         (parent-type (org-element-type parent))
         (attrs (t--make-attr__id paragraph info t)))
    (cond
     (;; Item's first line.
      (and (eq parent-type 'item)
           (not (org-export-get-previous-element paragraph info)))
      (if (string= attrs "") contents
        (format "<span%s>%s</span>" attrs contents)))
     (;; Standalone image.
      (t-standalone-image-p paragraph info)
      (let* ((caption (org-export-get-caption paragraph))
             (cap (or (and caption (org-export-data caption info)) "")))
        (t--wrap-image contents info cap attrs)))
     ;; Regular paragraph.
     (t (let ((c (t--trim contents)))
          (if (string= c "") ""
            (format "<p%s>%s</p>" attrs c)))))))

(defun t--wrap-image (contents _info caption attrs)
  "Wrap CONTENTS string within <figure> tag for images.
Also check attributes and caption of paragraph."
  (format "<figure%s>\n%s%s</figure>"
          ;; Attributes and contents.
          attrs contents
          ;; Caption.
          (if (not (org-string-nw-p caption)) ""
            (format "<figcaption>%s</figcaption>\n" caption))))

(defun t-paragraph-filter (value _backend _info)
  "Delete paragraph's trailing newlines."
  (concat (string-trim-right value) "\n"))

;;;; Verse Block
;; See (info "(org)Paragraphs")
;; Fixed export. Not customizable.
(defun t-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents."
  (format
   "<p%s>\n%s</p>"
   (t--make-attr__id verse-block info t)
   ;; Replace leading white spaces with non-breaking spaces.
   (replace-regexp-in-string
    "^[ \t]+" (lambda (m) (t--make-string (length m) "&#xa0;"))
    ;; Replace each newline character with line break. Also
    ;; remove any trailing "br" close-tag so as to avoid
    ;; duplicates.
    (let* ((re (format "\\(?:%s\\)?[ \t]*\n"
                       (regexp-quote "<br>"))))
      (replace-regexp-in-string
       re "<br>\n" (or contents ""))))))


;;; Objects (25 - 4 - 5 - 7 = 9)
;;; footnote-reference, inline-src-block are not here.
;;; latex-fragment, link and table-cell are not here.
;;; timestamp is not here.
;;; smallest objects are not here.

;;;; Entity
;; See (info "(org)Special Symbols")
;; Fixed export. Not customizable.
(defun t-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to HTML."
  (org-element-property :html entity))

;;;; Export Snippet
;; See (info "(org)Quoting HTML tags")
;; Fixed export. Not customizable.
(defun t-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML."
  (let* ((backend (org-export-snippet-backend export-snippet))
         (value (org-element-property :value export-snippet)))
    (pcase backend
      ;; plain html text.
      ((or 'h 'html) value)
      ;; Read, Evaluate, Print, no Loop :p
      ('e (format "%s" (eval (read (or (t--nw-p value) "\"\"")))))
      ;; sexp-style html data.
      ('d (t--sexp2html (read (or (t--nw-p value) "\"\""))))
      ;; sexp-style html data list.
      ('l (mapconcat #'t--sexp2html
                     (read (format "(%s)" value))))
      (_ ""))))

;;;; Line Break
;; See (info "(org)Paragraphs")
;; Fixed export. Not customizable.
(defun t-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to HTML."
  "<br>\n")

;;;; Target
;; FIXME: Add test after imporve t--reference
;; See (info "(org)Internal Links")
;; Fixed export. Not customizable.
(defun t-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((ref (t--reference target info)))
    (t--anchor ref nil nil info)))

;;;; Radio Target
;; FIXME: Add test after imporve t--reference
;; See (info "(org)Radio Targets")
;; Fixed export. Not customizable.
(defun t-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML."
  (let ((ref (t--reference radio-target info)))
    (t--anchor ref text nil info)))

;;;; Statistics Cookie
;; See (info "(org)Checkboxes")
;; Fixed export. Not customizable.
(defun t-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML."
  (let ((cookie-value
         (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))

;;;; Subscript
;; See (info "(org)Subscripts and Superscripts")
;; Fixed export. Not customizable.
(defun t-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML."
  (format "<sub>%s</sub>" contents))

;;;; Superscript
;; See (info "(org)Subscripts and Superscripts")
;; Fixed export. Not customizable.
(defun t-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML."
  (format "<sup>%s</sup>" contents))


;;; Smallest objects (7)

(defun t--get-markup-format (name info)
  "Get markup format string for NAME from INFO plist.
Returns \"%s\" if not found.

NAME is a symbol (like \\='bold), INFO is Org export info plist."
  (if-let* ((alist (plist-get info :html-text-markup-alist))
            (str (cdr (assq name alist))))
      str "%s"))

;;;; Bold
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-bold (_bold contents info)
  "Transcode BOLD from Org to HTML."
  (format (t--get-markup-format 'bold info) contents))

;;;; Italic
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-italic (_italic contents info)
  "Transcode ITALIC from Org to HTML."
  (format (t--get-markup-format 'italic info) contents))

;;;; Underline
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-underline (_underline contents info)
  "Transcode UNDERLINE from Org to HTML."
  (format (t--get-markup-format 'underline info) contents))

;;;; Verbatim
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-verbatim (verbatim _contents info)
  "Transcode VERBATIM from Org to HTML."
  (format (t--get-markup-format 'verbatim info)
          (t-encode-plain-text
           (org-element-property :value verbatim))))

;;;; Code
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-code (code _contents info)
  "Transcode CODE from Org to HTML."
  (format (t--get-markup-format 'code info)
          (t-encode-plain-text
           (org-element-property :value code))))

;;;; Strike-Through
;; See (info "(org) Emphasis and Monospace")
;; Change `org-w3ctr-text-markup-alist' to do customizations.
(defun t-strike-through (_strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML."
  (format (t--get-markup-format 'strike-through info) contents))

;;;; Plain Text
;; Fixed export. Not customizable.
(defconst t-special-string-regexps
  '(("\\\\-" . "&#x00ad;"); shy
    ("---\\([^-]\\)" . "&#x2014;\\1"); mdash
    ("--\\([^-]\\)" . "&#x2013;\\1"); ndash
    ("\\.\\.\\." . "&#x2026;")); hellip
  "Regular expressions for special string conversion.")

(defconst t-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by
`org-w3ctr-encode-plain-text'.")

(defun t-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (dolist (a t-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string
                    re rpl string t)))))

(defun t-encode-plain-text (text)
  "Convert plain text characters from TEXT to HTML equivalent.
Possible conversions are set in `org-w3ctr-protect-char-alist'."
  (dolist (pair t-protect-char-alist text)
    (setq text (replace-regexp-in-string
                (car pair) (cdr pair) text t t))))

(defun t-plain-text (text info)
  "Transcode a TEXT string from Org to HTML."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (t-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original
    ;; string since OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes
                    output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (t-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
            (replace-regexp-in-string
             "\\(\\\\\\\\\\)?[ \t]*\n"
             "<br>\n" output)))
    ;; Return value.
    output))

;;; Meta tags and <head> contents
(defun t-meta-tags-default (info)
  "A default value for `org-w3ctr-meta-tags'.

Generate a list items, each of which is a list of arguments
that can be passed to `org-w3ctr--build-meta-entry', to generate meta
tags to be included in the HTML head.

Use document's INFO to derive relevant information for the tags."
  (let ((author
         (and-let* (((plist-get info :with-author))
                    (auth (plist-get info :author)))
           ;; Return raw Org syntax.
           (org-element-interpret-data auth))))
    (thread-last
      (list
       (when (t--nw-p author)
         (list "name" "author" author))
       (when-let* ((desc (t--nw-p (plist-get info :description))))
         (list "name" "description" (t--trim desc)))
       (when-let* ((keyw (t--nw-p (plist-get info :keywords))))
         (list "name" "keywords" (t--trim keyw)))
       '("name" "generator" "Org Mode"))
      (remove nil))))

(defun t--build-meta-entry ( label identity
                             &optional content-format
                             &rest content-formatters)
  "Build a meta tag using the provided information.

Construct <meta> tag of form <meta LABEL=\"IDENTITY\">,
or when CONTENT-FORMAT is present:
<meta LABEL=\"IDENTITY\" content=\"{content}\">

Here {content} is determined by applying any CONTENT-FORMATTERS
to the CONTENT-FORMAT and encoding the result as plain text."
  (concat "<meta "
          (format "%s=\"%s" label identity)
          (when content-format
            (concat "\" content=\""
                    (replace-regexp-in-string
                     "\"" "&quot;"
                     (t-encode-plain-text
                      (if content-formatters
                          (apply #'format content-format
                                 content-formatters)
                        content-format)))))
          "\">\n"))

(defun t--get-info-title (info)
  "Extract title from INFO plist and return as plain text.

If title exists, is non-whitespace, and can be converted to plain text,
return the text. Otherwise return a left-to-right mark (invisible)."
  (if-let* ((title (plist-get info :title))
            (data (org-element-interpret-data title))
            ((t--nw-p data))
            (text (t-plain-text data info)))
      ;; Set title to an invisible character instead of
      ;; leaving it empty, which is invalid.
      text "&lrm;"))

(defun t-file-timestamp-default (_info)
  "Return current timestamp in ISO 8601 format (YYYY-MM-DDThh:mmZ)."
  (format-time-string "%FT%RZ" nil t))

(defun t--get-info-file-timestamp (info)
  "Get file timestamp from INFO plist using :html-file-timestamp.

If the function exists and is valid, call it with INFO as argument.
Otherwise, signal an error."
  (if-let* ((fun (plist-get info :html-file-timestamp))
            ((functionp fun)))
      (funcall fun info)
    (error ":html-file-timestamp's value is not a valid function!")))

(defun t--build-meta-info (info)
  "Return meta tags for exported document."
  (let* ((title (t--get-info-title info))
         (charset
          (if-let* ((coding t-coding-system)
                    (name (coding-system-get coding 'mime-charset)))
              (symbol-name name) "utf-8")))
    (concat
     ;; See `org-export-timestamp-file' or `org-export-time-stamp-file'
     (when (plist-get info :time-stamp-file)
       (format "<!-- %s -->\n" (t--get-info-file-timestamp info)))
     (t--build-meta-entry "charset" charset)
     (when-let*
         ((viewport-options
           (cl-remove-if-not (lambda (cell) (t--nw-p (cadr cell)))
                             (plist-get info :html-viewport))))
       (t--build-meta-entry
        "name" "viewport"
        (mapconcat (lambda (elm) (format "%s=%s" (car elm) (cadr elm)))
                   viewport-options ", ")))
     (format "<title>%s</title>\n" title)
     (mapconcat
      (lambda (args) (apply #'t--build-meta-entry args))
      (delq nil (if (functionp t-meta-tags) (funcall t-meta-tags info)
                  t-meta-tags))))))

(defun t--load-css (_info)
  "Load CSS content for HTML export from configured sources.

This function handles CSS loading in the following priority:
. If `org-w3ctr-default-style' is non-empty string, use it directly
. If `org-w3ctr-default-style-file' is non-nil, load CSS from that file
. If both are empty/nil, return empty string (no styles)

The loaded CSS will be wrapped in HTML <style> tags when non-empty."
  (let ((style t-default-style)
        (file t-default-style-file)
        it)
    (unless (stringp style)
      (error "Default CSS is not string"))
    (cond
     ((not (string= "" style)) (setq it style))
     ((null file) (setq it nil))
     (t (setq it (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-substring-no-properties
                    (point-min) (point-max)))
              t-default-style it)))
    (if (null it) ""
      (format "<style>\n%s\n</style>\n" it))))

(defun t--build-head (info)
  "Return information for the <head>..</head> of the HTML output."
  (t--normalize-string
   (concat
    (t--normalize-string (plist-get info :html-head))
    (t--normalize-string (plist-get info :html-head-extra))
    (when (plist-get info :html-head-include-default-style)
      (t--normalize-string (t--load-css info))))))

(defun t--build-mathjax-config (info)
  "Insert the user setup into the mathjax template."
  (let ((type (plist-get info :with-latex))
        (config (plist-get info :html-mathjax-config)))
    (unless (and (stringp (car config)) (stringp (cdr config)))
      (error "Not a valid mathjax config: %s" config))
    (pcase type
      (`nil "")
      (`mathjax (if (not (t--nw-p (car config)))
                    (org-html--build-mathjax-config info)
                  (t--normalize-string (car config))))
      (`mathml (if (not (t--nw-p (cdr config))) ""
                 (t--normalize-string (cdr config))))
      (other (error "Not a valid mathjax option: %s" other)))))

;;; Preamble and Postamble

;; Compared with org-html-format-spec, rename to make the name more
;; specific, and add some helpful docstring.
(defun t--pre/postamble-format-spec (info)
  "Return format specification for preamble and postamble.

%t means produce title.
%s means produce subtitle.
%d means produce (start)date.
%T means produce current time formatted with pre/postamble format.
%a means produce author.
%e means produce mailto link.
%c means produce creator string.
%C means produce file modification time (if exists).
%v means produce W3C HTML validation link."
  (let ((fmt (plist-get info :html-pre/post-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info fmt) info))
      (?T . ,(format-time-string fmt))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
              (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
              (split-string (plist-get info :email)  ",+ *")
              ", "))
      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
               (format-time-string
                fmt (and file (file-attribute-modification-time
                               (file-attributes file))))))
      (?v . ,(or (plist-get info :html-validation-link) "")))))

;; Modified preamble/postamble handling compared to ox-html:
;; . Removed org-html-pre/postamble-format mechanism; values are now
;;   set directly through org-html-pre/postamble
;; . Dropped the 'auto option for postamble; when value is a symbol:
;;   * Calls the symbol if it's a function
;;   * Otherwise formats the symbol's string value if present
(defun t--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or empty string.
TYPE is either `preamble' or `postamble'"
  (let ((section (plist-get info (intern (format ":html-%s" type))))
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
        (error "pre/postamble symbol's value must be string")))
     ;; not nil, string or symbol
     (t (error "pre/postamble's value is invalid: %s" section)))
    (or (and (t--nw-p it) (t--normalize-string it)) "")))

(defun t--get-info-date (info &optional boundary)
  "Extract date from INFO plist and format as timestamp. Returns
formatted timestamp string or nil if no valid timestamp found.

When BOUNDARY is non-nil, adjust timestamp to boundary (start/end)."
  (when-let* ((date (plist-get info :date))
              ((and date (proper-list-p date) (null (cdr date))))
              ((org-element-type-p (car date) 'timestamp)))
    (t-timestamp (car date) nil info boundary)))

(defvar t--cc-svg-hashtable (make-hash-table :test 'equal)
  "Hash table storing url-encoded svg file contents.

Include cc, by, sa, nc, nd")

(defun t--load-cc-svg (name)
  "Load SVG file with given NAME from assets directory, return as
base64 encoded string. If the file does not exist, raise an error."
  (let ((file (file-name-concat t--dir "assets" (concat name ".svg"))))
    (if (not (file-exists-p file))
        (error "svg file %s not exists" file)
      (with-work-buffer
        (insert-file-contents file)
        (base64-encode-region (point-min) (point-max) t)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun t--load-cc-svg-once (name)
  "Load SVG file with given NAME once and cache it in a hash table.
If the SVG is already cached, return the cached base64 string."
  (if-let* ((str (gethash name t--cc-svg-hashtable)))
      str (setf (gethash name t--cc-svg-hashtable)
                (t--load-cc-svg name))))

(defun t--build-cc-img (base64)
  "Create HTML img tag with embedded BASE64 encoded SVG.

The image has fixed height (22px) and vertical alignment for text
integration, which is the default style given by
https://chooser-beta.creativecommons.org/"
  (format "<img style=\"height:22px!important;margin-left:3px;\
vertical-align:text-bottom;\" src=\"data:image/svg+xml;base64,%s\" \
alt=\"\">" base64))

(defun t--get-cc-svgs (license)
  "Get HTML img tags for Creative Commons LICENSE icons.

For CC0 license, returns both `cc' and `zero' icons. For other licenses,
splits the license name to get individual component icons."
  (let ((names (if (eq license 'cc0) '("cc" "zero")
                 (split-string (symbol-name license) "[0-9.-]" t))))
    (mapconcat
     (lambda (name) (t--build-cc-img (t--load-cc-svg-once name)))
     names)))

(defun t--build-public-license (info)
  "Generate HTML string describing the public license for a work.

Extracts license information from INFO plist and formats it with author
attribution and appropriate Creative Commons icons when applicable."
  (let* ((license (plist-get info :html-license))
         (details (assq license t-public-license-alist))
         (is-cc (string-match-p "^cc" (symbol-name license)))
         (author (plist-get info :author)))
    (unless details
      (error "Not a known license name: %s" license))
    (cond
     ((eq license 'all-rights-reserved) (nth 1 details))
     ((eq license 'all-rights-reversed) (nth 1 details))
     (t
      (let* ((name (nth 1 details))
             (link (nth 2 details)))
        (concat
         "This work"
         (when-let* ((a author)
                     (str (org-export-data a info))
                     ((t--nw-p str)))
           (concat " by " (t--trim str)))
         " is licensed under "
         (if (null link) name
           (format "<a href=\"%s\">%s</a>" link name))
         (when is-cc (concat " " (t--get-cc-svgs license)))))))))

(defun t-preamble-default-function (info)
  "Generate HTML preamble with document metadata in a <details> section.

Includes creation time, publication time, last update time, creator
information, and license details. Times are formatted according to INFO
settings."
  (concat
   "<details open>\n"
   " <summary>More details about this document</summary>\n"
   " <dl>\n"
   "  <dt>Create Time:</dt> <dd>"
   (or (t--get-info-date info 'start) "[DATE Not Specified]")
   "</dd>\n"
   "  <dt>Publish Time:</dt> <dd>"
   (or (t--get-info-date info 'end) "[DATE Not Specified]")
   "</dd>\n"
   "  <dt>Update Time:</dt> <dd>"
   (format "<time datetime=\"%s\">%s</time>"
           (t--format-normalized-timestamp (current-time) info)
           (format-time-string "%F %R"))
   "</dd>\n"
   "  <dt>Creator:</dt> <dd>"
   (plist-get info :creator)
   "</dd>\n"
   "  <dt>License:</dt> <dd>"
   (t--build-public-license info)
   "</dd>\n"
   " </dl>\n"
   "</details>\n"
   "<hr>"))

(defvar t-preamble-default-template "\
<details open>
 <summary>More details about this document</summary>
 <dl>
  <dt>Date:</dt> <dd>%d</dd>
  <dt>Creator:</dt> <dd>%c</dd>
  <dt>License:</dt> <dd>This work is licensed under \
<a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">\
CC BY-SA 4.0</a></dd>
 </dl>
</details>
<hr>"
  "Default HTML template for document preamble metadata section.

Note: This variable is provided as an example only and may need
adaptation for actual project use.")

;;; Home and up

(defun t-legacy-format-home/up (info)
  "Format legacy-style home/up navigation links from export INFO.

Generates HTML navigation links using either :html-link-up or
:html-link-home from the INFO plist, falling back to each other when
empty. Returns nil if both links are empty strings."
  (let ((link-up (t--trim (plist-get info :html-link-up)))
        (link-home (t--trim (plist-get info :html-link-home))))
    (unless (and (string= link-up "")
                 (string= link-home ""))
      (format (plist-get info :html-home/up-format)
              (or link-up link-home)
              (or link-home link-up)))))

(defun t-format-home/up-default-function (info)
  "Generate HTML navigation links from the export INFO plist. This
function processes the :html-link-home/up property to create a
navigation section in the exported document.

When :html-link-home/up is a vector, it should contain cons cells in
the form (URL . LABEL) where URL is the target location and LABEL is
the display text.

When :html-link-home/up is a list, it is treated as containing Org
link elements. These links will be processed through `org-export-data'
to generate the final HTML output.

The output is always wrapped in a <nav> HTML element with
id=\"home-and-up\" for consistent styling and semantic markup.
Each link is separated by newlines for readability in the output HTML."
  (let* ((links (plist-get info :html-link-home/up)))
    (pcase links
      ((pred vectorp)
       (concat "<nav id=\"home-and-up\">\n"
               (thread-first
                 (pcase-lambda (`(,link . ,name))
                   (format "<a href=\"%s\">%s</a>" link name))
                 (mapconcat links "\n"))
               "\n</nav>\n"))
      ((pred listp)
       (let ((<a>s
              (thread-last
                (mapcar (lambda (x) (org-export-data x info)) links)
                (cl-remove-if-not #'t--nw-p)
                (funcall (lambda (x) (mapconcat #'identity x "\n"))))))
         (if (string= <a>s "")
             (or (t-legacy-format-home/up info) nil)
           (concat
            "<nav id=\"home-and-up\">\n" <a>s
            "\n</nav>\n"))))
      (_ (error "Seems not a valid home/up value: %s" links)))))

;;; Tables of Contents
(defun t-format-headline-default-function
    (todo _todo-type priority text tags info)
  "Default format function for a headline.
See `org-w3ctr-format-headline-function' for details and the
description of TODO, PRIORITY, TEXT, TAGS, and INFO arguments."
  (let ((todo (t--todo todo info))
        (priority (t--priority priority info))
        (tags (t--tags tags info)))
    (concat todo (and todo " ")
            priority (and priority " ")
            text
            (and tags "&#xa0;&#xa0;&#xa0;") tags)))

(defun t--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number
          (org-export-get-headline-number headline info))
         (todo (when-let* (((plist-get info :with-todo-keywords))
                           (todo (org-element-property
                                  :todo-keyword headline)))
                 (org-export-data todo info)))
         (todo-type (and todo (org-element-property
                               :todo-type headline)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         (text (org-export-data-with-backend
                (org-export-get-alt-title headline info)
                (org-export-toc-entry-backend 'w3ctr)
                info))
         (tags (and (eq (plist-get info :with-tags) t)
                    (org-export-get-tags headline info))))
    (format "<a href=\"#%s\">%s</a>"
            ;; Label.
            (t--reference headline info)
            ;; Body.
            (concat
             (and
              (not (org-export-low-level-p headline info))
              (org-export-numbered-headline-p headline info)
              (when headline-number
                (format "<span class=\"secno\">%s</span> "
                        (mapconcat #'number-to-string
                                   headline-number "."))))
             (format
              "<span class=\"content\">%s</span>"
              (funcall (plist-get info :html-format-headline-function)
                       todo todo-type priority text tags info))))))

(defun t--toc-text (toc-entries info)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
         (start-level prev-level)
         (tag (or (plist-get info :html-toc-tagname) 'ul))
         (open (if (eq tag 'ol) "\n<ol class=\"toc\">\n<li>"
                 "\n<ul class=\"toc\">\n<li>"))
         (close (if (eq tag 'ol) "</li>\n</ol>\n" "</li>\n</ul>\n")))
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
               times (cond ((> cnt 0) open)
                           ((< cnt 0) close)))
              (if (> cnt 0) open "</li>\n<li>")))
           headline)))
      toc-entries "")
     (t--make-string (- prev-level start-level) close))))

(defun t-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar
          (lambda (h) (cons (t--format-toc-headline h info)
                        (org-export-get-relative-level h info)))
          (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let* ((toc (t--toc-text toc-entries info)))
        (if scope
            (format "<div role=\"doc-toc\">\n%s</div>" toc)
          (concat
           "<nav id=\"toc\">\n"
           (let ((top-level (plist-get info :html-toplevel-hlevel)))
             (format "<h%d id=\"contents\">%s</h%d>"
                     top-level "Table of Contents" top-level))
           toc
           "</nav>\n"))))))

(defvar t--zeroth-section-output nil
  "Internal variable storing zeroth section's HTML output.

This is used to override the default ox-html behavior where TOC comes
first, allowing zeroth section's content to appear before the TOC while
the TOC remains near the beginning of the document.")

(defun t-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string."
  ;; See also `org-html-inner-template'
  (concat
   (prog1 t--zeroth-section-output
     (setq t--zeroth-section-output nil))
   (when-let* ((depth (plist-get info :with-toc)))
     (t-toc depth info))
   "<main>\n"
   contents
   "</main>\n"
   (t-footnote-section info)))

(defun t--build-title (info)
  (when (plist-get info :with-title)
    (let ((title (plist-get info :title))
          (subtitle (plist-get info :subtitle)))
      (concat
       "<h1 id=\"title\">"
       (let ((tit (org-export-data title info)))
         (or (t--nw-p tit)  "&lrm;"))
       "</h1>\n"
       (let ((sub (org-export-data subtitle info)))
         (format "<p id=\"w3c-state\">%s</p>\n" sub))))))

(defun t-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n"
           (plist-get info :language))
   "<head>\n"
   (t--build-meta-info info)
   (t--build-head info)
   (t--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   ;; home and up links
   (when-let* ((fun (plist-get
                     info :html-format-home/up-function)))
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
   (t--nw-p (plist-get info :html-fixup-js))
   ;; Closing document.
   "</body>\n</html>"))

;;; Headline

;;;; Anchor
(defun t--anchor (id desc attributes _info)
  "Format a HTML anchor."
  (let* ((attributes (concat (and id (format " id=\"%s\"" id))
                             attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

;;;; Todo
(defun t--todo (todo info)
  "Format TODO keywords into HTML."
  (when todo
    (format
     "<span class=\"%s %s%s\">%s</span>"
     (if (member todo org-done-keywords) "done" "todo")
     (or (plist-get info :html-todo-kwd-class-prefix) "")
     (t-fix-class-name todo)
     todo)))

;;;; Priority
(defun t--priority (priority _info)
  "Format a priority into HTML.
PRIORITY is the character code of the priority or nil."
  (and priority
       (format
        "<span class=\"priority\">[%c]</span>"
        priority)))

;;;; Tags
(defun t--tags (tags info)
  "Format TAGS into HTML.
INFO is a plist containing export options."
  (when tags
    (format "<span class=\"tag\">%s</span>"
            (mapconcat
             (lambda (tag)
               (format "<span class=\"%s\">%s</span>"
                       (concat
                        (plist-get info :html-tag-class-prefix)
                        (org-html-fix-class-name tag))
                       tag))
             tags "&#xa0;"))))

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

;;;; Section

(defun t-section (section contents _info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent)
        ;; the zeroth section
        (prog1 ""
          (setq t--zeroth-section-output
                (format "<div id=\"abstract\">\n%s</div>\n"
                        (or contents ""))))
      (or contents ""))))

;;;; Special Block
;; FIXME
;; See (info "(org)HTML doctypes")
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

;;; LATEX utilties.
(defun t--mathml-to-oneline (xml)
  "Convert a MathML XML structure into a single-line string.

If XML is a string and empty, return an empty string;
otherwise, recursively process the XML structure, converting
it into a single-line formatted string.

MathJax includes the original LaTeX code in the `data-latex'
attribute of the generated tags. Here, we remove them.

According to MathML Spec:
`xmlns=http://www.w3.org/1998/Math/MathML' may be used on the
math element; it will be ignored by the HTML parser."
  (if (stringp xml) (or (and (t--nw-p xml) (t--trim xml)) "")
    (let* ((tag (symbol-name (car xml)))
           (exclude-regex
            (rx (or "xmlns" "data-latex")))
           (props
            (thread-first
              (lambda (x)
                (let ((name (symbol-name (car x))))
                  (cond
                   ((and (string= name "display")
                         (string= (cdr x) "inline"))
                    "")
                   ((string-match-p exclude-regex name) "")
                   (t (concat " " name "=\"" (cdr x) "\"")))))
              (mapconcat (cadr xml))))
           (childs (mapconcat
                    #'t--mathml-to-oneline (cddr xml))))
      (format "<%s%s>%s</%s>"
              tag props childs tag))))

(defun t--reformat-mathml (str)
  "Reformat the given MathML STR into a one-line XML string.

In the MathML returned by MathJax, there are some attribute
values that are not particularly useful for browser rendering
and need to be removed."
  (with-work-buffer
    (insert str) (goto-char (point-min))
    (let ((xml (xml-parse-tag)))
      (t--mathml-to-oneline xml))))

;; FIXME: Test needed.
(defun t--normalize-latex (frag)
  "Normalize LaTeX fragments in the given string FRAG.

This function processes LaTeX fragments and environments in the
input string, converting inline and block LaTeX ($.$ and $$.$$)
to \\(.\\) and \\\\=[.\\\\=].

The code for this function is from `org-format-latex'."
  (let* ((math-regexp
          "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
    (org-export-with-buffer-copy
     :to-buffer (get-buffer-create " *Org HTML Export LaTeX*")
     :drop-visibility t :drop-narrowing t :drop-contents t
     (erase-buffer)
     (insert frag)
     (goto-char (point-min))
     (while (re-search-forward math-regexp nil t)
       (let* ((context (org-element-context))
              (type (org-element-type context)))
         (when (memq type '(latex-environment latex-fragment))
           (let ((value (org-element-property :value context))
                 (beg (org-element-begin context))
                 (end (save-excursion
                        (goto-char (org-element-end context))
                        (skip-chars-backward " \r\t\n")
                        (point))))
             (if (not (string-match "\\`\\$\\$?" value))
                 (goto-char end)
               (delete-region beg end)
               (if (string= (match-string 0 value) "$$")
                   (insert "\\[" (substring value 2 -2) "\\]")
                 (insert "\\(" (substring value 1 -1) "\\)")))))))
     (t--trim (buffer-string)))))

(defun t-format-latex (frag type _info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
TYPE designates the tool used for conversion.  It can
be `mathjax', `mathml' or `nil'(do nothing)."
  (if (null type) frag
    (let ((new-frag (t--normalize-latex frag)))
      (pcase type
        (`mathjax new-frag)
        ;; FIXME: Check if rpc server is available
        (`mathml (t--jstools-call 'tex2mml frag))
        (_ (error "Unknown Latex export type: %s" type))))))

;;;; Latex Fragment
(defun t-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML."
  (let* ((frag (org-element-property :value latex-fragment))
         (type (plist-get info :with-latex))
         (result (t-format-latex frag type info)))
    (if (eq type 'mathml) (t--reformat-mathml result)
      result)))

;;;; Latex Environment
;; FIXME: Consider #+name and #+attr_*, and something else.
(defun t-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML."
  (let* ((type (plist-get info :with-latex))
         (frag (org-remove-indentation
                (org-element-property :value latex-environment)))
         (result (t-format-latex frag type info)))
    (if (eq type 'mathml) (t--reformat-mathml result)
      result)))

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


;;;; Src Block
;; FIXME
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

;;;; Inline Src Block
;; FIXME
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

;;;; Timestamp
;; See (info "(org)Timestamps")
;; `org-w3ctr-timezone' - timezone for displayed timestamps
;; `org-w3ctr-export-timezone' - timezone used for datetime attribute

;; For example, when `org-w3ctr-timezone' is "+0800", the time
;; [2025-04-03 9:08] corresponds to UTC 2025-04-03 1:08. If
;; `org-w3ctr-export-timezone' is set to "+0900", the exported
;; <time> tag's `datetime' attribute will be like 2025-04-03 10:08.

;; When `org-w3ctr-timezone' is set to "local", both the exported
;; timestamps and datetime attributes will use local time, and
;; `org-w3ctr-export-timezone' will have no effect. When
;; `org-w3ctr-export-timezone' is nil, it means using the same
;; timezone as `org-w3ctr-timezone'.

(defun t--timezone-to-offset (zone-str)
  "Convert ZONE-STR timezone string to offset in seconds.
Valid formats are UTC/GMT±XX (e.g., UTC+8), ±HHMM (e.g., -0500) or
local, which means use system timezone.
Throws error if ZONE-STR doesn't match `org-w3ctr-timezone-regex'."
  (let ((case-fold-search t)
        (zone-str (t--trim zone-str)))
    (if (not (string-match t-timezone-regex zone-str))
        (error "Time zone format not correct: %s" zone-str)
      (if (string= zone-str "local")
          (or (car (current-time-zone)) 0)
        (let* ((time-str (or (match-string 1 zone-str)
                             (match-string 2 zone-str)))
               (len (length time-str))
               (number (string-to-number time-str)))
          (cond
           ;; UTC/GMT[+-]xx
           ((<= 2 len 3) (* number 3600))
           ;; [+-]MMMM
           ((= len 5)
            (let ((hour (/ number 100))
                  (minute (% number 100)))
              (+ (* hour 3600) (* minute 60))))))))))

(defsubst t--timestamp-option-to-tokens (option)
  "Convert `:html-datetime-option' to triplet [T colon zulu]"
  (pcase option
    ('space-none [" " "" "+0000"])
    ('space-none-zulu [" " "" "Z"])
    ('space-colon [" " ":" "+0000"])
    ('space-colon-zulu [" " ":" "Z"])
    ('T-none ["T" "" "+0000"])
    ('T-none-zulu ["T" "" "Z"])
    ('T-colon ["T" ":" "+0000"])
    ('T-colon-zulu ["T" ":" "Z"])
    (other (error "Unrecognized timestamp option: %s" other))))

(defun t--normalize-timezone-offset (offset tokens)
  "Convert OFFSET in seconds to standardized timezone string.
Returns a string in ±HHMM format (e.g. \"+0800\", \"-0500\").

OFFSET is the timezone offset in seconds (e.g. 28800 for UTC+8).
Zero offset returns \"+0000\" or \"Z\" (if OPTION allow Zulu)"
  (if (= offset 0) (aref tokens 2)
    (let* ((hours (/ (abs offset) 3600))
           (minutes (/ (- (abs offset) (* hours 3600)) 60)))
      (concat
       (if (>= offset 0) "+" "-")
       (when (< hours 10) "0")
       (number-to-string hours)
       (aref tokens 1)
       (when (< minutes 10) "0")
       (number-to-string minutes)))))

(defsubst t--get-info-timezone-tokens (info)
  (if-let* ((option (plist-get info :html-datetime-option)))
      (t--timestamp-option-to-tokens option)
    (error "Datetime option not found!")))

(defun t--get-info-timezone-offset (info)
  "Return timezone offset in seconds from INFO plist.
When `:html-timezone' is \"local\", just return \"local\".

The `:html-timezone' property can be either a number
(offset in seconds) or a string (timezone format like \"UTC+8\");
in the latter case it will be converted to seconds and cached in the
plist to avoid recomputation."
  (let ((zone (plist-get info :html-timezone)))
    (cond
     ((numberp zone) zone)
     ((string= zone "local") "local")
     (t (let ((time (t--timezone-to-offset zone)))
          (setf (plist-get info :html-timezone) time))))))

(defun t--get-info-export-timezone-offset (info)
  "Return export timezone offset in seconds from INFO plist.

The export timezone is determined by:
  If `:html-export-timezone' is nil, use `:html-timezone' value
  If `:html-timezone' is \"local\", always use \"local\"
  Otherwise use `:html-export-timezone' value"
  (let ((zone1 (t--get-info-timezone-offset info))
        (zone2 (plist-get info :html-export-timezone)))
    (cond
     ((not zone2) zone1)
     ((equal zone1 "local") "local")
     ((numberp zone2) zone2)
     (t (let ((time (t--timezone-to-offset zone2)))
          (setf (plist-get info :html-export-timezone) time))))))

(defun t--get-info-normalized-timezone (info)
  "Return normalized timezone string from INFO plist.

The timezone string format depends on `:html-datetime-option' in INFO:
- \"±HHMM\" (e.g. \"+0800\", \"-0500\")
- \"±HH:MM\" (e.g. \"+08:00\", \"-05:00\")
- \"Z\" for UTC (when option includes `zulu')
- \"\" for local time"
  (let ((zone (t--get-info-export-timezone-offset info))
        (tokens (t--get-info-timezone-tokens info)))
    (if (equal zone "local") ""
      (t--normalize-timezone-offset zone tokens))))

(defun t--get-timezone-delta (info)
  (let ((offset1 (t--get-info-timezone-offset info))
        (offset2 (t--get-info-export-timezone-offset info)))
    (cond
     ((equal offset1 "local") 0)
     ((equal offset2 "local") 0)
     ((equal offset1 offset2) 0)
     (t (- offset2 offset1)))))

(defun t--format-normalized-timestamp (time info)
  "Format TIME into a timestamp string with normalized timezone.
TIME is the time value to format; FMT is the format string for
`format-time-string'; INFO is a plist containing timezone information."
  (let* ((tokens (t--get-info-timezone-tokens info))
         (fmt (concat "%F" (aref tokens 0) "%R"))
         (delta (t--get-timezone-delta info))
         (zone-suffix (t--get-info-normalized-timezone info)))
    (concat (format-time-string fmt (time-add time delta))
            zone-suffix)))

(defun t--get-timestamp-format (type has-time info)
  "Format timestamp according to Org-mode conventions.
This is a wrapper around `org-time-stamp-format' that provides
customizable formatting."
  (let* ((w3c-formats (plist-get info :html-timestamp-format))
         (org-timestamp-formats w3c-formats))
    (org-time-stamp-format
     has-time (memq type '(inactive inactive-range))
     org-display-custom-times)))

(defun t-timestamp (timestamp _contents info &optional boundary)
  "Transcode a TIMESTAMP object from Org to HTML."
  (let ((type (org-element-property :type timestamp)))
    (if (eq type 'diary)
        (format "<time>%s</time>"
                (org-element-interpret-data timestamp))
      (let* ((has-time (org-timestamp-has-time-p timestamp))
             (fmt (t--get-timestamp-format type has-time info))
             (is-end (eq boundary 'end)))
        (pcase type
          ((or `active `inactive (guard boundary))
           (let* ((t0 (org-timestamp-to-time timestamp is-end)))
             (format "<time datetime=\"%s\">%s</time>"
                     (if has-time
                         (t--format-normalized-timestamp t0 info)
                       (format-time-string "%Y-%m-%d" t0))
                     (org-format-timestamp timestamp fmt is-end))))
          ((or `active-range `inactive-range)
           (let* ((t1 (org-timestamp-to-time timestamp))
                  (t2 (org-timestamp-to-time timestamp t)))
             (concat
              (format "<time datetime=\"%s\">%s</time>"
                      (if has-time
                          (t--format-normalized-timestamp t1 info)
                        (format-time-string "%Y-%m-%d" t1))
                      (org-format-timestamp timestamp fmt))
              "&#x2013;"
              (format "<time datetime=\"%s\">%s</time>"
                      (if has-time
                          (t--format-normalized-timestamp t2 info)
                        (format-time-string "%Y-%m-%d" t2))
                      (org-format-timestamp timestamp fmt t)))))
          (_ (error "Not a valid time type %s" type)))))))

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
;; no-native-compile: t
;; indent-tabs-mode: nil
;; End:

;;; ox-w3ctr.el ends here
