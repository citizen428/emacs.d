;;; htmlize.el --- Convert buffer text and decorations to HTML.

;; Copyright (C) 1997-2003,2005,2006,2009,2011 Hrvoje Niksic

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: hypermedia, extensions
;; Version: 1.39

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package converts the buffer text and the associated
;; decorations to HTML.  Mail to <hniksic@xemacs.org> to discuss
;; features and additions.  All suggestions are more than welcome.

;; To use it, just switch to the buffer you want HTML-ized and type
;; `M-x htmlize-buffer'.  You will be switched to a new buffer that
;; contains the resulting HTML code.  You can edit and inspect this
;; buffer, or you can just save it with C-x C-w.  `M-x htmlize-file'
;; will find a file, fontify it, and save the HTML version in
;; FILE.html, without any additional intervention.  `M-x
;; htmlize-many-files' allows you to htmlize any number of files in
;; the same manner.  `M-x htmlize-many-files-dired' does the same for
;; files marked in a dired buffer.

;; htmlize supports three types of HTML output, selected by setting
;; `htmlize-output-type': `css', `inline-css', and `font'.  In `css'
;; mode, htmlize uses cascading style sheets to specify colors; it
;; generates classes that correspond to Emacs faces and uses <span
;; class=FACE>...</span> to color parts of text.  In this mode, the
;; produced HTML is valid under the 4.01 strict DTD, as confirmed by
;; the W3C validator.  `inline-css' is like `css', except the CSS is
;; put directly in the STYLE attribute of the SPAN element, making it
;; possible to paste the generated HTML to other documents.  In `font'
;; mode, htmlize uses <font color="...">...</font> to colorize HTML,
;; which is not standard-compliant, but works better in older
;; browsers.  `css' mode is the default.

;; You can also use htmlize from your Emacs Lisp code.  When called
;; non-interactively, `htmlize-buffer' and `htmlize-region' will
;; return the resulting HTML buffer, but will not change current
;; buffer or move the point.

;; htmlize aims for compatibility with Emacsen 21 and later.  Please
;; let me know if it doesn't work on the version of XEmacs or GNU
;; Emacs that you are using.  The package relies on the presence of CL
;; extensions, especially for cross-emacs compatibility; please don't
;; try to remove that dependency.  Yes, I know I require `cl' at
;; runtime, and I prefer it that way.  When byte-compiling under GNU
;; Emacs, you're likely to get a few warnings; just ignore them.

;; The latest version is available as a git repository at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.git>
;;
;; The snapshot of the latest release can be obtained at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi>
;;
;; You can find a sample of htmlize's output (possibly generated with
;; an older version) at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.html>

;; Thanks go to the many people who have sent reports and contributed
;; comments, suggestions, and fixes.  They include Ron Gut, Bob
;; Weiner, Toni Drabik, Peter Breton, Thomas Vogels, Juri Linkov,
;; Maciek Pasternacki, and many others.

;; User quotes: "You sir, are a sick, sick, _sick_ person. :)"
;;                  -- Bill Perry, author of Emacs/W3


;;; Code:

(require 'cl)
(eval-when-compile
  (if (string-match "XEmacs" emacs-version)
      (byte-compiler-options
	(warnings (- unresolved))))
  (defvar font-lock-auto-fontify)
  (defvar font-lock-support-mode)
  (defvar global-font-lock-mode))

(defconst htmlize-version "1.39")

(defgroup htmlize nil
  "Convert buffer text and faces to HTML."
  :group 'hypermedia)

(defcustom htmlize-head-tags ""
  "*Additional tags to insert within HEAD of the generated document."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-output-type 'css
  "*Output type of generated HTML, one of `css', `inline-css', or `font'.
When set to `css' (the default), htmlize will generate a style sheet
with description of faces, and use it in the HTML document, specifying
the faces in the actual text with <span class=\"FACE\">.

When set to `inline-css', the style will be generated as above, but
placed directly in the STYLE attribute of the span ELEMENT: <span
style=\"STYLE\">.  This makes it easier to paste the resulting HTML to
other documents.

When set to `font', the properties will be set using layout tags
<font>, <b>, <i>, <u>, and <strike>.

`css' output is normally preferred, but `font' is still useful for
supporting old, pre-CSS browsers, and both `inline-css' and `font' for
easier embedding of colorized text in foreign HTML documents (no style
sheet to carry around)."
  :type '(choice (const css) (const inline-css) (const font))
  :group 'htmlize)

(defcustom htmlize-generate-hyperlinks t
  "*Non-nil means generate the hyperlinks for URLs and mail addresses.
This is on by default; set it to nil if you don't want htmlize to
insert hyperlinks in the resulting HTML.  (In which case you can still
do your own hyperlinkification from htmlize-after-hook.)"
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-hyperlink-style "
      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
"
  "*The CSS style used for hyperlinks when in CSS mode."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-replace-form-feeds t
  "*Non-nil means replace form feeds in source code with HTML separators.
Form feeds are the ^L characters at line beginnings that are sometimes
used to separate sections of source code.  If this variable is set to
`t', form feed characters are replaced with the <hr> separator.  If this
is a string, it specifies the replacement to use.  Note that <pre> is
temporarily closed before the separator is inserted, so the default
replacement is effectively \"</pre><hr /><pre>\".  If you specify
another replacement, don't forget to close and reopen the <pre> if you
want the output to remain valid HTML.

If you need more elaborate processing, set this to nil and use
htmlize-after-hook."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-charset nil
  "*The charset declared by the resulting HTML documents.
When non-nil, causes htmlize to insert the following in the HEAD section
of the generated HTML:

  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=CHARSET\">

where CHARSET is the value you've set for htmlize-html-charset.  Valid
charsets are defined by MIME and include strings like \"iso-8859-1\",
\"iso-8859-15\", \"utf-8\", etc.

If you are using non-Latin-1 charsets, you might need to set this for
your documents to render correctly.  Also, the W3C validator requires
submitted HTML documents to declare a charset.  So if you care about
validation, you can use this to prevent the validator from bitching.

Needless to say, if you set this, you should actually make sure that
the buffer is in the encoding you're claiming it is in.  (Under Mule
that is done by ensuring the correct \"file coding system\" for the
buffer.)  If you don't understand what that means, this option is
probably not for you."
  :type '(choice (const :tag "Unset" nil)
		 string)
  :group 'htmlize)

(defcustom htmlize-convert-nonascii-to-entities (featurep 'mule)
  "*Whether non-ASCII characters should be converted to HTML entities.

When this is non-nil, characters with codes in the 128-255 range will be
considered Latin 1 and rewritten as \"&#CODE;\".  Characters with codes
above 255 will be converted to \"&#UCS;\", where UCS denotes the Unicode
code point of the character.  If the code point cannot be determined,
the character will be copied unchanged, as would be the case if the
option were nil.

When the option is nil, the non-ASCII characters are copied to HTML
without modification.  In that case, the web server and/or the browser
must be set to understand the encoding that was used when saving the
buffer.  (You might also want to specify it by setting
`htmlize-html-charset'.)

Note that in an HTML entity \"&#CODE;\", CODE is always a UCS code point,
which has nothing to do with the charset the page is in.  For example,
\"&#169;\" *always* refers to the copyright symbol, regardless of charset
specified by the META tag or the charset sent by the HTTP server.  In
other words, \"&#169;\" is exactly equivalent to \"&copy;\".

By default, entity conversion is turned on for Mule-enabled Emacsen and
turned off otherwise.  This is because Mule knows the charset of
non-ASCII characters in the buffer.  A non-Mule Emacs cannot tell
whether a character with code 0xA9 represents Latin 1 copyright symbol,
Latin 2 \"S with caron\", or something else altogether.  Setting this to
t without Mule means asserting that 128-255 characters always mean Latin
1.

For most people htmlize will work fine with this option left at the
default setting; don't change it unless you know what you're doing."
  :type 'sexp
  :group 'htmlize)

(defcustom htmlize-ignore-face-size 'absolute
  "*Whether face size should be ignored when generating HTML.
If this is nil, face sizes are used.  If set to t, sizes are ignored
If set to `absolute', only absolute size specifications are ignored.
Please note that font sizes only work with CSS-based output types."
  :type '(choice (const :tag "Don't ignore" nil)
		 (const :tag "Ignore all" t)
		 (const :tag "Ignore absolute" absolute))
  :group 'htmlize)

(defcustom htmlize-css-name-prefix ""
  "*The prefix used for CSS names.
The CSS names that htmlize generates from face names are often too
generic for CSS files; for example, `font-lock-type-face' is transformed
to `type'.  Use this variable to add a prefix to the generated names.
The string \"htmlize-\" is an example of a reasonable prefix."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-use-rgb-txt t
  "*Whether `rgb.txt' should be used to convert color names to RGB.

This conversion means determining, for instance, that the color
\"IndianRed\" corresponds to the (205, 92, 92) RGB triple.  `rgb.txt'
is the X color database that maps hundreds of color names to such RGB
triples.  When this variable is non-nil, `htmlize' uses `rgb.txt' to
look up color names.

If this variable is nil, htmlize queries Emacs for RGB components of
colors using `color-instance-rgb-components' and `x-color-values'.
This can yield incorrect results on non-true-color displays.

If the `rgb.txt' file is not found (which will be the case if you're
running Emacs on non-X11 systems), this option is ignored."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-major-mode nil
  "The mode the newly created HTML buffer will be put in.
Set this to nil if you prefer the default (fundamental) mode."
  :type '(radio (const :tag "No mode (fundamental)" nil)
		 (function-item html-mode)
		 (function :tag "User-defined major mode"))
  :group 'htmlize)

(defvar htmlize-before-hook nil
  "Hook run before htmlizing a buffer.
The hook functions are run in the source buffer (not the resulting HTML
buffer).")

(defvar htmlize-after-hook nil
  "Hook run after htmlizing a buffer.
Unlike `htmlize-before-hook', these functions are run in the generated
HTML buffer.  You may use them to modify the outlook of the final HTML
output.")

(defvar htmlize-file-hook nil
  "Hook run by `htmlize-file' after htmlizing a file, but before saving it.")

(defvar htmlize-buffer-places)

;;; Some cross-Emacs compatibility.

;; I try to conditionalize on features rather than Emacs version, but
;; in some cases checking against the version *is* necessary.
(defconst htmlize-running-xemacs (string-match "XEmacs" emacs-version))

;; We need a function that efficiently finds the next change of a
;; property regardless of whether the change occurred because of a
;; text property or an extent/overlay.
(cond
 (htmlize-running-xemacs
  (defun htmlize-next-change (pos prop &optional limit)
    (if prop
        (next-single-property-change pos prop nil (or limit (point-max)))
      (next-property-change pos nil (or limit (point-max)))))
  (defun htmlize-next-face-change (pos &optional limit)
    (htmlize-next-change pos 'face limit)))
 ((fboundp 'next-single-char-property-change)
  ;; GNU Emacs 21+
  (defun htmlize-next-change (pos prop &optional limit)
    (if prop
        (next-single-char-property-change pos prop nil limit)
      (next-char-property-change pos limit)))
  (defun htmlize-overlay-faces-at (pos)
    (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))
  (defun htmlize-next-face-change (pos &optional limit)
    ;; (htmlize-next-change pos 'face limit) would skip over entire
    ;; overlays that specify the `face' property, even when they
    ;; contain smaller text properties that also specify `face'.
    ;; Emacs display engine merges those faces, and so must we.
    (or limit
        (setq limit (point-max)))
    (let ((next-prop (next-single-property-change pos 'face nil limit))
          (overlay-faces (htmlize-overlay-faces-at pos)))
      (while (progn
               (setq pos (next-overlay-change pos))
               (and (< pos next-prop)
                    (equal overlay-faces (htmlize-overlay-faces-at pos)))))
      (min pos next-prop))))
 (t
  (error "htmlize requires next-single-property-change or \
next-single-char-property-change")))

;;; Transformation of buffer text: HTML escapes, untabification, etc.

(defvar htmlize-basic-character-table
  ;; Map characters in the 0-127 range to either one-character strings
  ;; or to numeric entities.
  (let ((table (make-vector 128 ?\0)))
    ;; Map characters in the 32-126 range to themselves, others to
    ;; &#CODE entities;
    (dotimes (i 128)
      (setf (aref table i) (if (and (>= i 32) (<= i 126))
			       (char-to-string i)
			     (format "&#%d;" i))))
    ;; Set exceptions manually.
    (setf
     ;; Don't escape newline, carriage return, and TAB.
     (aref table ?\n) "\n"
     (aref table ?\r) "\r"
     (aref table ?\t) "\t"
     ;; Escape &, <, and >.
     (aref table ?&) "&amp;"
     (aref table ?<) "&lt;"
     (aref table ?>) "&gt;"
     ;; Not escaping '"' buys us a measurable speedup.  It's only
     ;; necessary to quote it for strings used in attribute values,
     ;; which htmlize doesn't do.
     ;(aref table ?\") "&quot;"
     )
    table))

;; A cache of HTML representation of non-ASCII characters.  Depending
;; on the setting of `htmlize-convert-nonascii-to-entities', this maps
;; non-ASCII characters to either "&#<code>;" or "<char>" (mapconcat's
;; mapper must always return strings).  It's only filled as characters
;; are encountered, so that in a buffer with e.g. French text, it will
;; only ever contain French accented characters as keys.  It's cleared
;; on each entry to htmlize-buffer-1 to allow modifications of
;; `htmlize-convert-nonascii-to-entities' to take effect.
(defvar htmlize-extended-character-cache (make-hash-table :test 'eq))

(defun htmlize-protect-string (string)
  "HTML-protect string, escaping HTML metacharacters and I18N chars."
  ;; Only protecting strings that actually contain unsafe or non-ASCII
  ;; chars removes a lot of unnecessary funcalls and consing.
  (if (not (string-match "[^\r\n\t -%'-;=?-~]" string))
      string
    (mapconcat (lambda (char)
		 (cond
		  ((< char 128)
		   ;; ASCII: use htmlize-basic-character-table.
		   (aref htmlize-basic-character-table char))
		  ((gethash char htmlize-extended-character-cache)
		   ;; We've already seen this char; return the cached
		   ;; string.
		   )
		  ((not htmlize-convert-nonascii-to-entities)
		   ;; If conversion to entities is not desired, always
		   ;; copy the char literally.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))
		  ((< char 256)
		   ;; Latin 1: no need to call encode-char.
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" char)))
		  ((encode-char char 'ucs)
                   ;; Must check if encode-char works for CHAR;
                   ;; it fails for Arabic and possibly elsewhere.
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" (encode-char char 'ucs))))
		  (t
		   ;; encode-char doesn't work for this char.  Copy it
		   ;; unchanged and hope for the best.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))))
	       string "")))

(defconst htmlize-ellipsis "...")
(put-text-property 0 (length htmlize-ellipsis) 'htmlize-ellipsis t htmlize-ellipsis)

(defun htmlize-match-inv-spec (inv)
  (member* inv buffer-invisibility-spec
           :key (lambda (i)
                  (if (symbolp i) i (car i)))))

(defun htmlize-decode-invisibility-spec (invisible)
  ;; Return t, nil, or `ellipsis', depending on how invisible text should be inserted.

  (if (not (listp buffer-invisibility-spec))
      ;; If buffer-invisibility-spec is not a list, then all
      ;; characters with non-nil `invisible' property are visible.
      (not invisible)

    ;; Otherwise, the value of a non-nil `invisible' property can be:
    ;; 1. a symbol -- make the text invisible if it matches
    ;;    buffer-invisibility-spec.
    ;; 2. a list of symbols -- make the text invisible if
    ;;    any symbol in the list matches
    ;;    buffer-invisibility-spec.
    ;; If the match of buffer-invisibility-spec has a non-nil
    ;; CDR, replace the invisible text with an ellipsis.
    (let ((match (if (symbolp invisible)
                     (htmlize-match-inv-spec invisible)
                   (some #'htmlize-match-inv-spec invisible))))
      (cond ((null match) t)
            ((cdr-safe (car match)) 'ellipsis)
            (t nil)))))

(defun htmlize-buffer-substring-no-invisible (beg end)
  ;; Like buffer-substring-no-properties, but don't copy invisible
  ;; parts of the region.  Where buffer-substring-no-properties
  ;; mandates an ellipsis to be shown, htmlize-ellipsis is inserted.
  (let ((pos beg)
	visible-list invisible show last-show next-change)
    ;; Iterate over the changes in the `invisible' property and filter
    ;; out the portions where it's non-nil, i.e. where the text is
    ;; invisible.
    (while (< pos end)
      (setq invisible (get-char-property pos 'invisible)
	    next-change (htmlize-next-change pos 'invisible end)
            show (htmlize-decode-invisibility-spec invisible))
      (cond ((eq show t)
	     (push (buffer-substring-no-properties pos next-change) visible-list))
            ((and (eq show 'ellipsis)
                  (not (eq last-show 'ellipsis))
                  ;; Conflate successive ellipses.
                  (push htmlize-ellipsis visible-list))))
      (setq pos next-change last-show show))
    (if (= (length visible-list) 1)
	;; If VISIBLE-LIST consists of only one element, return it and
	;; avoid creating a new string.
	(car visible-list)
      (apply #'concat (nreverse visible-list)))))

(defun htmlize-trim-ellipsis (text)
  ;; Remove htmlize-ellipses ("...") from the beginning of TEXT if it
  ;; starts with it.  It checks for the special property of the
  ;; ellipsis so it doesn't work on ordinary text that begins with
  ;; "...".
  (if (get-text-property 0 'htmlize-ellipsis text)
      (substring text (length htmlize-ellipsis))
    text))

(defconst htmlize-tab-spaces
  ;; A table of strings with spaces.  (aref htmlize-tab-spaces 5) is
  ;; like (make-string 5 ?\ ), except it doesn't cons.
  (let ((v (make-vector 32 nil)))
    (dotimes (i (length v))
      (setf (aref v i) (make-string i ?\ )))
    v))

(defun htmlize-untabify (text start-column)
  "Untabify TEXT, assuming it starts at START-COLUMN."
  (let ((column start-column)
	(last-match 0)
	(chunk-start 0)
	chunks match-pos tab-size)
    (while (string-match "[\t\n]" text last-match)
      (setq match-pos (match-beginning 0))
      (cond ((eq (aref text match-pos) ?\t)
	     ;; Encountered a tab: create a chunk of text followed by
	     ;; the expanded tab.
	     (push (substring text chunk-start match-pos) chunks)
	     ;; Increase COLUMN by the length of the text we've
	     ;; skipped since last tab or newline.  (Encountering
	     ;; newline resets it.)
	     (incf column (- match-pos last-match))
	     ;; Calculate tab size based on tab-width and COLUMN.
	     (setq tab-size (- tab-width (% column tab-width)))
	     ;; Expand the tab.
	     (push (aref htmlize-tab-spaces tab-size) chunks)
	     (incf column tab-size)
	     (setq chunk-start (1+ match-pos)))
	    (t
	     ;; Reset COLUMN at beginning of line.
	     (setq column 0)))
      (setq last-match (1+ match-pos)))
    ;; If no chunks have been allocated, it means there have been no
    ;; tabs to expand.  Return TEXT unmodified.
    (if (null chunks)
	text
      (when (< chunk-start (length text))
	;; Push the remaining chunk.
	(push (substring text chunk-start) chunks))
      ;; Generate the output from the available chunks.
      (apply #'concat (nreverse chunks)))))

(defun htmlize-extract-text (beg end trailing-ellipsis)
  ;; Extract buffer text, sans the invisible parts.  Then
  ;; untabify it and escape the HTML metacharacters.
  (let ((text (htmlize-buffer-substring-no-invisible beg end)))
    (when trailing-ellipsis
      (setq text (htmlize-trim-ellipsis text)))
    ;; If TEXT ends up empty, don't change trailing-ellipsis.
    (when (> (length text) 0)
      (setq trailing-ellipsis
            (get-text-property (1- (length text))
                               'htmlize-ellipsis text)))
    (setq text (htmlize-untabify text (current-column)))
    (setq text (htmlize-protect-string text))
    (values text trailing-ellipsis)))

(defun htmlize-despam-address (string)
  "Replace every occurrence of '@' in STRING with &#64;.
`htmlize-make-hyperlinks' uses this to spam-protect mailto links
without modifying their meaning."
  ;; Suggested by Ville Skytta.
  (while (string-match "@" string)
    (setq string (replace-match "&#64;" nil t string)))
  string)

(defun htmlize-make-hyperlinks ()
  "Make hyperlinks in HTML."
  ;; Function originally submitted by Ville Skytta.  Rewritten by
  ;; Hrvoje Niksic, then modified by Ville Skytta and Hrvoje Niksic.
  (goto-char (point-min))
  (while (re-search-forward
	  "&lt;\\(\\(mailto:\\)?\\([-=+_.a-zA-Z0-9]+@[-_.a-zA-Z0-9]+\\)\\)&gt;"
	  nil t)
    (let ((address (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"mailto:"
	      (htmlize-despam-address address)
	      "\">"
	      (htmlize-despam-address link-text)
	      "</a>&gt;")))
  (goto-char (point-min))
  (while (re-search-forward "&lt;\\(\\(URL:\\)?\\([a-zA-Z]+://[^;]+\\)\\)&gt;"
			    nil t)
    (let ((url (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"" url "\">" link-text "</a>&gt;"))))

;; Tests for htmlize-make-hyperlinks:

;; <mailto:hniksic@xemacs.org>
;; <http://fly.srk.fer.hr>
;; <URL:http://www.xemacs.org>
;; <http://www.mail-archive.com/bbdb-info@xemacs.org/>
;; <hniksic@xemacs.org>
;; <xalan-dev-sc.10148567319.hacuhiucknfgmpfnjcpg-john=doe.com@xml.apache.org>

(defun htmlize-defang-local-variables ()
  ;; Juri Linkov reports that an HTML-ized "Local variables" can lead
  ;; visiting the HTML to fail with "Local variables list is not
  ;; properly terminated".  He suggested chang