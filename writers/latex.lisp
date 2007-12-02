;;;; Misc string and vector handling for docutils
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; $Id: html.lisp,v 1.4 2006/07/07 18:43:20 willijar Exp $

(in-package :docutils.writer.latex)


(defpackage :docutils.writer.latex
  (:documentation "Latex writer for docutils")
  (:use :cl :docutils  :docutils.utilities  :docutils.nodes :ppcre)
  (:shadowing-import-from :cl #:warning #:error #:inline #:special)
  (:import-from :jarw.lib #:when-bind #:is-prefix-p)
  (:import-from :split-sequence #:split-sequence)
  (:export #:latex-writer))

(defparameter latex-named-character-map
  '((:|dagger| . "\\dag")
    (:|Dagger| . "\\ddag")
    (:|sect| . "\\S")
    (:|para| . "\\P")
    (:|pilcrow| ."\\P")
    (:|spades| . "\\spadesuit")
    (:|hearts| ."\\heartsuit")
    (:|diams| . "\\diamondsuit")
    (:|clubs| . "\\clubsuit")))


(docutils:register-settings-spec
 `((:latex-documentclass
    string "article"
    "Specify documentclass.")
   (:latex-document-options
    string "10pt,a4paper"
    "Specify document options.  Multiple options can be given, separated by commas.")
   (:use-latex-footnotes
    boolean nil
    "Use LaTeX footnotes. LaTeX supports only numbered footnotes (does it?). Default: no, uses figures.")
   (:footnote-references
    (member :type symbol :set (:superscript :brackets)) :brackets
    "Format for footnote references: one of :superscript or :brackets.")
   (:use-latex-citations
    boolean nil
    "Default: no, uses figures which might get mixed with images.")
   (:attribution
    (member :type (symbol :nil-allowed t) :set (:dash :parentheses nil)) :dash
    "Format for block quote attributions: one of 'dash',
'parentheses' or 'nil'")
   (:latex-stylesheet
    pathname nil
    "Specify a stylesheet file. The file will be \"input\" by latex in '
     the document header.  Default is no stylesheet")
   (:stylesheet-path
    pathname nil
   "Specify a stylesheet file, relative to the current working directory.  Overrides --stylesheet.")
   (:use-latex-toc
    boolean nil
    "Table of contents by docutils (default) or latex. Latex (writer) supports only one ToC per document, but docutils does not write  pagenumbers.")
   (:use-latex-docinfo
    boolean nil
    "Let LaTeX print author and date, do not show it in docutils
     document info.")
   (:hyperlink-colour
    string "blue"
    "Color of any hyperlinks embedded in text")
   (:compound-enumerators
    boolean nil
    "Enable compound enumerators for nested enumerated lists ")
   (:section-prefix-for-enumerators
    boolean nil
    "Enable section (\".\" subsection ...) prefixes for compound enumerators.  This has no effect without :compound-enumerators.")
   (:section-enumerator-separator
    string "-"
    "Set the separator between section number and enumerator for compound enumerated lists.")
   (:use-verbatim-when-possible
    boolean nil
    "When possibile, use verbatim for literal-blocks. Default is to always use the mbox environment.")
   (:table-style
    (member :type symbol :set (:standard :booktabs :nolines)) :standard
    "Table style. \"standard\" with horizontal and vertical lines, \"booktabs\" (LaTeX booktabs style) only horizontal lines above and below the table and below the header or \"nolines\".")
   (:graphicx-option
    (member :type (symbol :nil-allowed t) :set (:dvips :pdftex :auto nil)) nil
    "LaTeX graphicx package option. Possible values are \"dvips\", \"pdftex\". \"auto\" includes LaTeX code to use \"pdftex\" if processing with pdf(la)tex and dvips otherwise.")
   (:latex-font-encoding
    string "ae"
    "Possible values are \"T1\", \"OT1\", \"\" or some other fontenc option. The font encoding influences available symbols, e.g. \"<<\" as one character. Default is "" which leads to package \"ae\" (a T1 emulation using CM fonts).")))

(defclass table()
  ((latex-type :initform "longtable" :initarg :latex-type)
   (table-style :initform "booktabs" :initarg :table-style)
   (open :initform nil :reader open-p)
   (caption :initform nil)
   (attrs :initform nil)
   (col-width :initform nil)
   col-specs in-head
   (rowspan :initform nil))
  (:documentation " Manage a table while traversing.
        Maybe change to a mixin defining the visit/departs, but then
        class Table internal variables are in the Translator."))
(defun set-slots(entity slots values)
  (map nil #'(lambda(s v) (setf (slot-value entity s) v))))
(defun open-table(table)
  (set-slots table
             '(open col-specs caption attrs in-head)
             '(t  nil      nil     nil   nil)))
(defun close-table(table)
  (set-slots table
             '(open colspecs caption attrs)
             '(nil nil nil nil)))
(defun used-packages(table)
  (if (equal (slot-value table 'table-style) "booktabs")
      "\\usepackage{booktabs}"
      ""))

(defclass babel
    ((language :reader language :initarg :lang)
     (iso639-to-babel
      :allocation :class
      :initform
      '(("no" . "norsk")     ;;XXX added by hand ( forget about nynorsk?)
        ("gd" . "scottish")  ;;XXX added by hand
        ("hu" . "magyar")    ;;XXX added by hand
        ("pt" . "portuguese");;XXX added by hand
        ("sl" . "slovenian")
        ("af" . "afrikaans")
        ("bg" . "bulgarian")
        ("br" . "breton")
        ("ca" . "catalan")
        ("cs" . "czech")
        ("cy" . "welsh")
        ("da" . "danish")
        ("fr" . "french")
        ;; french francais canadien acadian
        ("de" . "ngerman")  ;;XXX rather than german
        ;; ngerman naustrian german germanb austrian
        ("el" . "greek")
        ("en" . "english")
        ;; english USenglish american UKenglish british canadian
        ("eo" . "esperanto")
        ("es" . "spanish")
        ("et" . "estonian")
        ("eu" . "basque")
        ("fi" . "finnish")
        ("ga" . "irish")
        ("gl" . "galician")
        ("he" . "hebrew")
        ("hr" . "croatian")
        ("hu" . "hungarian")
        ("is" . "icelandic")
        ("it" . "italian")
        ("la" . "latin")
        ("nl" . "dutch")
        ("pl" . "polish")
        ("pt" . "portuguese")
        ("ro" . "romanian")
        ("ru" . "russian")
        ("sk" . "slovak")
        ("sr" . "serbian")
        ("sv" . "swedish")
        ("tr" . "turkish")
        ("uk" . "ukrainian")))
     (double-quote-replacement :initform nil)
     (quotes :initform #("``" "''"))
     (quote-index :initform 0))
  (:documentaton "Language specifics for LaTeX."))

(defmethod initialize-instance :after ((babel babel) &key &allow-other-keys)
  (when (is-prefix-p "de" (language babel))
    (setf (slot-value babel 'quotes) #("{\\glqq}" "{\\grqq}")
          (slot-value babel 'double-quote-replacement) "{\\dq}")))

(defun next-quote(babel)
  (with-slots(quotes quote-index) babel
    (let ((q (aref quotes quote-index)))
      (setf quote-index (mod (1+ quote-index) 2))
      q)))

(defun quote-quotes(babel text)
  (with-output-to-string(os)
    (let ((parts (split-sequence #\" text)))
      (write-string (car parts) os)
      (dolist(part (cdr parts))
        (write-string (next-quote babel) os)
        (write-string part os)))))

(defun double-quotes-in-tt(babel text)
  (if (slot-value babel 'double-quote-replacement)
      (regex-replace-all "\"" text (slot-value babel 'double-quote-replacement))
      text))

(defun get-language(babel)
  (let* ((p (position #\_ (language babel))))
    (cdr (assoc (if p (subseq (language babel) 0 p) (language babel))
         (slot-value babel 'iso639-to-babel) :test #'string=))))

(defun to-latex-encoding(encoding)
  "Translate docutils encoding name into latex's.
Default fallback method is remove \"-\" and \"_\" chars from docutils_encoding."
  (let ((enc (cdr (assoc encoding
                         '(("iso-8859-1" . "latin1")     ;; west european
                           ("iso-8859-2" . "latin2")     ;; east european
                           ("iso-8859-3" . "latin3")     ;; esperanto) maltese
                           ("iso-8859-4" . "latin4")     ;; north european)scandinavian) baltic
                           ("iso-8859-5" . "iso88595")   ;; cyrillic (ISO)
                           ("iso-8859-9" . "latin5")     ;; turkish
                           ("iso-8859-15" . "latin9")    ;; latin9) update to latin1.
                           ("mac_cyrillic" . "maccyr")   ;; cyrillic (on Mac)
                           ("windows-1251" . "cp1251")   ;; cyrillic (on Windows)
                           ("koi8-r" . "koi8-r")         ;; cyrillic (Russian)
                           ("koi8-u" . "koi8-u")         ;; cyrillic (Ukrainian)
                           ("windows-1250" . "cp1250")   ;;
                           ("windows-1252" . "cp1252")   ;;
                           ("us-ascii" . "ascii")))
                  :test #'string-equal)))
    (or enc (string-downcase (regexp-replace-all "[-_]" encoding "")))))



(defclass latex-writer(writer)
  ((documentclass :reader documentclass :initform "article" :initarg documentclass)
   (optionlist-environment
    :initform
    "\\newcommand{\\optionlistlabel}[1]{\\bf #1 \\hfill}
\\newenvironment{optionlist}[1]
{\\begin{list}{}
{\\setlength{\\labelwidth}{#1}
\\setlength{\\rightmargin}{1cm}
\\setlength{\\leftmargin}{\\rightmargin}
\\addtolength{\\leftmargin}{\\labelwidth}
\\addtolength{\\leftmargin}{\\labelsep}
\\renewcommand{\\makelabel}{\\optionlistlabel}}
}{\\end{list}}")
   (lineblock-environment
    :initform
    "\\newlength{\\lineblockindentation}
\\setlength{\\lineblockindentation}{2.5em}
\\newenvironment{lineblock}[1]
{\\begin{list}{}
{\\setlength{\\partopsep}{\\parskip}
\\addtolength{\\partopsep}{\\baselineskip}
\\topsep0pt\\itemsep0.15\\baselineskip\\parsep0pt
\\leftmargin#1}
\\raggedright}
{\\end{list}}")
   (footnote-floats
    :initform
    "% begin: floats for footnotes tweaking.,
\\setlength{\\floatsep}{0.5em},
\\setlength{\\textfloatsep}{\\fill},
\\addtolength{\\textfloatsep}{3em},
\\renewcommand{\\textfraction}{0.5},
\\renewcommand{\\topfraction}{0.5},
\\renewcommand{\\bottomfraction}{0.5},
\\setcounter{totalnumber}{50},
\\setcounter{topnumber}{50},
\\setcounter{bottomnumber}{50},
% end floats for footnotes")
   (some-commands
    :initform
    "% some commands, that could be overwritten in the style file.
\\newcommand{\\rubric}[1]
{\\subsection*{~\\hfill {\\it #1} \\hfill ~}}
\\newcommand{\\titlereference}[1]{\\textsl{#1}}
% end of \"some commands\"")
   (class-sections
    :reader class-sections
    :initform
    '(("book" "chapter"  "section"  "subsection"  "subsubsection" "paragraph")
      ("scrbook" "chapter"  "section"  "subsection"  "subsubsection" "paragraph")
      ("report" "chapter"  "section"  "subsection"  "subsubsection" "paragraph")
      ("scrreprt" "chapter"  "section"  "subsection"  "subsubsection" "paragraph")
      ("article" "section"  "subsection"  "subsubsection" "paragraph")
      ("scrartcl" "section"  "subsection"  "subsubsection"  "paragraph") ))
   (active-table :reader active-table)
   (topic-class :initform "")
   (babel :reader babel)
   (mode :initform nil :documentation "List of Modes currently active e.g. :literal :mathmode :literal-block")
   (section-numbers :documentation "List of nested section numbers")
   ;; parts of document filled in during translate
   head-prefix
   head
   body-prefix
   body-suffix)
  (:default-initargs
      :parts '(head-prefix  head body-prefix body body-suffix))
  (:documentation "Docutils html writer"))

(defun mode(checklist writer)
  (if (listp checklist)
      (some #'(lambda(s) (member s (mode writer))) checklist)
      (member checklist (mode writer))))

(defun section(writer &optional (level (length *section-level*)))
  "Return the section name at the given level for the specific
            document class."
  (let ((sections (cdr (assoc  (document-class writer) (class-sections writer)
                               :test #'string-equal))))
    (if (< level (length sections))
        (elt sections level)
        (car (last sections)))))

(defmethod visit-node ((writer latex-writer) (document document))
    (with-slots(mode section-numbers active-table babel)
      (setf section-numbers (list 0)
            mode nil
            active-table (make-instance
                          'table
                          :latex-type "longtable"
                          :latex-style (setting :table-style document))
            babel (make-instance 'babel
                                 :lang (setting :language-code document))))
    (with-part(head-prefix)
      (part-prepend
       (format nil "\\documentclass[~A~@[,~A~]]{~A}~%"
               (setting :latex-document-options document)
               (get-language (babel writer))
               (setting :latex-documentclass document)))
      (part-append
       (format nil "~{\\usepackage{~A}~%~}"
                '("babel" "shortvrb" "tabularx" "longtable" "amsmath"
                  "color" "multirow" "ifthen")))
      (part-append (used-packages (active-table writer)) #\newline)
      (part-append
       (let ((color (setting :hyperlink-color document)))
         (if color
             (format nil "\\usepackage[colorlinks=true,linkcolor=~A,urlcolor=~:*~A]{hyperref}" color)
             "\\usepackage[colorlinks=false]{hyperref}"))
       #\newline)
      (let ((enc (setting :latex-font-encoding document)))
        (part-append
         (cond
           ((equal enc "OT1") "")
           ((equal enc "") "\\usepackage{ae}
\\usepackage{aeguill}")
           (t (format nil "\\usepackage[~A]{fontenc}" enc)))
         #\newline))
      (part-append
       (case (setting :graphicx-option document)
         ('nil "\\usepackage{graphicx}")
         (:auto "'%Check if we are compiling under latex or pdflatex
\\ifx\\pdftexversion\\undefined
  \\usepackage{graphicx}
\\else
  \\usepackage[pdftex]{graphicx}
\\fi")
         (t (format nil "\\usepackage[~A]{graphicx}"
                    (string-downcase (setting :graphicx-option document)))))
       #\newline)
      (part-append
       "\\setlength{\\extrarowheight}{2pt}
\\newlength{\\admonitionwidth}
\\setlength{\\admonitionwidth}{0.9\\textwidth}
\\newlength{\\docinfowidth}
\\setlength{\\docinfowidth}{0.9\\textwidth}
\\newlength{\\locallinewidth}")
      (dolist(s '(optionlist-environment lineblock-environment
                  footnote-floats some-commands))
        (part-append (slot-value writer s) #\newline))
      (when-bind (stylesheet (setting :stylesheet document))
                 (part-append (format nil "\\input{~A}~%" stylesheet))))
    (unless (setting :use-latex-docinfo document)
      (with-part(head)
        (part-append "\\author{}\\title{}" #\newline)))
    (with-part(body-suffix) (part-append #\newline))

(defun encode(writer string)
  "Encode special characters in `text` & return encoded string.
       # $ % & ~ _ ^ \ { }
   Escaping with a backslash does not help with backslashes, ~ and ^.
       < > are only available in math-mode or tt font. (really ?)
   $ starts math- mode.
 AND quotes:"
  (when (mode :verbatim writer) (return-from encode string))
  (flet((replace-all(regexp replacement)
          (setf string (regex-replace-all regexp string replacement))))
  ;; first the braces
  (replace-all "([\\{\\}])" "{\\\1}")
  ;; then backslash except in form '{\{}' or '{\}}
  (replace-all "(?<!{)(\\)(?![{}]})" "{\\textbackslash}")
  ;; then dollar
  (replace-all "$" "{\\$}")
  ;; then math type characters
  (unless (union (slot-value writer 'mode)
                 '(:literal-block :literal :mathmode))
    (replace-all "|" "{\\textbar}")
    (replace-all "<" "{\\textless}")
    (replace-all ">" "{\\textgreater}"))
  (replace-all "&" "{\\&}")
  (replace-all "^" "{\\textasciicircum}")
  (replace-all "%" "{\\%}")
  (replace-all "#" "{\\#}")
  (replace-all "~" "{\\textasciitilde}")
  ;; Separate compound characters, e.g. "--" to "-{}-".(The
  ;; actual separation is done later; see below.)
  (let ((separate-chars "-"))
    (if (union (slot-value writer 'mode) '(:literal-block :literal))
        (progn
          (setf separate-chars (concatenate 'string separate-chars ",`\'\"<>"))
          (setf string (double-quotes-in-tt (babel writer) string))
          (if (equal (setting :font-encoding document) "OT1")
              (progn
                (replace-all "_" "{\\underline{ }}")
                (replace-all "\\textbackslash" "\\reflectbox{/}"))
              (replace-all "_" "{\\_}")))
        (progn
          (setf string (quote-quotes (babel writer) string))
          (replace-all "_" "{\\_}")))
    (dotimes(x 2)
      (loop :for c :across separate-chars
            (replace-all (make-string 2 :initial-element c)
                         (format nil "~C{}~C" c))))
    (cond
      ((mode '(:insert-newline :literal-block) writer)
       (replace-all "
" "\\\\\n"))
      ((mode  :mbox-newline writer)
       (multiple-value-bind(openings closings)
           (if (mode :literal-block writer)
               (values reduce #'

(defun attval(string)
  (encode
   (with-output-to-string(os)
    (loop
     for c across string
     do (if (wsp-char-p c) (write-char #\space os)  (write-char c os))))))

(defun start-tag(node tagname &optional attributes &key (infix "") )
  (let ((tagname (string-downcase tagname))
        (atts (copy-list attributes)))
    (when node
      (when-bind(class (attribute node :class))
        (setf (getf atts :class)
              (if (getf atts :class)
                  (concatenate 'string (getf atts :class) " " class)
                  class)))
      (when-bind(id (attribute node :id))
        (setf (getf atts :id) id)))
    (when-bind(id (getf atts :id))
      (when (is-named-tag tagname) (setf (getf atts :name) id)))
    (loop for a on atts by #'cddr
       do (setf (first a) (string-downcase (first a)))
       when (listp (second a)) do (setf (second a) (join-strings (second a))))
    (format nil "<~A~{ ~A=\"~A\"~}~A>" tagname atts infix)))

(defun empty-tag(node tagname &optional attributes)
  (start-tag node tagname attributes :infix " /"))

(defun set-first-last(node)
  (when (> (number-children node) 0)
    (let ((first (child node 0))
          (last (child node (1- (number-children node)))))
      (when (typep first 'element) (add-class first "first"))
      (when (typep last 'element) (add-class last "last")))))

(defmethod visit-node((writer latex-writer) (text text))
  (part-append (encode (as-text text))))

(defmethod visit-node((writer latex-writer) (text extended-text))
  (part-append (as-text text)))

(defmacro def-simple-node(nodetype tagname &optional attributes
                          &key (suffix #\space) )
  (let ((writer (gensym))
        (tagname (string-downcase tagname))
        (node (gensym)))
    `(defmethod visit-node((,writer latex-writer) (,node ,nodetype))
      (part-append (start-tag ,node ,tagname ,attributes) ,suffix)
      (call-next-method)
      (part-append,(format nil "</~A>" tagname) ,suffix))))

;;; now node types (in order)

(def-simple-node abbreviation "abbr")
(def-simple-node acronym "acronym")

(defmacro add-docinfo-item((writer node name &key (meta t)) &body body)
  (declare (ignore writer))
  `(progn
     ,@(when meta
             `((with-part(head)
                 (part-append
                  (format nil "<meta name=~S content=~S />~%"
                          ,name (as-text ,node))))))
    (set-first-last ,node)
    (part-append
     (start-tag ,node "tr")
     ,(format nil "<th class=\"docinfo-name\">~A</th><td>" name))
    ,@body
    (part-append "</td></tr>")))

(defmethod visit-node((writer latex-writer) (node address))
  (add-docinfo-item(writer node "address" :meta nil)
    (part-append (start-tag node "pre"  '(:class "address"))
                  #\newline)
    (call-next-method)
    (part-append "</pre>" #\newline)))

(defmethod visit-node((writer latex-writer) (node admonition))
  (let ((name (string-downcase (class-name (class-of node)))))
    (part-append (start-tag node "div" `(:class ,name))
                  #\newline)
    (add-child node (make-node 'title (make-node 'text (translated-text name))) 0)
    (set-first-last node)
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer latex-writer) (node attribution))
  (multiple-value-bind(prefix suffix)
      (ecase (setting :attribution (document node))
        (:dash (values "&mdash; " ""))
        (:parentheses (values #\( #\)))
        ('nil (values "" "")))
    (part-append
     (start-tag node "p" `(:class "attribution")) prefix)
    (call-next-method)
    (part-append suffix "</p>" #\newline)))

(defmethod visit-node((writer latex-writer) (node author))
  (add-docinfo-item(writer node "author")
    (call-next-method)))

(def-simple-node block-quote "blockquote")

(defun simple-list-p(node)
  (when (or (typep node 'bullet-list) (typep node 'enumerated-list))
    (with-children(list-item node)
      (let ((found nil))
	(with-children(entry list-item)
	  (unless (typep entry 'invisible)
	    (when (and found (not (typep found 'paragraph)))
	      (return-from simple-list-p nil))
	    (if (typep entry 'paragraph)
		(if found
		    (return-from simple-list-p nil)
		    (setf found entry))
		(if (simple-list-p entry)
		    (setf found entry)
		    (return-from simple-list-p nil)))))))))

(defun parents(node)
  (unless (typep node 'document)
    (cons (parent node) (parents (parent node)))))

(defun topic-class(node)
  (typecase node
    (topic (attribute node :class))
    (document nil)
    (t (topic-class (parent node)))))

(defparameter *compact-simple* nil)
(defparameter *compact-p* t)
(defvar *topic-class* "")

(defmethod visit-node((writer latex-writer) (node bullet-list))
  (let* ((old-compact-simple *compact-simple*)
	 (*compact-p* nil)
	 (*compact-simple*
	  (and (setting :compact-lists (document node))
	       (or *compact-simple*
		   (equal *topic-class* "contents")
		   (simple-list-p node)))))
    (part-append
     (start-tag node "ul"
                (when (and *compact-simple*
                           (not old-compact-simple))
                  '(:class "simple")))
     #\newline)
    (call-next-method)
    (part-append "</ul>" #\newline)))

(def-simple-node caption "p" '(:class "caption") :suffix #\newline)

(defmethod visit-node((writer latex-writer) (node citation))
  (assert (typep (child node 0) 'label))
  (part-append
    (start-tag node "table"
	        '(:class "docutils citation" :frame "void" :rules "none"))
    "<colgroup><col class=\"label\" /><col /></colgroup>
<tbody valign=\"top\">
<tr>")
  (call-next-method)
  (part-append
   "</td></tr>
</tbody>
</table>
"))

(defmethod visit-node((writer latex-writer) (node citation-reference))
  (let ((href
         (cond ((attribute node :refid)
                (format nil "#~A" (attribute node :refid)))
               ((attribute node :refname)
                (format nil "#~A"
                        (gethash (attribute node :refname)
                                 (nameids (document node))))))))
    (part-append
     (start-tag node "a"  `(:class "citation-reference"
				   ,@(when-bind(id (attribute node :id))
				      `(:id ,(format nil "~A" id)))
				   ,@(when href `(:href ,href))))
     #\[)
    (call-next-method)
    (part-append "]</a>")))

(defmethod visit-node((writer latex-writer) (node classifier))
  (part-append
                " <span class=\"classifier-delimiter\">:</span> "
                (start-tag node "span"  `(:class "classifier")))
  (call-next-method)
  (part-append "</span>"))

(defmethod visit-node((writer latex-writer) (node colspec))
  (error "Colspec not inside a tgroup"))

(defmethod visit-node((writer latex-writer) (node comment))
  (part-append
   (format nil "<!-- ~A -->~%"
	   (cl-ppcre:regex-replace-all "-(?=-)" (as-text node) "- "))))

(defmethod visit-node((writer latex-writer) (node compound))
  (let ((n (number-children node)))
    (when (> n 1)
      (add-class (child node 0) "compound-first")
      (add-class (child node (1- n)) "compound-last")
      (dotimes(i (- n 2))
	(add-class (child node (1+ i)) "compount-middle"))))
  (part-append (start-tag node "div"  '(:class "compound")))
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node contact))
  (add-docinfo-item(writer node "contact" :meta nil)
		   (call-next-method)))

(defmethod visit-node((writer latex-writer) (node copyright))
  (add-docinfo-item(writer node "copyright")
		   (call-next-method)))

(defmethod visit-node((writer latex-writer) (node date))
  (add-docinfo-item(writer node "date")
		   (call-next-method)))

(defmethod visit-node((writer latex-writer) (node definition))
  (part-append "</dt>" #\newline (start-tag node "dd"))
  (set-first-last node)
  (call-next-method)
  (part-append "</dd>" #\newline))

(defmethod visit-node((writer latex-writer) (node definition-list))
  (part-append (start-tag node "dl" '(:class "docutils")))
  (call-next-method)
  (part-append "</dl>" #\newline))

(defmethod visit-node((writer latex-writer) (node description))
  (part-append (start-tag node "td"))
  (set-first-last node)
  (call-next-method)
  (part-append "</td>"))

(defmethod visit-node((writer latex-writer) (node docinfo))
  (with-part(docinfo)
    (part-append
     (start-tag node "table"
                '(:class "docinfo" :frame "void" :rules "none"))
     "<col class=\"docinfo-name\" />
<col class=\"docinfo-content\" />
<tbody valign=\"top\">" #\newline)
    (call-next-method)
    (part-append "</tbody>
</table>" #\newline)))

(defmethod visit-node((writer latex-writer) (node doctest-block))
  (part-append (start-tag node "pre"  '(:class "doctest-block")))
  (call-next-method)
  (part-append "</pre>" #\newline))



(defmethod visit-node((writer latex-writer) (node emphasis))
  (part-append "<em>")
  (call-next-method)
  (part-append "</em>"))

(defmethod visit-node((writer latex-writer) (node entry))
  (let ((tagname (if (typep (parent (parent node)) 'thead) "th" "td"))
	(morerows (attribute node :morerows))
	(morecols (attribute node :morecols))
	(atts nil))
    (when morerows (setf (getf atts :rowspan) (1+ morerows)))
    (when morecols (setf (getf atts :colspan) (1+ morecols)))
    (part-append (start-tag node tagname atts))
    (when (= 0 (number-children node))
      (part-append "&nbsp;"))
    (set-first-last node)
    (call-next-method)
    (part-append (format nil "</~A>~%" tagname))))

(defmethod visit-node((writer latex-writer) (node enumerated-list))
  (let ((atts nil)
	(old-compact-simple *compact-simple*)
	(*compact-p* nil)
	(*compact-simple*
	 (and (setting :compact-lists (document node))
	      (or *compact-simple*
		  (equal *topic-class* "contents")
		  (simple-list-p node)))))
    (when-bind(start (attribute node :start)) (setf (getf atts :start) start))
    (when-bind(type (attribute node :type))
      (setf (getf atts :class) (string-downcase type)))
    (when (and *compact-simple* (not old-compact-simple))
      (setf (getf atts :class)
	    (concatenate 'string (getf atts :class "") " simple")))
    (part-append  (start-tag node "ol" atts))
    (call-next-method)
    (part-append  "</ol>" #\newline)))

(defmethod visit-node((writer latex-writer) (node field))
  (part-append  (start-tag node "tr" '(:class "field")))
  (call-next-method)
  (part-append  "</tr>" #\newline))

(defmethod visit-node((writer latex-writer) (node field-body))
  (part-append  (start-tag node "td" '(:class "field-body")))
  (set-first-last node)
  (call-next-method)
  (part-append  "</td>" #\newline))

(defmethod visit-node((writer latex-writer) (node field-list))
  (part-append
   (start-tag node "table"
	      '(:class "field-list"
		:frame "void"
		:rules "none"))
   "<col class=\"field-name\" />
<col class=\"field-body\" />
<tbody valign=\"top\">
")
  (call-next-method)
  (part-append  "</tbody>
</table>
"))

(defmethod visit-node((writer latex-writer) (node field-name))
    (let ((docinfo-p (typep (parent (parent node)) 'docinfo))
	  (long  (> (length (as-text node)) 14)))
      (part-append
       (start-tag
	node "th" (list :colspan (if long 2 1)
			:class (if docinfo-p "docinfo-name" "field-name"))))
      (call-next-method)
      (part-append ":</th>")
      (when long
        (part-append "</tr>" #\newline "<tr><td>&nbsp;</td>"))))

(defmethod visit-node((writer latex-writer) (node figure))
  (let ((atts (list :class "figure")))
    (when-bind(width (attribute node :width))
              (setf (getf atts :style)
                    (format nil "width: ~d~[ps~;~A~]" (car width) (cdr width))))
    (part-append (start-tag node "div" atts))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer latex-writer) (node footer))
  (with-part(footer)
    (part-append
     "<hr class =\"docutils footer\" />"
     (start-tag node "div" '(:class "footer")))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer latex-writer) (node footnote))
  (assert (typep (child node 0) 'label))
  (part-append
   (start-tag node "div" '(:class "docutils footnote"
                           :frame "void" :rules "none"))
   "<colgroup><col class=\"label\" /><col /></colgroup>
<tbody valign=\"top\">
<tr>")
  (call-next-method)
  (part-append
   "</td></tr>
</tbody>
</table>
"))

(defmethod visit-node((writer latex-writer) (node footnote-reference))
  (let ((href
	 (let ((refid (or (attribute node :refid)
			  (gethash (attribute node :refname)
				   (nameids (document node))))))
	   (when refid  (list :href (format nil "#~A" refid))))))
    (multiple-value-bind(prefix suffix)
	(ecase (setting :footnote-references (document node))
	  (:brackets (values "[" "]"))
	  (:superscript (values "<sup>" "</sup>")))
      (part-append
	       (start-tag
		node "a" `(:class "footnote-reference"
				  ,@(when-bind(id (attribute node :id))
				      `(:id ,(format nil "~A" id)))
				  ,@href))
         prefix)
      (call-next-method)
      (part-append suffix "</a>"))))

(defmethod visit-node((writer latex-writer) (node header))
  (with-slots(header body body-prefix) writer
    (with-part(body-prefix)
      (call-next-method)
      (part-append writer 'header
                    (start-tag node "div" '(:class "header"))
                    (nreverse body)
                    "<hr class=\"docutils header\"/>
</div>
")
      (setf body-prefix (nconc header body-prefix)))))

(defmethod visit-node((writer latex-writer) (node image))
  (let ((atts nil))
    (with-attributes(k v node) (setf (getf atts k) v))
    (remf atts :class)
    (setf (getf atts :src) (as-text (child node 0)))
    (remf atts :uri)
    (let ((scale (getf atts :scale))
	  (width (car (getf atts :width)))
	  (height (car (getf atts :height))))
	(when scale
	  (setf width (if width (round (/ (* width scale) 100)) scale))
	  (setf height (if height (round (/ (* height scale) 100)) scale))
	  (remf atts :scale))
	(flet ((set-size(n symb)
		 (when n
		   (setf (getf atts symb)
			 (format nil "~,f~:[%~;~:*~A~]" n
				 (cdr (getf atts symb)))))))
	  (set-size width :width)
	  (set-size height :height))
    (unless (getf atts :alt) (setf  (getf atts :alt)  (getf atts :src)))
    (let ((inline-p (typep (parent node) 'text-element)))
      (unless inline-p
	(part-append
	 (start-tag nil "div" (image-div-atts node))))
      (part-append (empty-tag node "img" atts))
      (unless inline-p (part-append "</div>" #\newline))))))

(defun image-div-atts(node)
  (let ((atts (list :class "image")))
    (when-bind(class (attribute node :class))
      (setf (getf atts :class)
	    (concatenate 'string (getf atts :class) " " class)))
    (when-bind(align (attribute node :align))
      (setf (getf atts :align) (attval align))
      (setf (getf atts :class)
	    (concatenate 'string
			 (getf atts :class) " align-" (getf atts :align))))
    atts))

(defmethod visit-node((writer latex-writer) (node docutils.nodes:inline))
  (when (> (number-children node) 0)
    (part-append (start-tag node "span" nil))
    (call-next-method)
    (part-append "</span>")))

(defmethod visit-node((writer latex-writer) (node label))
  (assert (typep (parent node) '(or footnote citation)))
  (part-append (start-tag node "td" '(:class "label")))
  (let ((backrefs (backrefs (parent node)))
	(id (attribute (parent node) :id)))
    (cond ((not (and backrefs (setting :footnote-backlinks (document node))))
	   (part-append (format nil "<a name=\"~A\">[" id))
	   (call-next-method)
	   (part-append "]</a></td><td>"))
	  ((= 1 (length backrefs))
	   (part-append (format nil "<a class=\"fn-backref\" name=\"~A\" href=\"#~A\">[" id (first backrefs)))
	   (call-next-method)
	   (part-append "]</a></td><td>"))
	  ((part-append (format nil "<a name=\"~A\">[" id))
	   (call-next-method)
	   (part-append "]</a></td><td><em>(")
	   (let ((i 0))
	     (dolist(id backrefs)
	       (part-append
		(format nil "<a class=\"fn-backref\" href=\"#~a\">~d</a>"
			id (incf i)))))
	   (part-append ")</em>")))))

(defmethod visit-node((writer latex-writer) (node legend))
  (part-append (start-tag node "div" '(:class "legend"))
                #\newline)
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node line))
  (part-append (start-tag node "div" '(:class "line") ))
  (if (> (number-children node) 0)
      (call-next-method)
      (part-append "<br />"))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node line-block))
  (part-append (start-tag node "div" '(:class "line-block"))
                #\newline)
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node list-item))
  (part-append (start-tag node "li"))
  (when (> (number-children node) 0)
    (add-class (child node 0) "first")
    (call-next-method))
  (part-append "</li>" #\newline))

(defun words-and-spaces(text)
  (let((start 0)
       (tokens nil))
    (dolist(end (nconc
                 (cl-ppcre::all-matches
                  '(:greedy-repetition 1 nil :whitespace-char-class) text)
                 (list (length text))))
      (unless (= start end) (push (subseq text start end) tokens))
      (setf start end))
    (nreverse tokens)))

(defmethod visit-node((writer latex-writer) (node literal))
  (part-append (start-tag node "tt" '(:class "docutils literal"))
                #\newline)
  (dolist(token (words-and-spaces (as-text node)))
    (part-append
     (cond
       ((not (line-blank-p token))
        (format nil "<span clas=\"pre\">~A</span>"
                (encode token)))
       ((= 1 (length token)) token)
       ((with-output-to-string(os)
          (dotimes(x (1- (length token)))
            (write-string "&nbsp;" os))
          (write-char #\space os))))))
  (part-append "</tt>"))

(defmethod visit-node((writer latex-writer) (node literal-block))
  (part-append (start-tag node "pre" '(:class "literal-block"))
                #\newline)
  (call-next-method)
  (part-append #\newline "</pre>" #\newline))

(defmethod visit-node((writer latex-writer) (node docutils.nodes::meta))
  (with-part(head)
  (part-append
   (empty-tag node "meta" (slot-value node 'docutils::attributes))
   #\newline)))

(defmethod visit-node((writer latex-writer) (node option-group))
  (let ((atts nil)
        (long  (> (length (as-text node)) 14)))
    (when long (setf (getf atts :colspan) 2))
    (part-append
                  (start-tag node "td" (when long '(:colspan 2)))
                  "<kbd>")
    (let ((prev nil))
      (with-children(option node)
        (when prev (part-append ", "))
        (setf prev option)
        (visit-node writer option)))
    (part-append "</kbd></td>" #\newline)
    (when long
      (part-append "</tr>" #\newline "<tr><td>&nbsp;</td>"))))

(defmethod visit-node((writer latex-writer) (node option-argument))
  (part-append
                 (or (attribute node :delimiter) #\space)
                 (start-tag node "var"))
  (call-next-method)
  (part-append  "</var>"))

(defmethod visit-node((writer latex-writer) (node option-list))
  (part-append
                 (start-tag node "table"
                            '(:class "docutils option-list"
                              :frame "void" :rules "none"))
                 "<col class=\"option\" /><col class=\"description\" />"
                 #\newline
                 "<tbody valign=\"top\">"
                 #\newline)
  (call-next-method)
  (part-append  "</tbody>
</table>
"))

(defmethod visit-node((writer latex-writer) (node option-list-item))
  (part-append  (start-tag node "tr"))
  (call-next-method)
  (part-append  "</tr>"))

(defmethod visit-node((writer latex-writer) (node option-string))
  (part-append  (start-tag node "span" '(:class "option")))
  (call-next-method)
  (part-append  "</span>"))

(defmethod visit-node((writer latex-writer) (node organization))
  (add-docinfo-item(writer node "organization")
                   (call-next-method)))

(defun should-be-compact-p(node)
  "Determine if the <p> tags around paragraph ``node`` can be omitted."
  (and
   (not (typep (parent node) 'document)) ;;Never in document or compound.
   (not (typep (parent node) 'compound))
   (member (slot-value node 'docutils::attributes)
           '(nil (:class "first") (:class "last")
             (:class "first last"))
           :test #'equalp)
   (or *compact-simple*
       (and *compact-p*
            (or (= 1 (number-children (parent node)))
                (and (= 2 (number-children (parent node)))
                     (typep (child (parent node) 0) 'label)))))))

(defmethod visit-node((writer latex-writer) (node paragraph))
  (let ((compact-p (should-be-compact-p node)))
    (unless compact-p (part-append  (start-tag node "p")))
    (call-next-method)
    (unless compact-p (part-append  "</p>" #\newline))))

(defmethod visit-node((writer latex-writer) (node problematic))
  (let ((refid (attribute node :refid)))
    (when refid
      (part-append (format nil "<a href=\"#~A\" name=\"~A\">"
                           refid (attribute node :id))))
    (part-append (start-tag node "span" '(:class "problematic")))
    (call-next-method)
    (part-append "</span>")
    (when refid (part-append  "</a>"))))

(defmethod visit-node((writer latex-writer) (node raw))
  (when (member :html (attribute node :format))
    (let ((add-class (when-bind(class (attribute node :class))
		       (not (eq class :none))))
          (type (if (typep (parent node) 'text-element) "span" "div")))
      (when add-class (part-append (start-tag node type)))
      (part-append (as-text node))
      (when add-class (part-append "<" type "/>")))))


(defmethod visit-node((writer latex-writer) (node reference))
  (let ((inline-p (typep (parent node) 'text-element)))
    (unless inline-p
      (assert (and (= 1 (number-children node))
                   (typep (child node 0) 'image)))
      (let ((atts (image-div-atts (child node 0))))
        (setf (getf atts :class)
              (concatenate 'string (getf atts :class) " image-reference"))
        (part-append (start-tag nil "div" atts))))
    (let ((href
           (cond ((attribute node :refuri))
                 ((attribute node :refid)
                  (format nil "#~A" (attribute node :refid)))
                 ((attribute node :refname)
                  (format nil "#~A" (gethash (attribute node :refname)
                                             (nameids (document node))))))))
      (part-append
       (start-tag node "a" `(:class "reference"
				     ,@(when-bind(id (attribute node :id))
					 `(:id ,(format nil "~A" id)))
                             ,@(when href `(:href ,href))))))
    (call-next-method)
    (part-append "</a>")
    (unless inline-p (part-append "</div>" #\newline))))

(defmethod visit-node((writer latex-writer) (node revision))
  (add-docinfo-item(writer node "revision" :meta nil)
                   (call-next-method)))

(defmethod visit-node((writer latex-writer) (node row))
  (part-append (start-tag node "tr"))
  (call-next-method)
  (part-append "</tr>" #\newline))

(defmethod visit-node((writer latex-writer) (node rubric))
  (part-append (start-tag node "p" '(:class "rubric")))
  (call-next-method)
  (part-append "</p>" #\newline))

(defmethod visit-node((writer latex-writer) (node section))
  (let ((*section-level* (1+ *section-level*)))
    (part-append (start-tag node "div" '(:class "section")))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defvar *in-sidebar* nil)

(defmethod visit-node((writer latex-writer) (node sidebar))
  (part-append (start-tag node "div" '(:class "section")))
  (set-first-last node)
  (let ((*in-sidebar* t)) (call-next-method))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node status))
  (add-docinfo-item(writer node "status" :meta nil)
                   (call-next-method)))

(defmethod visit-node((writer latex-writer) (node strong))
  (part-append "<strong>")
  (call-next-method)
  (part-append "</strong>"))

(defmethod visit-node((writer latex-writer) (node subscript))
  (part-append (start-tag node "sub"))
  (call-next-method)
  (part-append "</sub>"))

(defmethod visit-node((writer latex-writer) (node substitution-definition)))


(defmethod visit-node((writer latex-writer) (node subtitle))
  (etypecase (parent node)
    (sidebar
     (part-append (start-tag node "p"
                             '(:class "sidebar-subtitle")))
     (call-next-method)
     (part-append "</p>" #\newline))
    (document
     (with-part(body-pre-docinfo)
       (part-append (start-tag node "h2" '(:class "subtitle")))
       (call-next-method)
       (part-append "</h2>")
       ))))

(defmethod visit-node((writer latex-writer) (node superscript))
  (part-append (start-tag node "sup"))
  (call-next-method)
  (part-append "</sup>"))

(defmethod visit-node((writer latex-writer) (node system-message))
  (unless (< (error-severity node) (setting :report-level (document node)))
    (part-append
     (start-tag node "div" '(:class "system-message"))
     "<p class=\"system-message-title\">")
    (let ((attr (when-bind(id (attribute node :id)) (list :name id)))
          (backref-text
           (let ((backrefs (attribute node :backrefs)))
             (cond
               ((not backrefs) "")
               ((not (rest backrefs))
                (format nil "; <em><a href=\"#~A\">backlink</a></em>"
                        (car backrefs)))
               ((with-output-to-string(os)
                 (write-string "; <em>backlinks: " os)
                 (do((ref backrefs (cdr ref))
                     (i 1 (1+ i)))
                    ((not ref))
                   (format os "<a href=\"#~A\">~A</a> " (car ref) i))
                 (write-string "</em>" os))))))
          (line (if (line node)
                    (format nil ", source line ~A"  (line node))
                    "")))
      (multiple-value-bind(a-start a-end)
          (if attr
              (values (start-tag nil "a" attr) "</a>")
              (values "" ""))
        (part-append
                      (format nil "System Message: ~A~A~A (<tt class=\"docutils\">"
                              a-start
			      (attribute node :level)
                               a-end)))
      (call-next-method)
      (part-append
       (format nil "</tt>~A)~A</p>~%</div>"
	       line backref-text)))))

(defmethod visit-node((writer latex-writer) (node table))
  (part-append (start-tag node "table"
					'(:class "docutils" :border "1")))
    (call-next-method)
    (part-append "</table>" #\newline))

(defmethod visit-node((writer latex-writer) (node target))
  (let ((a (or (attribute node :refuri)
               (attribute node :refid)
               (attribute node :refname))))
    (when (not a)
      (part-append (start-tag node "a" `(:class "target" :name ,a))))
    (call-next-method)
    (when (not a) (part-append "</a>"))))

(defmethod visit-node((writer latex-writer) (node tbody))
    (part-append (start-tag node "tbody" '(:valign "top" )))
    (call-next-method)
    (part-append "</tbody>" #\newline))

(defmethod visit-node((writer latex-writer) (node term))
  (part-append (start-tag node "dt" ))
  (call-next-method)
  ;; Leave the end tag to `self.visit_definition()`, in case there's a
  ;; classifier
  )

(defmethod visit-node((writer latex-writer) (node tgroup))
  (let (colwidths rest)
    (with-children(child node)
      (etypecase child
	(colspec (push (attribute child :colwidth) colwidths))
	(thead (push child rest))
	(tbody (push child rest))))
    (part-append (start-tag node "colgroup"))
    (let ((width (reduce #'+ colwidths)))
      (dolist(colwidth (nreverse colwidths))
	(part-append

	 (empty-tag
	  node "col"
	  (list :width
		(format nil "~d%" (round (/ (* 100 colwidth) width))))))))
    (part-append "</colgroup>" #\newline)
    (dolist(child (nreverse rest)) (visit-node writer child))))

(defmethod visit-node((writer latex-writer) (node thead))
  (part-append (start-tag node "thead" '(:valign "bottom" )))
  (call-next-method)
  (part-append "</thead>" #\newline))

(defmethod visit-node((writer latex-writer) (node title))
  (cond
    ((typep (parent node) 'document) ;; document title
     (with-part(head)
       (part-append (format nil "<title>~A</title>~%"
                            (encode (as-text node)))))
     (with-part(body-pre-docinfo)
       (part-append (start-tag node "h1" '(:class "title")))
       (call-next-method)
       (part-append "</h1>" #\newline)))
    ((typep (parent node) 'section)
     (let ((h (format nil "h~D"
                      (+ *section-level*
                         (setting :initial-header-level (document node)))))
           (atts nil))
       (part-append (start-tag node h))
       (when-bind(id (attribute (parent node) :id))
         (setf (getf atts :name) id))
       (when-bind(refid (attribute node :refid))
         (setf (getf atts :class) "toc-backref")
         (setf (getf atts :href) (format nil "#~A" refid)))
       (part-append (start-tag nil "a" atts))
       (call-next-method)
       (part-append "</a></" h ">" #\newline)))
    (t (multiple-value-bind(tagname class)
           (etypecase (parent node)
             (topic (values "p" "topic-title"))
             (sidebar (values "p" "sidebar-title"))
             (admonition (values "p" "admonition-title"))
             (table (values "table")))
         (part-append (start-tag node tagname (when class `(:class ,class))))
         (let ((pid (attribute (parent node) :id)))
           (when pid
             (part-append (start-tag nil "a" `(:name ,pid))))
           (call-next-method)
           (when pid (part-append "</a>")))
         (part-append "</" tagname ">" #\newline)))))

(defmethod visit-node((writer latex-writer) (node docutils.nodes:title-reference))
  (part-append (start-tag node "cite"))
  (call-next-method)
  (part-append "</cite>"))

(defmethod visit-node((writer latex-writer) (node topic))
  (part-append (start-tag node "div" '(:class "topic")))
  (let ((*topic-class* (attribute node :class)))
    (call-next-method))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer latex-writer) (node transition))
  (part-append (empty-tag node "hr" '(:class "docutils"))))

(defmethod visit-node((writer latex-writer) (node version))
  (add-docinfo-item(writer node "version" :meta nil)
                   (call-next-method)))
