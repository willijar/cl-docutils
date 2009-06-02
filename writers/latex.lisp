;;;; Misc string and vector handling for docutils
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; $Id: html.lisp,v 1.4 2006/07/07 18:43:20 willijar Exp $

(defpackage :docutils.writer.latex
  (:documentation "Latex writer for docutils")
  (:use :cl :docutils  :docutils.utilities  :docutils.nodes :cl-ppcre)
  (:shadowing-import-from :cl #:warning #:error #:inline #:special)
  (:import-from :jarw.media #:latex-length #:convert-length-unit)
  (:import-from :jarw.lib #:when-bind #:is-prefix-p)
  (:import-from :jarw.string #:join-strings)
  (:import-from :split-sequence #:split-sequence)
  (:export #:latex-writer))

(in-package :docutils.writer.latex)

(docutils:register-settings-spec
 `((:latex-document-class  string "article"  "Specify documentclass.")
   (:latex-document-options  string "10pt,a4paper"
    "Specify document options.  Multiple options can be given,
    separated by commas.")
   (:use-latex-footnotes boolean nil
    "Use LaTeX footnotes. LaTeX supports only numbered footnotes (does
    it?). Default: no, uses figures.")
   (:footnote-references
    (member :type symbol :set (:superscript :brackets)) :brackets
    "Format for footnote references: one of :superscript or :brackets.")
   (:use-latex-citations  boolean nil
    "Default: no, uses figures which might get mixed with images.")
   (:attribution
    (member :type (symbol :nil-allowed t) :set (:dash :parentheses nil)) :dash
    "Format for block quote attributions: one of 'dash',
'parentheses' or 'nil'")
   (:latex-stylesheet  pathname nil
    "Specify a stylesheet file. The file will be \"input\" by latex in '
     the document header.  Default is no stylesheet")
   (:stylesheet-path pathname nil
   "Specify a stylesheet file, relative to the current working
   directory.  Overrides --stylesheet.")
   (:use-latex-toc boolean nil
    "Table of contents by docutils (default) or latex. Latex (writer)
    supports only one ToC per document, but docutils does not write
    pagenumbers.")
   (:use-latex-docinfo boolean nil
    "Let LaTeX print author and date, do not show it in docutils
     document info.")
   (:hyperlink-colour string "blue"
    "Color of any hyperlinks embedded in text")
   (:compound-enumerators  boolean nil
    "Enable compound enumerators for nested enumerated lists ")
   (:section-prefix-for-enumerators  boolean nil
    "Enable section (\".\" subsection ...) prefixes for compound
    enumerators.  This has no effect without :compound-enumerators.")
   (:section-enumerator-separator string "-"
    "Set the separator between section number and enumerator for
    compound enumerated lists.")
   (:use-verbatim-when-possible boolean nil
    "When possibile, use verbatim for literal-blocks. Default is to
    always use the mbox environment.")
   (:table-style
    (member :type symbol :set (:standard :booktabs :nolines)) :standard
    "Table style. \"standard\" with horizontal and vertical lines,
    \"booktabs\" (LaTeX booktabs style) only horizontal lines above
    and below the table and below the header or \"nolines\".")
   (:graphicx-option
    (member :type (symbol :nil-allowed t) :set (:dvips :pdftex :auto nil)) nil
    "LaTeX graphicx package option. Possible values are \"dvips\",
    \"pdftex\". \"auto\" includes LaTeX code to use \"pdftex\" if
    processing with pdf(la)tex and dvips otherwise.")
   (:latex-font-encoding string ""
    "Possible values are \"T1\", \"OT1\", \"\" or some other fontenc
    option. The font encoding influences available symbols,
    e.g. \"<<\" as one character. Default is "" which leads to package
    \"ae\" (a T1 emulation using CM fonts).")))

(defclass latex-table()
  ((latex-type :initform "longtable" :initarg :latex-type :reader latex-type)
   (table-style :initform "booktabs" :initarg :table-style :reader table-style)
   (open :initform nil :accessor open-p)
   (caption :initform nil :reader caption)
   (attributes :initform nil)
   (col-width :initform nil)
   (preamble-written :initform nil)
   col-specs in-head
   (cell-in-row :reader entry-number)
   (rowspan :initform nil))
  (:documentation " Manage a table while traversing.
        Maybe change to a mixin defining the visit/departs, but then
        class Table internal variables are in the Translator."))

(flet ((set-slots(entity slots values)
         (map nil
              #'(lambda(s v) (setf (slot-value entity s) v))
              slots values )))
  (defun open-table(table)
    (set-slots table
               '(open col-specs caption in-head preamble-written)
               '(t    nil      nil        nil   nil)))

  (defun close-table(table)
    (set-slots table
               '(open col-specs caption)
               '(nil nil nil))))

(defun used-packages(table)
  (if (equal (table-style table) "booktabs")
      "\\usepackage{booktabs}"
      ""))

(defun vertical-bar(table)
  (if (equal (table-style table) "standard") "|" ""))

(defun opening(table)
  (format nil "\\begin{~A}[c]~%" (latex-type table)))

(defun closing(table)
  (let ((style (table-style table)))
    (format nil "~A\\end{~A}"
          (cond
            ((equal style "booktabs") "\\bottomrule")
            ((equal style "standard") "\\hline")
            (""))
          (latex-type table))))

(defmethod visit-node((table latex-table) (node colspec))
  (setf (slot-value table 'col-specs)
        (nconc (slot-value table 'col-specs)
               (list (attribute node :colwidth)))))

(defun col-specs(table)
  "Return col-specs for longtable"
  (with-slots(col-specs col-width rowspan) table
    (let* ((width 80)
           (total-width
            (/ (+ (reduce #'+ col-specs) (length col-specs)) width)))
      (let ((factor 0.93))
        (when (> total-width 1) (setf factor (/ factor total-width)))
        (setf col-width (mapcar
                         #'(lambda(w) (+ (/ (* factor (1+ w)) width) 0.005))
                         col-specs)
              rowspan (make-list (length col-specs) :initial-element 0))
        (let ((bar (vertical-bar table)))
          (format nil "~{~Ap{~,2f\\locallinewidth}~}~A"
                  (mapcan #'(lambda(w) (list bar w)) col-width)
                  bar))))))

(defun column-width(table)
  (with-slots(col-width cell-in-row) table
    (format nil "~,2f\\locallinewidth"
            (elt col-width (1- cell-in-row)))))

(defun visit-thead(table)
  (let ((style (table-style table)))
    (cond
      ((equal style "booktabs") (values "\\toprule" "\\midrule\\endhead"))
      ((equal style "standard") (values "\\hline" "\\endhead"))
      (""))))

(defun visit-row(table)
  (setf (slot-value table 'cell-in-row) 0))

(defun depart-row(table)
  (setf (slot-value table 'cell-in-row) nil)
  (with-output-to-string(os)
    (format os " \\\\~%")
    (with-slots(rowspan) table
      (setf rowspan (mapcar #'(lambda(v) (if (> v 0) (1- v) v)) rowspan))
      (when (equal (table-style table) "standard")
        (let ((rowspans nil))
          (do ((i 0 (1+ i)))
              ((> i (length rowspan)))
            (when (<= (nth i rowspan) 0) (push (1- i) rowspans)))
          (if (= (length rowspan) (length rowspans))
              (format os "\\hline~%")
              (format os "~{\\cline{~D-~:*~D}~%~}"
                      (nreverse rowspans))))))))

(defun rowspan(table cell)
  (nth cell (slot-value table 'rowspan)))

(defun (setf rowspan)(value table cell)
  (when (nth cell (slot-value table 'rowspan))
    (setf (nth cell (slot-value table 'rowspan)) value)))

(defun visit-entry(table )
  (incf (slot-value table 'cell-in-row)))

(defun to-latex-encoding(encoding)
  "Translate docutils encoding name into latex's.
Default fallback method is remove \"-\" and \"_\" chars from docutils_encoding."
  (let ((enc
         (cdr
          (assoc
           encoding
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
             ("us-ascii" . "ascii"))
           :test #'string-equal))))
    (or enc
        (string-downcase (regex-replace-all "[-_]" encoding "")))))

(defclass latex-writer(writer)
  ((class-sections
    :reader class-sections
    :initform
    '(("book" "chapter" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph")
      ("scrbook" "chapter" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph")
      ("report" "chapter" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph")
      ("scrreprt" "chapter" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph")
      ("article" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph")
      ("scrartcl" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph") ))
   (active-table :reader active-table)
   (topic-class :initform nil)
   (title :initform "" :accessor title)
   (date :accessor date :documentation "Set date")
   (author-stack :initform nil :accessor author-stack
                 :documentation "List of author information")
   (mode :initform nil
         :documentation "List of Modes currently active e.g. :literal :mathmode :literal-block")
   (literal-block-stack :initform nil :accessor literal-block-stack
                        :documentation "Nested literal blocks")
   (dependencies :initform nil :accessor dependencies
                 :documentation "List of dependencie uris")
   (section-numbers :initform nil :accessor section-numbers
                    :documentation "Stack of nested section numbers")
   (enumeration-counters :initform nil :accessor enumeration-counters
                         :documentation "Stack of enumeration counters")
   (max-enum-depth :initform 0 :accessor max-enum-depth
                   :documentation "Maximum enumeration counter depth so far")
   (metadata :initform (make-hash-table) :reader metadata
             :documentation "docinfo metadata for Latex and pdfinfo")
   ;; parts of document filled in during translate
   head-prefix
   pdfinfo
   pdfauthor
   head
   body-prefix
   docinfo
   body
   body-suffix
   bibitems
   footer
   tmp-parts)
  (:default-initargs :parts
      '(head-prefix pdfinfo pdfauthor head body-prefix docinfo body footer body-suffix))
  (:documentation "Docutils latex writer"))

(defmacro collect-parts(&body body)
  `(progn
     (setf (slot-value docutils::*current-writer* 'tmp-parts) nil)
     (with-part(tmp-parts) ,@body)
     (slot-value docutils::*current-writer* 'tmp-parts)))

(defmacro with-modes((writer &rest modes) &body body)
  (let ((tmp (gensym))
        (gwriter (gensym)))
    `(let* ((,gwriter ,writer)
            (,tmp (slot-value ,gwriter 'mode)))
       (setf (slot-value ,gwriter 'mode)
             (nconc (list ,@modes) (slot-value ,gwriter 'mode)))
       ,@body
       (setf (slot-value ,gwriter 'mode) ,tmp))))

(defun mode(checklist writer)
  "Return true if writer is in any of the checklist modes"
  (if (listp checklist)
      (intersection checklist (slot-value writer 'mode))
      (member checklist (slot-value writer 'mode))))

;; these are dependant on *language*
(defun quote-quotes(text)
  (let ((quotes (latex-quotes *language*))
        (quote-index 0))
    (flet ((next-quote()
             (prog1
                 (elt quotes quote-index)
               (setf quote-index (mod (1+ quote-index) 2)))))
      (with-output-to-string(os)
        (let ((parts (split-sequence #\" text)))
          (write-string (car parts) os)
          (dolist(part (cdr parts))
            (write-string (next-quote) os)
            (write-string part os)))))))

(defun double-quotes-in-tt(text)
  (let ((dqr (latex-double-quote-replacement *language*)))
    (if dqr
        (regex-replace-all "\"" text dqr)
        text)))

(defun section(writer &optional (level (1- (length (section-numbers writer)))))
  "Return the section name at the given level for the specific
            document class."
  (let ((sections (cdr (assoc (setting :latex-document-class writer)
                              (class-sections writer)
                              :test #'string-equal))))
    (if (< level (length sections))
        (elt sections level)
        (car (last sections)))))

(defmethod visit-node ((writer latex-writer) (document document))
    (with-slots(mode section-numbers active-table bibitems max-enum-depth)
        writer
      (setf section-numbers nil
            mode nil
            bibitems nil
            max-enum-depth 0
            active-table
            (make-instance
             'latex-table
             :latex-type
             (if (search "twocolumn" (setting :latex-document-options writer))
                 "tabular" "longtable")
             :table-style (setting :table-style writer)))
    (with-part(head-prefix)
      (part-prepend
       (format nil "\\documentclass[~A~@[,~A~]]{~A}~%"
               (setting :latex-document-options writer)
               (babel *language*)
               (setting :latex-document-class writer)))
      (part-append (used-packages (active-table writer)) #\newline)
      (part-append
       (let ((color (setting :hyperlink-color writer)))
         (if color
             (format nil "\\usepackage[colorlinks=true,linkcolor=~A,urlcolor=~:*~A]{hyperref}" color)
             "\\usepackage[colorlinks=false]{hyperref}"))
       #\newline)
      (let ((enc (setting :latex-font-encoding writer)))
        (part-append
         (cond
           ((equal enc "OT1") "")
           ((equal enc "") "\\usepackage{ae}
\\usepackage{aeguill}")
           (t (format nil "\\usepackage[~A]{fontenc}" enc)))
         #\newline))
      (part-append
       (case (setting :graphicx-option writer)
         ('nil "\\usepackage{graphicx}")
         (:auto "'%Check if we are compiling under latex or pdflatex
\\ifx\\pdftexversion\\undefined
  \\usepackage{graphicx}
\\else
  \\usepackage[pdftex]{graphicx}
\\fi")
         (t (format nil "\\usepackage[~A]{graphicx}"
                    (string-downcase (setting :graphicx-option writer)))))
       #\newline)
      (part-append "\\input{docutils.tex}" #\newline)
      (when-bind (stylesheet (setting :latex-stylesheet writer))
        (part-append (format nil "\\input{~A}~%" stylesheet))))
    (setq *document* document)
    (unless (setting :use-latex-docinfo writer)
      (with-part(head)
        (part-append "\\author{}\\title{}" #\newline)))
    (with-part(body-prefix)
      (part-append #\newline "\\begin{document}" #\newline)
      (when (setting :use-latex-docinfo writer)
        (part-append "\\maketitle" #\newline)))
    (with-part(body)
      (part-append "\\setlength{\\locallinewidth}{\\linewidth}" #\newline)
      (call-next-method)

      (when (and bibitems (setting :use-latex-citations writer))
        (let ((widest-label ""))
          (dolist(item bibitems)
            (when (> (length (first item)) (length widest-label))
              (setf widest-label (first item))))
          (part-append
           (format nil "\\begin{thebibliography}{~A}~%" widest-label)))
          (dolist(item bibitems)
            (part-append (format nil "\\bibitem[~A]{~A}{~A}~%"
                                 (first item)
                                 (first item)
                                 (second item))))
          (part-append "\\end{bibliography" #\Newline)))
    (with-part(body-suffix) (part-append "\\end{document}") #\newline)))

(defun encode(writer string)
  "Encode special characters in `text` & return encoded string.
       # $ % & ~ _ ^ \ { }
   Escaping with a backslash does not help with backslashes, ~ and ^.
       < > are only available in math-mode or tt font. (really ?)
   $ starts math- mode.\\begin{thebibliography}{%s}
 AND quotes:"
  (when (mode :verbatim writer) (return-from encode string))
  (flet((replace-all(regexp replacement)
          (setf string (regex-replace-all regexp string replacement))))
  ;; first the braces
  (replace-all "([\\{\\}])" "{\\\1}")
  ;; then backslash except in form '{\{}' or '{\}}
  (replace-all "(?<!{)(\\\\)(?![{}]})" "{\\textbackslash}")
  ;; do character replacements
  (setf string
        (with-output-to-string(os)
          (loop
             :for c :across string
             :do
             (flet ((char-replace(map prefix suffix)
                      (let ((repl (cdr (assoc c map))))
                        (when repl
                          (write-string prefix os)
                          (write-string repl os)
                          (write-string suffix os)))))
               (or
                (unless (mode '(:literal-block :literal :mathmode) writer)
                  (char-replace
                   '((#\| . "textbar")
                     (#\< . "textless")
                     (#\> . "textgreater"))
                   "{\\" "}"))
                (char-replace
                 '((#\$ ."$")
                   (#\& . "&")
                   (#\† . "dag")        ;; dagger
                   (#\‡ . "ddag")
                   ('\§ .q "S")          ;; section
                   (#\  . "P")           ;; paragraph
                   (#\¶ ."P")
                   (#\^ ."textasciicircum")
                   (#\% . "%")
                   (#\# ."#")
                   (#\® . "texttrademark")
                   (#\© . "copyright")
                   (#\♠ . "spadesuit") ;; spades
                   (#\♥ ."heartsuit")  ;; hearts
                   (#\♦ . "diamondsuit");; diamonds
                   (#\♣ . "clubsuit")
                   (#\~ ."textasciitilde"))
                 "{\\" "}")
                (char-replace
                 '((#\⇔ . "Leftrightarrow"))
                 "{\\ensuremathmode{\\" "}}")
                (write-char c os))))))
  ;; Separate compound characters, e.g. "--" to "-{}-".(The
  ;; actual separation is done later; see below.)
  (let ((separate-chars "-"))
    (if (mode '(:literal-block :literal) writer)
        (progn
          (setf separate-chars (concatenate 'string separate-chars ",`\'\"<>"))
          (setf string (double-quotes-in-tt string))
          (if (equal (setting :font-encoding writer) "OT1")
              (progn
                (replace-all "_" "{\\underline{ }}")
                (replace-all "\\textbackslash" "\\reflectbox{/}"))
              (replace-all "_" "{\\_}")))
        (progn
          (setf string (quote-quotes string))
          (replace-all "_" "{\\_}")))
    (dotimes(x 2)
      (loop
         :for c :across separate-chars
         :do (replace-all (make-string 2 :initial-element c)
                          (format nil "~C{}~C" c c)))))
  (cond
    ((mode '(:insert-newline :literal-block) writer)
     (replace-all "
" " \\\\\\\\
"))
    ((mode  :mbox-newline writer)
     (let ((openings (join-strings (literal-block-stack writer)))
           (closings (make-string (length (literal-block-stack writer))
                                  :initial-element #\})))
       (replace-all "
"
                    (format nil  "~s}\\\\
\\mbox{~s"
                            closings
                            openings)))))
  (when (mode :insert-non-breaking-blanks writer)
    (replace-all " " "~"))
  string))

(defun attval(writer string)
  (encode
   writer
   (with-output-to-string(os)
    (loop
     for c across string
     do (if (wsp-char-p c) (write-char #\space os)  (write-char c os))))))

(defmethod visit-node((writer latex-writer) (text text))
  (part-append (encode writer (as-text text))))

(defmacro visit-docinfo-item(writer node name)
  "Helper macro for docinfo items"
  `(if (setting :use-latex-docinfo ,writer)
       ,(case name
          ((author organization contact address)
           ;; We attach these to the last author.  If any of them
           `(let ((text
                   ,(if (eql name 'address)
                        `(with-modes(,writer :insert-newline)
                           (encode ,writer (as-text ,node)))
                        `(attval ,writer (as-text ,node)))))
              ,(if (eql name 'author)
                   `(progn
                      (push (list text) (author-stack ,writer))
                      (setf (slot-value ,writer 'pdfauthor)
                            (if (slot-value ,writer 'pdfauthor)
                                (concatenate 'string
                                             (slot-value ,writer 'pdfauthor)
                                             ", " text)
                                text)))
                   `(setf (first (author-stack ,writer))
                          (nconc (first (author-stack ,writer)) (list text))))))
          (date
           `(setf (date ,writer) (attval ,writer (as-text ,node)))))
       (with-part(docinfo)
         (part-append
          (format nil "\\textbf{~S}: & "
                  (translated-text ,(string-downcase name) *language*)))
         ,(case name
           (address
            `(with-modes(,writer :insert-newline)
               (part-append "{\\raggedright" #\newline)
               (call-next-method)
               (part-append "}")))
           (t '(call-next-method)))
         (part-append " \\\\" #\newline))))

(defmethod visit-node((writer latex-writer) (node address))
  (visit-docinfo-item writer node address))

(defmethod visit-node((writer latex-writer) (node admonition))
  (part-append (format nil "~%\\begin{admonition}{~A}~%"
                       (translated-text (string-downcase (type-of node))
                                        *language*)))
  (call-next-method)
  (part-append "\\end{admonition}" #\newline))

;; skip authors as author called for each one
(defmethod visit-node((writer latex-writer) (node author))
  (let ((authors (gethash 'author (metadata writer))))
      (setf (gethash 'author (metadata writer))
            (if authors
                (concatenate 'string authors
                             (elt (author-separators *language*) 0)
                             (attval writer (as-text node)))
                (attval writer (as-text node)))))
  (visit-docinfo-item writer node author))

(defmethod visit-node((writer latex-writer) (node block-quote))
  (part-append "\\begin{quote}" #\newline)
  (call-next-method)
  (part-append "\\end{quote}" #\newline))

(defmethod visit-node((writer latex-writer) (node bullet-list))
  (if (equal (slot-value writer 'topic-class) "contents")
      (unless (setting :use-latex-toc writer)
        (part-append "\\begin{list}{}{}" #\newline)
        (call-next-method)
        (part-append "\\end{list}" #\newline))
      (progn
        (part-append "\\begin{itemize}" #\newline)
        (call-next-method)
        (part-append "\\end{itemize}" #\newline))))

(defmethod visit-node((writer latex-writer) (node superscript))
  (part-append "$^{")
  (with-modes(writer :mathmode) (call-next-method))
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node subscript))
  (part-append "$_{")
  (with-modes(writer :mathmode) (call-next-method))
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node math))
    (part-append (as-text (child node 0))))

(defmethod visit-node((writer latex-writer) (node equation))
  (part-append #\newline (as-text (child node 0)) #\newline))

(defmethod visit-node((writer latex-writer) (node caption))
    (part-append "\\caption{")
    (call-next-method)
    (part-append "}"))

(defmethod visit-node((writer latex-writer) (node title-reference))
  (part-append "\\titlereference{")
  (call-next-method)
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node citation))
  (if (setting :use-latex-citations writer)
      (push (nreverse (collect-parts (call-next-method)))
            (slot-value writer 'bibitems))
      (progn
        (part-append
         #\newline
         (format nil "\\begin{figure}[b]~%\\hypertarget{~A}"
                 (attribute node :id)))
        (call-next-method)
        (bookmark writer node)
        (part-append "\\end{figure}" #\newline))))

(defmethod visit-node((writer latex-writer) (node citation-reference))
  (if (setting :use-latex-citations writer)
      (progn
        (part-append "\\cite{")
        (call-next-method)
        (part-append "}"))
      (let ((href
             (or (attribute node :refid)
                 (gethash (attribute node :refname)
                          (nameids (document writer))))))
        (part-append (format nil "[\\hyperlink{~A}{" href))
        (call-next-method)
        (part-append "}]"))))

(defmethod visit-node((writer latex-writer) (node classifier))
  (part-append "(\\textbf{")
  (call-next-method)
  (part-append "})" #\newline))

(defmethod visit-node((writer latex-writer) (node colspec))
  (visit-node (active-table writer) node))

(defmethod visit-node((writer latex-writer) (node comment))
  (part-append "%% "
   (ppcre:regex-replace-all "
" (as-text node) "
%")))

(defmethod visit-node((writer latex-writer) (node contact))
  (visit-docinfo-item writer node contact))

(defmethod visit-node((writer latex-writer) (node copyright))
  (visit-docinfo-item writer node copyright))

(defmethod visit-node((writer latex-writer) (node date))
  (visit-docinfo-item writer node date))

(defmethod visit-node((writer latex-writer) (node definition))
  (part-append "%[Start Definition]" #\newline)
  (call-next-method)
  (part-append "%[End Definition]" #\newline))

(defmethod visit-node((writer latex-writer) (node definition-list))
  (part-append "\\begin{description}" #\newline)
  (call-next-method)
  (part-append "\\end{description}" #\newline))

(defmethod visit-node((writer latex-writer) (node definition-list-item))
  (part-append "%[Start Definition List Item]" #\newline)
  (call-next-method)
  (part-append "%[End Definition List Item]" #\newline))

(defmethod visit-node((writer latex-writer) (node description))
  (part-append #\space)
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node docinfo))
  (with-part(docinfo)
    (part-append "
\\begin{center}
\\begin{tabularx}{\\docinfowidth}{lX}
")
    (call-next-method)
    (part-append "
\\end{tabularx}
\\end{center}
"))
  (with-part(pdfinfo)
    (when (slot-value writer 'pdfauthor)
      (part-append (format nil "\\hypersetup{pdfauthor={~A}" (slot-value writer 'pdfauthor)))
      (setf (slot-value writer 'pdfauthor) nil))
    (when (slot-value writer 'pdfinfo)
      (setf (slot-value writer 'pdfinfo)
            (list (join-strings (nreverse (slot-value writer 'pdfinfo)) ", ")))
      (part-append "}" #\newline)))
  (when (setting :use-latex-docinfo writer)
    (with-part(head)
      (part-append
       (format nil "\\date{~A}~%" (slot-value writer 'date))
       (format nil "\\author{~A}~%"
               (join-strings
                (mapcar
                 #'(lambda(a) (join-strings a "\\\\"))
                 (author-stack writer))
                " \\and "))))))

(defmethod visit-node((writer latex-writer) (node doctest-block))
  (part-append "\\begin{verbatim}" #\newline)
  (with-modes(writer :verbatim)
      (call-next-method))
  (part-append "\\end{verbatim}" #\newline))


(defmethod visit-node((writer latex-writer) (node emphasis))
  (let ((txt "\\emph{"))
    (part-append txt)
    (push txt (literal-block-stack writer))
    (call-next-method)
    (pop (literal-block-stack writer))
    (part-append "}")))

(defmethod visit-node((writer latex-writer) (node entry))
  (let ((table (active-table writer))
        (suffix nil))
    (visit-entry table)
    (if (= 1 (entry-number table))
        ;; if the firstrow is a multirow, this actually is the second row.
        ;; this gets hairy if rowspans follow each other.
        (unless (zerop (rowspan table 0))
          (part-append " & ")
          (visit-entry table))
        (part-append " & "))
    (let ((morerows (attribute node :morerows))
          (morecols (attribute node :morecols)))
      (cond
        ((and morerows morecols)
         (error
         "Cells that span multiple rows and columns are
not supported in Latex"))
        (morerows
         (let ((count (1+ morerows)))
           (setf (rowspan table (1- (entry-number table))) count)
           (part-append
            (format nil "\\multirow{~D}{~S}{" count (column-width table)))
           (push "}" suffix))) ; BUG following rows must have empty cells.
        (morecols
         (let ((bar1 (if (= 1 (entry-number table)) (vertical-bar table) ""))
               (count (1+ morecols)))
           ;; the vertical bar before column is missing if it is the first
           ;; column. the one after always.
           (part-append
            (format nil "\\multicolumn{~D}{~Al~A}{"
                    count bar1 (vertical-bar table)))
           (push "}" suffix)))))
    (when (typep (parent (parent node)) 'thead)
      (part-append "\\textbf{")
      (push "}" suffix))
    (call-next-method)
    (dolist(item (nreverse suffix)) (part-append item))
    ;;  if following row is spanned from above.
    (unless (zerop (or (rowspan table (entry-number table)) 0))
      (part-append " & ")
      (visit-entry table))))

(defmethod visit-node((writer latex-writer) (node row))
  (visit-row (active-table writer))
  (call-next-method)
  (part-append (depart-row (active-table writer))))

(defmethod visit-node((writer latex-writer) (node enumerated-list))
  (let ((enum-suffix (or (attribute node :suffix) "."))
        (enum-prefix (or (attribute node :prefix) ""))
        (enum-type
         (get (attribute node :type)
              '(:arabic "arabic" :loweralpha "alph" :upperalpha "Alph"
                :lowerroman "roman" :upperroman "Roman")
              "arabic")))
    (when (setting :compound-enumerators writer)
      (when (and (setting :section-prefix-for-enumerators writer)
                 (section-numbers writer))
        (setf enum-prefix
              (concatenate 'string enum-prefix
                           (format nil "~{~D.~}"
                                   (reverse (section-numbers writer))))))
      (setf enum-prefix
              (concatenate 'string enum-prefix
                           (format nil "~{~A.~}"
                                   (reverse (enumeration-counters writer))))))
    (let* ((n (1+ (length (enumeration-counters writer))))
          (counter-name (format nil "listcnt~D" n)))
      (push (format nil "\\~A{~A}" enum-type counter-name)
            (enumeration-counters writer))
      (when (> n (max-enum-depth writer))
        (setf (max-enum-depth writer) n)
        (part-append  (format nil "\\newcounter{~A}~%" counter-name)))
      (part-append
       (format nil "\\begin{list}{~A\\~A{~A}~A}~%"
               enum-prefix enum-type counter-name enum-suffix)
       (format nil "{~%\\usecounter{~A}~%" counter-name))
      (let ((start (attribute node :start)))
        (when start
          (part-append (format nil "\\setcounter{~A}{~D}~%"
                               counter-name (1- start)))))
      (part-append "\\setlength{\\rightmargin}{\\leftmargin}}" #\newline)))
  (call-next-method)
  (part-append "\\end{list}" #\newline)
  (pop (enumeration-counters writer)))

(defmethod visit-node((writer latex-writer) (node field))
  (part-append #\newline)
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node field-body))
  (call-next-method))

;;   (if (slot-value writer 'docinfo)
;;       (with-part(docinfo)
;;         (call-next-method)
;;         (part-append "\\\\" #\newline))

;;         (part-append
;;          (format nil "~A \\\\" (as-text node))))
;;       (call-next-method))
;;   (part-append #\newline))

(defmethod visit-node((writer latex-writer) (node field-list))
  (if (eql docutils::*current-writer-part* 'docinfo)
      (call-next-method)
      (progn
        (part-append "\\begin{description}" #\newline)
        (call-next-method)
        (part-append "\\end{description}" #\newline))))

(defmethod visit-node((writer latex-writer) (node field-name))
  (if (eql docutils::*current-writer-part* 'docinfo)
      (part-append (format nil "\\textbf{~A}: & " (encode writer (as-text node))))
      (progn
        (part-append "\\item[")
        (call-next-method)
        (part-append "]"))))

(defmethod visit-node((writer latex-writer) (node figure))
  (let* ((width (when-bind(width (attribute node :width))
                  (car (convert-length-unit width :%))))
         (env (if (and width (> width 50)) "figure*" "figure")))
    (part-append #\newline "\\begin{" env "}" #\newline "\\begin{center}")
    (call-next-method)
    (let ((prev (prev-sibling node)))
      (when (and (typep prev 'target) (attribute prev :id))
        (part-append (format nil "\\label{~A}~%" (attribute prev :id)))))
    (part-append "\\end{center}" #\newline "\\end{" env "}" #\newline)))

(defmethod visit-node((writer latex-writer) (node footer))
  (with-part(footer)
    (part-append #\newline "\\begin{center}\\small" #\newline)
    (call-next-method)
    (part-append #\newline "\\end{center}" #\newline #\newline)))

(defmethod visit-node((writer latex-writer) (node footnote))
  (if (setting :use-latex-footnotes writer)
      (progn
        (part-append
         "\\footnotetext[" (first (split-sequence #\space (as-text node)))
         "]{")
        (call-next-method)
        (part-append "}" #\newline))
      (progn
        (part-append #\newline
         (format nil "\\begin{figure}[b]~%\\hypertarget{~s}"
                 (attribute node :id)))
        (call-next-method)
        (part-append "\\end{figure}" #\newline))))

(defmethod visit-node((writer latex-writer) (node footnote-reference))
  (if (setting :use-latex-footnotes writer)
      (part-append "\\footnotemark[" (encode writer (as-text node)) "]")
      (let ((href (or (attribute node :refid)
                      (gethash (attribute node :refname)
                               (nameids (document writer))))))
        (multiple-value-bind(prefix suffix)
            (ecase (setting :footnote-references writer)
              (:brackets (values "[" "]"))
              (:superscript (values "\\raisebox{.5em}[0em]{\\scriptsize" "}")))
          (part-append prefix (format nil "\\hyperlink{~A}{" href))
          (call-next-method)
          (part-append suffix)))))

(defmethod visit-node((writer latex-writer) (node label))
  (etypecase (parent node)
    (footnote
      (unless (setting :use-latex-footnotes writer)
        (multiple-value-bind(prefix suffix)
            (ecase (setting :footnote-references writer)
              (:brackets (values "[" "]"))
              (:superscript (values "$^{" "}$")))
          (part-append prefix)
          (call-next-method)
          (part-append suffix))))
    (citation
     (unless (setting :use-latex-citations writer)
       (part-append "[")
       (call-next-method)
       (part-append "]")))))

(defmethod visit-node((writer latex-writer) (node header))
  (with-part(prefix)
    (part-append #\newline "\\verb|begin_header|" #\newline)
    (call-next-method)
    (part-append #\newline "\\verb|end_header|" #\newline)))

(defmethod visit-node((writer latex-writer) (node image))
  (push (attribute node :uri) (dependencies writer))
  (let ((pre nil)
        (post
         (list
          (format nil "\\includegraphics[clip=true~@[,scale=~A%~]~@[,width=~A~]~@[,height=~A~]]{~A}"
                  (when-bind(scale (attribute node :scale))
                    (* 100 scale))
                  (when-bind(width (attribute node :width))
                    (latex-length width))
                  (when-bind(height (attribute node :height))
                    (latex-length height))
                  (attribute node :uri)))))
    (when-bind(align (attribute node :align))
      (multiple-value-bind(a b)
          (case align
            (:middle (values "\\raisebox{-0.5\\height}{" "}"))
            (:bottom (values "\\raisebox{-\\height}{" "}"))
            (:center (values "{\\hfill" "\\hfill}"))
            (:left (values "{" "\\hfill}"))
            (:right (values"{\\hfill" "}"))
            (t (values "" "")))
        (push a pre)
        (push b post)))
    (unless (typep (parent node) 'text-element)
      (push #\newline pre)
      (push #\newline post))
    (map 'nil #'part-append pre)
    (map 'nil #'part-append (nreverse post))))

;(defmethod visit-node((writer latex-writer) (node interpretated))
;  (part-append "\\texttt{")
;  (with-modes(:literal) (call-next-method))
;  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node legend))
  (part-append "{\\small ")
  (call-next-method)
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node line))
  (part-append "\\item[] ")
  (call-next-method)
  (part-append #\newline))

(defmethod visit-node((writer latex-writer) (node line-block))
  (part-append
   (if (typep (parent node) 'line-block)
       "\\item[]
\\begin{lineblock}{\\lineblockindentation}"
       "
\\begin{lineblock}{0em}")
   #\newline)
  (call-next-method)
  (part-append "\\end{lineblock}" #\newline))

(defmethod visit-node((writer latex-writer) (node list-item))
  (part-append "\\item {} ")
  (call-next-method)
  (part-append #\newline))

(defmethod visit-node((writer latex-writer) (node literal))
  (part-append "\\texttt{")
  (with-modes(:literal) (call-next-method))
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node literal-block))
  (if (and (setting :use-verbatim-when-possible writer)
           (= 1 (number-children node))
           (typep (child node 0) 'text))
      (progn
        (part-append "\\begin{quote}\\begin{verbatim}" #\newline)
        (with-modes(writer :verbatim) (call-next-method))
        (part-append #\newline "\\end{verbatim}\\end{quote}" #\newline))
      (with-modes(writer :literal-block :insert-non-breaking-blanks)
        (if (open-p (active-table writer))
            (progn
              (part-append #\newline "{\\ttfamily \\raggedright \\noindent"
                         #\newline)
              (call-next-method)
              (part-append #\newline "}" #\newline))
            (progn
              (part-append "\\begin{quote}{\\ttfamily \\raggedright \\noindent"
                           #\newline)
              (call-next-method)
              (part-append #\newline "}\\end{quote}" #\newline))))))

(defmethod visit-node((writer latex-writer) (node meta))
  (part-append "%[visit meta]" #\newline)
  (call-next-method)
  (part-append "%[depart meta]" #\newline))

(defmethod visit-node((writer latex-writer) (node option))
  (unless (eql node (child (parent node) 0))
    (part-append ", "))
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node option-argument))
  (part-append
   (or (attribute node :delimiter) " "))
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node option-group))
  (part-append "\\item[")
  (call-next-method)
  (part-append "] "))

(defmethod visit-node((writer latex-writer) (node option-list))
  (part-append "\\begin{optionlist}{3cm}" #\newline)
  (call-next-method)
  (part-append "\\end{optionlist}"))

(defmethod visit-node((writer latex-writer) (node option-list-item))
  (call-next-method)
  (part-append "\\\\" #\newline))

(defmethod visit-node((writer latex-writer) (node organization))
  (visit-docinfo-item writer node organization))

(defmethod visit-node((writer latex-writer) (node paragraph))
  (let ((index (index (parent node) node)))
    (unless (or (equal (slot-value writer 'topic-class) "contents")
                (and (> index 0)
                     (let ((prev (child (parent node) (1- index))))
                       (and (not (typep prev 'paragraph))
                            (not (typep prev 'compound))))))
      (part-append #\newline)))
  (call-next-method)
  (part-append #\newline))

(defmethod visit-node((writer latex-writer) (node problematic))
  (part-append
   (format nil "~%~%\\color{red}{\\bfseries{\\hyperlink{~A}[\\#~:*~A]} \\href{~A}{"
           (attribute node :id)    (attribute node :refid) ))
  (call-next-method)
  (part-append "}}"))

(defmethod visit-node((writer latex-writer) (node system-message))
  (part-append
   (format nil "~%~%\\color{red}{\\bfseries{~@[\\hyperlink{~A}{}\\#~:*~A}~]"
           (attribute node :id)))
  (call-next-method)
  (part-append
   (format nil "~@[ See:~{ \\href{~A}{\\#~:*~A}~}~]}" (backrefs node))))

(defmethod visit-node((writer latex-writer) (node raw))
  (when (member :latex (attribute node :format))
    (part-append (as-text node))))

(defmethod visit-node((writer latex-writer) (node reference))
  (if (attribute node :refid)
      (if (typep (gethash (attribute node :refid) (docutils:ids (document node))) 'docutils.nodes:section)
          (progn
            (call-next-method)
            (part-append (format nil "~~(Section~~\\ref{~A})"
                                 (attribute node :refid))))
          (part-append (format nil "\\ref{~A})" (attribute node :refid))))
      (let* ((hash-char "\\#"))
        (part-append
         (format nil "\\href{~A"
                 (cond
                   ((attribute node :refuri)
                    (regex-replace "#" (attribute node :refuri) hash-char))
                   ((attribute node :refname)
                    (concatenate 'string hash-char
                                 (gethash (attribute node :refname)
                                          (nameids (document writer)))))
                   ((error "Unknown reference")))))
        (part-append "}{")
        (call-next-method)
        (part-append "}"))))

(defmethod visit-node((writer latex-writer) (node revision))
  (visit-docinfo-item writer node revision))

(defmethod visit-node((writer latex-writer) (node section))
  (push 1 (section-numbers writer))
  (call-next-method)
  (pop (section-numbers writer)))

(defmethod visit-node((writer latex-writer) (node sidebar))
  (part-append
"
\\setlength{\\locallinewidth}{0.9\\admonitionwidth}
\\begin{center}\\begin{sffamily}
\\fbox{\\colorbox[gray]{0.80}{\\parbox{\\admonitionwidth}{
")
  (call-next-method)
  (part-append
   "}}}
\\end{sffamily}\\end{center}
\\setlength{\\locallinewidth}{\\linewidth}
"))

(let ((attribution-formats '(:dash ("---" "")
                             :parentheses ("(" ")")
                             :parens ("(" ")")
                             :none ("" ""))))
(defmethod visit-node((writer latex-writer) (node attribution))
  (multiple-value-bind(prefix suffix)
      (values-list (get
                    (or (attribute node :attribution) :parens)
                    attribution-formats))
    (part-append #\newline "\\begin{flushright}" #\newline prefix)
    (call-next-method)
    (part-append suffix #\newline "\\end{flushright}" #\newline))))

(defmethod visit-node((writer latex-writer) (node status))
  (visit-docinfo-item writer node status))

(defmethod visit-node((writer latex-writer) (node strong))
  (let ((txt "\\texbf{"))
    (part-append txt)
    (push txt (literal-block-stack writer))
    (call-next-method)
    (pop (literal-block-stack writer))
    (part-append "}")))

(defmethod visit-node((writer latex-writer) (node substitution-definition))
  )

(defmethod visit-node((writer latex-writer) (node substitution-reference))
  (error "Substitution reference unimplemented"))

(defmethod visit-node((writer latex-writer) (node subtitle))
  (cond
    ((typep (parent node) 'sidebar)
     (part-append "~\\\\" #\newline "\\textbf{")
     (call-next-method)
     (part-append "}" #\newline "\\smallskip" #\newline))
    (t
     (setf (title writer)
           (format nil "~A\\\\~%\\large{~A}~%"
                   (title writer)
                   (encode writer (as-text node)))))))

(defmethod visit-node((writer latex-writer) (node table))
  (part-append #\newline "\\begin{table}" #\newline)
  (call-next-method)
  (part-append "\\end{table}" #\newline))

(defmethod visit-node((writer latex-writer) (node tgroup))
  (let ((table (active-table writer)))
    (when (open-p table)
      (error "Nested tables are not supported"))
    (open-table table)
    (part-append #\newline (opening table))
    (call-next-method)
    (part-append (closing table) #\newline)
    (close-table table)))

(defmethod visit-node((writer latex-writer) (node target))
  (if (or (attribute node :refuri)
          (attribute node :refid)
          (attribute node :refname))
      (call-next-method)
      (unless (typep (next-sibling node) 'figure)
        (progn
          (part-append (format nil "\\hypertarget{~A}{" (attribute node :id)))
          (call-next-method)
          (part-append "}")))))

(defun ensure-table-preamble(writer)
  (let ((table (active-table writer)))
    (unless (slot-value table 'preamble-written)
      (part-append
       (format nil "{~A}~@[~A~]~%" (col-specs table) (caption table))
       (visit-thead table))
      (setf (slot-value table 'preamble-written) t))))

(defmethod visit-node((writer latex-writer) (node tbody))
  (ensure-table-preamble writer)
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node term))
  (part-append "\\item[")
  (call-next-method)
  (part-append "]"))

(defmethod visit-node((writer latex-writer) (node thead))
  (ensure-table-preamble writer)
  (call-next-method)
  (part-append
   (second (multiple-value-list (visit-thead (active-table writer))))))

(defun bookmark(writer node)
  (when-bind(id (attribute (parent node) :id))
    (part-append (format nil "\\label{~A}~%" id))
    (unless (setting :use-latex-toc writer)
      (let ((l (length (section-numbers writer))))
        (when (> l 0) (decf l))
        (part-append (format nil "\\pdfbookmark[~d]{~s}{~s}~%"
                             l (encode writer (as-text node)) id))))))

(defmethod visit-node((writer latex-writer) (node title))
  (let ((parent (parent node)))
    (typecase parent
      (topic

       (part-append "\\subsubsection*{~\\hfill ")
       (bookmark writer node)
       (call-next-method)
       (part-append "\\hfill ~}" #\newline))
      ((or sidebar admonition)
       (part-append "\\textbf{\\large ")
       (call-next-method)
       (part-append "}" #\newline "\\smallskip" #\newline))
      (table
       (setf (slot-value (active-table writer) 'caption)
             (encode writer (as-text node))))
      (document
       ;; document title
       (let ((txt (encode writer (as-text node))))
         (setf (slot-value writer 'title) txt)
         (when (setting :use-latex-docinfo writer)
           (with-part(head) (part-append (format nil "\\title{~A}~%" txt))))
         (with-part(pdfinfo)
           (part-append (format nil "\\hypersetup{pdftitle={~A}}~%" txt)))))
      (otherwise
       (part-append "

%---------------------------------------------------------------------------
")
       (part-append (format nil "\\~A~:[~;*~]{"
                            (section writer)
                            (setting :use-latex-toc writer)))
       (call-next-method)
       (part-append "}" #\newline)
       (bookmark writer node)))))

(defmethod visit-node((writer latex-writer) (node topic))
  (if (setting :use-latex-toc writer)
      (progn
        (setf (slot-value writer 'topic-class) nil)
        (part-append "\\tableofcontents" #\newline #\newline
                     "\\bigskip" #\newline))
      (progn
        (setf (slot-value writer 'topic-class) (attribute node :class))
        (call-next-method)
        (setf (slot-value writer 'topic-class) nil)
        (part-append #\newline))))

(defmethod visit-node((writer latex-writer) (node docutils.nodes:inline))
  (if (attribute node :class)
      (progn
        (part-append (format nil "\\docutilsrole~A{"
                             (string-downcase (attribute node :class))))
        (call-next-method)
        (part-append "{"))
      (call-next-method)))

(defmethod visit-node((writer latex-writer) (node rubric))
  (part-append "\\rubric{")
  (call-next-method)
  (part-append "}"))

(defmethod visit-node((writer latex-writer) (node transition))
  (part-append "

%___________________________________________________________________________

\\hspace*{\\fill}\\hrulefill\\hspace*{\\fill}

")
  (call-next-method))

(defmethod visit-node((writer latex-writer) (node version))
  (visit-docinfo-item writer node version))
#|

testing

(setq *document*
      (read-document
       #p"/home/willijar/www/clews/tutorials/articles/adsl-transmitter.rst"
       (make-instance 'docutils.parser.rst:rst-reader)))

(write-document (make-instance 'latex-writer) *document* *standard-output*)

|#
