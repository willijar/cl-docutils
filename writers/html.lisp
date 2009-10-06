;;;; Misc string and vector handling for docutils
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; $Id: html.lisp,v 1.14 2007/07/26 13:28:36 willijar Exp willijar $

(defpackage :docutils.writer.html
  (:documentation "HTML with CSS writer for docutils")
  (:use :cl :docutils :docutils.utilities :docutils.nodes)
  (:import-from :docutils #:with-part)
  (:import-from :data-format-validation #:join-strings)
  (:shadowing-import-from :cl #:warning #:error #:inline #:special)
  (:export #:html-writer
           #:head-prefix #:head #:body-pre-docinfo #:docinfo #:body
           #:footer #:fragment #:body-suffix
           #:html-url))

(in-package :docutils.writer.html)

(defvar *section-level* 0)

(defclass html-writer(writer)
  ((stylesheet
    :initform "docutils.css" :initarg :stylesheet
    :documentation "Specify a stylesheet. If a URL put link in output
    HTML file, if a pathname embed stylesheet in output HTML")

   ;; parts of document filled in during translate
   head-prefix
   head
   body-prefix
   body-pre-docinfo
   docinfo
   body
   footer
   body-suffix
   ;; other infor needed during traversal

   (topic-class :initform ""))
  (:default-initargs
      :parts
      '((head-prefix  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
 \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"\>
<head>
")
        head
        (body-prefix "</head>
<body>
")
        body-pre-docinfo
        docinfo
        body
        footer
        (body-suffix  "</body>
</html>")))
  (:documentation "Docutils html writer"))

(defun docutils:write-html(os document)
  (if (streamp os)
      (let ((writer (make-instance 'html-writer)))
        (visit-node writer document)
        (write-document writer document os))
      (with-open-file(s os :direction :output :if-exists :supersede :if-does-not-exist :create)
        (docutils:write-html s document))))

(defmethod write-part((writer html-writer) (part (eql 'fragment)) (os stream))
  (write-part writer 'body-pre-docinfo os)
  (write-part writer 'docinfo os)
  (write-part writer 'body os))

(docutils:register-settings-spec
 `((:stylesheet
    string "default.css"
    "Specify a stylesheet URL, used verbatim.  Default is 'default.css'.")
   (:link-stylesheet boolean t
    "Link to the stylesheet in the output HTML
file. If false stylesheet will be embedded and must be readable")
   (:initial-header-level (integer :min 1 :max 6) 1
    "Specify the initial header level.")

   (:footnote-references
    (member :type symbol :set (:superscript :brackets)) :brackets
    "Format for footnote references: one of :superscript or :brackets.")
   (:footnote-backlinks boolean y "Put backinks to references in footnotes")
    (:attribution
     (member :type (symbol :nil-allowed t) :set (:dash :parentheses nil)) :dash
     "Format for block quote attributions: one of 'dash',
'parentheses' or 'nil'")
    (:compact-lists
     boolean t
     "Remove extra vertical whitespace between items of bullet lists
and enumerated lists, when list items are 'simple' (i.e., all items
each contain one paragraph and/or one 'simple' sublist only)")
   (:image-sizes boolean nil
    "If true images will have their width and height
specified. Default is not to do this (as recommended).")
   (:xml-declaration boolean t "Use XML declaration")))

(defmethod visit-node ((writer html-writer) (document document))
  (let ((*section-level* 0))
    (with-part(head)
      (when (setting :stylesheet writer)
        (part-append
         (if (setting :link-stylesheet writer)
             (format nil
                     "<link rel=\"stylesheet\" href=\"~A\" type=\"text/css\" />~%"
                     (setting :stylesheet writer))
             (with-output-to-string(os)
               (write-line "<style type=\"text/css\">" os)
               (with-open-file(is (setting :stylesheet writer)
                                  :direction :input :if-does-not-exist nil)
                 (when is (copy-stream is os)))
               (write-line "</style>" os)))))
      #+nil(when (or (< (number-children document) 1)
                (not (typep (child document 0) 'title)))
        (part-append "<title></title>" #\newline)))
    (with-part(body-pre-docinfo)
      (part-append (start-tag document "div" '(:class "document"))))
    (with-part(body) (call-next-method))
    (with-part(body-suffix) (part-prepend "</div>" #\newline))))

(defun is-named-tag(tagname)
  (member tagname '("a" "applet" "form" "frame" "iframe" "img" "map")
          :test #'string=))

(defun encode(string)
  "Encode special characters in `text` & return encoded string."
  (with-output-to-string(os)
    (loop for c across string
          do (case c
               (#\~ (write-string "&nbsp;" os))
               (#\& (write-string "&amp;" os))
               (#\< (write-string "&lt;" os))
               (#\> (write-string "&gt;" os))
               (#\Null (write-string "&nbsp;" os))
               (t (write-char c os))))))

(defun attval(string)
  (encode
   (with-output-to-string(os)
    (loop
     for c across (string string)
     do (if (wsp-char-p c) (write-char #\space os)  (write-char c os))))))

(defun start-tag(node tagname &optional attributes (infix "") )
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
  (start-tag node tagname attributes " /"))

(defun set-first-last(node)
  (when (> (number-children node) 0)
    (let ((first (child node 0))
          (last (child node (1- (number-children node)))))
      (when (typep first 'element) (add-class first "first"))
      (when (typep last 'element) (add-class last "last")))))

(defmethod visit-node((writer html-writer) (text text))
  (part-append (encode (as-text text))))

(defmacro def-simple-node(nodetype tagname &optional attributes
                          &key (suffix #\space) )
  (let ((writer (gensym))
        (tagname (string-downcase tagname))
        (node (gensym)))
    `(defmethod visit-node((,writer html-writer) (,node ,nodetype))
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
                          (translated-text ,name) (as-text ,node))))))
    (set-first-last ,node)
    (part-append
     (start-tag ,node "tr")
     ,(format nil "<th class=\"docinfo-name\">~A</th><td>" name))
    ,@body
    (part-append "</td></tr>")))

(defmethod visit-node((writer html-writer) (node address))
  (add-docinfo-item(writer node "Address" :meta nil)
    (part-append (start-tag node "pre"  '(:class "address"))
                  #\newline)
    (call-next-method)
    (part-append "</pre>" #\newline)))

(defmethod visit-node((writer html-writer) (node admonition))
  (let ((name (string-downcase (class-name (class-of node)))))
    (part-append (start-tag node "div" `(:class ,name))
                  #\newline)
    ;;(add-child node (make-node 'title (make-node 'text (translated-text name))) 0)
    (set-first-last node)
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer html-writer) (node attribution))
  (multiple-value-bind(prefix suffix)
      (ecase (setting :attribution writer)
        (:dash (values "&mdash; " ""))
        (:parentheses (values #\( #\)))
        ('nil (values "" "")))
    (part-append
     (start-tag node "p" `(:class "attribution")) prefix)
    (call-next-method)
    (part-append suffix "</p>" #\newline)))

(defmethod visit-node((writer html-writer) (node author))
  (add-docinfo-item(writer node "Author")
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

(defmethod visit-node((writer html-writer) (node bullet-list))
  (let* ((old-compact-simple *compact-simple*)
	 (*compact-p* nil)
	 (*compact-simple*
	  (and (setting :compact-lists writer)
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

(defmethod visit-node((writer html-writer) (node caption))
  (multiple-value-bind(tagname class)
      (etypecase (parent node)
        (figure (values "p" "caption"))
        (table (values "caption")))
    (part-append (start-tag node tagname (when class `(:class ,class))))
    (let ((pid (attribute (parent node) :id)))
      (when pid
        (part-append (start-tag nil "a" `(:name ,pid))))
      (call-next-method)
      (when pid (part-append "</a>")))
    (part-append "</" tagname ">" #\newline)))

;(def-simple-node caption "p" '(:class "caption") :suffix #\newline)

(defmethod visit-node((writer html-writer) (node citation))
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

(defmethod visit-node((writer html-writer) (node citation-reference))
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

(defmethod visit-node((writer html-writer) (node classifier))
  (part-append
   " <span class=\"classifier-delimiter\">:</span> "
   (start-tag node "span"  `(:class "classifier")))
  (call-next-method)
  (part-append "</span>"))

(defmethod visit-node((writer html-writer) (node colspec))
  (error "Colspec not inside a tgroup"))

(defmethod visit-node((writer html-writer) (node comment))
  (part-append
   (format nil "<!-- ~A -->~%"
	   (cl-ppcre:regex-replace-all (load-time-value (cl-ppcre::create-scanner "-(?=-)"))
                                 (as-text node) "- "))))

(defmethod visit-node((writer html-writer) (node compound))
  (let ((n (number-children node)))
    (when (> n 0)
      (add-class (child node 0) "compound-first")
      (add-class (child node (1- n)) "compound-last")
      (dotimes(i (- n 2))
        (add-class (child node (1+ i)) "compount-middle"))))
  (part-append
   (start-tag node "div"
              (list :class (format nil "compound~@[ ~A~]"
                                   (attribute node :class)))))
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node contact))
  (add-docinfo-item(writer node "Contact" :meta nil)
		   (call-next-method)))

(defmethod visit-node((writer html-writer) (node copyright))
  (add-docinfo-item(writer node "Copyright")
		   (call-next-method)))

(defmethod visit-node((writer html-writer) (node date))
  (add-docinfo-item(writer node "Date")
    (call-next-method)))

(defmethod visit-node((writer html-writer) (node definition))
  (part-append "</dt>" #\newline (start-tag node "dd"))
  (set-first-last node)
  (call-next-method)
  (part-append "</dd>" #\newline))

(defmethod visit-node((writer html-writer) (node definition-list))
  (part-append (start-tag node "dl" '(:class "docutils")))
  (call-next-method)
  (part-append "</dl>" #\newline))

(defmethod visit-node((writer html-writer) (node description))
  (part-append (start-tag node "td"))
  (set-first-last node)
  (call-next-method)
  (part-append "</td>"))

(defmethod visit-node((writer html-writer) (node docinfo))
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

(defmethod visit-node((writer html-writer) (node doctest-block))
  (part-append (start-tag node "pre"  '(:class "doctest-block")))
  (call-next-method)
  (part-append "</pre>" #\newline))

(defmethod visit-node((writer html-writer) (node emphasis))
  (part-append "<em>")
  (call-next-method)
  (part-append "</em>"))

(defmethod visit-node((writer html-writer) (node entry))
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

(defmethod visit-node((writer html-writer) (node enumerated-list))
  (let ((atts nil)
	(old-compact-simple *compact-simple*)
	(*compact-p* nil)
	(*compact-simple*
	 (and (setting :compact-lists writer)
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

(defmethod visit-node((writer html-writer) (node field))
  (part-append  (start-tag node "tr" '(:class "field")))
  (call-next-method)
  (part-append  "</tr>" #\newline))

(defmethod visit-node((writer html-writer) (node field-body))
  (part-append  (start-tag node "td" '(:class "field-body")))
  (set-first-last node)
  (call-next-method)
  (part-append  "</td>" #\newline))

(defmethod visit-node((writer html-writer) (node field-list))
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

(defmethod visit-node((writer html-writer) (node field-name))
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

(defmethod visit-node((writer html-writer) (node figure))
  (let ((atts (list :class "figure")))
    (when-bind(width (attribute node :width))
              (setf (getf atts :style)
                    (format nil "width: ~d~:[ps~;~:*~A~];" (car width) (cdr width))))
    (when-bind(align (attribute node :align))
      ;(setf (getf atts :align) (attval align))
      (setf (getf atts :class)
            (concatenate 'string
                         (getf atts :class) " align-" (string align))))
    (part-append (start-tag node "div" atts))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer html-writer) (node footer))
  (with-part(footer)
    (part-append
     "<hr class =\"docutils footer\" />"
     (start-tag node "div" '(:class "footer")))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defmethod visit-node((writer html-writer) (node footnote))
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

(defmethod visit-node((writer html-writer) (node footnote-reference))
  (let ((href
	 (let ((refid (or (attribute node :refid)
			  (gethash (attribute node :refname)
				   (nameids (document node))))))
	   (when refid  (list :href (format nil "#~A" refid))))))
    (multiple-value-bind(prefix suffix)
	(ecase (setting :footnote-references writer)
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

(defmethod visit-node((writer html-writer) (node header))
  (with-slots(head body body-prefix) writer
    (with-part(body-prefix)
       (part-append (start-tag node "div" '(:class "header")))
       (call-next-method)
       (part-append "<hr class=\"docutils header\"/>
</div>
"))))

(defgeneric html-url(writer uri &rest args)
  (:documentation "Resolve an external media reference to a url for
  this writer on the basis of content-type and args. May be
  specialised to, for example, use a media server or automatically do
  conversions etc.")
  (:method((writer html-writer) uri &rest args)
    (declare (ignore args))
    uri))

(defun math-out(writer text)
  (part-append
   (let ((url (html-url writer text :content-type "text/x-eqn")))
     (if url
         (format nil "<img class=\"math\" alt=\"~A\" src=\"~A\" />" text url)
         (format nil " <code class=\"math\">~A</code> " text)))))

(defmethod visit-node((writer html-writer) (node math))
  (math-out writer
            (concatenate
             'string
             "$" (whitespace-normalise-name (as-text (child node 0))) "$")))

(defmethod visit-node((writer html-writer) (node equation))
    (part-append #\newline "<p class=\"equation\">")
    (math-out writer (whitespace-normalise-name
                                 (as-text (child node 0))))
    (when (> (number-children node) 1)
      (part-append
       " <span class=\"equation-label\">"
       (as-text (child node 1)) "</span>"))
    (part-append "</p>" #\newline))

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

(defmethod visit-node((writer html-writer) (node image))
  (let* ((atts nil)
         (uri (attribute node :uri)))
    (with-attributes(k v node) (setf (getf atts k) v))
    (setf (getf atts :src)
          (html-url writer uri
                    :width (attribute node :width)
                    :height (attribute node :height)
                    :angle (attribute node :angle)
                    :scale (or (attribute node :scale) 1.0)))
    (remf atts :uri)
    (if (setting :image-sizes writer)
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
            (set-size height :height)))
        (progn
          (remf atts :scale)
          (remf atts :width)
          (remf atts :height)))
      (unless (getf atts :alt) (setf  (getf atts :alt)  (getf atts :src)))
      (let ((inline-p (typep (parent node) 'text-element)))
        (unless inline-p
          (part-append
           (start-tag nil "div" (image-div-atts node))))
        (part-append (empty-tag node "img" atts))
        (unless inline-p (part-append "</div>" #\newline)))))

(defmethod visit-node((writer html-writer) (node docutils.nodes:inline))
  (when (> (number-children node) 0)
    (part-append (start-tag node "span" nil))
    (call-next-method)
    (part-append "</span>")))

(defmethod visit-node((writer html-writer) (node label))
  (assert (typep (parent node) '(or footnote citation)))
  (part-append (start-tag node "td" '(:class "label")))
  (let ((backrefs (backrefs (parent node)))
	(id (attribute (parent node) :id)))
    (cond ((not (and backrefs (setting :footnote-backlinks writer)))
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

(defmethod visit-node((writer html-writer) (node legend))
  (part-append (start-tag node "div" '(:class "legend"))
                #\newline)
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node line))
  (part-append (start-tag node "div" '(:class "line") ))
  (if (> (number-children node) 0)
      (call-next-method)
      (part-append "<br />"))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node line-block))
  (part-append (start-tag node "div" '(:class "line-block"))
                #\newline)
  (call-next-method)
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node list-item))
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

(defmethod visit-node((writer html-writer) (node literal))
  (part-append (start-tag node "tt" '(:class "docutils literal"))
                #\newline)
  (dolist(token (words-and-spaces (as-text node)))
    (part-append
     (cond
       ((not (line-blank-p token))
        (format nil "<span class=\"pre\">~A</span>"
                (encode token)))
       ((= 1 (length token)) token)
       ((with-output-to-string(os)
          (dotimes(x (1- (length token)))
            (write-string "&nbsp;" os))
          (write-char #\space os))))))
  (part-append "</tt>"))

(defmethod visit-node((writer html-writer) (node literal-block))
  (part-append (start-tag node "pre" '(:class "literal-block"))
                #\newline)
  (call-next-method)
  (part-append #\newline "</pre>" #\newline))

(defmethod visit-node((writer html-writer) (node docutils.nodes::meta))
  (with-part(head)
  (part-append
   (empty-tag node "meta" (slot-value node 'docutils::attributes))
   #\newline)))

(defmethod visit-node((writer html-writer) (node option-group))
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

(defmethod visit-node((writer html-writer) (node option-argument))
  (part-append
                 (or (attribute node :delimiter) #\space)
                 (start-tag node "var"))
  (call-next-method)
  (part-append  "</var>"))

(defmethod visit-node((writer html-writer) (node option-list))
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

(defmethod visit-node((writer html-writer) (node option-list-item))
  (part-append  (start-tag node "tr"))
  (call-next-method)
  (part-append  "</tr>"))

(defmethod visit-node((writer html-writer) (node option-string))
  (part-append  (start-tag node "span" '(:class "option")))
  (call-next-method)
  (part-append  "</span>"))

(defmethod visit-node((writer html-writer) (node organization))
  (add-docinfo-item(writer node "Organisation")
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

(defmethod visit-node((writer html-writer) (node paragraph))
  (let ((compact-p (should-be-compact-p node)))
    (unless compact-p (part-append  (start-tag node "p")))
    (call-next-method)
    (unless compact-p (part-append  "</p>" #\newline))))

(defmethod visit-node((writer html-writer) (node problematic))
  (let ((refid (attribute node :refid)))
    (when refid
      (part-append (format nil "<a href=\"#~A\" name=\"~A\">"
                           refid (attribute node :id))))
    (part-append (start-tag node "span" '(:class "problematic")))
    (call-next-method)
    (part-append "</span>")
    (when refid (part-append  "</a>"))))

(defmethod visit-node((writer html-writer) (node raw))
  (when (member :html (attribute node :format))
    (let ((add-class (when-bind(class (attribute node :class))
		       (not (eq class :none))))
          (type (if (typep (parent node) 'text-element) "span" "div")))
      (when add-class (part-append (start-tag node type)))
      (part-append (as-text node))
      (when add-class (part-append "<" type "/>")))))


(defmethod visit-node((writer html-writer) (node reference))
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
       (start-tag node "a"
                  `(:class "reference"
                    ,@(when-bind(id (attribute node :id)) `(:id ,id))
                    ,@(when href `(:href ,href))))))
    (call-next-method)
    (part-append "</a>")
    (unless inline-p (part-append "</div>" #\newline))))

(defmethod visit-node((writer html-writer) (node revision))
  (add-docinfo-item(writer node "Revision" :meta nil)
                   (call-next-method)))

(defmethod visit-node((writer html-writer) (node row))
  (part-append (start-tag node "tr"))
  (call-next-method)
  (part-append "</tr>" #\newline))

(defmethod visit-node((writer html-writer) (node rubric))
  (part-append (start-tag node "p" '(:class "rubric")))
  (call-next-method)
  (part-append "</p>" #\newline))

(defmethod visit-node((writer html-writer) (node section))
  (let ((*section-level* (1+ *section-level*)))
    (part-append (start-tag node "div" '(:class "section")))
    (call-next-method)
    (part-append "</div>" #\newline)))

(defvar *in-sidebar* nil)

(defmethod visit-node((writer html-writer) (node sidebar))
  (part-append (start-tag node "div" '(:class "section")))
  (set-first-last node)
  (let ((*in-sidebar* t)) (call-next-method))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node status))
  (add-docinfo-item(writer node "Status" :meta nil)
                   (call-next-method)))

(defmethod visit-node((writer html-writer) (node strong))
  (part-append "<strong>")
  (call-next-method)
  (part-append "</strong>"))

(defmethod visit-node((writer html-writer) (node subscript))
  (part-append (start-tag node "sub"))
  (call-next-method)
  (part-append "</sub>"))

(defmethod visit-node((writer html-writer) (node substitution-definition)))


(defmethod visit-node((writer html-writer) (node subtitle))
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

(defmethod visit-node((writer html-writer) (node superscript))
  (part-append (start-tag node "sup"))
  (call-next-method)
  (part-append "</sup>"))

(defmethod visit-node((writer html-writer) (node system-message))
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
             line backref-text))))

(defmethod visit-node((writer html-writer) (node table))
  (let ((atts (list :class "docutils")))
    (when-bind(width (attribute node :width))
      (setf (getf atts :style)
            (format nil "width: ~d~:[ps~;~:*~A~];" (car width) (cdr width))))
    (when-bind(class (attribute node :class))
      (setf (getf atts :class) class))
    (when-bind(align (attribute node :align))
      (setf (getf atts :class)
            (concatenate 'string
                         (getf atts :class) " align-" (string align))))
    (part-append (start-tag node "table" atts)))
  (call-next-method)
  (part-append "</table>" #\newline))

(defmethod visit-node((writer html-writer) (node target))
  (let ((a (or (attribute node :refuri)
               (attribute node :refid)
               (attribute node :refname))))
    (when (not a)
      (part-append
       (start-tag node "a" `(:class "target" :name ,a))
       "</a>"))))

(defmethod visit-node((writer html-writer) (node tbody))
    (part-append (start-tag node "tbody" '(:valign "top" )))
    (call-next-method)
    (part-append "</tbody>" #\newline))

(defmethod visit-node((writer html-writer) (node term))
  (part-append (start-tag node "dt" ))
  (call-next-method)
  ;; Leave the end tag to `self.visit_definition()`, in case there's a
  ;; classifier
  )

(defmethod visit-node((writer html-writer) (node tgroup))
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

(defmethod visit-node((writer html-writer) (node thead))
  (part-append (start-tag node "thead" '(:valign "bottom" )))
  (call-next-method)
  (part-append "</thead>" #\newline))

(defmethod visit-node((writer html-writer) (node title))
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
                         (setting :initial-header-level writer))))
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
             (admonition (values "p" "admonition-title")))
         (part-append (start-tag node tagname (when class `(:class ,class))))
         (let ((pid (attribute (parent node) :id)))
           (when pid
             (part-append (start-tag nil "a" `(:name ,pid))))
           (call-next-method)
           (when pid (part-append "</a>")))
         (part-append "</" tagname ">" #\newline)))))

(defmethod visit-node((writer html-writer) (node docutils.nodes:title-reference))
  (part-append (start-tag node "cite"))
  (call-next-method)
  (part-append "</cite>"))

(defmethod visit-node((writer html-writer) (node topic))
  (part-append (start-tag node "div" '(:class "topic")))
  (let ((*topic-class* (attribute node :class)))
    (call-next-method))
  (part-append "</div>" #\newline))

(defmethod visit-node((writer html-writer) (node transition))
  (part-append (empty-tag node "hr" '(:class "docutils"))))

(defmethod visit-node((writer html-writer) (node version))
  (add-docinfo-item(writer node "Version" :meta nil)
                   (call-next-method)))

(defmethod visit-node ((writer html-writer) (node docutils.nodes:evaluateable))
  (if (eql (output-format node) :html)
        (part-append (evaluate node))
        (call-next-method)))


