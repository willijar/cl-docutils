;; $Id: directives.lisp,v 1.11 2007/07/26 13:26:51 willijar Exp $
;; Directives API and base directive definitions
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the Common Lisp Docutils package

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :docutils.parser.rst)

(defstruct directive
  "Directive Specification"
  (name "" :type string)
  (arguments nil :type list)
  (allow-spaces-p t :type boolean)
  (options nil :type list)
  (content-p nil :type boolean)
  (function))

(defmacro def-directive(name (nodevar &rest lambda-list) &body body)
  "Define a directive handler for directive named name. lambda-list is a
directive lambda-list as follows
lambda-list::= ({var | (var [specializer])}*
                [&allow-spaces]
                [&option {var | (var [specializer])}* ]
                [{{&content {var}}]
                [{&parsed-content {var} [{kwargs}]] )"
  (let ((allow-spaces-p nil)
        (options-spec nil)
        (args-spec nil)
        (options-var nil)
        (content-var nil)
        (content-parser nil)
        (state :args))
    (flet ((is-vardef(item)
             (or (and (listp item) (symbolp (first item)))
                 (and (symbolp item) (not (eql (char (string item) 0) #\&)))))
           (varname(s) (if (listp s) (car s) s)))
      (dolist(arg lambda-list)
        (unless (or (symbolp arg) (listp arg))
          (error "Invalid definition argument ~S" arg))
        (case arg
          (&content (setf state :content))
          (&content-parser (setf state :parse-content))
          (&all-options (setf state :options))
          (t
           (ecase state
             (:args
              (case arg
                (&allow-spaces
                 (setf allow-spaces-p t
                       state :allow-spaces))
                (&option (setf state :option))
                (:allow-other-options
                 (error ":allow-other-option must occur after :option"))
                (t (if (is-vardef arg)
                       (push arg args-spec)
                       (error "Invalid variable definition ~S" arg)))))
             (:allow-spaces
              (case arg
                (&option (setf state :option))
                (t (error "Argument ~S out of place" arg))))
             (:option
              (if (is-vardef arg)
                  (push arg options-spec)
                  (error  "Argument ~S out of place" arg)))
             (:options
              (if (and (is-vardef arg) (not options-var))
                  (setf options-var arg)
                  (error  "Argument ~S out of place" arg))
              (setf state nil))
             (:parse-content
              (unless (symbolp arg)
                (error "Invalid content parser definition ~S" arg))
              (when content-parser
                (error "Wrong number of content parser arguments"))
              (setf content-parser arg))
             (:content
              (unless (symbolp arg) (error "Invalid content spec ~S" arg))
              (when content-var (error "Wrong number of content arguments"))
              (setf content-var arg) )))))
      (setf args-spec (nreverse args-spec)
            options-spec (nreverse options-spec))
      (let ((nodevarname (or nodevar (gensym)))
            (gcontent (or content-var (gensym)))
            (gparser (or content-parser (gensym)))
            (options  (gensym))
            (goptions-var (or options-var (gensym)))
            (arguments (gensym)))
        `(setf (get-dictionary ,(string-downcase name) *directives*)
          (make-directive
           :name ,(string-downcase name)
           :arguments ',args-spec
           :allow-spaces-p ,allow-spaces-p
           :options ',options-spec
           :content-p ,(when (or content-var content-parser) t)
           :function
           #'(lambda(,nodevarname ,arguments ,options ,gcontent
                     ,gparser ,goptions-var)
               ,@(unless nodevar
                         `((declare (ignore ,nodevarname))))
               ,@(unless content-parser
                         `((declare (ignore ,gparser))))
               ,@(unless options-var
                         `((declare (ignore ,goptions-var))))
               ,@(unless content-var
                         `((declare (ignore ,gcontent))))
               (multiple-value-bind(,@(mapcar #'varname args-spec))
                   (values-list (mapcar #'second ,arguments))
                 (multiple-value-bind(,@(mapcar #'varname options-spec))
                     (values-list (mapcar #'second ,options))
                   ,@body)))))))))

(defmacro def-admonition(name &optional
                         (node-class (intern (string-upcase name))))
  (let ((classname (format nil "admonition-~A" (string-downcase name))))
    `(def-directive ,name (parent
                           &content content
                           &content-parser parser)
      (if content
          (let ((admonition
                 (make-node ',node-class :class ,classname)))
            (add-child parent admonition)
            (funcall parser admonition))
          (report :error
                  ,(format nil "The ~s admonition is empty; content required."
                           name))))))

(def-admonition "danger")
(def-admonition "attention")
(def-admonition "caution")
(def-admonition "error")
(def-admonition "hint")
(def-admonition "important")
(def-admonition "note")
(def-admonition "tip")
(def-admonition "warning")

(def-directive admonition(parent title &allow-spaces
                                 &option (classname class "admonition")
                                 &content content
                                 &content-parser parser)
  (if content
      (let ((admonition (make-node 'admonition :class classname)))
        (add-child parent admonition)
        (when title
          (let ((titlenode (make-node 'title)))
            (add-child admonition titlenode)
            (add-child titlenode (parse-inline rst-patterns title))))
        (funcall parser admonition))
      (report :error "The admonition is empty; content required.")))


(defmethod parse-input((spec (eql 'align)) input &key &allow-other-keys)
  (if (member input '("top" "middle" "bottom" "left" "center" "right")
              :test #'string-equal)
      input
      (invalid-input input "Not a valid alignment argument")))

(defmethod parse-input((spec (eql 'class)) input &key &allow-other-keys)
  (when (jarw.parse::is-nil-string input)
    (invalid-input input "Class argument required but none supplied"))
  (join-strings
   (mapcar
    #'(lambda(s)
        (let ((id (make-id s)))
          (if (jarw.parse::is-nil-string s)
              (invalid-input input
                             (format nil "cannot make ~S into a class name" s))
              id)))
    (split-string input))))

;; figure sizes are numbers
;; if 0<=x<=1  it is a fraction of the natural size (text width or height)
;; if >1 then it is number of pixels. This is deprecated

(defmethod parse-input((spec (eql 'figwidth)) input &key &allow-other-keys)
  (if (string-equal input "image")
      :image
      (parse-input '(number :min 0) input)))

;; Lengths are either a number of pixels >1 or 0<=x<=1.0 for a fraction

(defmethod parse-input((spec (eql 'length)) input &key  &allow-other-keys)
   "Parser for entity sizes - returns a cons of a number and units"

   (multiple-value-bind(value pos) (parse-integer input :junk-allowed t)
     (unless (and value (> value 0))
       (invalid-input input "Size must be an non-negative integer"))

     (let ((unit (when (< pos (length input))
                   (let ((s (strip (subseq input pos))))
                     (when (find #\' input)
                       (invalid-input s "Invalid length unit"))
                     (intern (string-upcase s) :keyword)))))
       (length-unit unit) ;; called for side affect of checking unit
       (cons value unit))))

(defmethod parse-input((spec (eql 'scale)) input &key  &allow-other-keys)
  (let ((jarw.media:*length-units* '((:% . 75/8)
                                      (nil . 1))))
    (let ((v (parse-input 'length input)))
      (if (eql (cdr v) :%) (/ (car v) 100.0) (car v)))))

(defun make-image-nodes(uri alt height width scale align
                        target class)
  (let ((image-node (make-node 'image)))
    (when (find #\space uri)
      (report :error "Image URI contains whitespace"
              :line *current-line-number*))
    (if alt
        (setf (attribute image-node :alt) alt)
        (report :warning "Image should have an alt option"
                :line *current-line-number*))
    (map nil #'(lambda(k v) (when v (setf (attribute image-node k) v)))
         '(:height :width :scale :align :class)
         (list height width scale align class))
    (setf (attribute image-node :uri) uri)
    (list
     (if target
         (let ((ref
                (multiple-value-bind(type data)
                    (parse-target (split-lines (escape2null target)))
                  (ecase type
                    (:refuri (make-node 'reference :refuri data))
                    (:refname (make-node 'reference
                                         :refname
                                         (normalise-name data)))))))
           (add-child ref image-node)
           ref)
         image-node))))

(def-directive image(parent uri &option alt
                            (height length)
                            (width length)
                            (scale scale)
                            (align align)
                            target
                            (class class))
  (add-child
   parent
   (make-image-nodes uri alt height width scale align target class)))

(def-directive figure(parent uri
                             &option alt
                             (height length)
                             (width length)
                             (scale scale)
                             (align align)
                             target
                             (class class)
                             (figwidth figwidth)
                             (figclass class)
                             &content content
                             &content-parser parser)
  (let ((figure-node (make-node 'figure)))
    (when figwidth (setf (attribute figure-node :width) figwidth))
    (when figclass (setf (attribute figure-node :class) figclass))
    (when align (setf (attribute figure-node :align) align))
    (add-child figure-node
               (make-image-nodes uri alt height width scale
                                 "center" target class))
    (when content
      (let ((node (make-node 'element))
            (legend nil)
            (firstp t))
        (funcall parser node)
        (with-children(child node :copy t)
          (rem-child node child)
          (if firstp
              (typecase child
                (docutils.nodes:paragraph
                 (let ((caption (make-node 'caption)))
                   (add-child figure-node caption)
                   (with-children(grandchild child :copy t)
                     (rem-child child grandchild)
                     (add-child caption grandchild))
                   (setf firstp nil)))
                (docutils.nodes:comment )
                (t (report
                    :error "Figure caption must be a paragraph or empty comment.")))
              (progn
                (unless legend (setf legend (make-node 'legend)))
                (add-child legend child))))
        (when legend (add-child figure-node legend))))
    (add-child parent figure-node)))

(def-directive equation(parent &content content)
  (let ((node (make-node 'equation)))
    (add-child node (join-strings content #\newline))
    (add-child parent node)))

(def-directive replace(parent &content content &content-parser parser)
  (cond
    ((not (typep parent 'docutils.nodes:substitution-definition))
     (report :error
             "Invalid context: the \"replace\" directive can only be used with a substitution definition"
             :data content))
    ((not content)
     (report :error "The \"replace\" directive is empty; content required"))
    (t
     (funcall parser parent)

     (cond
       ((and (= (number-children parent) 1)
             (typep (child parent 0) 'docutils.nodes:paragraph))
        (let ((p (child parent 0)))
          (with-children(child p)
            (rem-child p child)
            (add-child parent child))
          (rem-child parent 0)))
       ((with-children(node parent :copy t)
          (if (typep node 'docutils.nodes:system-message)
              (rem-attribute node :backrefs)
              (rem-child parent node)))
        (with-reports-to-node(parent)
          (report :error
                  "Error in \"replace\" directive: may contain a single paragraph only")))))))

;; ------------------------
;; HTML specific directive
;; ------------------------

(defun extract-name-value(line)
  "Return a list of (:name value) from a line of the form name=value"
  (let ((attlist nil))
    (loop
     (when (= 0 (length line)) (return attlist))
     (let* ((equals (position #\= line)))
       (unless equals (invalid-input line "missing '=''"))
       (let ((attname (strip (subseq line 0 equals)))
             (data nil))
         (when (= 0 (length attname))
           (invalid-input line "missing attribute before '='"))
         (setf line (string-left-trim +wsp+ (subseq line (1+ equals))))
         (when (= 0 (length line))
           (invalid-input line
                          (format nil "missing value after ~s" attname)))
         (let ((q (char line 0)))
           (if (find q "\"'")
               (let ((endquote (position q line :start 1)))
                 (unless endquote
                   (invalid-input
                    line
                    (format nil "attribute ~S missing end quote ~S"
                            attname q)))
                 (when (and (< (1+ endquote) (length line))
                            (not (member (char line (1+ endquote)) +wsp+)))
                   (invalid-input line
                                  (format nil "attribute ~s end quote ~s not followed by whitespace" attname q)))
                 (setf data (subseq line 1 endquote)
                       line (subseq line (min (1+ endquote) (length line)))))
               (let ((space (position #\space line)))
                 (if space
                     (setf data (subseq line 0 space)
                           line (string-left-trim +wsp+
                                                  (subseq line (1+ space))))
                     (setf data line
                           line "")))))
         (push (cons (intern (string-upcase attname) :keyword) data)
               attlist))))))

(defclass docutils.nodes::meta(docutils.nodes:special
                               docutils.nodes:prebibliographic
                               docutils.nodes:element)())

(defmethod allowed-child-p((parent docutils.nodes::meta) child &optional index)
  (declare (ignore child index))
  nil)

(defclass meta-body(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'field-marker))))

(defmethod field-marker((state meta-body) match)
  (let ((name (match-group match 0))
        (indented (get-indented (state-machine state)
                                :first-indent (match-end match))))
    (if (not indented)
        (report :info `("No content for meta tag ~S" ,name)
                :data indented)
        (let ((node (make-node 'meta))
              (tokens (split-sequence #\space name :remove-empty-subseqs t)))
          (add-child (parent state) node)
          (setf (attribute node :content) (join-strings indented))
          (handler-case
              (let ((attlist (extract-name-value (first tokens))))
                (setf (attribute node (caar attlist)) (cdar attlist)))
            (invalid-input(c)
              (declare (ignore c))
              (setf (attribute node :name) (first tokens))))
          (dolist(token (rest tokens))
            (handler-case
                (let ((attlist (extract-name-value token)))
                  (setf (attribute node (caar attlist)) (cdar attlist)))
              (invalid-input(c)
                (report :error
                        `("Error parsing meta tag attribute ~S: ~S"
                          ,token ,c)
                        :data indented)
                (return)))) ))
    nil))



;;; Processing for meta changed from origianal docutils - meta elements
;;; are not conditional on output format as this isn't known during parsing

(def-directive meta(parent &content content &content-parser parser)
  (if (not content)
      (report :error "Empty meta directive" :data content)
      (funcall parser parent
               :initial-state 'meta-body
               :states '(meta-body)))
  nil)

;;---------------
;; Document Parts
;;---------------

(def-directive contents(parent title-text &allow-spaces &option
                               (depth (integer :min 0))
                               (local boolean nil)
                               (backlinks (member :type (symbol :nil-allowed t)
                                                  :set '(:top :entry nil)))
                               (class class))
  (let* ((label (translated-text "contents" (language parent)))
         (title (cond
                  (title-text
                   (make-node 'title (parse-inline rst-patterns title-text )))
                  ((not local) (make-node 'title label))))
         (topic (make-node 'topic :class "contents"))
         (name (normalise-name (if title (as-text title) label)))
         (pending (make-node 'pending
                             :depth depth :backlinks backlinks :local local)))
    (add-child parent topic)
    (when class (add-class topic class))
    (when title (add-child topic title))
    (add-child topic pending)
    (add-transform (make-instance 'docutils.transform:contents :node pending))
    (unless (named-node (document parent) name)
      (setf (attribute topic :name) name)))
  nil)

(def-directive sectnum(parent &option
                              (depth (integer :min 0))
                              (start (integer :min 0))
                              (prefix string)
                              (suffix string))
  (let ((pending (make-node 'pending)))
    (setf (attribute pending :depth) depth
          (attribute pending :start) start
          (attribute pending :prefix) prefix
          (attribute pending :suffix) suffix)
    (add-child parent pending)
    (add-transform (make-instance 'docutils.transform:sectnum :node pending)))
  nil)

(def-directive class(parent (class class))
  (let ((pending (make-node 'pending)))
    (add-child parent pending)
    (setf (attribute pending :class) class)
    (add-transform
     (make-instance 'docutils.transform:class-attribute :node pending)))
  nil)

(def-directive target-notes(parent)
  (let ((pending (make-node 'pending)))
    (add-child parent pending)
    (add-transform
     (make-instance 'docutils.transform:target-notes :node pending)))
  nil)

;;---------------
;; Body
;;---------------

(defun topic(name parent title subtitle class content parser &optional
             (nodeclass 'docutils.nodes:topic))
  (unless content
    (report :warning
            `("Content block expected for the ~S directive; none found."
              ,name))
    (return-from topic))
  (let ((topic (make-instance nodeclass))
        (titlenode (make-node 'title)))
    (add-child parent topic)
    (add-child topic titlenode)
    (add-child titlenode (parse-inline rst-patterns title))
    (when subtitle
      (let ((titlenode (make-node 'subtitle)))
        (add-child topic titlenode)
        (add-child titlenode (parse-inline rst-patterns title))))
    (when class (add-class topic class))
    (when content (funcall parser topic))))

(def-directive topic(parent title &allow-spaces
                            &option (class class)
                            &content content
                            &content-parser parser)
  (topic "topic" parent title nil class content parser))

(def-directive sidebar(parent title &allow-spaces
                              &option subtitle (class class)
                              &content content
                              &content-parser parser)
  (topic "sidebar" parent title subtitle class content parser
         'docutils.nodes:sidebar))

(def-directive rubric(parent title &allow-spaces
                             &option (class class))
  (let ((rubric (make-node 'rubric)))
    (add-child parent rubric)
    (when class (add-class rubric class))
    (add-child rubric (parse-inline rst-patterns title))))

(def-directive epigraph(parent &content content)
  (let ((node (make-node 'block-quote :class "eipgraph")))
    (add-child parent node)
    (add-child node content)))

(def-directive highlights(parent &content content)
  (let ((node (make-node 'block-quote :class "highlights")))
    (add-child parent node)
    (add-child node content)))

(def-directive compound(parent &option (class class)
                               &content content &content-parser parser)
  (if (find-if-not #'line-blank-p content)
      (let ((node (make-node 'compound)))
        (add-child parent node)
        (when class (add-class node class))
        (funcall parser node))
      (report :error "The 'compound' directive is empty; content required"
              :data content)))

(def-directive parsed-literal(parent &option (class class) &content content)
  (let ((node (make-node
               'literal-block
               (parse-inline rst-patterns (join-strings content #\newline)) )))
    (add-child parent node)
    (when class (add-class node class))))

;;;; misc

(defmethod (setf attribute) (format (element docutils.nodes:raw)
                             (label (eql ':format)))
  (call-next-method
   (mapcar #'(lambda(s) (intern (string-upcase s) :keyword))
           (split-string format nil '(#\space #\newline)
                         :remove-empty-subseqs t))
   element
   label))

(def-directive raw(parent format &allow-spaces
                          &option (file (pathname :must-exist)) url
                          &content content)
  (let ((node (make-node 'raw))
        (blocktext (join-strings content #\newline))
        (text nil))
    (setf (attribute node :format) format)
    (cond
      (content
       (if (or file url)
           (report
            :error
            "'raw' directive must not both specify an external file and have content"
            :data blocktext)
           (setf text blocktext)))
      (file
       (if url
           (report :error "'raw' directive must not both url and file options"
                   :data blocktext)
           (with-open-file(is file :direction :input)
             (setf (attribute node :source) file)
             (setf text
                   (with-output-to-string(os)
                     (jarw.io:copy-stream is os))))))
      (url (setf (attribute node :source) url)
           ;;(setf text (inet.http::url-retrieve :http (inet.uri:url url)))
           (report :severe "URL reading not implemented"))
      (t (report :error "The 'raw' directive requires content; non supplied")))
    (when text
      (add-child node text)
      (add-child parent node))))

(def-directive include(parent (pathname pathname) &option (literal boolean))
  (when-bind(docpath (setting :source-path *document*))
    (setf pathname (merge-pathnames pathname docpath)))
  (if (probe-file pathname)
      (with-open-file(is pathname :direction :input)
        (let ((lines nil))
          (do ((line (read-line is nil) (read-line is nil)))
              ((not line))
            (push line lines))
          (let((textblock (make-array (length lines)
                                      :element-type 'string :initial-contents
                                      (nreverse lines))))
            (if literal
                (add-child parent (make-node 'literal-block textblock))
                (signal 'insert-lines :text-block textblock) ))))
      (report :severe `("Included file ~S not found" ,pathname))))

(def-directive role(nil argument
                        &allow-spaces
                        &all-options options
                        &content content)
  ;;;; Dynamically create and register a custom interpreted text role.
  (block role
    (flet((report(msg)
            (report :error msg :data argument)
            (return-from role)))
      (unless argument
        (report "'role' directive requires arguments on the first line"))
      (let ((match (match '(:sequence (:greedy-repetition 0 nil wsp)
                            (:register simplename)
                            (:greedy-repetition 0 nil wsp)
                            #\(
                            (:greedy-repetition 0 nil wsp)
                            (:register simplename)
                            (:greedy-repetition 0 nil wsp)
                            #\)
                            (:greedy-repetition 0 nil wsp)
                            :end-anchor)
                          argument)))
        (unless match
          (report "'role' directive requires arguments not valid role names"))
        (let* ((new-role-name (match-group match 0))
               (base-role-name (match-group match 1))
               (base-role
                (if (> (length base-role-name) 0)
                    (or (gethash base-role-name *interpreted-roles*)
                        (report `("Unknown interpreted text role ~s"
                                  ,base-role-name)))
                    (make-instance 'generic-custom-role))))
          (unless (slot-value base-role 'options)
            (report `("Supplemental directive arguments for raw
directive not supported (specified by ~s role) " base-role-name)))
          (setf (gethash new-role-name *interpreted-roles*)
                (make-instance 'custom-role
                               :base-role base-role
                               :name new-role-name
                               :option-values (parse-options
                                               (slot-value base-role 'options)
                                               options)
                               :content content)))))))

(def-directive container(parent argument &allow-spaces &content-parser parser)
  (let ((node (make-node 'container)))
    (add-child parent node)
    (add-class node argument)
    (funcall parser node)))


