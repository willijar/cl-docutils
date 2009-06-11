;;; Docutils document tree element classes and implementation
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; $Id: nodes.lisp,v 1.28 2007/07/26 08:56:05 willijar Exp willijar $

(in-package :docutils)

(defvar *current-line-number* -1
  "Current line number - used for line in node creation")

(defvar *document* nil
  "Document (root element) currently being operated on")

;;;; node interface
(defgeneric document(node)
  (:documentation "Return the document root of the tree containing this node"))

(defgeneric parent(node)
  (:documentation
   "Back-reference to the Node immediately containing this Node."))

(defgeneric language(node)
  (:documentation "Language for this node"))

(defgeneric as-text(element)
  (:documentation "Return text representation of an element as a string"))

(defgeneric (setf as-text)(value element)
  (:documentation "Set the text value of a node (if allowed)"))

(defgeneric line(element)
  (:documentation
   "The line number (1-based) of the beginning of this Node in `source`."))

(defgeneric tagname(element)
  (:documentation "The element generic identifier."))

(defgeneric as-sexp(element)
  (:documentation "Return lisp s-exp representation of an element"))

(defgeneric attribute(node label)
  (:documentation "Return an attribute of a node corresponding to label"))

(defgeneric (setf attribute)(value node label)
  (:documentation  "Set a named attribute of an element"))

(defgeneric rem-attribute(node label)
  (:documentation "Remove an attribute from a node"))

(defgeneric make-node(node-type &rest attributes)
  (:documentation "Make a node with given initialisers"))

(defgeneric child(parent index)
  (:documentation "Return a child at position index"))

(defgeneric index(parent child)
  (:documentation "Return the posiiton of child in element"))

(defgeneric add-child(parent child &optional index)
  (:documentation "Add child as a child node of node, either inserting
it at position index or appending it to the end if no index is
specified (default)"))

(defgeneric rem-child(parent child-ref)
  (:documentation "Remove a child element from parent"))

(defgeneric remove-node(node &optional index)
  (:documentation "Remove a node"))

(defgeneric number-children(node)
  (:documentation "Reutrn the  number of children a node has"))

(defgeneric allowed-child-p(parent node &optional index)
  (:documentation "Return true if node is allowed as a child of parent"))

(defmethod make-node((node-type symbol) &rest contents)
  (let ((node (make-instance node-type)))
    (apply #'make-node (cons node contents))
    node))

(defclass node()
  ((line :type integer :initarg :line :reader line
	 :initform *current-line-number*
	 :documentation
	 "The line number (1-based) of the beginning of this Node in source.")
   (parent :type node :reader parent
	   :documentation "The parent of this node in the document tree"))
  (:documentation "Abstract base class of nodes in a document tree"))

(defmethod print-object((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~@[line ~D ~]"  (line node))))

(defmethod document((node node)) (or *document* (document (parent node))))

(defmethod namespace((node node))
  (or (attribute node :namespace) (namespace (parent node))))

(defmethod language((node node)) (language (parent node)))

;(defmethod attribute((node node) (label (eql :line))) (line node))

;(defmethod (setf attribute)(value (node node) (label (eql :line)))
;  (setf (slot-value node 'line) value))

(defmethod tagname((node node))
  (string-downcase (class-name (class-of node))))

(defgeneric setup-child(node chld &optional index)
  (:documentation "Setup up a child after adding to (arent at index")
  (:method ((node node) (child node) &optional index)
    (declare (ignore index))
    (when (slot-boundp child 'parent)
      (error "Unable to setup node ~A as it already has a parent." child))
    (setf (slot-value child 'parent) node)))

(defclass text(node)
  ((text :type vector :initform "" :initarg :text))
  (:documentation "Instances are terminal nodes (leaves) containing
text only; no child nodes or attributes."))

(defmethod make-node((node (eql 'text)) &rest attributes)
  (let ((text (first attributes)))
    (if (stringp text)
        (make-instance 'text :text text)
        (apply #'make-instance (cons 'text  attributes)))))

(defgeneric copy-of-node(node)
  (:documentation "Deep copy a node")
  (:method(node) (make-instance (type-of node))))

(defmethod copy-of-node((node node))
  (let ((copy (call-next-method)))
    (setf (slot-value copy 'line) (slot-value node 'line))
    copy))

(defmethod copy-of-node((node text))
  (let ((copy (call-next-method)))
    (setf (slot-value copy 'text) (slot-value node 'text))
    copy))

(defmethod copy-of-node((node string)) node)

(defmethod as-text((node text)) (slot-value node 'text))
(defmethod as-sexp((node text)) (slot-value node 'text))
(defmethod (setf as-text)((value string) (node text))
  (setf (slot-value node 'text) value))

(defclass element(node)
  ((children
    :initform nil
    :documentation "List of child nodes (elements and/or `Text`).")
   (attributes
    :initarg :attributes
    :initform nil
    :documentation "p-list of attributes associated with this node."))
  (:documentation
   "Abstract base to all specific elements containing attributes and
child nodes."))

(defmethod print-object((node element) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~@[~S ~]~@[line ~D ~]~:[~;~D children~]"
	    (attribute node :name)
	    (line node)
	    (> (number-children node) 0)
	    (number-children node))))


(defmethod make-node ((node element) &rest contents)
  (do((item (pop contents) (pop contents)))
     ((not contents))
    (etypecase item
      (symbol (case item
                (:class (add-class node (pop contents)))
                (:line (setf (slot-value node 'line) (pop contents)))
                (t (setf (attribute node item) (pop contents)))))
      (node (add-child node item))
      (list (setf contents (append item contents)))))
  node)

(defgeneric child-text-separator(element)
  (:documentation "Return separator for child nodes in as-text")
  (:method ((element element))
    "

"))

(defmethod copy-of-node((node element))
  (let ((copy (call-next-method)))
    (setf (slot-value copy 'attributes)
	  (copy-list (slot-value node 'attributes)))
    (dolist(child (slot-value node 'children))
      (add-child copy (copy-of-node child)))
    copy))

(defun move-children(from-element to-element)
  "Move the children from one node to another. Returns the list of
children in to-element"
  (check-type from-element element)
  (check-type to-element element)
  (dolist(child (slot-value from-element 'children))
    (slot-makunbound child 'parent)
    (add-child to-element child))
  (setf (slot-value from-element 'children) nil)
  (slot-value to-element 'children))

(defmethod language((node element))
  (or (attribute node :language) (call-next-method)))

(defclass text-element(element)
  ()
  (:documentation "An element which directly contains text.
    Its children are all `Text` or `TextElement` subclass nodes"))

(defmethod make-node((node text-element) &rest contents)
  (do((item (pop contents) (pop contents)))
     ((not (or contents item)))
    (etypecase item
      (symbol
       (when (< (length contents) 1)
         (error "Invalid initialisation item ~S for ~S node" item node))
       (case item
         (:class (add-class node (pop contents)))
         (:line (setf (slot-value node 'line) (pop contents)))
         (t (setf (attribute node item) (pop contents)))))
      (text (add-child node item))
      (text-element (add-child node item))
      (list (setf contents (append item contents)))
      (string (add-child node (make-node 'text item)))
      (array
       (add-child node (make-node 'text (join-strings item #\newline))))))
  node)

(defclass evaluate(text)
  ((expr :initarg :expr :documentation "Text to be read and evaluated at some later time")
   (result :documentation "Result obtained from  evaluating expr"))
  (:documentation "A node containing an expression to be evaluated"))

(defmethod as-sexp((node evaluate))
  `(eval ',(slot-value node 'expr)
    ,@(when (slot-boundp node 'result)
            `(" => " ,(slot-value node 'result)))))

(defmethod as-text((node evaluate))
  (write-to-string
   (slot-value node (if (slot-boundp node 'result) 'result 'expr))
   :readably nil :escape nil))

(defmethod copy-of-node((node evaluate))
  (let ((copy (call-next-method)))
    (dolist(slot '(expr result))
      (when (slot-boundp node slot)
        (setf (slot-value copy slot) (slot-value node slot))))
    copy))

(defclass fixed-text-element(text-element)
  ()
  (:documentation "An element which directly contains preformatted text."))

(defclass docutils.nodes:inline(text-element)())

(defmethod allowed-child-p((element element) child &optional index)
  (declare (ignore index))
  t)
(defmethod allowed-child-p((element element) (child docutils.nodes:inline) &optional index)
  (declare (ignore index))
  nil)

(defmethod allowed-child-p((element text-element) child  &optional index)
    (declare (ignore index))
    nil)

(defmethod allowed-child-p((element text-element) (child text) &optional index)
    (declare (ignore index))
    t)
(defmethod allowed-child-p((element text-element) (child text-element) &optional index)
    (declare (ignore index))
    t)

(defmethod initialize-instance :after ((element element)
				       &key children attributes
				       &allow-other-keys)
  (dolist(child children) (add-child element child))
  (loop for a on attributes by #'cddr
     do (setf (attribute element (car a)) (cadr a))))

(defmethod setup-child((node element) (child node) &optional index)
  (if (allowed-child-p node child index)
      (call-next-method)
      (let ((msg (format nil
                         "Element type ~S is not allowed as a child of an ~S element"
                         (tagname child) (tagname node))))
        (report :error msg)
        (restart-case
            (error msg)
          (add() :report "Ignore error, add element and continue"
                      (call-next-method))
          (not-add()
            :report "Continue without adding element" )))))


;;; attributes interface

(defmethod attribute((node element) (label symbol))
  "Return an attribute corresponding to label"
  (getf (slot-value node 'attributes) label))

(defmethod (setf attribute)(value (node element) (label symbol))
  "Set an attribute of an element"
  (setf (getf (slot-value node 'attributes) label) value))

(defmethod rem-attribute((node element) (label symbol))
  (remf (slot-value node 'attributes) label))

(defgeneric add-class(node name)
  (:documentation "Add class to given node")
  (:method ((node element) (name string))
    (setf name (strip (string-downcase name)))
    (let ((class (attribute node :class)))
      (unless (member name (split-sequence:split-sequence #\space  class)
                      :test #'string=)
        (setf (attribute node :class)
              (if class (concatenate 'string class " " name) name))))))

;;; child interface
(defmethod child((node element) (index integer))
  "Return a child at position index"
  (elt (slot-value node 'children) index))

(defmethod index((node element) (child node))
  (position child (slot-value node 'children)))

(defmethod add-child((node element) (child node) &optional index)
  (check-type index (or null (integer 0)))
  (restart-case
      (setup-child node child index)
    (continue() :report "Add the child anyway and continue")
    (drop()
      :report "Do not add the child and continue"
      (return-from add-child)))
  (if (and index (< index (number-children node)))
      (restart-case
          (progn
            (when (> index (length (slot-value node 'children)))
              (error "Attempting to add ~A beyond the end of the
children of ~A" child node))
            (if (= 0 index)
                (push child (slot-value node 'children))
                (let ((tail (nthcdr index (slot-value node 'children))))
                  (setf (cdr tail) (cons (car tail) (cdr tail))
                        (car tail) child))))
        (drop()
          :report "Do not add the child and continue"
          (return-from add-child))
        (append() :report "Add the child at the end and continue"))
      (setf (slot-value node 'children)
            (nconc (slot-value node 'children) (list child))))
  child)

(defmethod add-child((node element) (children sequence)
		     &optional index)
  "Append a set of children into a node"
  (map 'nil
       (if index
       #'(lambda(c) (add-child node c index) (incf index))
       #'(lambda(c) (add-child node c)))
       children))

(defmethod add-child((node text-element) (child string) &optional index)
  (add-child node (make-instance 'text :text child) index))

(defgeneric substitute-node(new old &optional parent)
  (:documentation "Substitute new node for and old node in document tree"))

(defmethod substitute-node((new node) (old node)
			   &optional (parent (parent old)))
  "Substitute new node for old node in document tree"
  (substitute-node new (index parent old) parent))

(defmethod substitute-node((new element) (index integer)
			   &optional parent)
  "Substitute new element for old node at index of parent node"
  (setup-child parent new index)
  (slot-makunbound (elt (slot-value parent 'children) index) 'parent)
  (setf (elt (slot-value parent 'children) index) new))

(defmethod rem-child((node element) (index integer))
  "Remove an child element at index"
  (let ((children (slot-value node 'children)))
    (if (= index 0)
        (progn
          (slot-makunbound (car children) 'parent)
          (setf (slot-value node 'children) (cdr children)))
        (let((tail (nthcdr (1- index) children)))
          (slot-makunbound (cadr tail) 'parent)
          (setf (cdr tail) (cddr tail))))))

(defmethod rem-child((node element) (item node))
  (let ((p (position item (slot-value node 'children))))
    (when p (rem-child node p))))

(defmethod remove-node((node node) &optional index)
  (if index
      (rem-child node index)
      (rem-child (parent node) node)))

(defmethod as-text((node element))
  (join-strings (mapcar #'as-text (slot-value node 'children))
		(child-text-separator node)))

(defmethod as-sexp((node element))
  `((,(class-name (class-of node))
     ,@(slot-value node 'attributes) :line ,(line node))
    ,@(mapcar #'as-sexp (slot-value node 'children))))

(defmethod number-children((node element))
  (length (slot-value node 'children)))

(defmethod initialize-instance :after ((node text-element)
                                       &key text &allow-other-keys)
  (when text
    (let ((text-node
           (make-node 'text
                      (if (stringp text) text (join-strings text #\newline)))))
      (setup-child node text-node)
      (push text-node (slot-value node 'children)))))


;;; mixins

(defclass resolvable()
  ((resolved :type boolean :initform nil :accessor resolved)))
(defclass backlinkable()
  ((backrefs :type list :initform nil :reader backrefs)))

(defun add-backref(element id)
  (push id (slot-value element 'backrefs)))

(defclass root()())
(defclass titular()())
(defclass predecorative()()
  (:documentation
   "Category of Node which may occur before Decorative Nodes."))
(defclass prebibliographic(predecorative)()
  (:documentation
   "Category of Node which may occur before Bibliographic Nodes."))
(defclass bibliographic(predecorative)())
(defclass decorative()())

(defclass body()())
(defclass general(body)())
(defclass sequential(body)())
(defclass abmonition(body)())
(defclass docutils.nodes:special(body)()
  (:documentation "Special internal body elements."))
(defclass invisible(prebibliographic)()
  (:documentation "Internal elements that don't appear in output."))
(defclass part()())
(defclass referential(resolvable)())
(defclass targetable(resolvable)
  ((referenced :type boolean :initform nil :accessor referenced)))

(defmethod add-child :after(node (child targetable)  &optional index)
  (declare (ignore index))
  (set-id child))

(defclass labeled()()
  (:documentation "Contains a `label` as its first element."))

;; ================
;;  Title Elements
;; ================

(defclass title(titular prebibliographic text-element)())
(defclass subtitle(titular prebibliographic text-element)())
(defclass rubric(titular text-element)())

;; ========================
;;  Bibliographic Elements
;; ========================

(defclass docinfo(bibliographic element)())
(defclass author(bibliographic text-element)())
(defclass authors(bibliographic element)())
(defclass organization(bibliographic text-element)())
(defclass address(bibliographic text-element)())
(defclass contact(bibliographic text-element)())
(defclass version(bibliographic text-element)())
(defclass revision(bibliographic text-element)())
(defclass status(bibliographic text-element)())
(defclass date(bibliographic text-element)())
(defclass copyright(bibliographic text-element)())

;; =====================
;;  Decorative elements
;; =====================

(defclass decoration(decorative element)())
(defclass header(decorative element)())
(defclass footer(decorative element)())

;; =====================
;;  Structural elements
;; =====================

(defclass structural()())

;; ======================
;;  Root Document Element
;; ======================

(defgeneric cmp(a b)
  (:documentation "Comparison function for ordering sequences of entities"))

(defclass document(root structural element)
  ((namespace :reader namespace :initarg :namespace :initform nil)
   (settings :reader settings :initform (make-hash-table) :initarg :settings
             :documentation "Runtime settings data record.")
   (current-id :type integer :initform 0 :initarg :id-start
               :accessor current-id :documentation "current id."))
  (:default-initargs :line 0)
  (:documentation "The main document root element"))

(defmethod initialize-instance :after ((document document)
				       &key source-path &allow-other-keys)
  (when source-path
    (setf (setting :source-path document) source-path)))

(defmethod parent((node document)) nil)
(defmethod document((node document)) node)
(defmethod language((node document))
  (or (attribute node :language) (setting :language node) *language*))

(defclass section(structural element)())
(defclass topic(structural element)
  ()
  (:documentation "Topics are terminal, 'leaf' mini-sections, like
block quotes with titles, or textual figures.  A topic is just like a
section, except that it has no subsections, and it doesn't have to
conform to section placement rules."))

(defmethod allowed-child-p((parent element) (child topic) &optional index)
    (declare (ignore index))
    nil)
(defmethod allowed-child-p((parent structural) (child topic) &optional index)
  (declare (ignore index))
  t)
(defmethod allowed-child-p((parent topic) (child structural) &optional index)
  (declare (ignore index))
  nil)

(defclass sidebar(structural element)()
  (:documentation " Sidebars are like miniature, parallel documents
that occur inside other documents, providing related or reference
material.  A sidebar is typically offset by a border and 'floats' to
the side of the page; the document's main text may flow around it.
Sidebars can also be likened to super-footnotes; their content is
outside of the flow of the document's main text."))

(defclass transition(structural element)())

(defmethod allowed-child-p((parent section) (child structural) &optional index)
  (declare (ignore index))
  t)
(defmethod allowed-child-p((parent document) (child structural) &optional index)
  (declare (ignore index))
  t)
(defmethod allowed-child-p(parent (child structural) &optional index)
  (declare (ignore index))
  nil)

;; ===============
;;  Body elements
;; ===============

(defclass paragraph(general text-element)())
(defclass compound(general element)())
(defclass container(general element)())
(defclass bullet-list(sequential element)())
(defclass enumerated-list(sequential element)())
(defclass list-item(part element)())
(defclass definition-list(sequential element)())
(defclass definition-list-item(part element)())
(defclass term(part text-element)())
(defclass classifier(part text-element)())
(defclass definition(part element)())
(defclass field-list(sequential element)())
(defclass field(part element)())
(defclass field-name(part text-element)())
(defclass field-body(part element)())
(defmethod allowed-child-p((parent field-list) child &optional index)
  (declare (ignore index))
  (typep child 'field))

(defmethod allowed-child-p((parent field) child &optional index)
  (let ((index (or index (number-children parent))))
    (typecase child
      (field-name (evenp index))
      (field-body (oddp index)))))

(defmethod print-object((node field-name) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~S ~@[line ~D ~]"  (as-text node) (line node))))

(defmethod print-object((node field) stream)
  (print-unreadable-object (node stream :type t :identity t)
        (format stream "~S ~@[line ~D ~]"
                (when (child node 0) (as-text (child node 0)))
                (line node))))

(defclass option(part element)())

(defmethod child-text-separator((element option)) "")

(defclass option-argument(part text-element)())

(defmethod as-text((element option-argument))
  (concatenate 'string (or (attribute element :delimiter) " ")
	       (call-next-method)))

(defclass option-group(part element)())

(defmethod child-text-separator((element option-group)) ", ")

(defclass option-list(sequential element)())

(defclass option-list-item(part element)())

(defmethod child-text-separator((element option-list-item)) "  ")

(defclass option-string(part text-element)())
(defclass description(part element)())
(defclass literal-block(general fixed-text-element)())
(defclass doctest-block(general fixed-text-element)())
(defclass line-block(general element)())
(defclass line(general text-element)
  ((indent :initform nil)))
(defclass admonition(element)())
(defclass block-quote(general element)())
(defclass attribution(part text-element)())
(defclass attention(admonition element)())
(defclass caution(admonition element)())
(defclass danger(admonition element)())
(defclass docutils.nodes:error(admonition element)())
(defclass important(admonition element)())
(defclass note(admonition element)())
(defclass tip(admonition element)())
(defclass hint(admonition element)())
(defclass docutils.nodes:warning(admonition element)())
(defclass comment(docutils.nodes:special invisible fixed-text-element)())
(defclass substitution-definition(docutils.nodes:special invisible text-element)())
(defclass target(docutils.nodes:special invisible docutils.nodes:inline
					text-element targetable)())
(defmethod allowed-child-p((element element) (child target) &optional index)
  (declare (ignore index))
  t)
(defclass footnote(general element targetable labeled backlinkable)())
(defclass citation(general element targetable labeled backlinkable)())
(defclass label(part text-element)())
(defmethod allowed-child-p((element element) (child label)  &optional index)
  (and (typep element 'labeled)
       (= (or index (number-children element)) 0)))

(defclass figure(general element)())
(defclass caption(part text-element)())
(defclass legend(part element)())
(defclass table(general element)())
(defclass tgroup(part element)())
(defclass colspec(part element)())
(defclass thead(part element)())
(defclass tbody(part element)())
(defclass row(part element)())
(defclass entry(part element)())

(defclass system-message(docutils.nodes:special
			 prebibliographic element backlinkable)())

(defmethod add-child((node text-element) (child system-message)
                     &optional index)
  "Add system message to part of a text element"
  (assert (not index))
  (add-child (parent node) child))

(defmethod make-node((node system-message) &rest message-args)
  (let* ((message
          (if (and (= 1 (length message-args))
                   (typep (car message-args) 'markup-condition))
              (let* ((e (car message-args))
                     (line (error-line e))
                     (level (error-level e))
                     (source (error-source e))
                     (data (error-data e)))
                (setf (attribute node :level) level)
                (when source (setf (attribute node :source) source))
                (when line (setf (slot-value node 'line) line))
                (when data (add-child node (make-node 'literal-block data)))
                (when-bind(backrefs (error-backrefs e))
                  (dolist(bref backrefs) (add-backref node bref)))
                (error-message e))
              (apply #'format (cons nil message-args)))))
    (let ((p (make-node 'paragraph)))
      (add-child node p 0)
      (add-child p message))
    node))

(defmethod as-text((node system-message))
  (format nil "~A: ~A~@[, source line ~D~]"
          (attribute node :level) (call-next-method)
          (line node))
#| old definition
  (format nil "~s:~s: (~s/~s) ~s"
	  (attribute node :source)
	  (line node)
	  (attribute node :type)
	  (attribute node :level)
	  (call-next-method)))
|#)

(defmethod error-severity((c system-message))
  (getf +error-levels+ (attribute c :level) 6))

(defclass pending(docutils.nodes:special invisible element)
  ()
  (:documentation
   "The 'pending' element is used to encapsulate a pending operation:
the operation (transform), the point at which to apply it, and any
data it requires."))

(defclass raw(docutils.nodes:special docutils.nodes:inline
                                     prebibliographic fixed-text-element)()
  (:documentation "Raw data that is to be passed untouched to the Writer."))
(defmethod allowed-child-p((parent element) (child raw) &optional index)
  (declare (ignore index))
  t)

;; =================
;;  inline elements
;; =================
(defclass emphasis(docutils.nodes:inline)())
(defclass strong(docutils.nodes:inline)())
(defclass literal(docutils.nodes:inline)())
(defclass reference(general docutils.nodes:inline referential)())
(defclass footnote-reference(docutils.nodes:inline referential)())
(defclass citation-reference(docutils.nodes:inline referential)())
(defclass substitution-reference(docutils.nodes:inline)())
(defclass title-reference(docutils.nodes:inline)())
(defclass abbreviation(docutils.nodes:inline)())
(defclass acronym(docutils.nodes:inline)())
(defclass superscript(docutils.nodes:inline)())
(defclass subscript(docutils.nodes:inline)())
(defclass image(general docutils.nodes:inline)())
(defmethod as-text((node image)) (or (attribute node :alt) ""))
(defclass equation(general text-element)())
(defclass math(general docutils.nodes:inline)())
(defclass problematic(docutils.nodes:inline)())
(defclass generated(docutils.nodes:inline)())

(defmethod allowed-child-p((element element) (child reference) &optional index)
  (declare (ignore index))
  t)
(defmethod allowed-child-p((element element) (child image) &optional index)
  (declare (ignore index))
  t)

#|
(defmethod allowed-child-p((element element) (child equation) &optional index)
  (declare (ignore index))
  t)
|#

;; ========================
;; output specific elements
;; ========================

;; Element tree traversal

(defmacro with-children((node parent &key (copy nil)) &body body)
  "Exevute body over the children of a node
During execution of the body a catch tags is available for
:skip-siblings which will terminate the iteration"
  `(catch :skip-siblings
     (map 'nil #'(lambda(,node) ,@body)
	  ,(if copy
	       `(copy-list (slot-value ,parent 'children))
	       `(slot-value ,parent 'children)))))

(defmacro with-nodes((node root  &key (copy nil)) &body body)
  "Traverse a node tree depth first executing body for side affects.
The body is executed then children are traversed (if an
element). During execution of the body catch tags are available for
:skip-children and :skip-siblings to finish processing children of
current node or children of parent node. Execution is inside
a nil block. Returns nil"
  (let ((func (gensym)))
    `(block nil
      (labels((,func(,node)
                (catch :skip-children
                  ,@body
                  (when (typep ,node 'element)
                    (catch :skip-siblings
                      (map 'nil #',func
                           ,(if copy
                                `(copy-list (slot-value ,node 'children))
                                `(slot-value ,node 'children))))))))
        (,func ,root)))))

(defmacro with-attributes((key value node &key (copy nil)) &body body)
  "Interate body over the attributes of a node setting key and value
in turn. If keyword copy is true, the iteration will be over a list of
the attribute list may be modified during iteration, otherwise it may
not."
  (let ((a (gensym)))
    `(loop :for ,a :on ,(if copy
                              `(copy-list (slot-value ,node 'attributes))
                              `(slot-value ,node 'attributes)) :by #'cddr
	:do (let ((,key (car ,a))
               (,value (cadr ,a)))
           ,@body))))

(defun next-sibling(element)
  (cadr (member element (slot-value (parent element) 'children))))

(defun prev-sibling(element)
  (loop :for a :on  (slot-value (parent element) 'children)
        :when (eql (cadr a) element) :return (car a)))

(defun set-id(node &optional (document (document node))
                  (ids (ids document)))
  "This will set and return the id for a node."
  (let ((namespace (namespace document))
        (id (attribute node :id)))
    (flet((new-id() (format nil "~@[~A.~]id~D" namespace
                            (incf (current-id document)))))
      (if id
	  (progn
	    (when namespace
	      (let ((prefix  (concatenate 'string namespace ".")))
		(unless (= 0 (search prefix id))
		  (setf id (concatenate 'string prefix id)))))
	    (unless (eql node (gethash id ids node))
	      (report :severe (format nil "Duplicate ID: ~S." id))))
	  (let ((name  (attribute node :name)))
	    (when (and name (not namespace))  (setf id (make-id name)))
	    (when (or (not id) (gethash id ids))
	      (setf id (do ((id (new-id) (new-id)))
			   ((not (gethash id ids)) id))))))
      (setf (gethash id ids) node
	    (attribute node :id) id))))

;;; Collated information

(defmacro collate-nodes((node root) test)
  (let((targets (gensym)))
    `(let ((,targets nil))
      (with-nodes(,node ,root)
        (when ,test (push ,node ,targets)))
      (nreverse ,targets))))

(defgeneric implicit-target-p(node)
  (:documentation "Return true if an implicit target")
  (:method (node) nil)
  (:method ((node section)) t))

(defun refnames(element)
  "Return mapping of names to lists of referencing nodes."
  (let ((refnames (make-hash-table :test 'equal)))
    (with-nodes(node element)
      (when (typep node 'element)
        (let ((refname (attribute node :refname)))
          (when refname
            (push node (gethash refname refnames))))))
    refnames))

(defun refids(element)
  "Mapping of ids to lists of referencing nodes."
  (let ((refids (make-hash-table :test 'equal)))
    (with-nodes(node element)
      (when (typep node 'element)
        (let ((refid (attribute node :refid)))
          (when refid
            (push node (gethash refid refids))))))
    refids))

(defun ids(element)
  "Return mapping of ids to nodes in element."
  (let ((ids (make-hash-table :test #'equal)))
    (with-nodes(node element)
      (when (typep node 'element)
        (when-bind(id (attribute node :id))
          (setf (gethash id ids) node))))
    ids))

(defun nameids(element  &optional (ids (ids (document element))))
  "Mapping of names to unique id's."
  (let ((nameids (make-hash-table :test #'equal))
        (nametypes (make-hash-table :test #'equal)))
    (with-nodes(node element)
      (when (typep node 'element)
        (when-bind(name (attribute node :name))
          (let ((id (or (attribute node :id)
                        (set-id node (document node) ids))))
            (if (gethash name nameids) ;; duplicate
                (set-duplicate-name-id ids nameids nametypes node)
                (progn
                  (setf (gethash name nameids) id)
                  (setf (gethash name nametypes)
                        (not (implicit-target-p node)))))))))
    nameids))

(defun named-node(element name)
  (with-nodes(node element)
    (when (and (typep node 'element) (equalp (attribute node :name) name))
      (return-from named-node node))))

(defun set-duplicate-name-id(ids nameids nametypes node)
  "The following state transition table shows how `self.nameids` wand
`self.nametypes`change with new input and what actions are performed:

====  =====  ========  ========  =======  ====  =====  =====
Old State    Input          Action        New State   Notes
-----------  --------  -----------------  -----------  -----
ids   types  new type  sys.msg.  dupname  ids   types
====  =====  ========  ========  =======  ====  =====  =====
--    --     explicit  --        --       new   True
--    --     implicit  --        --       new   False
None  False  explicit  --        --       new   True
old   False  explicit  implicit  old      new   True
None  True   explicit  explicit  new      None  True
old   True   explicit  explicit  new,old  None  True   [#]_
None  False  implicit  implicit  new      None  False
old   False  implicit  implicit  new,old  None  False
None  True   implicit  implicit  new      None  True
old   True   implicit  implicit  new      old   True
====  =====  ========  ========  =======  ====  =====  =====

.. [#] Do not clear the name-to-id map or invalidate the old target if
both old and new targets are external and refer to identical URIs.
The new target is invalidated regardless."
  (let* ((name (attribute node :name))
         (id (attribute node :id))
         (explicit (not (implicit-target-p node)))
         (old-id (gethash name nameids))
         (old-explicit (gethash name nametypes)))
    (setf (gethash name nametypes) (or old-explicit explicit))
    (flet ((dupname(node)
             (setf (attribute node :dupname) (attribute node :name))
             (rem-attribute node :name)))
      (if explicit
          (if old-explicit
              (let ((level :warn))
                (when old-id
                  (let ((old-node (gethash old-id ids)))
                    (unless
                        (when-bind(refuri (attribute node :refuri))
                          (and (attribute old-node :name)
                               (equal (attribute old-node :refuri)
                                      refuri)))
                      (setf level :info)
                      (dupname old-node)
                      (remhash name nameids))))
                (report level `("Duplicate explicit target name: ~s." ,name)
                        :node node :backrefs (list id))
                (dupname node))
              (progn
                (setf (gethash name nameids) id)
                (when old-id (dupname (gethash old-id ids)))))
          (progn
            (when (and old-id (not old-explicit))
              (remhash name nameids)
              (dupname (gethash old-id ids)))
            (dupname node))))
    (when (or (not explicit) (and (not old-explicit) old-id))
      (report :info
              `("Duplicate explicit target name: ~s." ,name)
              :node node :backrefs (list id)))))

;;; Accessors for document components

(defgeneric docinfo(document)
  (:documentation "Return the docinfo component of a document")
  (:method((document document))
    (with-children(node document)
      (when (typep node 'docinfo) (throw :skip-siblings node)))))

(defgeneric title(entity)
  (:documentation "Return the title for an entity")
  (:method((element element))
    (docutils:with-children(node element)
      (when (typep node 'docutils.nodes:title)
        (return-from title (as-text node))))))

(defgeneric abstract(entity)
  (:documentation "Return the abstract of an entity")
  (:method ((document document))
    (docutils:with-children(node document)
      (when (and (typep node 'docutils.nodes:topic)
                 (string-equal (title node) "abstract"))
        (return-from abstract node)))))

(defgeneric field-value(name field-list)
  (:documentation "Return the field text for a specific field name")
  (:method(name field-list)
    (let ((name (string-upcase name))
          (results nil))
      (with-children(item field-list)
        (etypecase item
          (field
           (when (string-equal name (as-text (child item 0)))
             (push (as-text (child item 1)) results)))
          (bibliographic
           (when (string-equal name (string (type-of item)))
             (push (as-text item) results)))))
      (nreverse results)))
  (:method(name (document document))
    (field-value name (docinfo document))))

(defgeneric resolve-dependancy(node uri)
  (:documentation "Return full path corresponding to uri in a node")
  (:method ((node node) uri)
    (let ((document (document node)))
      (jarw.io:find-file uri
                         :search-path
                         `(,(setting :source-path document)
                           ,(setting :search-path document)
                           ,@jarw.io::*search-path*)))))

