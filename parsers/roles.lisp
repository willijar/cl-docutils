;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; An attempt to parse the rstructuredtext syntax into markup
;;;; See
;;;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html

(in-package :docutils.parser.rst)

(defvar *interpreted-roles* (make-hash-table :test #'equalp)
  "Mapping of roles to role functions which take the interpretate
text content and an alist of directive options to be interprated by
the role function")

(defvar *default-interpreted-role* "title-reference"
  "The canonical name of the default interpreted role. This role is used
when no role is specified for a piece of interpreted text.")

(defmethod parse-role((rolename string) text &optional option-values content)
  (let ((role (gethash
               (if (= 0 (length rolename))
                   *default-interpreted-role*
                   (string-downcase rolename))
               *interpreted-roles*)))
    (if role
        (parse-role role text option-values content)
        (report :info (format nil "No role entry for ~S" rolename)))))

(defmethod parse-role((role (eql nil)) text &optional option-values content)
  (parse-role *default-interpreted-role* text option-values content))

(defclass role()
  ((name :type string :initarg :name)))

(defclass standard-role(role)
  ((options :initform nil :type list :initarg :options)
   (content :initform nil :initarg :content )
   (function :type function :initarg :function)))

(defmethod parse-role((role standard-role) text
                      &optional
                      (option-values
                       (mapcar #'third (slot-value role 'options)))
                      (content (slot-value role 'content)))
  (funcall (slot-value role 'function) text option-values content))

(defclass custom-role(role)
  ((base-role :type standard-role :initarg :base-role)
   (option-values :initarg :option-values :type list :initform nil)
   (supplied-content :initarg :content :initform nil)))

(defmethod parse-role((role custom-role) text
                      &optional option-values content)

  (parse-role (slot-value role 'base-role) text
              (or option-values
                  (mapcar #'second  (slot-value role 'option-values)))
              (or content (slot-value role 'supplied-content))))

(defclass generic-custom-role(standard-role)
  ((options :initform '((:class class))
	    :type list :initarg :options)))

(defmethod parse-role((role generic-custom-role) text &optional
                      option-values
                      content)
  (declare (ignore content))
  (let ((node (make-node 'inline (parse-inline rst-patterns text))))
    (when option-values (add-class node (first option-values)))
    node))

(defmacro def-role(name (textvar &rest lambda-list) &body body)
  "Define a role handler for role with cannonical name name. content
and options will come from the role directive. lambda list is as follows
lambda-list::= ({var | (var [[specializer] [default]])}*
                [{{&content {var [[specializer] [default]]}}] )"
  (let ((options-spec nil)
        (content-spec nil)
        (state :option))
    (flet ((is-vardef(item)
             (or (and (listp item) (symbolp (first item)))
                 (and (symbolp item) (not (eql (char (string item) 0) #\&)))))
           (varname(s) (if (listp s) (car s) s)))
      (dolist(arg lambda-list)
        (unless (or (symbolp arg) (listp arg))
          (error "Invalid definition argument ~S" arg))
        (case arg
          (&content (setf state :content))
          (t
           (ecase state
             (:option
              (if (is-vardef arg)
                  (push arg options-spec)
                  (error  "Argument ~S out of place" arg)))
             (:content
              (unless (is-vardef arg) (error "Invalid content spec ~S" arg))
              (when content-spec (error "Wrong number of content arguments"))
              (setf content-spec arg))))))
      (setf options-spec (nreverse options-spec))
      (let ((gcontent (or content-spec (gensym)))
            (options (gensym)))
        `(eval-when(:load-toplevel :execute)
          (setf (gethash ,(string-downcase name) *interpreted-roles*)
           (make-instance
            'standard-role
            :name ,(string-downcase name)
            :options ',options-spec
            :content ,content-spec
            :function
            #'(lambda(,textvar ,options ,gcontent)
                ,@(unless content-spec
                          `((declare (ignore ,gcontent))))
                (multiple-value-bind(,@(mapcar #'varname options-spec))
                    (values-list ,options)
                  ,@body)))))))))

(defmacro def-generic-role(name)
  (let ((text (gensym))
        (class (gensym))
        (node (gensym)))
    `(def-role ,name (,text (,class class))
      (let ((,node (make-instance
                    ',(intern (string-upcase name) :docutils.nodes))))
        (add-child ,node ,text)
        (when ,class (add-class ,node ,class))
        ,node))))

(def-generic-role "abbreviation")
(def-generic-role "acronym")
(def-generic-role "emphasis")
(def-generic-role "literal")
(def-generic-role "strong")
(def-generic-role "subscript")
(def-generic-role "superscript")
(def-generic-role "title-reference")

(defvar *rfc-url* "ftp://ftp.rfc-editor.org/in-notes/rfc4~D.txt")

(def-role rfc-reference(text)
  (let ((rfcnum (parse-integer text :junk-allowed t)))
    (if (and rfcnum (> rfcnum 0))
        (let ((node (make-node 'reference
                               :refuri (format nil *rfc-url* rfcnum))))
          (add-child node (format nil "RFC~A" (unescape text)))
          node)
        (report
         :error
         `("RFC must be a number greater than 1; ~A is invalid." ,text)))))

(defvar *pep-url* "http://www.python.org/dev/peps/pep-~4,'0d/")

(def-role pep-reference(text)
  (let ((pepnum (parse-integer text :junk-allowed t)))
    (if (and pepnum (>= pepnum 0) (<= pepnum 9999))
        (let ((node (make-node 'reference
                               :refuri (format nil *pep-url* pepnum))))
          (add-child node (format nil "PEP~A" (unescape text)))
          node)
        (report
         :error
         `("PEP must be a number from 0 to 9999; ~A is invalid." ,text)))))

(def-role raw(text format (class class))
  (if format
      (let ((node (make-node 'raw :format format
                             (unescape text :restore-backslashes t))))
        (when class (add-class node class))
        node)
      (progn
        (report :error "No format (Writer name) is associated with this raw role:
use the \'role\' directive to create a new role with an associated format")
        (make-node 'problematic text))))

(def-role math(text)
  (make-node 'math (concatenate 'string "$" text "$")))

(def-role eval(text)
  (handler-case
      (with-input-from-string(is text)
        (make-instance
         'docutils.nodes:inline-evaluation
         :expression (read is  t nil)
         :format (read is nil nil)))
    (error(e)
      (make-node 'problematic
                 (write-to-string e :escape nil :readably nil )))))



#|


(def-unimplemented-role index)
(def-unimplemented-role named-reference)
(def-unimplemented-role anonymous-reference)
(def-unimplemented-role uri-reference)
(def-unimplemented-role footnote-reference)
(def-unimplemented-role citation-reference)
(def-unimplemented-role substitution-reference)
(def-unimplemented-role target)

;;# This should remain unimplemented, for testing purposes:
(def-unimplemented-role restructuredtext-unimplemented-role)

|#