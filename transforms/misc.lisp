;; $Id: misc.lisp,v 1.9 2007/07/03 06:59:36 willijar Exp $
;; Miscellaneous transforms for restructured text
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Cl-docutils

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; class-attribute form carries our class directive
;; simple-transform provides a way of applying a function as a transform
;; evaluate transform and function execute eval nodes

;;; Code:

(in-package :docutils.transform)

(defclass class-attribute(transform)
  ()
  (:default-initargs :priority 210)
  (:documentation
   "Move class attribute of pending node to next non-comment element"))

(defmethod transform((transform class-attribute))
  (let* ((pending (node transform))
         (class (attribute pending :class)))
    (do*((child pending parent)
         (parent (parent pending) (parent parent)))
        ((not parent))
      (let((f nil))
        (with-children(element parent)
          (if f
              (unless (or (typep element 'docutils.nodes:Invisible)
                          (typep element 'docutils.nodes:system-message))
                (add-class element class)
                (remove-node pending)
                (return-from transform t))
              (when (eql element child) (setf f t))))))
    (report :error "No suitable element following class directive")))

(defclass simple-transform(transform)
  ((function :initarg :function))
  (:documentation "A simple transform which calls the closure with the node"))

(defmethod transform((transform simple-transform))
  (funcall (slot-value transform 'function) (node transform)))

(defclass evaluate-transform(transform)
  ()
  (:documentation "eval evaluate nodes"))

(defgeneric evaluate(node)
  (:documentation "Evaluate the node in current dynamic context")
  (:method(node) node)
  (:method((node docutils.nodes:element))
    (with-children(child node) (evaluate child))
    node)
  (:method((node docutils.nodes:evaluate))
    (handler-case
          (setf (slot-value node 'docutils::result)
                (eval (slot-value node 'docutils::expr)))
         (error(e)
           (report :warn (write-to-string e :escape nil :readably nil ) )))
    node))

(defmethod transform((transform evaluate-transform))
  (evaluate (node transform)))
