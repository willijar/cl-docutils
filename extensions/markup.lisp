;;;; Misc string and vector handling for docutils
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; $Id: markup.lisp,v 1.3 2006/08/08 08:15:34 willijar Exp willijar $

(in-package :asdf)

(defpackage :docutils.writer.markup
  (:documentation "Latex writer for docutils")
  (:use :cl)
  (:import-from :docutils.parser.rst #:rst-reader)
  (:import-from :jarw.port #:make-mutex #:with-lock)
  (:import-from :docutils #:read-document #:document  #:write-part)
  (:import-from :docutils.writer.html #:html-writer #:body)
  (:import-from :markup #:html))

(in-package :docutils.writer.markup)

(export '(markup::rst) (find-package :markup))

(defvar *markup-rst-reader*  'docutils.parser.rst:rst-reader)
(defvar *markup-rst-writer* 'docutils.writer.html:html-writer)

(defmethod html((stream stream) (tag (eql 'markup::rst))
                &optional attr content)
  (when (first content)
    (html stream
          (read-document
           (first content)
           (make-instance (getf attr :reader *markup-rst-reader*)))
          attr
          content)))

(defmethod html((stream stream) (document docutils.nodes:document)
                &optional attr content)
  (declare (ignore content))
  (unless (getf attr :initial-header-level)
    (setf (getf attr :initial-header-level) (1+  markup:*section-level*)))
  (let ((writer (make-instance
                 (getf attr :writer *markup-rst-writer*)
                 :settings attr)))
    (docutils:visit-node writer document)
    (docutils:write-part writer 'body stream)))

(defmethod html(stream (node docutils.nodes:node) &optional attr content)
  (declare (ignore attr content))
  ;; we clone the document with settings - note parent of node will
  ;; be the original parent NOT the clone.
  (let ((document (make-instance
                   'docutils.nodes:document
                   :settings (docutils::settings (document node)))))
    (setf (slot-value document 'docutils::children) (list node))
    (html stream document)))

(defun markup::parse-restructured-text(text)
  `(markup::rst ,text))

(in-package :docutils.writer.latex)

(defmethod visit-node ((writer latex-writer)
                       (node evaluateable))
  (case (output-format node)
    (:markup
     (part-append
      (with-output-to-string(os) (markup:latex os (evaluate node)))))
    (:latex (part-append (evaluate node)))
    (t (call-next-method))))

(in-package :docutils.writer.html)

(defmethod visit-node((writer html-writer)
                      (node evaluateable))
  (case (output-format node)
    (:markup
     (part-append
      (with-output-to-string(os) (markup:html os (evaluate node)))))
    (:latex (part-append (evaluate node)))
    (t (call-next-method))))

;; make markup the default

(register-settings-spec
 '((:default-evaluation-format symbol nil
    "The format of output from evaluation nodes")))