;; Extend docutils to use the jarw media server
;; Copyright (C) 2009 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of docutils.extensions

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; FOOBAR is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(in-package :docutils.writer.html)

(defclass html-writer-using-media-server(html-writer)
  ())

(export 'html-writer-using-media-server)

(setq docutils.writer.markup::*markup-rst-writer* 'html-writer-using-media-server)

(defmethod html-url((writer html-writer-using-media-server)
                    (uri string) &rest args)
  (if (equalp (getf args :content-type) "text/x-eqn")
      (let ((media-server jarw.media:*media-server*))
        (when media-server
          (jarw.media:media-variant-url
           (jarw.media:media-variant
            (jarw.media:register-media uri media-server
                                       :content-type "text/x-eqn")
            media-server (getf args :output-format "image/png"))
           media-server)))
      uri))

(defmethod html-url((writer html-writer-using-media-server)
                    (uri pathname) &rest args)
   (let ((media-server jarw.media:*media-server*))
     (if media-server
         (jarw.media:media-variant-url
          (jarw.media:media-variant
                (jarw.media:register-media uri media-server)
                media-server
                (getf args :output-format
                      (or (find (jarw.media:file-suffix-content-type uri)
                                '("image/png" "image/gif" "image/jpeg")
                                :test #'string-equal)
                          "image/png"))
                args)
          media-server)
         (namestring uri))))


(in-package :docutils)

(defun publish(src)
  (write-html (merge-pathnames (make-pathname :type "html") src)
              (read-rst src)))

;;;; VIDEO handling

(in-package :docutils)

(defclass docutils.nodes::video(general element)())

(in-package :docutils.parser.rst)

(def-directive video(parent uri &option
                            (height length)
                            (width length)
                            (align align)
                            (class class))
  (let((node (make-node 'video)))
    (add-child parent node)
    (when (find #\space uri)
      (report :error "Image URI contains whitespace"
              :line *current-line-number*))
    (map nil #'(lambda(k v) (when v (setf (attribute node k) v)))
         '(:height :width :align :uri :class)
         (list height width align uri class))
    node))

(in-package :docutils.writer.html)

(defmethod visit-node((writer html-writer) (node docutils.nodes::video))
  (let* ((atts (list :controls 1))
         (uri (attribute node :uri)))
    (with-attributes(k v node) (setf (getf atts k) v))
    (setf (getf atts :src) uri)
    (remf atts :uri)
    (flet ((set-size(symb)
                   (when (getf atts symb)
                     (setf (getf atts symb)
                           (format nil "~,f~:[%~;~:*~A~]" (car (getf atts symb))
                                   (cdr (getf atts symb)))))))
      (set-size :width)
      (set-size :height))
    (part-append  (start-tag nil "div" (image-div-atts node)))
    (part-append (start-tag node "video" atts) #\newline)
    (part-append (format nil " <a href=~S>Play: ~A</a>~%" uri uri))
    (part-append "</video>" #\newline)
    (part-append "</div>" #\newline)))

(in-package :docutils.writer.latex)

(defmethod visit-node((writer latex-writer) (node docutils.nodes::video))
  (part-append
   (format nil
           (if (typep (parent node) 'text-element)
               " (Video: ~A) "
               "~%~%{\bf Video:} ~A~%~%")
           (attribute node :uri))))
