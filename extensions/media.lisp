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