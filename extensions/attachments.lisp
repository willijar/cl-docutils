;; Attachments directive>
;; Copyright (C) 2009 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of CL-DOCUTILS

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

(in-package :docutils)

(defclass attachment(element invisible)
  ()
  (:documentation "Base node type for attachments"))

(defmethod write-node((node attachment) (os line-wrap-stream))
  (format os ".. attachment:: ~a~%" (attribute node :name))
  (with-block-indentation(3 os)
    (format os ":content-type: ~A~%~%" (attribute node :content-type))
    (write (attribute node :content) :stream os)))

(defmethod visit-node(writer (node attachment))
  (declare (ignore writer)))

(in-package :docutils.parser.rst)

;; note that docutils doesn not deal with attachment content at all. It is up
;; to applications to parse and decode the data.
(def-directive attachment
    (parent name
            &allow-spaces
            &option
            content-type
            encoding
            &content content)
  (add-child parent
             (make-node 'docutils::attachment
                        :name name
                        :encoding encoding
                        :content-type content-type
                        :content content)))





