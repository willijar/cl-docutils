;;;; Translations for the docutils utilities
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

;;;; $Id: translate.lisp,v 1.2 2006/06/25 08:33:38 willijar Exp willijar $

(in-package :docutils.utilities)

(defvar *language-directory* #p"/home/willijar/dev/lisp/src/docutils/languages/"
  "Directory in which translation data is held. Each language has a file
containing an a-list mapping  translated form to a cannonical form")

(defstruct translation
  (translated (make-hash-table :test #'equalp) :type hash-table)
  (canonical (make-hash-table :test #'equalp) :type hash-table))

(defvar *translations* (make-hash-table :test #'equalp)
  "Hash table mapping language name to languages")

(defvar *language* "en" "Default language for processing")

(defun get-translation(language)
  (let ((translation
         (or
          (gethash language *translations*)
          (let ((fname (merge-pathnames *language-directory*
                                        (make-pathname :name language))))
            (when (probe-file fname)
              (let ((translation (make-translation)))
                (dolist(entry
                         (with-open-file(is fname :direction :input)
                           (read is)))
                  (setf (gethash (cdr entry)
                                 (translation-translated translation))
                        (car entry)
                        (gethash (car entry)
                                 (translation-canonical translation))
                        (cdr entry)))
                (setf (gethash language *translations*) translation)))))))
    (unless translation
      (error "No translation available for ~S language" language))
    translation))

(defun canonical-text(text &optional (language *language*))
  (gethash text (translation-canonical (get-translation language)) text))

(defun translated-text(text &optional (language *language*))
  (gethash text (translation-translated (get-translation language)) text))

(defun author-separators( &optional (language *language*))
  (gethash :author-separators (translation-canonical (get-translation language)) '('\, #\;)))
