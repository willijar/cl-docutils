;;;; Translations for the docutils utilities
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

;;;; $Id: translate.lisp,v 1.2 2006/06/25 08:33:38 willijar Exp willijar $

(in-package :docutils.utilities)

(defvar *language-directory* #p""
  "Directory in which translation data is held. Each language has a file
containing an a-list mapping  translated form to a cannonical form")

(defvar *language* "en" "Default language for processing")

(declaim (special *language-directory*))

(eval-when(:compile-toplevel)
    (setq *language-directory*
          (merge-pathnames (make-pathname :name :wild
                                          #-clisp :type #-clisp :unspecific)
                           *compile-file-truename*)))
(defstruct translation
  (translated (make-hash-table :test #'equalp))
  (canonical (make-hash-table :test #'equalp)))

(defvar *translations* (make-hash-table :test #'equalp)
  "Hash table mapping language name to languages")

(defun parse-translations(entries)
  (let ((translation (make-translation)))
    (dolist(entry entries)
      (setf (gethash (cdr entry)
                     (translation-translated translation))
            (car entry)
            (gethash (car entry)
                     (translation-canonical translation))
            (cdr entry)))
    translation))

(defun get-translation(language)
  (or (gethash language *translations*)
      (let ((fname (merge-pathnames (make-pathname :name language :type nil)
                                    *language-directory*)))
        (unless (probe-file fname)
          (error "No translation available for ~S language" language))
        (setf (gethash language *translations*)
              (parse-translations
               (with-open-file(is fname :direction :input)
                 (read is)))))))

(defun canonical-text(text &optional (language *language*))
  (gethash text (translation-canonical (get-translation language)) text))

(defun translated-text(text &optional (language *language*))
  (gethash text (translation-translated (get-translation language)) text))

(defun author-separators( &optional (language *language*))
  (gethash :author-separators
           (translation-canonical (get-translation language)) '('\, #\;)))

(defun babel( &optional (language *language*))
  (gethash :babel (translation-canonical (get-translation language)) "english"))

(defun latex-quotes( &optional (language *language*))
  (gethash :latex-quotes
           (translation-canonical (get-translation language)) '("``" "''")))

(defun latex-double-quote-replacement( &optional (language *language*))
  (gethash :latex-double-quote-replacement
           (translation-canonical (get-translation language))))