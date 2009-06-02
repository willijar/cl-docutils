;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2002-2006 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; An attempt to parse the rstructuredtext syntax into markup
;;;; See
;;;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
;;;;
;;;; Mechanisms for reporting parser errors

(in-package :docutils)

;;; reporting levels

;; Debug report level: an internal reporting issue. Typically, there
;; is no effect on the processing.
;; Info report level: a minor issue that can be ignored. Typically there is
;; no effect on processing, and level-1 system messages are not reported.
;; Warning report level: an issue that should be addressed. If
;; ignored, there may be unpredictable problems with the output.
;; Error report level: an error that should be addressed. If ignored,
;; the output will contain errors
;; Severe report level: a severe error that must be addressed. If ignored,
;; the output will contain severe errors. Typically severe system
;; messages are turned into exceptions which halt processing.

(defparameter +error-levels+
  '(:debug 0 :info 2 :warning 4 :error 6 :severe 8 :terminal 10))

(define-condition docutils-condition(condition)())
(define-condition markup-condition(docutils-condition)
  ((node :initform nil :reader error-node :initarg :node)
   (line :initform nil :initarg :line :accessor error-line)
   (source :initform nil :initarg :source :reader error-source)
   (level :initform :warning :initarg :level  :reader error-level)
   (data :initform nil :initarg :data :reader error-data)
   (backrefs :initform nil :initarg :backrefs :reader error-backrefs)
   (message :initform nil :initarg :message :reader error-message))
  (:report (lambda(c os)
             (let ((msg (error-message c)))
               (format os "~A~@[ ~S~] ~@[for ~S node~] ~@[at line ~D~]"
		       (error-level c)
                       (if (and msg (listp msg))
                           (apply #'format (cons nil msg))
                           msg)
                       (when (error-node c)
                         (class-name (class-of (error-node c))))
                       (error-line c)))))
  (:documentation "An condition in the structured text markup. level
can be used by handlers to determine whether to ignore, print, record or abort
parsing"))

(defgeneric error-severity(entity)
  (:documentation "Return the error-severity (0-10) for an entity"))

(defmethod error-severity((c markup-condition))
  (getf +error-levels+ (error-level c) 6))

(defmethod error-severity((c symbol))
  (getf +error-levels+ c 6))

(defvar *system-message-destination* nil
  "Destination node for any system messages")

(defun report(level message &key
              (node *system-message-destination*)
              (line (if node (line node) *current-line-number*))
              source backrefs data)
  "Signals a report. Handlers will check settings to either halt
processing or issue a report and return. If a system message has been
added by handler, it is returned using the system-message restart"
  (restart-case
    (signal 'markup-condition
            :line line
            :data data
            :source source
            :backrefs backrefs
            :level level
            :message (if (listp message)
                         (apply #'format (cons nil message))
                         message)
            :node node)
    (system-message(&optional system-message)
      (when system-message (add-child node system-message))
      system-message)))

(defmacro with-reports-to-node((node) &body body)
  `(let ((*system-message-destination* ,node))
    ,@body))