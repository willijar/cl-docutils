;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :cl-user)

(defpackage :docutils.parser
  (:documentation "Library for docutils parsers")
  (:use :cl :docutils :docutils.utilities)
  (:import-from :cl-ppcre #:scan)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :jarw.string #:join-strings)
  (:import-from :cl-ppcre #:scan)
  (:export
   #:state-machine-eof #:unexpected-indentation #:state-machine
   #:state-machine-run  #:state-correction #:transition-correction
   #:goto-line #:current-line #:next-line #:previous-line
   #:abs-line-offset #:abs-line-number #:add-states #:get-text-block
   #:input-lines #:line-offset #:next-line-blank-p
   #:state #:no-match #:bof #:eof #:nop #:initial-state
   #:wsp-state-machine #:wsp-state #:+wsp-transitions+ #:wsp-state-machine
   #:blank #:indent #:get-indented #:blank-finish
   #:known-indent #:known-first-indent
   #:initial-transitions #:add-transitions #:remove-transition
   #:match #:matches #:match-string #:match-start #:match-end
   #:match-group #:match-group-length #:+wsp-transitions+
   #:match-reg-starts #:match-reg-ends #:matches
   #:transition-name #:transitions #:transition-match
   #:insert-lines #:apply-transition))


(defpackage :docutils.parser.tables
   (:documentation "CALS Table parser")
   (:use :cl :docutils :docutils.utilities)
   (:import-from :cl-ppcre #:create-scanner #:scan)
   (:import-from :jarw.lib #:while #:for)
   (:export #:parse-table #:simple-table-parser #:grid-table-parser
	    #:table-condition))

(defpackage :docutils.parser.rst
  (:documentation "Restructured text parser for docutils")
  (:use :cl :docutils :docutils.parser :docutils.utilities
        :docutils.parser.tables)
  (:shadow #:make-node #:line)
  (:import-from :docutils #:add-transform)
  (:import-from :cl-ppcre #:create-scanner #:scan #:do-scans
                #:define-parse-tree-synonym)
  (:import-from :jarw.parse #:invalid-input #:parse-input #:format-output
                #:parse-arguments #:parse-options)
  (:import-from :jarw.string #:split-string #:join-strings
                #:to-roman #:from-roman)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :jarw.lib #:is-prefix-p #:is-suffix-p #:when-bind)
  (:import-from :jarw.media #:length-unit #:convert-length-unit)
  (:export #:rst-reader #:def-directive #:def-role #:&allow-spaces
           #:&option #:&content #:&content-parser))
