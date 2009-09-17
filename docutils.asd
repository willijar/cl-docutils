;;;; System definition for doc handling -*- Lisp -*-
;;;; Copyright (C) 2006-2008 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: docutils.asd,v 1.5 2007/07/14 08:13:26 willijar Exp $

(in-package :asdf)

(defsystem docutils
  :name "Docutils"
  :description "Document utilities and Restructured text parser"
  :author "Dr. John A.R. Williams"
  :version "0.0.1"
  :maintainer "Dr. John A.R. Williams"
  :licence "GPL v3"
  :depends-on (:cl-ppcre :jarw :split-sequence :media)
  :components
  ((:file "defpackage")
   (:file "nodes" :depends-on ("defpackage" "report" "utilities"
                                            "languages"))
   (:file "publisher" :depends-on ("defpackage" "nodes"))
   (:file "utilities" :depends-on ("defpackage"))
   (:file "report" :depends-on ("defpackage" "utilities"))
   (:module "languages" :depends-on ("defpackage")
            :components ((:file "translate")))
   (:module "parsers"
            :depends-on ("publisher"
                         "nodes" "utilities" "report" "transforms" "languages")
            :components
            ((:file "defpackage")
             (:file "state-machine" :depends-on ("defpackage"))
             (:file "inline" :depends-on ("defpackage"))
             (:file "regexp" :depends-on ("defpackage"))
             (:file "tables" :depends-on ("defpackage"))
             (:file "rst" :depends-on
                    ("defpackage" "inline" "regexp" "state-machine" "tables"))
             (:file "directives" :depends-on ("rst" "inline" "roles"))
             (:file "roles" :depends-on ("rst" "inline"))))
   (:module "transforms"
            :depends-on ("nodes" "utilities" "publisher" "report" "languages")
            :components
            ((:file "defpackage")
             (:file "filter" :depends-on ("defpackage"))
             (:file "universal" :depends-on ("defpackage"))
             (:file "references" :depends-on ("defpackage"))
             (:file "frontmatter" :depends-on ("defpackage"))
             (:file "misc" :depends-on ("defpackage"))
             (:file "parts" :depends-on ("defpackage"))))
   (:module "writers"
            :depends-on ("parsers")
            :components
            ((:file "html")
             (:file "latex")))))
