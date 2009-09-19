;;;; System definition for doc handling -*- Lisp -*-
;;;; Copyright (C) 2006-2008 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: docutils.asd,v 1.5 2007/07/14 08:13:26 willijar Exp $

(in-package :asdf)

(defsystem "docutils.extensions"
  :name "Docutils Markup Extensions"
  :description "Adds docutils parsing to markup handline"
  :author "Dr. John A.R. Williams"
  :version "0.0.1"
  :maintainer "Dr. John A.R. Williams"
  :licence "GPL v3"
  :depends-on (:docutils :markup :media)
  :components
  ((:file "markup")
   (:file "media")))
