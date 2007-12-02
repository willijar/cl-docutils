;;;; System definition for doc handling -*- Lisp -*-
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: docutils.asd,v 1.1 2006/07/07 18:34:43 willijar Exp willijar $

(in-package :asdf)
(defsystem docutils.tests
  :name "Docutils Tests"
  :depends-on (:docutils :docutils.figures)
  :components ((:file "tests")))
