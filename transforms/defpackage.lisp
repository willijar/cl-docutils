;;;;
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

;; This package contains modules for standard tree transforms available
;; to Docutils components. Tree transforms serve a variety of purposes:

;; - To tie up certain syntax-specific "loose ends" that remain after the
;;   initial parsing of the input plaintext. These transforms are used to
;;   supplement a limited syntax.

;; - To automate the internal linking of the document tree (hyperlink
;;   references, footnote references, etc.).

;; - To extract useful information from the document tree. These
;;   transforms may be used to construct (for example) indexes and tables
;;   of contents.

;; Each transform is an optional step that a Docutils Reader may choose to
;; perform on the parsed document, depending on the input context. A Docutils
;; Reader may also perform Reader-specific transforms before or after performing
;; these standard transforms.

(in-package :docutils.transform)

(define-condition transform-condition(condition)())