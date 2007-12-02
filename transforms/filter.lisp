;;;; 
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :docutils.transform)

(defclass filter(transform)
  ()
  (:default-initargs :priority 780)
  (:documentation
   "Include or exclude elements - for use with pending elements"))

(defmethod transform((filter filter))
  (let* ((pending (node filter))
         (parent (parent pending)))
    (when (funcall (attribute pending :test) pending)
      (let ((index (index parent pending)))
	(rem-child parent index)
        (dolist(child (attribute pending :nodes))
          (add-child parent child index))))))
  
