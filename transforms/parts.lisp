;;;;
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :docutils.transform)

(defclass contents(transform)
  ()
  (:default-initargs :priority 720)
  (:documentation
   "Include or exclude elements - for use with pending elements"))

(register-settings-spec
 '((:toc-backlinks boolean t
   "Apply backlinks from titles to table of contents")))

(defmethod transform((transform contents))
  (let* ((node (node transform))
         (document (document node))
         (depth (or (attribute node :depth) 99))
         (backlinks (or (attribute node :backlinks)
                        (setting :toc-backlinks document)))
         (start-node
          (if (attribute node :local)
              (do ((n (parent (parent node)) (parent n)))
                  ((typep n 'docutils.nodes:structural)
                   n))
              (document node)))
         (tocid (when (parent start-node)
                  (attribute (parent start-node) :id)))
         (ids (ids document)))
    (labels((build-contents(node &optional (level 0))
              (incf level)
              (let ((sections nil)
                    (auto nil)
                    (entries nil))
                (with-children(child node)
                  (when (typep child 'docutils.nodes:section)
                    (push child sections)))
                (dolist(section (nreverse sections))
                  (let*((title (child section 0))
                        (reference (make-instance 'docutils.nodes:reference)))
                    (setf auto (attribute title :auto))
                    (setf (attribute reference :refid)
                          (set-id section document ids))
                    (copy-contents-title reference title)
                    (let ((refid (set-id reference document ids))
                          (entry (make-instance 'docutils.nodes:paragraph))
                          (item (make-instance 'docutils.nodes:list-item)))
                      (add-child entry reference)
                      (add-child item entry)
                      (case backlinks
                        (:entry (setf (attribute title :refid) refid))
                        (:top (setf (attribute title :refid) tocid)))
                      (when (< level depth)
                        (add-child item (build-contents section level)))
                      (push item entries))))
                (when entries
                  (let ((contents (make-instance 'docutils.nodes:bullet-list)))
                    (add-child contents (nreverse entries))
                    (when auto (add-class contents "auto-toc"))
                    contents)))))
      (let ((contents (build-contents start-node)))
        (when contents
          (add-child (parent node) contents (index (parent node) node)))
        (rem-child (parent node) node)))))

(defun copy-contents-title(destination title)
  (labels((deep-copy(parent node)
            (typecase node
              ((or docutils.nodes:citation-reference
                   docutils.nodes:footnote-reference))
              (docutils.nodes:image
               (jarw.lib:when-bind(alt (attribute node :alt))
                 (add-child parent
                            (make-instance 'docutils.nodes:text :text alt))))
              ((or docutils.nodes:problematic
                   docutils.nodes:reference
                   docutils.nodes:target)
               (with-children(child node)
                 (deep-copy parent child)))
              (t (add-child parent (copy-of-node node))))))
    (with-children(child title)
      (deep-copy destination child))))

(defclass sectnum(transform)
  ()
  (:default-initargs :priority 710) ;; before contents
  (:documentation
   "Automatically assigns numbers to the titles of document sections."))

(defmethod transform((transform sectnum))
  (let* ((node (node transform))
         (maxdepth (or (attribute node :depth) 9999))
         (start  (or (attribute node :start) 1))
         (prefix-txt (or (attribute node :prefix) ""))
         (suffix-txt (or (attribute node :suffix) "")))
    (labels((update-section-numbers(node &optional prefix (depth 0))
              (incf depth)
              (let ((sectnum (if prefix 1 start)))
                (with-children(child node)
                  (when (typep child 'docutils.nodes:section)
                    (let* ((numbers (append prefix (list sectnum)))
                           (title (child child 0))
                           (generated (make-node 'docutils.nodes:generated
                                                 :line (line node))))
                      (add-child generated
                                 (format nil "~A~D~{.~D~}~A "
                                         prefix-txt
                                         (car numbers)
                                         (cdr numbers)
                                         suffix-txt))
                      (add-class generated "sectnum")
                      (add-child title generated 0)
                      (setf (attribute title :auto) t)
                      (when (< depth maxdepth)
                        (update-section-numbers child numbers depth)))
                    (incf sectnum))))))
      (update-section-numbers (document node))
      (rem-child (parent node) node))))

(register-settings-spec
 '((:figure-number (integer :nil-allowed t) 1 "Number figures")
   (:table-number (integer :nil-allowed t) 0 "Number tables. If >0 number number tables separately from figures. If 0 number as figures.")
   (:equation-number (integer :nil-allowed t) 1 "Number equations")))

(defun number-figures(document &key
                      (figlabel #'write-to-string)
                      (eqnlabel #'(lambda(n) (format nil "(~D)" n))))
  (let ((figc (setting :figure-number document))
        (eqnc (setting :equation-number document))
        (tabc 0))
    (let ((labels (make-hash-table))) ;; collate refnames and labels
      (flet ((add-fig-caption(target next counter text)
               (let* ((caption
                       (or
                        (with-children(c next)
                          (when (typep c 'docutils.nodes:caption)
                            (throw :skip-siblings c)))
                        (let ((caption (make-node 'docutils.nodes:caption)))
                          (add-child next caption)
                          caption)))
                      (label (funcall figlabel counter))
                      (generated
                       (make-node 'docutils.nodes:generated
                                  (concatenate
                                   'string
                                   (translated-text text (language next))
                                   " " label ". "))))
                 (setf (gethash (attribute target :id) labels) label)
                 (add-child caption generated 0))))
      ;; Add generated
        (dolist(target (internal-targets document))
          (let ((next (next-sibling target)))
            (cond
              ((and figc (typep next 'docutils.nodes:figure))
               (add-fig-caption target next figc "Figure")
               (incf figc))
              ((and tabc (typep next 'docutils.nodes:table))
               (cond
                 ((zerop tabc)
                  (add-fig-caption target next figc "Figure")
                  (incf figc))
                 (t
                  (add-fig-caption target next tabc "Table")
                  (incf tabc))))
              ((and eqnc (typep next 'docutils.nodes:equation))
               (let ((label (funcall eqnlabel eqnc)))
                 (incf eqnc)
                 (add-child next (make-node 'docutils.nodes:generated label))
                 (setf (gethash (attribute target :id) labels) label))))))
        (let ((refids (refids document)))
          (maphash
           #'(lambda(name label)
               (let ((refs (gethash name refids)))
                 (dolist(ref refs)
                   (rem-child ref 0)
                   (add-child ref (make-node 'docutils.nodes:text label) 0))))
           labels))))))

(defclass fignum(transform)
  ()
  (:default-initargs :priority 711) ;; before contents
  (:documentation
   "Automatically assigns numbers to Figures, tables and equations."))

(defmethod transform((transform fignum))
  (number-figures (document (node transform))))




