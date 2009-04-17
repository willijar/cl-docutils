;;;; Copyright (C) 2002-2006 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: references.lisp,v 1.7 2007/07/26 13:26:19 willijar Exp willijar $

(in-package :docutils.transform)

(register-settings-spec
 '((:autofootnote-start integer 1
    "Start number for automatically numbered footnotes")
   (:symbol-footnote-start integer 0
    "Start symbol index for automatically symbolled footnotes")))

(defun internal-targets(element)
  (collate-nodes(node element)
     (and (typep node 'docutils.nodes:target) (attribute node :name))))

(defun external-targets(element)
  (collate-nodes(node element)
     (and (typep node 'docutils.nodes:target) (attribute node :refuri))))

(defun anonymous-targets(element)
  (collate-nodes(node element)
                (and (typep node 'docutils.nodes:element)
                     (not (typep node 'docutils.nodes:reference))
                     (attribute node :anonymous))))

(defun anonymous-references(element)
  "Return a list of external targets in a document in order of occurance"
  (collate-nodes(node element)
                (and (typep node 'docutils.nodes:reference)
                     (attribute node :anonymous))))

(defclass target-notes(transform)
  ()
  (:default-initargs :priority 540)
  (:documentation
   "Creates a footnote for each external target in the text, and corresponding
    footnote references after each reference."))

(defmethod transform((transform target-notes))
  (let* ((notes (make-hash-table :test #'equal))
         (node (node transform))
         (document (document (node transform)))
         (refnames (refnames document))
         (nodelist))
    (dolist(target (external-targets document ))
      (let ((name (attribute target :name)))
        (if name
            (let ((refs (gethash name refnames)))
              (when refs
                (let ((footnote (make-target-footnote
                                 document target refs notes)))
                  (unless (gethash (attribute target :refuri) notes)
                    (setf (gethash (attribute target :refuri) notes) footnote)
                    (push footnote nodelist)))))
            (warn "No name on target: ~S" target))))
    (let ((anonymous-targets (anonymous-targets document))
          (anonymous-refs (anonymous-references document)))
      (when (= (length anonymous-targets) (length anonymous-refs))
        (map 'nil
             #'(lambda(target ref)
                 (let ((refuri (attribute target :refuri)))
                   (when refuri
                     (let ((footnote (make-target-footnote
                                      transform target (list ref) notes)))
                       (unless (gethash refuri notes)
                         (setf (gethash refuri notes) footnote)
                         (push footnote nodelist))))))
             anonymous-targets anonymous-refs)))
    (add-child (parent node) nodelist (index (parent node) node))
    (rem-child (parent node) node)))

(defun make-target-footnote(document target refs notes)
  (let ((refuri (attribute target :refuri)))
    (multiple-value-bind(footnote footnote-name)
        (let ((dup (gethash refuri notes)))
          (if dup
              (values dup (attribute dup :name))
              (let* ((footnote (make-node 'docutils.nodes:footnote))
                     (footnote-name
                      (format nil "target-note:~A" (set-id footnote document)))
                     (paragraph (make-node 'docutils.nodes:paragraph))
                     (reference (make-node 'docutils.nodes:reference)))
                (setf (attribute footnote :auto) 1
                      (attribute footnote :name) footnote-name)
                (add-child footnote paragraph)
                (add-child paragraph reference)
                (add-child reference refuri)
                (setf (attribute reference :refuri) refuri)
                (values footnote footnote-name))))
      (dolist(ref refs)
        (when (typep ref 'docutils.nodes:target)
          (let* ((refnode (make-node 'docutils.nodes:footnote-reference
                                     :auto 1 :refname footnote-name))
                 (index (1+ (index (parent ref) ref))))
            (add-child (parent ref) refnode index)
            (when (setting :trim-footnote-reference-space document)
              (add-child (parent ref)
                         (make-instance 'docutils.nodes:text :text " ")
                         index)))))
      footnote)))

(defclass chained-targets(transform)
  ()
  (:default-initargs :priority 420)
  (:documentation
   "Attributes \"refuri\" and \"refname\" are migrated from the final direct
    target up the chain of contiguous adjacent internal targets"))

(defmethod transform((transform chained-targets))
  (let ((atts '(:refuri :refname :refid)))
    (with-nodes(node (document (node transform)))
      (when (typep node 'docutils.nodes:target)

        (multiple-value-bind(k v)
            (dolist(k atts)
              (let ((v (attribute node k)))
                (when v (return (values k v)))))
          (when k
            (let ((parent (parent node)))
              (do ((index (1-  (index parent node)) (1- index)))
                  ((< index 0))
                (let ((sibling (child parent index)))
                  (when (or (not (typep sibling 'docutils.nodes:target))
                            (some #'(lambda(a) (attribute sibling a)) atts))
                    (return))
                  (setf (attribute sibling k) v))))))))))

(defclass anonymous-hyperlinks(transform)
  ()
  (:default-initargs :priority 440)
  (:documentation "Link anonymous references to targets"))

(defmethod transform((transform anonymous-hyperlinks))
  (let* ((document (document (node transform)))
         (refs (anonymous-references document))
         (targets (anonymous-targets document)))
    (if (/= (length refs) (length targets))
        (let* ((msg (report :error `("Anonymous hyperlink mismatch: ~d
references but ~d targets. See :backrefs attribute for IDs"
                                     ,(length refs) ,(length targets))))
               (msgid (set-id msg document)))
          (dolist(ref refs)
            (let* ((prb (make-node 'docutils.nodes:problematic
                                   :refid msgid (attribute ref :refid)))
                   (prbid (set-id prb document)))
              (add-backref msg prbid)
              (add-child (parent ref) prb (index (parent ref) ref))
              (remove-node ref))))
        (map 'nil
             #'(lambda(ref target)
                 (setf (referenced target) t)
                 (if (attribute target :refuri)
                     (progn
                       (setf (attribute ref :refuri)
			     (attribute target :refuri))
                       (setf (resolved ref) t))
                     (setf (attribute ref :refname) (attribute target :refname))))
             refs targets))))

(defclass indirect-hyperlinks(transform)
  ()
  (:default-initargs :priority 460)
  (:documentation "process internal and external indirect hyperlinks"))

(defun indirect-targets(document)
  (collate-nodes(node document)
                (and (typep node 'docutils.nodes:target)
                     (not (attribute node :anonymous))
                     (attribute node :refname))))

(defmethod transform((transform indirect-hyperlinks))
  (let* ((document (document (node transform)))
         (nameids (nameids document))
         (ids (ids document)))
    (dolist(target (indirect-targets (document (node transform))))
      (unless (resolved target) (resolve-indirect-target target nameids ids))
      (resolve-indirect-references target))))

(defun resolve-indirect-target(target
                               &optional
                               (nameids (nameids (document target)))
                               (ids (ids (document target)))
                               (refnames (refnames (document target)))
                               (refids (refids (document target))))
  (let* ((refname (attribute target :refname))
         (reftarget-id (gethash refname nameids)))
    (flet ((indirect-target-error(target explanation)
             (let* ((name (attribute target :name))
                    (id (attribute target :id))
                    (naming (format nil "~@[~A ~]~@[(id=~A)~]" name id))
                    (reflist (if name
                                 (gethash name refnames)
                                 (gethash id refids)))
                    (msg (report :error `("Indirect hyperlink target ~s refers to target ~s, ~s." ,naming ,refname ,explanation)))
                    (msgid (set-id msg)))
               (dolist(ref reflist)
                 (let* ((prb (make-node 'docutils.nodes:problematic
                                        (as-text ref) :refid msgid)))
                   (substitute-node prb ref)
                   (add-backref msg (set-id prb))))
               (setf (resolved target) t))
             (return-from resolve-indirect-target))
           (set-refid(target id)
             (setf (attribute target :refid) id)
             (push target (gethash id refids))))
      (unless reftarget-id
        (dolist(f *unknown-reference-resolvers*)
          (when (funcall f target) (return-from resolve-indirect-target)))
        (indirect-target-error target "which does not exist"))
      (let ((reftarget (gethash reftarget-id ids)))
        (when (and (typep reftarget 'docutils.nodes:target)
                   (not (resolved target))
                   (attribute target :refname))
          (when (attribute target :multiply-indirect)
            (indirect-target-error target "forming a circular reference")
            (return-from resolve-indirect-target))
          (setf (attribute target :multiply-indirect) t)
          (resolve-indirect-target reftarget nameids ids refnames refids)
          (rem-attribute  target :multiply-indirect))
        (cond
          ((attribute reftarget :refuri)
           (setf (attribute target :refuri) (attribute reftarget :refuri)))
          ((attribute reftarget :refid)
           (set-refid target (attribute reftarget :refid)))
          ((attribute reftarget :id)
           (set-refid target  (attribute reftarget :id)))
          (t (indirect-target-error
              target
              "which is a duplicate, and cannot be used as a unique reference")))
        (rem-attribute target :refname)
        (setf (resolved target) t)
        (when (typep reftarget 'docutils.nodes:targetable)
          (setf (referenced reftarget) t))))))

(defun resolve-indirect-references(target
                                   &optional
                                   (refnames (refnames (document target)))
                                   (refids (refids (document target))))
  (let* ((attname
          (cond ((attribute target :refid) :refid)
                ((attribute target :refuri) :refuri)
                ((return-from resolve-indirect-references))))
         (attval (attribute target attname)))
    (multiple-value-bind(refatt delatt reflist)
        (cond ((attribute target :name)
               (values :name :refname
                       (or (gethash (attribute target :name) refnames)
                           (error 'transform-condition))))
              ((attribute target :id)
               (values :id :refid (or (gethash (attribute target :id) refids)
                                      (error 'transform-condition))))
              (t (return-from resolve-indirect-references)))
      (unless reflist
        (unless (referenced target)
            (report :info
                    `("Indirect hyperlink target ~S is not referenced."
                      ,(attribute target refatt))
                    :node target)
            (setf (referenced target) t))
        (return-from resolve-indirect-references))
      (dolist(ref reflist)
        (unless (resolved ref)
          (rem-attribute ref delatt)
          (setf (attribute ref attname) attval
                (resolved ref) t)
          (when (typep ref 'docutils.nodes:target)
            (resolve-indirect-references ref))))))
  (setf (referenced target) t))

(defclass external-targets(transform)
  ()
  (:default-initargs :priority 640)
  (:documentation "replace :refname attribute with the direct :refuri attribute for external targets."))

(defmethod transform((transform  external-targets))
  (let* ((document (document (node transform)))
         (refnames (refnames document)))
    (dolist(target (external-targets document))
      (let ((name (attribute target :name))
            (refuri (attribute target :refuri)))
        (when (and name refuri)
          (let ((reflist (gethash name refnames)))
            (if reflist
                (progn
                  (dolist(ref reflist)
                    (unless (resolved ref)
                      (rem-attribute ref :refname)
                      (setf (attribute ref :refuri) refuri
                            (resolved ref) t)))
                  (setf (referenced target) t))
                (unless (or (not (typep target 'docutils.nodes:target))
                            (referenced target))
                  (report :info
                          `("External hyperlink target ~s is not referenced."
                            ,name)
                          :node target)
                  (setf (referenced target) t)))))))))

(defclass internal-targets(transform)
  ()
  (:default-initargs :priority 660)
  (:documentation "replace :refname attribute with the direct :refid attribute for internal targets."))

(defmethod transform((transform internal-targets))
  (let* ((document (document (node transform)))
         (refnames (refnames document)))
    (dolist(target (internal-targets document))
      (let ((name (attribute target :name))
            (refid (set-id target)))
        (when (and name
                   (not (attribute target :refuri))
                   (not (attribute target :refid)))
          (let ((reflist (gethash name refnames)))
            (if reflist
                (progn
                  (dolist(ref reflist)
                    (unless (resolved ref)
                      (rem-attribute ref :refname)
                      (setf (attribute ref :refid) refid
                            (resolved ref) t)))
                  (setf (referenced target) t))
                (unless (referenced target)
                  (report :info
                          `("Internal hyperlink target ~s is not referenced."
                            ,name)
                          :node target)
                  (setf (referenced target) t)))))))))

(defclass footnotes(transform)
  ((autofootnote-labels :initform nil :accessor autofootnote-labels)
   (symbols :initarg :symbols :reader symbols
            :initform "*†‡§¶#♤♡♢♧"))
; '(#\* :|dagger| :|Dagger| :|sect| :|para| :|pilcrow| #\#
;                        :|spades| :|hearts| :|diams| :|clubs|)))
  (:default-initargs :priority 620)
  (:documentation "Assign numbers to autonumbered footnotes, and
resolve links to footnotes, citations, and their references."))

(defmethod transform((transform footnotes))
  (let* ((document (document (node transform)))
         (startnum (or (setting :autofootnote-start document) 0)))
    (setf (setting :autofootnote-start document)
          (number-footnotes transform startnum ))
    (number-footnote-references transform)
    (symbolise-footnotes transform)
    (resolve-footnotes-and-citations document)))

(defun number-footnotes(transform startnum)
  "Assign numbers to autonumbered footnotes.
For labeled autonumbered footnotes, copy the number over to
corresponding footnote references."
  (let* ((document (document (node transform)))
         (nameids (nameids document)))
    (dolist(footnote (autofootnotes document))
      (let ((label
             (loop
		(let ((label (write-to-string startnum)))
		  (incf startnum)
		  (when (not (gethash label nameids))
		    (return label))))))
        (add-child footnote (make-node 'docutils.nodes:label label) 0)
        (unless (attribute footnote :dupname)
          (let ((name (attribute footnote :name)))
            (if name
                (dolist(ref (gethash name (footnote-refs document)))
                  (add-child ref (make-instance 'docutils.nodes:text
                                                :text label))
                  (rem-attribute ref :refname)
                  (setf (attribute ref :refid) (attribute footnote :id))
                  (add-backref footnote (set-id ref))
                  (setf (resolved ref) t))
                (progn
                  (setf (attribute footnote :name) label)
                  (push label (autofootnote-labels transform))))))))
    startnum))

(defun autofootnote-refs(document)
  (collate-nodes(node document)
     (and (typep node 'docutils.nodes:footnote-reference)
          (typep (attribute node :auto) 'number))))

(defun autofootnotes(document)
  (collate-nodes(node document)
     (and (typep node 'docutils.nodes:footnote)
          (typep (attribute node :auto) 'number))))

(defun symbol-footnote-refs(document)
  (collate-nodes(node document)
     (and (typep node 'docutils.nodes:footnote-reference)
          (typep (attribute node :auto) 'character))))

(defun symbol-footnotes(document)
  (collate-nodes(node document)
                (and (typep node 'docutils.nodes:footnote)
                     (typep (attribute node :auto) 'character))))

(defun citation-refs(document)
  (let ((h (make-hash-table :test #'equalp)))
    (with-nodes(node document)
      (when (typep node 'docutils.nodes:citation-reference)
	(let ((refname (attribute node :refname)))
	  (when refname
	    (setf (gethash refname h)
		  (nconc (gethash refname h) (list node)))))))
    h))

(defun citations(document)
  (collate-nodes(node document)
                (typep node 'docutils.nodes:citation)))

(defun footnote-refs(document)
  (let ((h (make-hash-table :test #'equalp)))
    (with-nodes(node document)
      (when (typep node 'docutils.nodes:footnote-reference)
	(let ((refname (attribute node :refname)))
	  (when refname
	    (setf (gethash refname h)
		  (nconc (gethash refname h) (list node)))))))
    h))

(defun footnotes(document)
  (collate-nodes(node document)
                (typep node 'docutils.nodes:footnote)))


(defun number-footnote-references(transform
                                  &optional (nameids (nameids (document (node transform))))
                                  (ids (ids (document (node transform)))))
  "Assign numbers to autonumbered footnote references."
  (do* ((labels (reverse (autofootnote-labels transform)) (rest labels))
	(refs (autofootnote-refs (document (node transform))) (rest refs))
        (label (first labels) (first labels))
	(ref (first refs) (first refs)))
       ((or (not ref) (not label))
        (when labels
          (let* ((msg (report :error
                              `("Too many autonumbered footnote references: only  corresponding footnotes available." ,(length (autofootnote-labels transform)))
                              :node ref))
                 (msgid (set-id msg)))
            (dolist(ref refs)
              (unless (or (resolved ref) (attribute ref :refname))
                (let ((prb (make-node 'docutils.nodes:problematic
                                      (as-text ref) :refid msgid)))
                  (substitute-node prb ref)
                  (add-backref msg (set-id prb)) ))))))
    (let* ((id (gethash label nameids))
           (footnote (gethash id ids)))
      (add-child ref (make-instance 'docutils.nodes:text :text label))
      (setf (attribute ref :refid) id)
      (add-backref footnote (set-id ref))
      (setf (resolved ref) t))))

(defun symbolise-footnotes(transform)
  "Add symbols indexes to '[*]'-style footnotes and references."
  (let ((document (document (node transform))))
    (do* ((footnotes (symbol-footnotes document) (rest footnotes))
          (refs (symbol-footnote-refs document) (rest refs))
          (footnote (first footnotes) (first footnotes))
          (ref (first refs) (first refs)))
         ((or (not ref) (not footnote))
          (when refs
            (let* ((msg (report :error
                                `("Too many symbol footnote references: only ~s corresponding footnotes available." ,(length (symbol-footnotes transform)))
                                :node ref))
                   (msgid (set-id msg)))
              (dolist(ref refs)	;; remaining refs
                (unless (or (resolved ref) (attribute ref :refid))
                  (let ((prb (make-node 'docutils.nodes:problematic
                                        (as-text ref) :refid msgid)))
                    (substitute-node prb ref)
                    (add-backref msg (set-id prb)) ))))))
      (multiple-value-bind(reps index)
          (floor (or (setting :symbol-footnote-start document) 0)
                 (length (symbols transform)))
        (let ((label (make-array (1+ reps)
                                 :initial-element
                                 (elt (symbols transform) index))))
          (add-child ref  label)
          (add-child footnote
                     (make-node 'docutils.nodes:label label)
                     0)))
      (setf (setting :symbol-footnote-start document)
            (1+ (or (setting :symbol-footnote-start document) 0)))
      (setf (attribute ref :refid) (set-id footnote))
      (add-backref footnote (set-id ref)))))

(defun resolve-footnotes-and-citations(document)
  (resolve-references (footnotes document) (footnote-refs document))
  (resolve-references (citations document) (citation-refs document)))

(defun resolve-references(notes refs)
  (dolist(note notes)
    (let ((refs (gethash (attribute note :name) refs)))
      (when refs
        (let ((id (attribute note :id)))
          (dolist(ref refs)
            (unless (resolved ref)
              (rem-attribute ref :refname)
              (setf (attribute ref :refid) id)
              (add-backref note (set-id ref))
              (setf (resolved ref) t))))
        (setf (resolved note) t)))))

(defclass substitutions(transform)
  ()
  (:default-initargs :priority 220)
  (:documentation "Replace substitution-references with  the
    contents of the corresponding substitution-definitions."))

(defun substitution-defs(element)
  "Return hashtable of substitution definitions in a document"
  (let ((refs (make-hash-table :test #'equal)))
    (with-nodes(node element)
      (when (typep node 'docutils.nodes:substitution-definition)
        (let* ((name (normalise-name (attribute node :subname)))
              (oldnode (gethash name refs)))
          (when oldnode
            (report :error `("Duplicate substitution definition name: ~s" ,name)
                    :node oldnode)
            (setf (attribute oldnode :dupname) (attribute oldnode :subname))
            (rem-attribute oldnode :subname))
          (setf (gethash name refs) node))))
    refs))

(defun substitution-refs(element)
  "Return a hashtable of lists of substitution refs keyed by name"
  (let ((refs (make-hash-table :test #'equal)))
    (with-nodes(node element)
      (when (typep node 'docutils.nodes:substitution-reference)
        (let ((name (normalise-name (attribute node :refname))))
          (push node (gethash name refs)))))
    refs))

(defmethod transform((transform substitutions))
  (let* ((document (document (node transform)))
         (defs (substitution-defs document)))
    (maphash
     #'(lambda(refname refs)
         (let ((subdef (gethash refname defs)))

           (dolist(ref refs)
             (if subdef
                 (let* ((parent (parent ref))
                        (index (index parent ref)))
                   (when (and (or (attribute subdef :ltrim)
                                  (attribute subdef :trim))
                              (> index 0))
                     (let ((prev (child parent (1- index))))
                       (when (typep prev  'docutils.nodes:text)
                         (substitute-node
                          (make-instance 'docutils.nodes:Text
                                         :text (rstrip (as-text prev)))
                          prev))))
                   (when (and (or (attribute subdef :rtrim)
                                  (attribute subdef :trim))
                              (< index (1- (number-children parent))))
                     (let ((next (child parent (1+ index))))
                       (when (typep next 'docutils.nodes:text)
                         (substitute-node
                          (make-instance 'docutils.nodes:Text
                                         :text (lstrip (as-text next)))
                          next))))
                   (remove-node ref)
                   (add-child parent
                              (mapcar #'copy-of-node
                                      (slot-value subdef
                                                  'docutils::children))
                              index))
                 (let* ((msg (report :error `("Undefined subsititution referenced: ~s" ,refname)
                                     :node ref))
                        (msgid (set-id msg document))
                        (prb (make-node 'docutils.nodes:problematic :refid msgid (as-text ref)))
                        (prbid (set-id prb document)))
                   (add-backref msg prbid)
                   (substitute-node prb ref))))))
     (substitution-refs document))))

(register-settings-spec
 '((:resolve-media booelan t "Reolve media references to files")))

(defclass resolve-media(transform)
  ()
  (:default-initargs :priority 850)
  (:documentation "Resolve media dependancies"))

(defmethod transform((transform resolve-media))
  (let ((document (document (node transform))))
    (when (setting :resolve-media document)
      (with-nodes(node document)
        (typecase node
          (docutils.nodes:image
           (let ((uri (attribute node :uri)))
               (let ((path (docutils::resolve-dependancy document uri)))
                 (if path
                     (setf (attribute node :uri) path)
                     (report
                      :warning
                      (list "Media uri ~S is either relative or the media file was not found." uri)
                      :node node))))))))))

