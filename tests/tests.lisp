;; some testing data

(in-package :docutils.parser.rst)

(setf (logical-pathname-translations "DOCUTILS")
      '(("TEST;" "/home/willijar/dev/lisp/src/docutils/tests/*.txt")))

(defun docinfo-lines(document)
  "Return range of lines occupied by docinfo in a document."
  (let ((start nil)
        (end nil))
    (docutils:with-children(node document)
      (cond
        (start
         (when (not (and (typep node 'docutils.nodes:topic)
                         (or (equalp (attribute node :class) "abstract")
                             (equalp (attribute node :class) "dedication"))))
           (let ((line (docutils:line node)))
             (when (> line start)
               (setf end (if end (min end line) line))))))
        ((typep node 'docutils.nodes:docinfo)
         (setf start (docutils:line node)))))
    (values start end)))

(defun run-tests(&optional (dir (directory #p"/home/willijar/dev/lisp/src/docutils/tests/*.txt"))
                 (os *debug-io*))
  (let* ((reader (make-instance 'docutils.parser.rst:rst-reader))
         (writer (make-instance 'docutils.writer.html:html-writer)))
    (dolist(fname dir)
      (format os "~%~%;; ~A~%" fname)
      (let ((document (docutils:read-document fname reader)))
        (setf *document* document)
        (multiple-value-bind(start end)(docinfo-lines document)
          (format *debug-io* "Docinfo from ~A to ~A~%" start end))
        (let ((*package* (find-package :docutils.nodes)))
          (with-open-file(os (merge-pathnames (make-pathname :type "lisp") fname)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (pprint (docutils:as-sexp document) os)))
        (with-open-file(os (merge-pathnames (make-pathname :type "html") fname)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (docutils:write-document writer document os))))))

