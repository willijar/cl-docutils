;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; An attempt to parse the rstructuredtext syntax into markup
;;;; See
;;;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html

(in-package :docutils.parser.rst)

(defgeneric parse-role(role text &optional option-values supplied-content)
  (:documentation "Apply given role to text, returning a list of
markup elements to be inserted in place"))

(defparameter openers "'\"([{<")
(defparameter closers "'\")]}>")

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defun not-quoted(&rest expr)
    "Return a parse tree expression for expr not quoted"
    `(:alternation
      (:sequence
       (:negative-lookbehind (:alternation  #\' #\" #\( #\[ #\{ #\<)) ,@expr)
      (:sequence (:positive-lookbehind #\') ,@expr (:negative-lookahead #\'))
      (:sequence (:positive-lookbehind #\") ,@expr (:negative-lookahead #\"))
      (:sequence (:positive-lookbehind #\() ,@expr (:negative-lookahead #\)))
      (:sequence (:positive-lookbehind #\[) ,@expr (:negative-lookahead #\]))
      (:sequence (:positive-lookbehind #\{) ,@expr (:negative-lookahead #\}))
      (:sequence (:positive-lookbehind #\<) ,@expr (:negative-lookahead #\>))))
  (defun quoted-pattern(&rest expr)
    "Return a parse tree expression for a quoted expression quoted"
    `(:alternation
      (:sequence
       (:positive-lookbehind (:alternation  #\' #\" #\( #\[ #\{ #\<)) ,@expr)
      (:sequence (:negative-lookbehind #\') ,@expr (:positive-lookahead #\'))
      (:sequence (:negative-lookbehind #\") ,@expr (:positive-lookahead #\"))
      (:sequence (:negative-lookbehind #\() ,@expr (:positive-lookahead #\)))
      (:sequence (:negative-lookbehind #\[) ,@expr (:positive-lookahead #\]))
      (:sequence (:negative-lookbehind #\{) ,@expr (:positive-lookahead #\}))
      (:sequence (:negative-lookbehind #\<) ,@expr (:positive-lookahead #\>))))
  )

(defmacro define-inline-element-parse-tree
    (name start
     &optional
     (end start)
     (middle '(:register (:NON-GREEDY-REPETITION 0 NIL :everything))))
  `(define-parse-tree-synonym ,name
    (:sequence :single-line-mode-p
     start-phrase-prefix
     ,(not-quoted start)
     non-whitespace-after
     ,middle
     non-whitespace-before
     ,end
     end-phrase-suffix)))

(define-inline-element-parse-tree strong
    "**")

(define-inline-element-parse-tree math
    "$$")

(define-inline-element-parse-tree emphasis
    (:sequence #\* (:negative-lookahead  #\* ) )
    (:sequence (:negative-lookbehind   #\*) #\*))

(define-parse-tree-synonym role
    (:GREEDY-REPETITION 1 1 (:sequence #\: (:register simplename) #\:)))

(eval-when(:compile-toplevel :load-toplevel :execute)
  (setf (cl-ppcre:parse-tree-synonym 'single-quoted)
        `(:sequence
          ,(not-quoted #\` '(:negative-lookahead #\`))
	  non-whitespace-before
          (:register (:non-greedy-repetition
		      1 NIL
		      (:alternation (:inverted-char-class #\`)
				    ,(quoted-pattern #\`))))
          non-whitespace-before
          ,(not-quoted '(:negative-lookbehind #\`) #\`))))

(define-parse-tree-synonym interpreted
    (:sequence
     start-phrase-prefix
     (:alternation
      (:sequence role single-quoted)
      (:sequence single-quoted role)
      single-quoted)
     end-phrase-suffix))

(define-inline-element-parse-tree literal
    "``")

(define-inline-element-parse-tree internal-target
    "_`"  (:sequence (:negative-lookbehind #\`) #\`))

(define-inline-element-parse-tree substitution-reference
    #\| (:sequence #\| (:register (:greedy-repetition 0 2 #\_) )))

(define-inline-element-parse-tree footnote-reference
    "[" "]_")

(define-parse-tree-synonym reference
    (:sequence
     start-phrase-prefix
     (:alternation single-quoted (:register simplename))
     non-whitespace-before
     (:register (:greedy-repetition 1 2 #\_))
     end-phrase-suffix))

(defparameter rst-patterns
  (mapcar #'(lambda(pattern)
              (cons pattern  pattern))
          `(strong emphasis math interpreted literal internal-target
                   substitution-reference footnote-reference reference  uri)))

;;; algorithm is for each pattern, scan through string collecting all matches
;;; order matches by start, and group by recursion. Remove overlapping patterns
;;; call handler function for outside element of each group
;;; handler function decides whether to recurse or not.

(defun parse-inline(patterns string &key line
                    (start 0) (end (length string)) (language *language*))
  "Parse a string for inline patterns. Return a list of inline markup
elements and system messages. Patterns is a list of patterns to be
applied in turn. Each pattern can either be a symbol naming both the
parse-tree synonym and a function or a cons of a regular expression
pattern and a function.

The pattern functions are called with two arguments, the match
corresponding to the regexp match and a list of remaining patterns to
be applied recursively. They should return a list of inline elements
to be inserted.

The :start and :end keyword arguments have their usual meanings."
  (let ((*language* language))
    (cond
      ((= start end) nil)
      ((null patterns)
       (list (unescape string :restore-backslashes t :start start :end end)))
      ((let ((matches nil))
         ;; get all matches of all patterns in ascending order but ignore
         ;; those which have a quoted start
         (dolist(pattern patterns)
           (multiple-value-bind(pattern function)
               (etypecase pattern
                 (symbol (values pattern pattern))
                 (cons (values (car pattern) (cdr pattern))))
             (setf matches
                   (merge 'list matches
                          (mapcar #'(lambda(m) (cons m function))
                                  (matches pattern string
                                           :start start :end end))
                          #'< :key #'(lambda(r) (match-start (car r)))))))
         ;; remove nested and overlapping elements
         (let ((cursor matches))
           (loop
            (unless (cdr cursor) (return))
            (let ((end (match-end (caar cursor)))
                  (next-start (match-start (caadr cursor)))
                  (next-end (match-end (caadr cursor))))
              (cond
                ((> next-start end))    ; outside - move to next
                ((> next-end end)       ; overlapping - report and remove
                 (report :warning
                         (format nil "Inline markup for ~S not nested"
                                 (cdar matches))
                         :line line
                         :source (unescape (match-string (caadr matches))))
                 (setf (cdr cursor) (cddr cursor)))
                (t (setf (cdr cursor) (cddr cursor))))
              (setf cursor (cdr cursor)))))
         ;; build up result
         (let ((cursor start))
           (flet((make-txt(end)
                   (when (> end cursor) ; only if not zero length
                     (list
                      (make-instance    ; add a text element
                       'docutils.nodes:text
                       :text (unescape
                              string
                              :restore-backslashes t
                              :start cursor :end end))))))
             (nconc
              (mapcan
               #'(lambda(m)
                   (let ((match (car m))
                         (func (cdr m)))
                     (nconc
                      (make-txt (match-start match))
                      (prog1
                          (funcall func match :line line)
                        (setf cursor (match-end match))))))
               matches)
              (make-txt end)))))))))

(defmacro make-inline-nodes(type attributes &optional children)
  (let ((node (gensym)))
    `(list
      (let ((,node
             (apply #'docutils:make-node
                    (cons
                     ',(intern (string-upcase (eval type)) :docutils.nodes)
                     ,attributes))))
        (add-child ,node ,children)
        ,node))))

(defun emphasis(match  &rest attributes)
   (make-inline-nodes 'emphasis attributes
         (parse-inline rst-patterns (match-string match)
           :start (elt (match-reg-starts match) 0)
           :end (elt (match-reg-ends match) 0))))

(defun strong(match  &rest attributes)
   (make-inline-nodes 'strong attributes
        (parse-inline rst-patterns (match-string match)
          :start (elt (match-reg-starts match) 0)
          :end (elt (match-reg-ends match) 0))))

(defun math(match  &rest attributes)
  (make-inline-nodes
   'math attributes
   (list (concatenate 'string "$"
                      (unescape (match-group match 0) :restore-backslashes t) "$"))))

(defun literal(match &rest attributes)
   (make-inline-nodes 'literal attributes
          (list (unescape (match-group match 0) :restore-backslashes t))))

(defun interpreted(match &rest attributes)
  (declare (ignore attributes))
  (let ((role (or (match-group match 0) (match-group match 3)))
        (phrase (or (match-group match 1) (match-group match 2)
                    (match-group match 4))))
    (let ((result (parse-role (or (canonical-text role) role) phrase)))
      (if (listp result) result (list result)))))

(defun internal-target(match &rest attributes )
  (make-inline-nodes
   'target
   `(,@attributes :name ,(normalise-name (unescape (match-group match 0))))
   (parse-inline rst-patterns (match-string match)
                 :start (elt (match-reg-starts match) 0)
                 :end (elt (match-reg-ends match) 0))))

(defun substitution-reference(match &rest attributes)
  (let ((end (match-group-length match 1))
        (refname (normalise-name (match-group match 0))))
    (let ((sub (make-inline-nodes 'substitution-reference attributes
                                  (list refname))))
      (setf (attribute (first sub) :refname) refname)
      (if (> end 0)
          (make-inline-nodes
           'reference
           `(:refname ,refname ,@(when (= 2 end) `(:anonymous t)))
           sub)
          sub))))

(defun footnote-reference(match &rest attributes)
  (let* ((label (match-group match 0))
         (refname (normalise-name label)))
    (cond
      ((string= label "*") ;; auto symbol footnote
       (make-inline-nodes 'footnote-reference `(:auto #\* ,@attributes)))
      ((string= label "#") ;; auto number footnote
       (make-inline-nodes 'footnote-reference `(:auto 1 ,@attributes)))
      ((char= (char label 0) #\#) ;;labeled and autonumbered footnote
       (make-inline-nodes 'footnote-reference
                          `(:auto 1 :refname ,(subseq refname 1)
				  ,@attributes)))
      ((digit-char-p (char label 0)) ;; nanually labelled footnote
       (make-inline-nodes 'footnote-reference
			  `(:refname ,refname ,@attributes)
			  refname))
      (t ;; citation reference
       (make-inline-nodes 'citation-reference `(:refname ,refname ,@attributes)
                          label)))))

(defun add-attributes(node attributes)
  (loop :for a :on attributes :by #'cddr
        :do (setf (attribute node (first a)) (second a))))

(defun reference(match  &rest attributes)
  (let* ((rawtext (or (match-group match 0) (match-group match 1)))
         (uri-match (match 'embedded-uri rawtext))
         (anon-p (> (match-group-length match 2) 1)))
    (flet((reference-node(text)
            (let ((node
                   (docutils:make-node 'docutils.nodes:reference
                                       :anonymous anon-p text)))
              (add-attributes node attributes)
              node)))
      (if uri-match
          (let* ((uri (cl-ppcre::regex-replace
                       "\\s+" (match-group uri-match 0) ""))
                 (text (rstrip
                        (unescape (subseq rawtext 0 (match-start uri-match)))))
                 (refname (normalise-name text))
                 (reference (reference-node text)))
            (setf (attribute reference :refuri) uri)
            (setf (resolved reference) t)
            (if anon-p
                (list reference)
                (let ((target (docutils:make-node
                               'docutils.nodes:target
                               :refuri uri
                               :name refname
                               (match-group uri-match 0))))
                  (add-attributes target attributes)
                  (setf (referenced target) t)
                  (list reference target) )))
          (let* ((text (unescape rawtext))
                 (reference (reference-node text)))
            (setf (attribute reference :refname) (normalise-name text))
            (list reference))))))

(defun uri(match  &rest attributes)
  (declare (ignore attributes))
  (let* ((unescaped (unescape (match-group match)))
         (uri (if (and (not (match-group match 1)) (match-group match 2))
                  (concatenate 'string "mailto:" unescaped)
                  unescaped)))
    (list (docutils:make-node
           'docutils.nodes:reference :refuri uri unescaped))))

