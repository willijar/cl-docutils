;; $Id: rst.lisp,v 1.19 2007/08/03 08:19:08 willijar Exp willijar $
;; Restructured text parser
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CL-docutils

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Attempt to parse the rstructuredtext syntax into markup
;; See http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html


;;; Code:

(in-package :docutils.parser.rst)

(defclass rst-reader(reader)
  ()
  (:documentation "The ReStructured text parser"))

(defmethod transforms((reader rst-reader))
  (append (call-next-method)
          '(docutils.transform:doctitle
            docutils.transform:docinfo
            docutils.transform:substitutions
            docutils.transform:chained-targets
            docutils.transform:anonymous-hyperlinks
            docutils.transform:indirect-hyperlinks
            docutils.transform:footnotes
            docutils.transform:external-targets
            docutils.transform:internal-targets
            docutils.transform:resolve-media)))

(register-settings-spec
 '((:tab-width integer 8
    "Set number of spaces for tab expansion")
   (:trim-footnote-reference-space boolean t
    "Remove spaces before footnote references.")))

(defmethod setting((name (eql :trim-footnote-reference-space)) entity)
  (multiple-value-bind(v v-p) (call-next-method)
    (if v-p
        (values v v-p)
        (values (eql (setting :footnote-references entity) :superscript) nil))))

(defmethod read-document(source (reader rst-reader))
  (let ((state-machine (make-instance 'rst-state-machine)))
    (state-machine-run state-machine (read-lines source) :document *document*)
    (node state-machine)))

(defun docutils:read-rst(input)
  (read-document input (make-instance 'rst-reader)))

(defmacro make-node(type &rest arguments)
  `(docutils:make-node
    ',(intern (string-upcase (eval type)) :docutils.nodes)
    ,@arguments))

(defvar *rst-state-classes*
  '(body bullet-list definition-list enumerated-list
    field-list option-list line-block extension-options explicit text
    definition line substitution-def rfc2822-body rfc2822-list)
  "set of State classes used with `rst-state-machine")

;;; dynamic variables used to record during parsing - replaces python memo
(defvar *title-styles* nil "List of title styles in order")
(defvar *section-level* 0 "Current section level - index in styles")
(defvar *section-bubble-up-kludge* nil)

(defvar *directives* (make-hash-table :test #'equalp)
  "Mapping of directive types to directive functions which take four
arguments, an argument string, an a-list of options, an unparsed
content block and a callback to parse the content block")

(defgeneric get-directive(name directives)
  (:documentation "Given a directive name and a directives entity
  return the directive function. This is implemented as a generic
  function so that the directives can be stored in a class 'shadowing'
  the main rst directives hash.")
  (:method(name (directives hash-table))
    (gethash name directives)))

(defgeneric (setf get-directive)(value name directives)
  (:documentation "Given a directive name and a directives entity
  return the directive function. This is implemented as a generic
  function so that the directives can be stored in a class 'shadowing'
  the main rst directives hash.")
  (:method(value name (directives hash-table))
    (setf (gethash name directives) value)))


;;;; Transition functions
(defgeneric bullet(state match))
(defgeneric list-item(state match))
(defgeneric doctest(state match))
(defgeneric line-block(state match))
(defgeneric option-marker(state match))
(defgeneric grid-table-top(state match))
(defgeneric simple-table-top(state match))
(defgeneric line(state match))
(defgeneric text(state match))
(defgeneric underline(state match))
(defgeneric embedded-directive(state match))
(defgeneric initial-quoted(state match))

(defclass rst-state-machine(wsp-state-machine)
  ((document :type list :accessor document
             :documentation "Top level Document Markup")
   (node :type list :accessor node
         :documentation "Current node in document")
   (match-titles :type boolean :accessor match-titles :initform t))
  (:default-initargs :initial-state 'body :states *rst-state-classes*)
  (:documentation "reStructuredText's master StateMachine."))

(defmethod state-machine-run((state-machine rst-state-machine)
                             (input-lines vector)
                             &key
                             (document (make-node 'document))
                             (inliner rst-patterns)
                             (node document)
                             (match-titles t)
                             &allow-other-keys)
  (setf (document state-machine) document
        (node state-machine) node
        (match-titles state-machine) match-titles)
  (let ((*title-styles* nil)
        (rst-patterns inliner)
        (*section-level* 0)
        (halt-severity (setting :halt-level document))
        (report-severity (setting :report-level document)))
    (handler-bind
        ((markup-condition
          #'(lambda(e)
              (let* ((line (or (error-line e) (abs-line-number state-machine)))
                     (level (error-level e))
                     (message (error-message e))
                     (severity (error-severity e))
                     (node  (error-node e))
                     (msg nil))
                (setf (error-line e) line)
                (when (>= severity report-severity)
                  (when node (setf msg (make-node 'system-message e)))
                  (format *error-output*
                          "~A~@[ line ~A~] ~@[~% ~A~%~]~A~%" ;; and print
                          level line (docutils::error-source e) message ))
                (if (>= severity halt-severity)
                    (error e)
                    (invoke-restart 'docutils.nodes:system-message msg))))))
      (when
          (with-reports-to-node((node state-machine))
            (call-next-method))
        (error "Restructured text state machine results not nil")))))

(defclass nested-state-machine(wsp-state-machine)
  ((node :type list :accessor node
         :documentation "Current node in document")
   (match-titles :type boolean :accessor match-titles :initform t))
  (:default-initargs :initial-state 'body :states *rst-state-classes*)
  (:documentation "StateMachine run from within other StateMachine
runs, to parse nested document structures."))

(defmethod state-machine-run((state-machine nested-state-machine)
                             (input-lines simple-vector)
                             &key
                             (input-offset 0)
                             node
                             (match-titles t)
                             &allow-other-keys)
  (declare (ignore input-offset))
  (setf (node state-machine) node
        (match-titles state-machine) match-titles)
  (when (with-reports-to-node((node state-machine))
          (call-next-method))
    (error "Nested State Machine results not nil")))

(defclass rst-state(wsp-state)
  ((parent :accessor parent
           :documentation "parent node being processed"))
  (:documentation "Associated methods used by all State subclasses."))

(defmethod initialize-instance :after ((state rst-state)
                                       &key &allow-other-keys)
  (setf (parent state) (node (state-machine state))))

(defmethod apply-transition((state rst-state) transition match)
  (with-reports-to-node((parent state))
    (call-next-method)))

(defun unindent-warning(state node-name)
  (report :warning
          (format nil "~S ends without a blank line; unexpected unindent."
                  node-name)
          :line (1+ (abs-line-number (state-machine state)))))

(defmethod no-match((state rst-state) transitions)
  "This code should never be run."
  (report :severe
          (format nil
                  "No transition pattern match for state ~A,~% transitions ~A."
                  state transitions)
          :node (parent state)
          :line (abs-line-number (state-machine state))))

(defun nested-parse(state block input-offset node &key
                        (match-titles nil)
                        (states *rst-state-classes*)
                        initial-state)
  "Create a new StateMachine rooted at `node` and run it over the
input `block`."
  (let ((state-machine (make-instance 'nested-state-machine :states states)))
    (state-machine-run state-machine
                       block
                       :input-offset input-offset
                       :node node
                       :initial-state (or initial-state
                                          (initial-state state-machine))
                       :match-titles match-titles)
    (setf (blank-finish (state-machine state)) (blank-finish state-machine))
    (abs-line-offset state-machine)))

(defun section(state title source style lineno)
  "Check for a valid subsection and create one if it checks out."
  (when (check-subsection state style source lineno)
    (new-subsection state title lineno)))

(defun check-subsection(state style source lineno)
  "Check for a valid subsection header.

When a new section is reached that isn't a subsection of the current
section, back up the line count (use ``previous_line(-x)``), then
 terminate-state-machine so the
 calling StateMachine can re-examine the title. This will work its way
back up the calling chain until the correct section level is reached."
  (flet ((title-inconsistent()
           (report :severe
                   (format nil "Title Level Inconsistent:~%~S~%" source)
                   :source source
                   :line lineno)))
    ;;;; check for existing title style
    (let ((level (position style *title-styles* :test #'equal)))
      (cond ((not level)
             ;; unknown title style
             (if (= (length *title-styles*) *section-level*)
                 ;; new subsection if at lowest lebel
                 (setf *title-styles* (nconc *title-styles* (list style)))
                 (title-inconsistent))) ;; otherwise incosistent style
            ((< level *section-level*) ;; sibling or supersection
             (when (> (length style) 1)
               (setf *section-bubble-up-kludge* t))
             (previous-line (state-machine state) (1+ (length style)))
                                        ;let parent section re-evaluate
             (throw 'state-machine-eof nil))
            ((= level *section-level*)) ;; immediate subsection
            ((title-inconsistent)))))) ;; invalid subsection

(defun new-subsection(state title lineno)
  (let* ((section-node (make-node 'section :line lineno))
         (title-node (make-node 'title)))
    (add-child (node (state-machine state)) section-node)
    (add-child section-node title-node)
    (add-child title-node (inline-text title lineno))
    (setf (attribute section-node :name) (as-text title-node)
          ;;(normalise-name (as-text title-node))
          )
    (let* ((mylevel *section-level*)
           (*section-level* (1+ *section-level*))
           (offset (1+ (line-offset (state-machine state))))
           (abs-offset (1+ (abs-line-offset (state-machine state))))
           (new-abs-offset
            (nested-parse
             state
             (subseq (input-lines (state-machine state)) offset)
             abs-offset
             section-node
             :match-titles t)))
      (goto-line state new-abs-offset)
      (when (<= *section-level* mylevel)
        ;; can't handle next section so bubble up
        (throw 'state-machine-eof nil)))))

(defun paragraph(lines lineno)
  "Return a paragraph node & a boolean: literal-block next?"
  (let* ((data (join-strings (map 'list #'rstrip lines)))
         (literal-next nil)
         (text nil)
         (p (make-node 'paragraph :line lineno)))
    (if (is-suffix-p "::" data) ;; literal next
        (let ((text-end (- (length data) 3)))
          (cond
            ((< text-end 0) (return-from paragraph (values nil t)))
            ((not (graphic-char-p (char data text-end)))
             (setf data (rstrip (subseq data 0 text-end))))
            (t (setf text (subseq data 0 (1- (length data))))))
          (setf literal-next t))
        (setf text data))
    (add-child p (inline-text text lineno));;;XXX
    (values p literal-next)))

(defun inline-text(text lineno)
  (parse-inline rst-patterns text :line lineno ))

(defparameter +roman-numeral-map+
  '(("M"  . 1000) ("CM" . 900) ("D"  . 500) ("CD" . 400)
    ("C"  . 100) ("XC" . 90) ("L"  . 50) ("XL" . 40)
    ("X"  . 10) ("IX" . 9) ("V"  . 5) ("IV" . 4) ("I"  . 1)))

(defun to-roman(n)
  "convert integer to Roman numeral"
  (unless (< 0 n 4000) (error "number out of range (must be 1..3999)"))
  (unless (integerp n) (error "non-integers can not be converted"))
  (with-output-to-string(os)
    (dolist(item +roman-numeral-map+)
      (let ((numeral (car item))
            (int (cdr item)))
        (loop
         (when (< n int) (return))
         (write-sequence numeral os)
         (decf n int))))))

(defun from-roman(s)
  "Convert roman numeral to integer"
  (let ((result 0)
        (index 0)
        (slen (length s)))
    (dolist(item +roman-numeral-map+)
      (let* ((numeral (car item))
             (int (cdr item))
             (len (length numeral)))
        (loop
         (let ((end (+ index len)))
           (unless (<= end slen) (return))
           (unless (string-equal numeral s :start2 index :end2 end)
             (return))
           (incf result int)
           (incf index len)))))
    (when (< index slen) (error "Invalid Roman numeral ~S" s))
    result))

(defparameter grid-table-top-pattern "^\\+-[-+]+-\\+ *$")
(defparameter simple-table-top-pattern  "^=+( +=+)+ *$")
(defparameter +enumerated-lists+
  `((:arabic "[0-9]+"
     ,#'parse-integer
     ,#'princ-to-string
     "1")
    (:loweralpha "[a-h,j-z]"
     ,#'(lambda(s) (- (char-code (char s 0)) (1- (char-code #\a))))
     ,#'(lambda(v) (code-char (+ v (1- (char-code #\a)))))
     "a")
    (:upperalpha "[A-H,J-Z]"
     ,#'(lambda(s) (- (char-code (char s 0)) (1- (char-code #\A))))
     ,#'(lambda(v) (code-char (+ v (1- (char-code #\A)))))
     "A")
    (:lowerroman "[ivxlcdm]+"
     ,#'from-roman
     ,#'(lambda(v) (string-downcase (to-roman v)))
     "i")
    (:upperroman "[IVXLCDM]+"
     ,#'from-roman
     ,#'to-roman
     "I"))
  "List of enumerated list types. For each we have its label, regex
fragment, function to convert to ordinal, function to convert from
ordinal and the type value to be given in html")

(defparameter +enum-scanner+
  (create-scanner
   (format nil "^(\\(*)(?:(~A)~{|(~A)~})(\\.|\\))(?: +|$)"
           (second (first +enumerated-lists+))
           (mapcar #'second (rest +enumerated-lists+))))
  "Regex for matching against enumerated lists.")

(defun enum-matcher(string &key (start 0) (end (length string)))
  "pattern matcher for enumerated lists - determins format,type and count"
  (scan +enum-scanner+ string :start start :end end))

(defparameter +rst-transitions+
  (append
   +wsp-transitions+
   `((bullet "^([-+*])(?: +|$)")
     (enumerator ,#'enum-matcher)
     (field-marker "^:([^:]+):(?: +|$)")
     (option-marker option-marker)
     (doctest "^>>>( +|$)")
     (line-block "^\\|( +|$)")
     (grid-table-top ,grid-table-top-pattern)
     (simple-table-top ,simple-table-top-pattern)
     (explicit-markup  "^\\.\\.( +|$)")
     (anonymous "^__( +|$)")
     (line line)
     (text ,#'(lambda(s &key (start 0) (end (length s)))
                (values start end)) 'body))))

(defclass body(rst-state)
  ((initial-transitions :allocation :class :initform +rst-transitions+))
  (:documentation "Generic classifier of the first line of a block."))

(defmethod indent((state body) match)
  "Block quote."
  (multiple-value-bind(indented line-offset)
      (get-indented (state-machine state))
    (let ((block-quote (block-quote state indented line-offset)))
      (add-child (parent state) block-quote)
      (when (not (blank-finish state)) (unindent-warning state :blockquote)))
    nil))

(defun block-quote(state indented line-offset)
  (multiple-value-bind(blockquote-lines attribution-lines attribution-offset)
      (check-attribution indented line-offset)
    (let ((block-quote (make-node 'block-quote)))
      (nested-parse state blockquote-lines line-offset block-quote)
      (when attribution-lines
        (add-child
         block-quote
         (parse-attribution state attribution-lines attribution-offset)))
      block-quote)))

(defun check-attribution(indented line-offset)
  "Check for an attribution in the last contiguous block of `indented`.
   * First line after last blank line must begin with '--' (etc.).
   * Every line after that must have consistent indentation.
   Return 2 values:
     (block quote lines, attribution lines, attribution offset)"
  (multiple-value-bind(blank match-end)
      (let* ((last-blank
              (position-if #'line-blank-p indented
                           :from-end t
                           :end (position-if (complement #'line-blank-p)
                                             indented
                                             :from-end t))))
        (when last-blank
          (multiple-value-bind(start end)
              (scan "-{2,3} *(?=[^ \n])" (aref indented (1+ last-blank)))
            (declare (ignore start))
            (values last-blank end))))
    (unless match-end (return-from check-attribution indented))
    (let ((indent 0))
      (when (and blank (> (- (length indented) blank) 2)) ; multi-line
        (setf indent (indent-level (aref indented (+ 2 blank))))
        (when (position-if #'(lambda(line) (/= indent (indent-level line)))
                           indented
                           :start (+ 3 blank))
          (return-from check-attribution indented))) ; bad shape
      (let ((a-lines (subseq indented (1+ blank))))
        (setf (aref a-lines 0) (subseq (aref a-lines 0) match-end))
        (setf (subseq a-lines 1)
              (map 'vector #'(lambda(line) (subseq line indent))
                   (subseq a-lines 1)))
        (values (subseq indented 0 blank)
                a-lines
                (+ line-offset blank 1))))))

(defun parse-attribution(state indented line-offset)
  (let* ((text (join-strings (map 'vector #'rstrip indented)))
         (lineno (+ (abs-line-offset (state-machine state)) line-offset)))
    (make-node 'attribution :line lineno (inline-text text lineno) )))

(defmethod bullet((state body) match)
  "Bullet list item."

  (let ((bulletlist (make-node 'bullet-list
                               :bullet (char (match-group match 0) 0))))
    (add-child (parent state) bulletlist)
    (let ((item  (list-item state (match-end match))))
      (add-child bulletlist item)
      (goto-line state
                 (nested-parse
                  state
                  (subseq (input-lines (state-machine state))
                          (1+ (line-offset (state-machine state))))
                  (1+ (abs-line-offset (state-machine state)))
                  bulletlist
                  :initial-state 'bullet-list))
      (unless (blank-finish state) (unindent-warning state "Bullet list"))))
  nil)

(defun enum-args(match)
  "Given a match returned from enum-matcher return the format,
sequence,ordinal and text parameters"
  (when (match-start match)
    (let ((prefix (match-group match 0))
          (suffix (match-group match
                               (1- (length (match-reg-starts match))))))
      (when (or (= 0 (length prefix)) (equal suffix ")"))
        (let* ((p (position-if #'identity
                               (match-reg-starts match) :start 1))
               (enum (elt +enumerated-lists+ (1- p))))
          (values
           (cond ((> (length prefix) 0) :parens)
                 ((equal suffix ")") :rparens)
                 ((equal suffix ".") :period)) ; format
           (first enum)                 ; sequence
           (funcall (third enum) (match-group match p)) ; ordinal and text
           (subseq (match-string match) (match-end match))))))))

(defgeneric enumerator(state match)
  (:documentation  "Parse an enumerated list item")
  (:method((state body) match)
    (multiple-value-bind(format sequence ordinal text) (enum-args match)
      (when (not (is-enumerated-list-item state ordinal sequence format))
        (invoke-restart 'transition-correction 'text))
      (let ((enumlist (make-node 'enumerated-list
                                 :type sequence :start ordinal)))
        (add-child (parent state) enumlist)
        (when (/= ordinal 1)
          (setf (attribute enumlist :start) ordinal)
          (report
           :info `("Enumerated list start value not ordinal 1: ~S (ordinal ~S)"
                    ,text ,ordinal)))
        (let ((listitem (list-item state (match-end match))))
          (add-child enumlist listitem)
          (goto-line state
                     (nested-parse
                      state
                      (subseq (input-lines (state-machine state))
                              (1+ (line-offset (state-machine state))))
                      (1+ (abs-line-offset (state-machine state)))
                      enumlist
                      :initial-state `(enumerated-list :ordinal ,ordinal
                                       :format ,format)))
          (unless (blank-finish state)
            (unindent-warning state "Enumerated List")))))
    nil))

(defmethod list-item(state indent)
  (multiple-value-bind(indented line-offset)
      (get-indented (state-machine state) :block-indent indent)
    (let ((node (make-node 'list-item)))
      (when indented (nested-parse state indented line-offset node))
      node)))

(defun is-enumerated-list-item(state ordinal sequence format)
  "Check validity based on the ordinal value and the second line.
   Return true if the ordinal is valid and the second line is blank,
   indented, or starts with the next enumerator."
  (unless ordinal (return-from is-enumerated-list-item nil))
  (let ((next-line
         (or
          (catch 'state-machine-eof
            (unwind-protect
                 (next-line (state-machine state))
              (previous-line (state-machine state))))
          (return-from is-enumerated-list-item t))))
    (when (or (= 0 (length next-line))
              (wsp-char-p (aref next-line 0)))
      (return-from is-enumerated-list-item t))
    (is-prefix-p (make-enumerator (1+ ordinal) sequence format)
                 next-line)))

(defun make-enumerator(ordinal sequence format)
  (let ((enum (assoc sequence +enumerated-lists+)))
    (when enum
      (format nil
              (ecase format
                (:parens "(~A)")
                (:rparens "~A)")
                (:period "~A."))
              (funcall (fourth enum) ordinal)))))

(defgeneric field-marker(state match)
  (:method((state body) match)
    (let ((field-list (make-node 'field-list)))
      (add-child (parent state) field-list)
      (add-field field-list state match)
      (goto-line state
                 (nested-parse
                  state
                  (subseq (input-lines (state-machine state))
                          (1+ (line-offset (state-machine state))))
                  (1+ (abs-line-offset (state-machine state)))
                  field-list
                  :initial-state 'field-list))
      (unless (blank-finish state) (unindent-warning state "Field List")))
    nil))

(defun add-field(parent state match)
  (let* ((name (match-group match 0))
         (lineno (abs-line-number (state-machine state)))
         (indented (get-indented (state-machine state)
                                 :first-indent (match-end match)))
         (field-node (make-node 'field :line lineno))
         (field-name (make-node 'field-name))
         (field-body (make-node 'field-body)))
    (add-child parent field-node)
    (add-child field-node (list field-name field-body))
    (add-child field-name (inline-text name lineno))
    (when indented
      (parse-field-body
       state indented (abs-line-offset (state-machine state)) field-body))
    field-node))

(defgeneric parse-field-body(state indented offset node)
  (:method (state indented offset node)
    (nested-parse state indented offset node)))

(defmethod doctest((state body) match)
  (let ((doctest-block (make-node 'doctest-block)))
    (add-child (parent state) doctest-block)
    (add-child doctest-block
               (join-strings
                (get-text-block (state-machine state)) #\newline)))
  nil)

(defmethod line-block((state body) match)
  "First line of a line block."
  (let ((line-block (make-node 'line-block)))
    (add-child (parent state) line-block)
    (let ((line (line-block-line state match
                                 (abs-line-number (state-machine state)))))
      (add-child line-block line)
      (unless (blank-finish state)
        (goto-line state
                   (nested-parse
                    state
                    (subseq (input-lines (state-machine state))
                            (1+ (line-offset (state-machine state))))
                    (1+ (abs-line-offset (state-machine state)))
                    line-block
                    :initial-state 'line-block)))
      (unless (blank-finish state)
        (report :warning "Line block ends without a blank line."
                :line (1+ (abs-line-number (state-machine state)))))
      (when (and (> (number-children line-block) 0)
                 (not (attribute line-block :indent)))
        (setf (attribute line-block :indent) 0))
      (nest-line-block-lines line-block))
    nil))

(defun line-block-line(state match lineno)
  "Return one line element of a line_block."
  (let ((line (make-node 'line))
        (indented
         (get-indented (state-machine state)
                       :first-indent (match-end match)
                       :until-blank t)))
    (add-child line (inline-text (join-strings indented) lineno))
    (when (/= (match-end match) (length (match-string match))) ; not empty
      (setf (attribute line :indent)
            (1- (match-group-length match 0))))
    line))

(defun nest-line-block-lines(line-block)
  (let ((indent (attribute line-block :indent)))
    (with-children(child line-block)
      (if (attribute child :indent)
          (setf indent (attribute child :indent))
          (when indent (setf (attribute child :indent) indent)))))
  (labels((nest-line-block-segment(line-block)
            (let ((least (attribute (child line-block 0) :indent))
                  (new-items nil)
                  (new-block (make-node 'line-block)))
              (with-children(child line-block)
                (setf least (min least (attribute child :indent))))
              (with-children(item line-block :copy t)
                (if (> (attribute item :indent) least)
                    (progn
                      (rem-child line-block item)
                      (add-child new-block item))
                    (progn
                      (when (> (number-children new-block) 0)
                        (nest-line-block-segment new-block)
                        (push new-block new-items)
                        (setf new-block (make-node 'line-block)))
                      (push item new-items))))
              (when (> (number-children new-block) 0)
                (nest-line-block-segment new-block)
                (push new-block new-items))
              (dotimes(x (number-children line-block))
                (rem-child line-block 0))
              (add-child line-block (nreverse new-items)))))
    (nest-line-block-segment line-block)))

(defmethod option-marker((state body) match)
  "Option list item."
  (let ((option-list (make-node 'option-list))
        (listitem
         (handler-case
             (option-list-item state match)
           (markup-condition(e)
             ;; This shouldn't happen- pattern won't match.
             (report :error
                     (format nil "Invalid option list marker: ~S"
                             (error-message e))
                     :line (error-line e))
             (multiple-value-bind(indented line-offset blank-finish)
                 (get-indented (state-machine state)
                               :first-indent (match-end match))
               (add-child (parent state)
                          (block-quote state indented line-offset))
               (unless blank-finish (unindent-warning state "Option List")))
             (return-from option-marker)))))
    (add-child (parent state) option-list)
    (add-child option-list listitem)
    (goto-line state
               (nested-parse
                state
                (subseq (input-lines (state-machine state))
                        (1+ (line-offset (state-machine state))))
                (1+ (abs-line-offset (state-machine state)))
                option-list
                :initial-state 'option-list))
    (unless (blank-finish state) (unindent-warning state option-list))
    nil))

(defun parse-option-marker(match)
  "Return a list of `node.option` and `node.option_argument` objects,
parsed from an option marker match."
  (let ((results nil))
    (dolist(option (cl-ppcre:split ", " (match-string match)))
      (let ((match (or (match 'longopt option) (match 'shortopt option) )))
        (let ((option (make-node 'option))
              (option-string (make-node 'option-string)))
          (push option results)
          (add-child option option-string)
          (add-child option-string (match-group match 0))
          (when (> (match-group-length match 2) 0)
            (let ((option-argument (make-node 'option-argument)))
              (add-child option option-argument)
              (add-child option-argument (match-group match 2)))))))
    (nreverse results)))

(defun option-list-item(state match)
  (let ((options (parse-option-marker match))
        (offset (abs-line-offset (state-machine state))))
    (multiple-value-bind(indented line-offset)
        (get-indented (state-machine state) :first-indent (match-end match))
      (when (= 0 (length indented))
        (goto-line state offset)
        (invoke-restart 'transition-correction 'text))
      (let ((option-list-item (make-node 'option-list-item))
            (option-group (make-node 'option-group))
            (description (make-node 'description)))
        (add-child option-list-item (list option-group description))
        (add-child option-group options)
        (nested-parse state indented line-offset description)
        option-list-item))))

(defmethod grid-table-top((state body) match)
  (declare (ignore match))
  (table-top state #'isolate-grid-table 'grid-table-parser))

(defmethod simple-table-top((state body) match)
  (declare (ignore match))
  (table-top state #'isolate-simple-table 'simple-table-parser))

(defun table-top(state isolator parser)
  (multiple-value-bind(text-block blank-finish) (funcall isolator state)
    (when text-block
      (handler-case
          (build-table state (parse-table parser text-block)
                       (1+ (- (abs-line-number (state-machine state))
                              (length text-block))))
        (table-condition(c)
          (malformed-table state text-block (error-message c)))))
    (unless blank-finish
      (report :warning "Blank line required after table."
              :line (1+ (abs-line-number (state-machine state)))))
    nil))

(defun isolate-grid-table(state)
  (let* ((blank-finish t)
         (text-block
          (map 'vector #'rstrip
               (handler-case
                   (get-text-block (state-machine state) :flush-left t)
                 (unexpected-indentation(e)
                   (report :error "Unexpected indentation"
                           :line (error-line e)
                           :source (error-text-block e))
                   (setf blank-finish nil)
                   (error-text-block e)))))
         (width (length (aref text-block 0)))
         (len (length text-block)))
	;;; check left edge
    (let ((p (position-if #'(lambda(s) (not (member (char s 0) '(#\+ #\|))))
                          text-block)))
      (when p
        (setf blank-finish nil)
        (previous-line (state-machine state) (- len p))
        (setf text-block (subseq text-block 0 p))
        (unless (> p 1)
          (return-from isolate-grid-table
            (values (malformed-table state text-block "Table must have at least 1 row") blank-finish)))))
      ;;; check end
    (let ((p (position-if #'(lambda(s) (scan grid-table-top-pattern s))
                          text-block :start 2 :from-end t))) ; find bottom
      (unless p
        (malformed-table state text-block)
        (return-from isolate-grid-table))
      (when (< p (1- len))
        (setf blank-finish nil)
        (previous-line (state-machine state) (- len p))
        (setf text-block (subseq text-block 0 p))))
      ;;; check right edge
    (if (find-if
         #'(lambda(s) (or (/= (length s) width)
                          (not (member (char s (1- (length s)))
                                       '(#\+ #\|)))))
         text-block)
        (values (malformed-table state text-block)  blank-finish)
        (values text-block blank-finish))))

(defun malformed-table(state block &optional detail)
  (report :error (format nil "Malformed table:~@[~A~]" detail)
          :source (join-strings block #\newline)
          :line (1+ (- (abs-line-number (state-machine state))
                       (length block))))
  nil)

(defun isolate-simple-table(state)
  (let* ((start (line-offset (state-machine state)))
         (lines (input-lines (state-machine state)))
         (limit (1- (length lines)))
         (toplen (line-length (aref lines start)))
         (found 0)
         (end nil)
         (found-at nil)
         (i (1+ start)))
    (loop
     (when (> i limit) (return))
     (let ((line (aref lines i)))
       (when (scan simple-table-top-pattern line)
         (when (/= toplen (line-length line))
           (next-line (state-machine state) (- i start))
           (return-from isolate-simple-table
             (values
              nil
              (list
               (malformed-table
                state
                (subseq lines start (1+ i))
                "Bottom/header table border does not match top border."))
              (or (= i limit) (line-blank-p (aref lines (1+ i)))))))
         (incf found)
         (setf found-at i)
         (when (or (= found 2) (= i limit)
                   (not (line-blank-p (aref lines (1+ i)))))
           (setf end i)
           (return))))
     (incf i))
    (unless end ;; reached end of input-lines
      (multiple-value-bind(extra block)
          (cond ((> found 0)
                 (next-line (state-machine state) (- found-at start))
                 (values " or no blank line after table bottom"
                         (subseq lines start (1+ found-at))))
                (t
                 (next-line (state-machine state) (- i start 1))
                 (values nil (subseq lines start))))
        (return-from isolate-simple-table
          (values nil (list (malformed-table
                             state block
                             (format nil
                                     "No bottom table border found~A" extra)))
                  (not extra)))))
    (next-line (state-machine state) (- end start))
    (values (subseq lines start (1+ end))
            nil
            (or (= end limit)
                (line-blank-p (aref lines (1+ end)))))))

(defun build-table(state tabledata tableline)
  (destructuring-bind(colspecs headrows bodyrows) tabledata
    (let ((table (make-node 'table))
          (tgroup (make-node 'tgroup :cols (length colspecs))))
      (add-child (parent state) table)
      (add-child table tgroup)
      (add-child tgroup
                 (mapcar #'(lambda(v) (make-node 'colspec :colwidth v))
                         colspecs))
      (when headrows
        (let ((thead (make-node 'thead)))
          (add-child tgroup thead)
          (add-table-rows state thead headrows tableline)))
      (let ((tbody (make-node 'tbody)))
        (add-child tgroup tbody)
        (add-table-rows state tbody bodyrows tableline)))))

(defun add-table-rows(state parent array tableline)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (dotimes(row rows)
      (add-child parent
                 (build-table-row
                  state
                  (make-array cols
                              :displaced-to array
                              :displaced-index-offset (* row cols))
                  tableline)))))

(defun build-table-row(state rowdata tableline)
  (let ((row (make-node 'row)))
    (map nil
         #'(lambda(cell)
             (when cell
               (destructuring-bind(morerows morecols offset cellblock) cell
                 (let ((entry (make-node 'entry)))
                   (when (> morerows 0)
                     (setf (attribute entry :morerows) morerows))
                   (when (> morecols 0)
                     (setf (attribute entry :morecols) morecols))
                   (add-child row entry)
                   (if (> (reduce #'+ (map 'list #'length cellblock)) 0)
                       (nested-parse state cellblock
                                     (+ tableline offset) entry))))))
         rowdata)
    row))

(defun footnote(state match)
  (let ((name (normalise-name (match-group match 0)))
        (footnote (make-node 'footnote
                             :line (abs-line-number
                                    (state-machine state)))))
    (multiple-value-bind(indented  offset)
        (get-indented (state-machine state) :first-indent (match-end match))
      (cond
        ((string= name "*") ;; auto symbol
         (setf (attribute footnote :auto) #\*))
        ((string= name "#") ;; autonumbered, no label
         (setf (attribute footnote :auto) 1))
        ((char= (char name 0) #\#) ;; autonumbered and labeled
         (setf (attribute footnote :name) (subseq name 1)
               (attribute footnote :auto) 1))
        (t ;; manually numbered
         (setf (attribute footnote :name) name)
         (let ((label (make-node 'label)))
           (add-child label name)
           (add-child footnote label))))
      (add-child (parent state) footnote)
      (when indented (nested-parse state indented offset footnote))
      (list footnote))))

(defun citation(state match)
  (let* ((label (match-group match 0))
         (name (normalise-name label))
         (citation
          (make-node 'citation
                     :name name
                     :line (abs-line-number (state-machine state)))))
    (add-child (parent state) citation)
    (multiple-value-bind(indented offset)
        (get-indented (state-machine state) :first-indent (match-end match))
      (let ((nlabel (make-node 'label)))
        (add-child nlabel name)
        (add-child citation nlabel))
      (when indented (nested-parse state indented offset citation)))
    (list citation)))

(defparameter +explicit-target-scanner+
  '(:GROUP
    (:ALTERNATION
     #\_
     (:SEQUENCE
      (:REGISTER (:GREEDY-REPETITION 0 1 #\`))
      (:NEGATIVE-LOOKAHEAD (:CHAR-CLASS #\  #\`))
      (:REGISTER (:GREEDY-REPETITION 1 NIL :EVERYTHING))
      non-whitespace-escape-before
      (:BACK-REFERENCE 1)))
    non-whitespace-escape-before
    (:GREEDY-REPETITION 0 1 #\ )
    #\: (:ALTERNATION (:GREEDY-REPETITION 1 NIL #\space ) :END-ANCHOR)))

(defun hyperlink-target(state match)
  (let* ((lineno (abs-line-number (state-machine state)))
         (text-block
          (map 'vector #'escape2null
               (get-indented (state-machine state)
                             :first-indent (match-end match)
                             :until-blank t
                             :strip-indent nil)))
         (escaped (aref text-block 0))
         (block-index 0)
         (target-match nil))
    (loop
     (when (setf target-match (match +explicit-target-scanner+ escaped))
       (return))
     (incf block-index)
     (if (< block-index (length text-block))
         (setf escaped (concatenate 'string escaped
                                    (aref text-block block-index)))
         (progn
           (report :error "malformed hyperlink target." :line lineno)
           (return-from hyperlink-target))))
    (setf text-block (subseq text-block block-index))
    (setf (aref text-block 0)
          (string-trim docutils.utilities::+wsp+
                       (subseq (concatenate 'string (aref text-block 0) " ")
                               (1+ (match-group-length target-match 1)))))
    (add-child (parent state)
               (make-target text-block lineno (match-group target-match 1)))))

(defun make-target(text-block lineno target-name)
  (multiple-value-bind(target-type data)
      (parse-target text-block)
    (case target-type
      (:refname
       (let ((target (make-node 'target
                                :refname (normalise-name data)
                                :indirect-reference-name data)))
         (add-target target-name nil target lineno)
         target))
      (:refuri
       (let ((target (make-node 'target)))
         (add-target target-name data target lineno)
         target))
      (t (report :warn "malformed hyperlink target." :line lineno)
         data))))

(defun parse-target(text-block)
  "Determine the type of reference of a target.
   Returns two values
            - :refname and the indirect reference name or
            - :refuri and the URI"
  (let ((stripped-block (map 'list #'strip text-block)))
    (let* ((last-line (car (last stripped-block)))
           (l (length last-line)))
      (when (and (> l 0) (eql (aref last-line (1- l)) #\_))
                                        ; possible indirect target
        (let ((refname (is-reference (join-strings stripped-block #\space))))
          (when refname
            (return-from parse-target (values :refname refname))))))
    (values :refuri
            (let ((data (unescape (join-strings stripped-block nil))))
              (when (> (length data) 0) data)))))

(defun add-target(targetname refuri target lineno)
  (setf (attribute target :line) lineno)
  (cond
    (targetname
     (setf (attribute target :name)
           (normalise-name (unescape targetname)))
     (when refuri
       (setf (attribute target :refuri)
             (if (match 'email-pattern refuri)
                 (concatenate 'string "mailto:" refuri)
                 refuri) )))
    (t
     (when refuri (setf (attribute target :refuri) refuri))
     (setf (attribute target :anonymous) t))))

(defparameter +explicit-reference-scanner+
  (create-scanner
   '(:alternation
     (:sequence (:register simplename) #\_)
     (:sequence #\` (:NEGATIVE-LOOKAHEAD #\space)
      (:register (:non-greedy-repetition 1 nil :everything))
      non-whitespace-escape-before
      "'_"))))

(defun is-reference(reference)
  (multiple-value-bind(start end starts ends)
      (scan +explicit-reference-scanner+ reference)
    (declare (ignore end))
    (when start
      (let ((group (if (aref starts 0) 0 1)))
        (whitespace-normalise-name
         (subseq reference (aref starts group) (aref ends group)))))))

(defparameter +explicit-substitution-scanner+
  (create-scanner
   '(:sequence (:negative-lookahead #\space)
     (:register (:non-greedy-repetition 1 nil :everything))
     non-whitespace-escape-before
     #\|
     (:alternation (:greedy-repetition 1 nil #\space)
      :end-anchor))))

(defun substitution-def(state match)
  (let((lineno (abs-line-number (state-machine state)))
       (subname nil))
    (multiple-value-bind(indented offset)
        (get-indented (state-machine state)
                      :first-indent (match-end match)
                      :strip-indent nil)
      (flet ((blocktext()
               (concatenate 'string
                            (subseq (match-string match) 0 (match-end match))
                            (join-strings indented #\newline))))
        (do* ((index 0 (1+ index))
              (escaped (escape2null (rstrip (aref indented index)))
                       (concatenate 'string escaped " "
                                    (escape2null
                                     (strip (aref indented index))))))
             ((> index (length indented)))
          (multiple-value-bind(start end starts ends)
              (scan +explicit-substitution-scanner+ escaped)
            (when start
              (setf subname (normalise-name (subseq escaped (aref starts 0) (aref ends 0))))
              (setf indented (subseq indented index))
              (setf (aref indented 0)
                    (subseq (concatenate 'string (strip (aref indented 0)) " ")
                            end))
              (return))))
        (unless subname
          (report :severe "malformed substitution definition."
                  :line lineno :data (blocktext))
          (return-from substitution-def nil))
        (setf indented
              (subseq indented
                      (if (line-blank-p (aref indented 0))
                          (progn (incf offset) 1) 0)
                      (let ((p (position-if-not #'line-blank-p indented :from-end t)))
                        (when p (1+ p)))))
        (if (> (length indented) 0)
            (let ((substitution-node
                   (make-node 'substitution-definition
                              :line lineno :subname subname)))
              (add-child (parent state) substitution-node)
              (setf (aref indented 0) (strip (aref indented 0)))
              (goto-line state
                         (nested-parse
                          state indented offset substitution-node
                          :initial-state 'substitution-def))
              (let ((not-inline nil))
                (with-children(child substitution-node)
                  (when (not (typep child '(or docutils.nodes:text
                                            docutils.nodes:inline)))
                    (push child not-inline)))
                (dolist(child (nreverse not-inline))
                  (rem-child substitution-node child)
                  (add-child (parent substitution-node) child)))
              (when (= (number-children substitution-node) 0)
                (rem-child (parent substitution-node) substitution-node)))
            (progn
              (report :warning `("Substitution definition ~s empty or invalid."
                                 ,subname)
                      :line lineno)
              (add-child (parent state)
                         (make-node 'literal-block :line lineno (blocktext)))))))))

(defun directive(state match)
  "A directive block"
  (let* ((type-name (match-group match 0))
         (directive (get-directive (canonical-text type-name) *directives*))
         (lineno (abs-line-number (state-machine state)))
         (initial-line-offset (line-offset (state-machine state)))
         (*current-line-number* lineno))
    (multiple-value-bind(indented line-offset)
        (get-indented (state-machine state)
                      :first-indent (match-end match)
                      :strip-top nil)
      (flet((blocktext() (subseq (input-lines (state-machine state))
                                 initial-line-offset
                                 (1+ (line-offset (state-machine state))))))
        (when (not directive)
          (report :error (format nil "Unknown directive type ~S" type-name)
                  :line lineno
                  :data (blocktext))
          (return-from directive))
        (multiple-value-bind(arguments options content
                                       content-offset option-block)
            (parse-directive-block directive state indented line-offset)
          (handler-bind
              ((invalid-format
                #'(lambda(e)
                    (report :error (write-to-string e :escape nil)
                            :line lineno
                            :data (blocktext)))))
            (funcall (directive-function directive)
                     (parent state)
                     arguments options content
                     #'(lambda(node &rest args)
                         (apply
                          #'nested-parse
                          (nconc
                           (list state content content-offset node)
                           args)))
                     option-block)))))))

(defun parse-directive-block(directive state indented line-offset)
  "Parse a directive block made up of arguments, keyword options and content"
  (when (> (length indented) 0) ;; trim blank ends off indented
    (let ((start-blank (line-blank-p (aref indented 0)))
          (end-blank (line-blank-p (aref indented (1- (length indented))))))
      (cond
        ((and start-blank end-blank (< (length indented) 2))
         (setf indented nil))
        ((or start-blank end-blank)
         (setf indented
               (subseq
                indented
                (if start-blank 1 0)
                (if  end-blank (1- (length indented)) (length indented))))))))
  (setf indented (map 'vector #'rstrip indented))
  (multiple-value-bind(arg-block option-block content-block content-offset)
      (if (and indented (or (directive-arguments directive)
                            (directive-options directive)))
          (let* ((blank-line-index (position-if #'line-blank-p indented))
                 (option-start
                  (position-if #'(lambda(line) (eq (aref line 0) #\:))
                               indented
                               :end (or blank-line-index (length indented))))
                 (content-start
                  (when blank-line-index
                    (position-if-not #'line-blank-p
                                     indented
                                     :start (1+ blank-line-index)))))
            (values (if (or option-start blank-line-index)
                        (subseq indented 0 (or option-start blank-line-index))
                        indented)
                    (when option-start
                      (subseq indented option-start
                              (or blank-line-index (length indented))))
                    (when (and content-start
                               (< content-start (length indented)))
                      (subseq indented content-start (length indented)))
                    (when content-start (1- (+ line-offset content-start)))))
          (values nil nil indented line-offset))
    (when content-block
      (let ((p (position-if-not #'line-blank-p content-block)))
        (setf content-block (if p (subseq content-block p) nil))))
    (unless (directive-content-p directive)
      (when (> (length content-block) 0)
        (report :error `("no content permitted in ~s directive"
                         ,(directive-name directive)))
        (setf content-block nil)))
    (when option-block
      (setf option-block (parse-extension-options state option-block)))
    (handler-bind
        ((invalid-format
          #'(lambda(e)
              (let ((restart
                     (find-if  #'find-restart
                           '(use-default ignore-extra-arguments continue))))
                (report (if (eql restart 'ignore-extra-arguments) :info :error)
                      (write-to-string e :escape nil))
                (invoke-restart restart)))))
      (values
       (when (directive-arguments directive)
         (parse-arguments (directive-arguments directive)
                          (join-strings arg-block #\newline)
                          (directive-allow-spaces-p directive)))
       (when (directive-options directive)
         (parse-options (directive-options directive) option-block))
       content-block
       content-offset
       option-block))))

(defun parse-extension-options(state datalines)
  (let ((node (make-node 'field-list)))
    (let ((offset
           (nested-parse state datalines 0 node
                         :initial-state 'extension-options)))
      (when (or (/= offset (length datalines)) (not (blank-finish state)))
        (report :error  "Invalid Option Block"))
      (extract-extension-options node))))

(defun extract-extension-options(field-list)
  "Given a field list return an alist of names and values"
  (let ((results nil))
    (with-children(field field-list)
      (let ((name (as-text (child field 0))))
        (when (find #\space name)
          (report :error
                  "Extension option field name may not contain multiple words"))
        (let* ((body (child field 1))
               (data
                (when body
                  (if (or (> (number-children body) 1)
                          (not (typep (child body 0)
                                      'docutils.nodes:paragraph)))
                      (report :error (format nil "extension option
field body may contain a single paragraph only (option ~s" name))
                      (as-text (child body 0))))))
          (push (cons name data) results))))
    results))

(defun comment(state match)
  (let ((comment (make-node 'comment)))
    (add-child (parent state) comment)
    (unless (and (line-blank-p
                  (subseq (match-string match) (match-end match)))
                 (next-line-blank-p (state-machine state)))
      (let ((indented
             (get-indented (state-machine state)
                           :first-indent (match-end match))))
        (add-child comment
                   (join-strings
                    (let ((end (position-if-not #'line-blank-p indented :from-end t)))
                      (if (and end (< (1+ end) (length indented)))
                          (subseq indented 0 (1+ end))
                          indented))))))
    nil))

(defun anonymous-target(state match)
  (let ((lineno (abs-line-number (state-machine state))))
    (make-target
     (map 'vector #'escape2null
          (get-indented (state-machine state)
                        :first-indent (match-end match)
                        :until-blank t))
     lineno
     nil)))

(defparameter explicit-constructs
  `((,#'footnote
     . ,(create-scanner
         '(:SEQUENCE
           ".."
           (:GREEDY-REPETITION 1 NIL #\space)
           #\[
           (:REGISTER
            (:ALTERNATION
             (:GREEDY-REPETITION 1 NIL (:CHAR-CLASS (:RANGE #\0 #\9)))
             #\#
             (:sequence #\# simplename)
             #\*))
           #\]
           (:ALTERNATION (:GREEDY-REPETITION 1 NIL #\space) :END-ANCHOR))))
    (,#'citation
     . ,(create-scanner
         '(:SEQUENCE
           ".."
           (:GREEDY-REPETITION 1 NIL #\space)
           #\[ (:REGISTER simplename) #\]
           (:ALTERNATION (:GREEDY-REPETITION 1 NIL #\space) :END-ANCHOR))))
    (,#'hyperlink-target
     . , (create-scanner
          '(:SEQUENCE
            ".."
            (:GREEDY-REPETITION 1 NIL #\space)
            #\_
            (:NEGATIVE-LOOKAHEAD #\space))))
    (,#'substitution-def
     . ,(create-scanner
         '(:SEQUENCE
           ".."
           (:GREEDY-REPETITION 1 NIL #\space)
           #\|
           (:NEGATIVE-LOOKAHEAD #\space))))
    (,#'directive
     . ,(create-scanner
         '(:SEQUENCE
           ".."
           (:GREEDY-REPETITION 1 NIL #\space)
           (:REGISTER simplename)
           (:GREEDY-REPETITION 0 1 #\space)
           "::"
           (:ALTERNATION (:GREEDY-REPETITION 1 NIL #\space) :END-ANCHOR))))))

(defgeneric explicit-markup(state match)
  (:method(state match)
    (explicit-construct state match)
    (explicit-list state)
    nil))

(defun explicit-construct(state match)
  (dolist(construct explicit-constructs)
    (let ((expmatch (match #'(lambda(s &key start end)
                               (scan (cdr construct) s :start start :end end))
                           (match-string match))))
      (when expmatch
        (funcall (car construct) state expmatch)
        (return-from explicit-construct))))
  (comment state match))

(defun explicit-list(state)
  "Create a nested state machine for a series of explicit markup
   constructs (including anonymous hyperlink targets)."
  (goto-line state
             (nested-parse
              state
              (subseq (input-lines (state-machine state))
                      (1+ (line-offset (state-machine state))))
              (1+ (abs-line-offset (state-machine state)))
              (parent state)
              :initial-state 'explicit
              :match-titles (match-titles (state-machine state))))
  (unless (blank-finish state)
    (unindent-warning state (parent state))))

(defgeneric anonymous(state match)
  (:method (state match)
    (add-child (parent state) (anonymous-target state match))
    (explicit-list state)
    nil))

(defvar *context* nil "Context passed to text state")

(defmethod line(state match)
  "Section title overline or transition marker."
  (let ((stripped (strip (match-string match))))
    (cond
      ((match-titles (state-machine state))
       (setf *context* (list stripped))
       (values nil 'line))
      ((string-equal stripped "::")
       (invoke-restart 'transition-correction 'text))
      ((< (length stripped) 4)
       (report :info "Unexpected possible title overline or
transition.  Treating it as ordinary text because it's so short."
               :line (abs-line-number (state-machine state)))
       (invoke-restart 'transition-correction 'text))
      (t
       (report :info (format
                      nil
                      "Unexpected section title or transition:~%~S~%"
                      (current-line (state-machine state)))
               :line (abs-line-number (state-machine state)))
       (values nil 'body)))))

(defmethod text(state match)
  (declare (special *context*))
  (setf *context* (match-string match))
  (values nil 'text))

(defclass rfc2822-body(body)
  ((initial-transitions
    :allocation :class
    :initform (mapcan
               #'(lambda(transition)
                   `(,@(when (eql (transition-name transition) :text)
                             '((rfc2822 "([!-9;-~]+):( +|$)'" body)))
                     ,(list (first transition) (second transition) 'body)))
               +rst-transitions+)))
  (:documentation "RFC2822 headers are only valid as the first
constructs in documents.  As soon as anything else appears, the `Body`
state should take over."))

(defgeneric rfc2822(state match)
  (:documentation "RFC2822-style field list item.")
  (:method ((state rfc2822-body) match)
    (let ((field-list (make-node 'field-list :class :rfc2822)))
      (add-child field-list (parent state))
      (add-child (rfc2822-field state match) field-list)
      (goto-line state
                 (nested-parse
                  (state-machine state)
                  (subseq (input-lines (state-machine state))
                          (1+ (line-offset (state-machine state))))
                  (1+ (abs-line-offset (state-machine state)))
                  field-list
                  :initial-state 'rfc2822-list))
      (when (not (blank-finish (state-machine state)))
        (unindent-warning state field-list))
      nil)))

(defun rfc2822-field(state match)
  (let ((name (match-group match 0))
        (field-node (make-node 'field))
        (field-name (make-node 'field-name))
        (field-body (make-node 'field-body)))
    (add-child field-node (list field-name field-body))
    (add-child field-name name)
    (let ((indented
           (get-indented (state-machine state)
                         :first-indent (match-end match))))
      (when indented
        (nested-parse state
                      indented
                      (line-offset (state-machine state))
                      field-body)))
    field-node))

(defclass specialized-body(body)
  ((initial-transitions :allocation :class :initform nil))
  (:documentation
   "Superclass for second and subsequent compound element members.  Compound
    elements are lists and list-like constructs.

    All transition methods are disabled (redefined as `invalid_input`).
    Override individual methods in subclasses to re-enable.

    For example, once an initial bullet list item, say, is recognized, the
    `BulletList` subclass takes over, with a \"bullet_list\" node as its
    container.  Upon encountering the initial bullet list item, `Body.bullet`
    calls its ``self.nested_list_parse`` (`RSTState.nested_list_parse`), which
    starts up a nested parsing session with `BulletList` as the initial state.
    Only the ``bullet`` transition method is enabled in `BulletList`; as long
    as only bullet list items are encountered, they are parsed and inserted
    into the container.  The first construct which is *not* a bullet list item
    triggers the `invalid_input` method, which ends the nested parse and
    closes the container.  `BulletList` needs to recognize input that is
    invalid in the context of a bullet list, which means everything *other
    than* bullet list items, so it inherits the transition list created in
    `Body`."))

(defun select-rst-transitions(&rest rest)
  (mapcan #'(lambda(trans) (when (member (transition-name trans) rest)
                             (list trans)))
          +rst-transitions+))

(defun invalid-specialised-input(state match)
  "Not a compound element member. Abort this state machine."
  (declare (ignore match))
  (previous-line (state-machine state))
  (throw 'state-machine-eof nil))

(defmethod no-match((state specialized-body) transitions)
  (invalid-specialised-input state nil))

(defclass bullet-list(specialized-body)
  ((initial-transitions :allocation :class
                        :initform (select-rst-transitions 'blank 'bullet)))
  (:documentation "Second and subsequent bullet_list list_items."))

(defmethod bullet((state bullet-list) match)
  "Bullet list item."
  (if (char/= (char (match-string match) 0)
              (attribute (parent state) :bullet))
      (invalid-specialised-input state match)
      (add-child (parent state) (list-item state (match-end match))))
  nil)

(defclass definition-list(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform  (select-rst-transitions 'explicit-markup 'text)))
  (:documentation "Second and subsequent definition_list_items."))

(defmethod text((state definition-list) match)
  "Definition lists."
  (setf *context*  (match-string match))
  (values nil 'definition))

;; a bug fix on Python vesion which didn't recognise explicit markup
;; immediately following a definition list!!
(defmethod explicit-markup((state definition-list) match)
  (declare (ignore match))
  (previous-line (state-machine state))
  (throw 'state-machine-eof nil))

(defclass enumerated-list(specialized-body)
  ((lastordinal :initform 0 :initarg :ordinal :accessor lastordinal)
   (list-format :initarg :format :accessor list-format)
   (initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'blank 'enumerator)))
  (:documentation "Second and subsequent enumerated-list listitems."))

(defmethod enumerator((state enumerated-list) match)
  "Enumerated list item."
  (multiple-value-bind(format sequence ordinal) (enum-args match)
    (when (not (and (eq sequence (attribute (parent state) :type))
                    (eq format (list-format state))
                    (eq ordinal (1+ (lastordinal state)))
                    (is-enumerated-list-item state ordinal sequence format)))
      (invalid-specialised-input state nil))
    (let ((item (list-item state (match-end match))))
      (add-child (parent state) item)
      (setf (lastordinal state) ordinal)))
  nil)

(defclass field-list(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'field-marker)))
  (:documentation "Second and subsequent field_list fields."))

(defmethod field-marker((state field-list) match)
  (add-field (parent state) state match)
  nil)

(defclass option-list(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform  (select-rst-transitions 'option-marker)))
  (:documentation "Second and subsequent field_list fields."))

(defmethod option-marker((state option-list) match)
  (handler-case
      (add-child (parent state) (option-list-item state match))
    (markup-error(c)
      (declare (ignore c))
      (invalid-specialised-input state match)))
  nil)


(defclass rfc2822-list(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'rfc2822-body)))
  (:documentation "Second and subsequent field_list fields."))

(defmethod rfc2822((state rfc2822-list) match)
  "Second and subsequent RFC2822-style field_list fields."
  (add-child (parent state) (rfc2822-field state match))
  (values nil 'rfc2822-list))

(defmethod blank((state rfc2822-list) match)
  (invalid-specialised-input state match))

(defclass extension-options(field-list)
  ())

(defmethod parse-field-body((state extension-options) indented offset node)
  "Override `Body.parse_field_body` for simpler parsing."
  (let ((p (make-node 'paragraph)))
    (add-child p (join-strings (map 'list #'strip indented) #\newline))
    (add-child node p)))

(defclass line-block(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'line-block 'blank)))
  (:documentation "Second and subsequent lines of a line_block."))

(defmethod blank((state line-block) match)
  (invalid-specialised-input state match)
  nil)

(defmethod line-block((state line-block) match)
  "New line of line block."
  (add-child (parent state)
             (line-block-line
              state match (abs-line-number (state-machine state))))
  nil)

(defclass explicit(specialized-body)
  ((initial-transitions
    :allocation :class
    :initform (select-rst-transitions 'explicit-markup)))
  (:documentation "Second and subsequent explicit markup construct."))

(defmethod explicit-markup((state explicit) match)
  "Footnotes, hyperlink targets, directives, comments."
  (explicit-construct state match)
  nil)

(defmethod anonymous((state explicit) match)
  "Anonymous hyperlink targets."
  (add-child (parent state) (anonymous-target state match))
  nil)

(defclass substitution-def(body)
  ((initial-transitions
    :allocation :class
    :initform
    `((embedded-directive
       (:sequence (:register simplename) "::"
        (:alternation (:greedy-repetition 1 nil #\space)
                      :end-anchor)))
      (text #'(lambda(a) a)))))
  (:documentation "Parser for the contents of a
substitution_definition element."))

(defmethod embedded-directive((state substitution-def) match)
  (directive state match)
  (throw 'state-machine-eof nil))

(defmethod text((state substitution-def) match)
  (throw 'state-machine-eof nil))

(defclass text(rst-state)
  ((initial-transitions
    :allocation :class
    :initform
    `(,@+wsp-transitions+
      (underline line body)
      (text ,#'(lambda(s &key (start 0) (end (length s)))
                 (values start end)) body))))
  (:documentation "Classifier of second line of a text block. Could be
a paragraph, a definition list item, or a title."))

(defmethod blank((state text) match)
  "End of paragraph."
  (declare (special *context*))
  (multiple-value-bind(paragraph literalnext)
      (paragraph (list *context*)
                 (1- (abs-line-number (state-machine state))))
    (add-child (parent state) paragraph)
    (when literalnext
      (add-child (parent state) (literal-block state)))
    (values nil 'body)))

(defmethod eof((state text))
  (declare (special *context*))
  (when *context* (blank state nil)))

(defmethod indent((state  text) match)
  "Definition list item."
  (declare (special *context*))
  (let ((definitionlist (make-node 'definition-list))
        (definitionlistitem (definition-list-item state *context*)))
    (add-child definitionlist definitionlistitem)
    (add-child (parent state) definitionlist)
    (goto-line
     state
     (nested-parse
      state
      (subseq (input-lines (state-machine state))
              (1+ (line-offset (state-machine state))))
      (1+ (abs-line-offset (state-machine state)))
      definitionlist
      :initial-state 'definition-list))
    (unless (blank-finish state) (unindent-warning state "Definition list")))
  (values nil 'body))

(defmethod underline((state  text) match)
  "Section title"
  (declare (special *context*))
  (let* ((lineno (abs-line-number (state-machine state)))
         (title (rstrip  *context*))
         (underline (match-string match))
         (source (format nil "~A~%~A" title underline)))
    (flet ((report-bad(level &rest args)
             (report level args :line lineno :data source)))
      (with-reports-to-node((parent state))
        (when (> (length title) (length underline))
          (if (< (length underline) 4)
              (progn
                (when (match-titles (state-machine state))
                  (report :info
                          "Possible title underline, too short for the title. Treating it as ordinary text because it's so short."
                          :line lineno))
                (invoke-restart 'transition-correction 'text))
              (report-bad :warning
                          "Title underline too short:~%~A~%~A~%"
                          *context*
                          (current-line (state-machine state)))))
        (if (match-titles (state-machine state))
            (progn
              (setf *context* nil)
              (section state title source (subseq underline 0 1)
                       (1- lineno)))
            (report-bad :severe "Unexpected Section Title"))))))

(defmethod text((state  text) match)
  "Paragraph."
  (declare (special *context*))
  (let* ((startline (1- (abs-line-number (state-machine state))))
         (textblock
          (handler-case
              (get-text-block (state-machine state) :flush-left t)
            (unexpected-indentation(c)
              (report :error  "Unexpected Indentation:"
                      :source (error-text-block c)
                      :line (error-line c))
              (error-text-block c)))))
    (multiple-value-bind(paragraph literal-next-p)
        (paragraph
         (concatenate 'vector (list *context*) textblock)
         startline)
      (add-child (parent state) paragraph)
      (when literal-next-p
        (catch 'state-machine-eof (next-line (state-machine state)))
        (add-child (parent state) (literal-block state)))))
  nil)

(defun literal-block(state)
  "Return a list of nodes."
  (multiple-value-bind(indented offset blank-finish-p)
      (get-indented (state-machine state))
    (let ((end (position-if-not #'line-blank-p indented :from-end t)))
      (if end
          (adjust-array indented (1+ end))
          (return-from literal-block (quoted-literal-block state))))
    (let ((literal-block (make-node 'literal-block
                                    :line (1+ offset) indented)))
      (unless blank-finish-p
        (unindent-warning state literal-block))
      (list literal-block))))

(defun quoted-literal-block(state)
  (let ((abs-line-offset (abs-line-offset (state-machine state)))
        (offset (line-offset (state-machine state)))
        (parent-node (make-node 'element)))
    (let ((*context* nil))
      (declare (special *context))
      (goto-line
       state
       (nested-parse
        state
        (subseq (input-lines (state-machine state)) offset)
        abs-line-offset
        parent-node
        :states '(quoted-literal-block)
        :initial-state 'quoted-literal-block)))
    (let (children)
      (with-children(child parent-node :copy t)
        (rem-child parent-node child)
        (push child children))
      (nreverse children))))

(defun definition-list-item(state termline)
  (multiple-value-bind(indented line-offset blank-finish)
      (get-indented (state-machine state) )
    (let* ((lineno (1- (abs-line-number (state-machine state))))
           (definition-list-item
               (make-node 'definition-list-item :lineno lineno)))
      (multiple-value-bind(termlist messages) (term termline lineno)
        (add-child definition-list-item termlist)
        (let ((definition (make-node 'definition)))
          (add-child definition messages)
          (add-child definition-list-item definition)
          (when (is-suffix-p "::" termline)
            (report :info
                    "Blank line missing before literal block
 (after the \"::\") 'Interpreted as a definition list item."
                    :node  definition
                    :line line-offset))
          (nested-parse state indented line-offset definition)))
      (setf (blank-finish (state-machine state)) blank-finish)
      definition-list-item)))

(defun term(termline lineno)
  (let* ((text-nodes(inline-text termline lineno))
         (term-node (make-node 'term))
         (node-list (list term-node)))
    (dolist(node text-nodes)
      (if (typep node 'docutils.nodes:text)
          (let* ((parts (cl-ppcre:split " +: +" (as-text node)))
                 (txt (make-instance
                       'docutils.nodes:text
                       :text (if (rest parts) (rstrip (car parts)) (car parts)))))
                  (add-child (car node-list) txt)
                  (dolist(part (rest parts))
                    (let((classifier (make-node 'classifier)))
                      (add-child classifier part)
                      (push classifier node-list))))
          (add-child (car node-list) node)))
    (nreverse node-list)))

(defclass specialized-text(text)
  ()
  (:documentation
   "Superclass for second and subsequent lines of Text-variants."))

(defmethod blank((state specialized-text) match)
  (declare (ignore match))
  (throw 'state-machine-eof nil))

(defmethod indent((state specialized-text) match)
  (declare (ignore match))
  (throw 'state-machine-eof nil))

(defmethod underline((state  specialized-text) match)
  (declare (ignore match))
  (throw 'state-machine-eof nil))

(defmethod text((state  specialized-text) match)
  (declare (ignore match))
  (throw 'state-machine-eof nil))

(defmethod eof((state specialized-text))
  nil)

(defclass definition(specialized-text)
  ()
  (:documentation "Second line of potential definition-list-item."))

(defmethod eof((state definition))
  "Not a definition."
  ;; restore blank finish and backtrack so SM can reassess
  (previous-line (state-machine state) 2)
  nil)

(defmethod indent((state definition) match)
  "Definition list item."
  (declare (special *context*))
  (add-child (parent state) (definition-list-item state *context*))
  (values nil 'definition-list))

(defclass line(specialized-text)
  ((eof-check :initform t))
  (:documentation
   "Second line of over- & underlined section title or transition marker."))

(defmethod eof((state line))
  (declare (special *context*))
  (let ((marker (strip (elt *context* 0))))
    (cond
      (*section-bubble-up-kludge* (setf *section-bubble-up-kludge* nil))
      ((< (length marker) 4) (state-correction)))
    (when (slot-value state 'eof-check) ;; ignore EOFError with sections
      (add-child (parent state)
                 (make-node 'transition
                            :line (1- (abs-line-number (state-machine state))))))
    (setf (slot-value state 'eof-check) t)
    nil))

(defmethod blank((state line) match)
  "Transition marker."
  (declare (special *context*))
  (let ((marker (strip (elt *context* 0))))
    (when (< (length marker) 4) (state-correction))
    (add-child (parent state)
               (make-node 'transition
                          :line (1- (abs-line-number (state-machine state)))
                          :source marker))
    (values nil 'body)))

(defmethod text((state line) match)
  "Potential over- & underlined title."
  (declare (special *context*))
  (let* ((lineno (1- (abs-line-number (state-machine state))))
         (overline  (elt *context* 0))
         (title (match-string match))
         (underline nil))
    (labels((source-data()
              (join-strings
               `(,overline ,title
                 ,@(when underline (list underline)))))
            (bad-line-check(msg)
              (if (< (length (rstrip overline)) 4)
                  (short-overline state lineno 2)
                  (progn
                    (report :severe msg :line lineno :data (source-data))
                    (return-from text (values nil 'body))))))
      (unless (catch 'state-machine-eof
                (setf underline (next-line (state-machine state))))
        (bad-line-check "Incomplete Section Title"))
      (setf overline (rstrip overline)
            underline (rstrip underline))
      (cond
        ((not (transition-match
               (find 'underline (transitions state) :key #'transition-name)
               underline))
         (bad-line-check
          "Missing matching underline for section title overline."))
        ((not (string= overline underline))
         (bad-line-check "Title overline & underline mismatch.")))
      (let ((title (rstrip title)))
        (when (> (length title) (length overline))
          (bad-line-check "Title overline too short."))
        (setf (slot-value state 'eof-check) nil)
        (section state (strip title) (source-data)
                 (list (char overline 0) (char underline 0))
                 (1+ lineno))
        (setf (slot-value state 'eof-check) t)
        (values nil 'body)))))

(defmethod indent((state line) match)
  (text state match)) ;; indented title

(defmethod underline((state line) match)
  (declare (special *context*))
  (let* ((underline (elt *context* 0))
         (lineno (1- (abs-line-number (state-machine state)))))
    (when (< (length (rstrip underline)) 4)
      (short-overline state lineno 1))
    (report :error "Possible incomplete section title.
Treating the overline as ordinary text because it's so short."
            :node (parent state)
            :line lineno)
    (values nil 'body)))

(defun short-overline(state lineno
                          &optional (lines 1))
  (report :info
          "Possible incomplete section title.  Treating the overline
as ordinary text because it's so short."
          :node (parent state)
          :line lineno)
  (state-correction lines))

(defun state-correction(&optional (lines 1))
  (invoke-restart 'state-correction 'body :transitions '(text) :lines lines))

(defclass quoted-literal-block(rst-state)
  ((initial-transitions
    :allocation :class
    :initform
    (append +wsp-transitions+
            `((initial-quoted  nonalphanum7bit)
              (text ,#'(lambda(s &key (start 0) (end (length s)))
                         (values start end))))))
   (initial-lineno :accessor initial-lineno :initform -1
                   :initarg :initial-line)
   (context :initform nil :accessor context))
  (:documentation "
    Nested parse handler for quoted (unindented) literal blocks.
    Special-purpose.  Not for inclusion in `state_classes`."))

(defmethod blank((state quoted-literal-block) match)
  (when (context state) (throw 'state-machine-eof nil))
  nil)

(defmethod eof((state quoted-literal-block))
  (if (context state)
      (let ((literal-block (make-node 'literal-block
                                      :line (initial-lineno state))))
        (add-child literal-block (join-strings (nreverse (context state))
                                               #\newline ))
        (add-child (parent state) literal-block))
      (progn
        (report :warning
                "Literal block expected; none found."
                :line (abs-line-number (state-machine state)))
        (previous-line (state-machine state))))
  nil)

(defmethod indent((state quoted-literal-block) match)
  (unless (context state)
    (error "QuotedLiteralBlock.indent: context should not be empty!"))
  (report :error "Unexpected indentation.")
  (previous-line (state-machine state))
  (throw 'state-machine-eof nil))

(defmethod initial-quoted((state quoted-literal-block) match)
  "Match arbitrary quote character on the first line only."
  (remove-transition state 'initial-quoted)
  (add-transitions
   state
   `((quoted
      (:sequence :start-anchor
       (:char-class ,(char (match-string match) 0))))))
  (setf (initial-lineno state) (abs-line-number (state-machine state))
        (context state) (list (match-string match)))
  nil)

(defun quoted(state match)
  "Match consistent quotes on subsequent lines."
  (push (match-string match) (context state))
  nil)

(defmethod text((state quoted-literal-block) match)
  (when (context state)
    (report :error "Inconsistent literal block quoting.")
    (previous-line (state-machine state)))
  (throw 'state-machine-eof nil))

(defclass metadata-reader(rst-reader)
  ((title-only-p :initform nil :initarg :title-only-p))
  (:documentation "An rst reader which reads only title and docinfo"))

(defmethod read-document(source (reader metadata-reader))
  (flet ((is-in(child ancestor)
           (do((parent (parent child) (parent parent)))
              ((or (typep parent 'docutils.nodes:document)
                   (eql parent ancestor))
               (eql parent ancestor)))))
  (let* ((title-only-p (slot-value reader 'title-only-p))
         (docutils.parser::*state-change-hooks*
          (cons
           #'(lambda(state-machine state)
                                        ;(declare (ignore state-machine state))

               (let ((title nil)
                     (docinfo nil))
                 (format
                  t
                  "state=~A~%node=~A~%document=~S~%title=~S~%docinfo=~S~%~%"
                  state (node state-machine) *document* title docinfo)
                 (with-nodes(child *document*)
                   (cond
                     ((and (not title) (typep child 'docutils.nodes:title))
                      (setf title child))
                     ((and (not docinfo)
                           (typep child 'docutils.nodes:field-list))
                      (setf docinfo child))
                    ((and title-only-p title (not (is-in child title)))
                     (throw :finished-metadata *document*))
                    ((and title docinfo
                          (not (is-in child title))
                          (not (is-in child docinfo)))
                     (throw :finished-metadata *document*))))))
          docutils.parser::*state-change-hooks*)))
    (catch :finished-metadata (call-next-method)))))

;;; interface for more sophisticated multisource documents.

(defgeneric title(source)
  (:documentation "Return the subsection title for a source"))

(defgeneric subsections(source)
  (:documentation "Return an ordered list of subsection sources for a source")
  (:method(source) nil))

(defgeneric insert-subsection(source parent-node title)
  (:documentation "Called for each subsection to insert appropriate nodes into a parent node. By default inserts a section entity with given title.")
  (:method (source parent-node title)
    (let ((section-node (make-node 'section))
          (title-node (make-node 'title)))
      (add-child parent-node section-node)
      (add-child section-node title-node)
      (add-child title-node (inline-text title 0))
      (setf (attribute section-node :name)
            (make-name (as-text title-node)))
      section-node)))

(defun insert-metadata(metadata parent-node)
  "Helper function can be called to insert field data into a document"
  (let ((documentp (typep parent-node 'document)))
    (when metadata
      (let ((container
             (docutils:make-node (if documentp 'docinfo 'field-list))))
        (add-child container parent-node)
        (dolist(metadata-item metadata)
          (let ((name (string-downcase (car metadata-item)))
                (value (cdr metadata-item)))
            (if documentp
                (let ((node-type
                       (member name
                               '(author authors organization address contact
                                 version revision status date copyright)
                               :key #'string-downcase :test #'equal)))
                  (when node-type
                    (add-child container (docutils:make-node node-type value))))
                (let ((field (make-node 'field)))
                  (add-child container field)
                  (add-child field (make-node 'field-name name))
                  (add-child field (make-node 'field-body value))))))))))

(defclass recursive-rst-reader(rst-reader)
  ()
  (:documentation "A reader which will recursively read from an entity
  and recurse down through subsections, reading them in turn"))

(defmethod namespace((p pathname)) (pathname-name p))

(defmethod read-document :around (source (reader recursive-rst-reader))
 (let ((docutils::*pending-transforms* (transforms reader))
       (*document* (new-document source)))
   (labels((read-as-subsection(source parent-node)
             (format t "%Reading ~S~%" source)
             (let* ((node
                     (let ((*namespace* nil))
                       (insert-subsection source parent-node (title source))))
                    (*namespace*
                     (or (namespace source) (attribute node :name))))
               (setf (attribute node :namespace) *namespace*)
               (state-machine-run
                (make-instance 'rst-state-machine)
                (read-lines source)
                :document *document*
                :node node
                :match-titles t)
               (dolist(source (subsections source))
                 (read-as-subsection source node)))))
     (read-as-subsection source *document*)
     (docutils::do-transforms docutils::*pending-transforms* *document*)
     *document*)))
