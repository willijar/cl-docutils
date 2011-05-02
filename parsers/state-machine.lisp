;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; An attempt to parse the rstructuredtext syntax into markup
;;;; See
;;;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
;;;; $Id: state-machine.lisp,v 1.11 2007/08/03 08:18:32 willijar Exp $

;;;; Where the symantics differ from the document above it should be
;;;; considered an error EXCEPT that we will allow nested inline
;;;; markup.

(in-package :docutils.parser)

(defvar *goto-line-hooks* nil
  "A list of functions called when state machine moves to another line.
Functions take two functions, the state machine and the absolute line offset")

(defvar *state-change-hooks* nil
  "A list of functions called when state is changed. Called with state
machine and the new state object")

(define-condition unexpected-indentation(condition)
  ((line-number :initform nil
		:initarg :line :reader error-line)
   (text-block :initarg :text-block :initform nil :reader error-text-block))
  (:report (lambda(c stream)
	     (format stream "Unexpected indentation~@[ at line number ~D~]:~% ~S"
		     (error-line c) (error-text-block c)))))

(define-condition insert-lines(condition)
	((text-block :initarg :text-block :initform nil :reader error-text-block)))

(defclass state-machine()
  ((input-lines :initarg :input-lines :reader input-lines
		:documentation "vector of input lines (without newlines)")
   (states :type list :initform nil :initarg :states :accessor states
	   :documentation "a list of allowed state classe names.")
   (initial-state :initarg :initial-state :reader initial-state
		  :documentation "the initial state name.")
   (current-state :reader current-state
		  :documentation "the current state.")
   (input-offset
    :initform 0 :reader input-offset :type fixnum :initarg :input-offset
    :documentation "Offset of input-lines from the beginning of the file.")
   (line-offset
    :initform -1 :accessor line-offset :type fixnum
    :documentation
    "Current input line offset from beginning of input-lines."))
  (:documentation
   "A finite state machine for text filters using matching functions

	The input is provided in the form of a list of one-line strings (no
	newlines) which may be modified. States are subclasses of the `State`
	class. Transitions consist of regular expression patterns and
	transition methods, and are defined in each state.

	The state machine is started with the `run()` method, which returns the
	results of processing in a list."))

(defun (setf current-state)(state state-machine)
  (dolist(hook *state-change-hooks*)
    (funcall hook state-machine state))
  (setf (slot-value state-machine 'current-state) state))

(declaim (inline current-line))
(defun current-line(state-machine
		    &optional (index (line-offset state-machine)))
  (aref (input-lines state-machine) index))

(defun insert-lines(state-machine lines
		    &optional (offset (1+ (line-offset state-machine))))
  (assert (> offset (line-offset state-machine))
	  (offset)
	  "Offset must be greater than current line offset ~S"
	  (line-offset state-machine))
  (with-slots(input-lines) state-machine
    (setf input-lines
	  (if (> offset (length input-lines))
	      (concatenate 'vector input-lines lines)
	      (concatenate 'vector
			   (subseq input-lines 0 offset)
			   lines
			   (subseq input-lines offset))))))


(defgeneric state-machine-run(state-machine input-lines
                              &key input-offset initial-state
                              &allow-other-keys)
  (:documentation "Run state machine over input lines filling in document"))


(defmethod state-machine-run((state-machine state-machine) (input-lines vector)
                             &key
                             (input-offset 0)
                             (initial-state (initial-state state-machine))
                             &allow-other-keys)
  "Run the state machine on INPUT-LINES. Return results (a list).

INPUT-LINES: a list of strings without newlines.
INPUT-OFFSET: the line offset of `input_lines` from the beginning
					of the data.
INPUT-SOURCE: name or path of source of INPUT_LINES."
  (setf (slot-value state-machine 'input-lines) input-lines
        (slot-value state-machine 'input-offset) input-offset
        (line-offset state-machine) -1)
  (let* ((state (get-state state-machine initial-state))
         (transitions (transitions state))
         (results nil)
         (*current-line-number* (abs-line-number state-machine)))
    (handler-bind((insert-lines
                   #'(lambda(e)
                       (insert-lines state-machine (error-text-block e))
                       (continue e))))
      (flet((extend-results(result) (when result (push result results))))
        (extend-results (bof state))
        (catch 'state-machine-eof
          (loop
           (next-line state-machine)
           (restart-case
               (multiple-value-bind(result next-state)
                   (check-line state-machine state transitions)
                 (when next-state
                   (setf state (get-state state-machine next-state)))
                 (setf transitions (transitions state))
                 (extend-results result))
             (transition-correction(new-transition)
               (previous-line state-machine)
               (setf transitions (list (assoc new-transition transitions))))
             (state-correction(next-state-name &key transitions lines)
               (previous-line state-machine lines)
               (setf state (get-state state-machine next-state-name)
                     transitions (or (mapcar
                                      #'(lambda(tr)
                                          (member tr (transitions state)
                                                  :key #'car))
                                      transitions)
                                     (transitions state)) )))))
        (extend-results (eof state))))
    (nreverse results)))

(defun get-state(state-machine &optional next-state)
  "Return new state class object"
  (when next-state
    (unless (member (if (listp next-state) (first next-state) next-state)
                    (states state-machine))
      (error "Unknown state ~A for ~A" next-state state-machine))
    (setf (current-state state-machine)
          (apply #'make-instance
                 `(,@(if (listp next-state) next-state (list next-state))
		     :state-machine ,state-machine))))
  (current-state state-machine))

(defun next-line(state-machine &optional (n 1))
  (with-slots (line-offset input-lines) state-machine
    (incf line-offset n)
    (if (or (< line-offset 0) (>= line-offset (length input-lines)))
        (throw 'state-machine-eof nil)
        (current-line state-machine))))

(defun next-line-blank-p(state-machine)
  "TRUE if the next line is blank or non-existant."
  (let ((next (1+ (line-offset state-machine))))
    (or (>= next (length (input-lines state-machine)))
        (line-blank-p (current-line state-machine next)))))

(defun at-eof(state-machine)
  "True if the input is at or past end-of-file."
  (>= (line-offset state-machine) (1- (length (input-lines state-machine)))))

(defun at-bof(state-machine)
  "True if the input is at or before beginning-of-file."
  (<= (line-offset state-machine) 0))

(defun previous-line(state-machine &optional (n 1))
  (decf (line-offset state-machine) n)
  (when (>= (line-offset state-machine) 0)
    (current-line state-machine)))

(defun goto-line(state abs-line-offset)
  "Jump to absolute line offset abs-line-offset, load and return it."
    (let* ((state-machine (state-machine state))
           (line-offset (- abs-line-offset (input-offset state-machine))))
      (setf (line-offset state-machine) line-offset)
      (unless (or (< line-offset 0) ;; unless eof
                  (>= abs-line-offset (length (input-lines state-machine))))
        (dolist(hook *goto-line-hooks*)
          (funcall hook state-machine abs-line-offset))
        (current-line state-machine line-offset))))

(defun abs-line-offset(state-machine)
  "Return Return line offset of current line, from beginning of file."
  (+ (line-offset state-machine) (input-offset state-machine)))

(defun abs-line-number(state-machine)
  (+ 1 (line-offset state-machine) (input-offset state-machine)))

(defun get-text-block(state-machine
			  &key flush-left (start (line-offset state-machine)))
  "Return a contiguous block of text.

If `flush_left` is true, signal `UnexpectedIndentationError` if an
indented line is encountered before the text block ends (with a blank
line)."
  (let* ((input-lines (input-lines state-machine))
         (last (length input-lines))
         (text-block
          (subseq input-lines start
                  (do((end start (1+ end)))
                     ((>= end last) end)
                    (let ((line (aref input-lines end)))
                      (when (line-blank-p line) (return end))
                      (when (and flush-left (char= (char line 0) #\space))
                        (next-line state-machine
                                   (1- (- end start))) ; go to last line
                        (signal 'unexpected-indentation
                                :line (1+ end)
                                :text-block (subseq input-lines start))
			(return (1- end))))))))
    (next-line state-machine (1- (length text-block)))
    text-block))

(defun check-line(state-machine state
                      &optional (transitions (transitions state)))
  (let ((line (current-line state-machine))
        (*current-line-number* (abs-line-number state-machine)))
    (dolist (transition transitions)
      (let ((match (transition-match transition line)))
        (when match
          (return-from check-line
            (apply-transition state transition match))))))
  (no-match state transitions))

(defun add-states(state-machine state-classnames)
  "register state classes with this state engine"
  (dolist(state-classname state-classnames)
    (when (member state-classnames (states state-machine))
      (error "Duplicate State ~A in ~A ~%" state-classname state-machine))
    (push state-classname (states state-machine))))

(declaim (inline transition-name transition-pattern transition-function
				  transition-next-state))

(defun transition-name(transition) (first transition))
(defun transition-pattern(transition) (second transition))
(defun transition-next-state(transition) (third transition))
(defun transition-function(transition) (first transition))

(defun transition-match(transition string)
  (match (transition-pattern transition) string))

(defgeneric apply-transition(state transition match)
  (:documentation "Execute transition from state with match")
  (:method(state transition match)
    (multiple-value-bind(result next-state)
        (funcall (transition-function transition) state match)
      (when result
        (break "Result not nil for state ~S transition ~S" state transition))
      (values result (or next-state (transition-next-state transition))))))

(defclass state()
  ((state-machine
    :initarg :state-machine :reader state-machine
    :documentation "A reference to the controlling StateMachine object.")
   (initial-transitions
    :allocation :class :initform nil :reader initial-transitions
    :documentation "The initial set of transitions for this state")
   (transitions
    :accessor transitions
    :documentation "List of transitions in order."))
  (:documentation "State superclass."))

(defgeneric make-nested-state-machine(state &optional initial-state)
  (:documentation "Created a nested state machine to parse nested
document structures.")
  (:method ((state state) &optional (initial-state (class-name state)))
    (make-instance (class-of (state-machine state))
                   :initial-state initial-state
                   :state-classes (list (class-name state)))))

(defmethod initialize-instance :after ((state state) &key &allow-other-keys)
  (setf (transitions state) (slot-value state 'initial-transitions)))

(defun add-transitions(state transitions)
  "Add a list of transitions to the start of the transition list."
  (dolist(transition transitions)
    (when (member transition (transitions state) :key #'transition-name)
      (error "Duplicate Transition ~A" transition))
    (push transition (transitions state))))

(defun remove-transition(state name)
  "Remove a transition by `name`"
  (setf (transitions state)
	(remove name (transitions state) :key #'transition-name)))

(defgeneric no-match(state transitions)
  (:documentation "Called when there is no match from
`StateMachine.check_line()`.")
  (:method (state transitions) nil))

(defgeneric bof(state)
  (:documentation  "Beginning of file transition")
  (:method(state) nil))

(defgeneric eof(state)
  (:documentation "End of file transition")
  (:method (state) nil))

(defgeneric nop(state match)
  (:documentation  "A do nothing transition method.")
  (:method (state match) nil))

(defclass wsp-state-machine(state-machine)
  ((blank-finish :initform t :accessor blank-finish
		 :documentation "Used to keep track of blank lines"))
  (:documentation
   "state-machine subclass specialized for whitespace recognition"))

(defun get-indented(state-machine &key
                        (until-blank nil)
                        (strip-indent t)
                        first-indent
                        block-indent
                        (strip-top first-indent))
  "Return an indented block and info.

   Extract an indented block where the indent is known for all lines.
   Starting with the current line, extract the entire text block with at
   least `indent` indentation (which must be whitespace, except for the
   first line).

  :Parameters:
       - `block-indent`: The number of indent columns/characters if the
           indent is known for all lines.
       - first-indent: The indent  where the indent is known for the first line
                       and unknown for all other lines.
       - `until_blank`: Stop collecting at the first blank line if true
         (1).
       - `strip_indent`: Strip `indent` characters of indentation if true
         (1, default).
       - `strip_top`: Strip blank lines from the beginning of the block.

  :Return:
       - the indented block,
       - its first line offset from BOF, and
       - whether or not it finished with a blank line.
       - then indent,
        "
  (let ((offset (abs-line-offset state-machine)))
    (multiple-value-bind(indented indent blank-finish)
        (indented-block (input-lines state-machine)
                        :start (line-offset state-machine)
                        :until-blank until-blank
                        :strip-indent strip-indent
                        :first-indent first-indent
                        :block-indent block-indent)
      ;; advance to last indented line
      (next-line state-machine (1- (length indented)))
      (setf (blank-finish state-machine) blank-finish)
      (when strip-top
        (let ((p (position-if-not #'line-blank-p indented)))
          (when p
            (setf indented (subseq indented p))
            (incf offset p))))
      (values (when (> (length indented) 0) indented)
              offset blank-finish indent))))

(defgeneric blank(state match)
  (:documentation "Handle blank lines."))

(defgeneric indent(state  match)
  (:documentation "Handle an indented text block. Extend or override
in subclasses.  Recursively run the state machine for indented blocks"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +wsp-transitions+
    '((blank "^\\s*$" )
      (indent "^ +" ))
    "Transitons for a wsp state machine"))

(defclass wsp-state(state)
  ((initial-transitions :allocation :class :initform +wsp-transitions+))
  (:documentation "State superclass specialized for
whitespace (blank lines & indents).

Use this class with `StateMachineWS`.  The transitions 'blank' (for blank
lines) and 'indent' (for indented text blocks) are added automatically,
before any other transitions.  The transition method `blank()` handles
blank lines and `indent()` handles nested indented blocks.  Indented
blocks trigger a new state machine to be created by `indent()` and run.
The class of the state machine to be created is in `indent_sm`, and the
constructor keyword arguments are in the dictionary `indent_sm_kwargs`."))

(defmethod blank-finish(state)
  (blank-finish (state-machine state)))

(defun make-indent-state-machine(state)
  (make-nested-state-machine state))

(defun make-known-indent-state-machine(state)
  (make-indent-state-machine state))

(defmethod blank((state wsp-state) match)
  "Handle blank lines. Does nothing. Override in subclasses."
  (nop state match))

(defmethod indent((state wsp-state) match)
  "Handle an indented text block. Extend or override in subclasses.
Recursively run the state machine for indented blocks"
  (multiple-value-bind(indented line-offset)
      (get-indented (state-machine state))
    (state-machine-run (make-indent-state-machine state)
		       indented
		       :input-offset line-offset)))

(defun known-indent(state match)
  "Handle a known-indent text block. Extend or override in subclasses.
Recursively run the state machine for indented blocks"
  (multiple-value-bind(indented  line-offset)
      (get-indented (state-machine state) :block-indent (match-end match))
    (state-machine-run (make-known-indent-state-machine state)
		       indented
		       :input-offset line-offset)))

(defun known-first-indent(state match)
  "Handle a known-indent text block (first line's indent
known). Extend or override in subclasses.  Recursively run the state
machine for indented blocks"
  (multiple-value-bind(indented line-offset)
      (get-indented (state-machine state) :first-indent (match-end match))
    (state-machine-run (make-known-indent-state-machine state)
                       indented
                       :input-offset line-offset)))

(defstruct match
  "Results of a transition match"
  (start 0 :type fixnum) ;; start index of match
  (end 0 :type fixnum) ;; end index of match
  string ;; line being matched
  reg-starts ;; indices of register starts
  reg-ends) ;; other arguments to be passed from matcher to transition

(defun match-group(match &optional (n nil))
  "Return a new subsequence corresponding to match group n of
match. If n is not specified returns entire match"
  (if n
      (let ((start (svref (match-reg-starts match) n)))
        (when start
          (subseq (match-string match)
                  start
                  (svref (match-reg-ends match) n))))
      (subseq (match-string match) (match-start match) (match-end match))))

(defun match-group-length(match n)
  "Return length of the subsequence corresponding to match group n of match"
  (if (aref (match-reg-ends match) n)
    (- (aref (match-reg-ends match) n) (aref (match-reg-starts match) n))
    0))

(defparameter *scan-cache* (make-hash-table))

(defun match(pattern string &key (start 0) (end (length string)))
  (multiple-value-bind(start end reg-starts reg-ends)
      (typecase pattern
        (function (funcall pattern string :start start :end end))
        (t (let ((scanner (or (gethash pattern *scan-cache*)
                              (setf (gethash pattern *scan-cache*)
                                    (cl-ppcre::create-scanner pattern)))))
             (scan scanner string :start start :end end))))
    (when start
      (make-match :string string :start start :end end
                  :reg-starts reg-starts :reg-ends reg-ends))))

(defun matches(pattern string &key (start 0) (end (length string)))
  (let ((matches nil))
    (do((match (match pattern string :start start :end end)
	       (match pattern string :start start :end end)))
       ((not match))
      (push match matches)
      (setf start (match-end match)))
    (nreverse matches)))

