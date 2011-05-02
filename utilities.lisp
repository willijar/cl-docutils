;;;; Misc string and vector handling for docutils
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; A document node has format ((tagname attributes) content) where
;;;; tagname is a keyword symbol, attributes a p-list
;;;; and content a list of nodes or content atoms
;;;; $Id: utilities.lisp,v 1.12 2007/07/26 08:56:35 willijar Exp willijar $

(in-package :docutils.utilities)

(defvar *tab-size* 8 "The amount of space that a tab is equivalent to")

(defparameter +wsp+ (mapcar #'code-char '(32 9 13 10 12 11))
  "White space characters: Space, Tab, Return, Newline, Page, PageUp")

(deftype wsp () `(member ,@(mapcar #'code-char '(32 9 13 10 12 11))))

(declaim (inline wsp-char-p line-blank-p))
(defun wsp-char-p(c) (typep c 'wsp))

(declaim (inline line-blank-p line-length))
(defun line-blank-p(line)
  (declare (string line))
  (every #'wsp-char-p line))

(defun line-length(line)
  "Return length of line excluding trailing whitespace"
  (1+ (position-if-not #'wsp-char-p line :from-end t)))

(defun indent-level(line &key (tab-size *tab-size*))
  "Returns the indentation level of the line, including tabs as expanded"
  (declare (type simple-string line)
	   (fixnum tab-size))
  (let((l 0))
    (declare (fixnum l))
    (loop for c across (the simple-string line)
	 while (wsp-char-p c)
	 do (incf l (if (char= c #\tab) tab-size 1)))
    l))

(defun nsubvector(array start &optional (end (length array)))
  "Returns a displaced array on array of element from start to end (default
length array)"
  (if (and (= 0 start) (= end (length array)))
      array
      (multiple-value-bind(displaced-to index-offset)
          (array-displacement array)
        (if displaced-to
            (make-array (- (or end (length array)) start)
                        :element-type (array-element-type array)
                        :displaced-to displaced-to
                        :displaced-index-offset (+ start index-offset))
            (make-array (- (or end (length array)) start)
                        :element-type (array-element-type array)
                        :displaced-to array
                        :displaced-index-offset start)))))

(defmacro do-vector((element vector &key (counter (gensym)) (start 0) end)
		    &body body)
  "Iterate over the elements of a vector. Aon each iteration element
is bound to the current element and counter to the index of this
element. start and end may be used to specify the range of indexes to
be iterated over."
  (let ((gvector (gensym))
	(gend (gensym)))
    `(let* ((,gvector ,vector)
	    (,gend ,(or end `(length ,gvector))))
      (do*((,counter ,start (1+ ,counter)))
	  ((>= ,counter ,gend))
	(let ((,element (aref ,gvector ,counter)))
	  ,@body)))))

(declaim (inline rstrip strip))
(defun rstrip(string)
  "Remove trailing white space from string"
  (string-right-trim +wsp+ string))
(defun strip(string)
  "Remove prefixing and trailing white space from string"
  (string-trim +wsp+ string))
(defun lstrip(string)
  (string-left-trim +wsp+ string))

(defun lines-left-trim(lines length &key (start 0) (end (length lines)))
  "Trim `length` characters off the beginning of each line,
from index `start` to `end`.  No whitespace-checking is done on the
trimmed text."
  (map 'vector #'(lambda(s) (subseq s (min length (length s))))
       (nsubvector lines start end)))

(defun escape2null(string &key (start 0) (end (length string)))
  "Return a string with escape-backslashes converted to nulls."
  (with-output-to-string(os)
    (with-input-from-string(is string :start start :end end)
      (do((c (read-char is nil) (read-char is nil)))
         ((not c))
        (cond((and (eq c #\\) (eq (peek-char nil is nil) #\\))
              (read-char is nil)
              (write-char #\null os))
             (t (write-char c os)))))))

(defun unescape(text &key restore-backslashes (start 0) end)
  "Return a string with nulls removed or restored to backslashes.
    Backslash-escaped spaces are also removed."
  (with-output-to-string(os)
    (with-input-from-string(is text :start start :end end)
      #+debug(when  (< (or end (length text)) start)
               (break "start=~S end=~D length=~D" end (length text)))
      (do((c (read-char is nil) (read-char is nil)))
         ((not c))
        (cond((eq c #\null)
              (if restore-backslashes
                  (write-char #\\ os)
                  (let ((next (peek-char nil is nil)))
                    (when (wsp-char-p next) (read-char is nil)))))
             (t (write-char c os)))))))

(defun split-lines(string)
  "Return a vector of lines split from string"
  (let ((lines (split-string string :delimiter #\newline)))
    (make-array (length lines)
                :element-type 'string
                :initial-contents lines)))

(defun indented-block(lines &key
                      (start 0)  until-blank (strip-indent t)
                      block-indent  first-indent)
  "Extract and return a vector of indented lines of text.

Collect all lines with indentation, determine the minimum
indentation, remove the minimum indentation from all indented lines
unless STRIP-INDENT is false, and return them. All lines up to but
not including the first unindented line will be returned in a new vector.

Keyword arguments:
 START: The index of the first line to examine.
 UNTIL-BLANK: Stop collecting at the first blank line if true.
 STRIP-INDENT: Strip common leading indent if true (default).
 BLOCK-INDENT: The indent of the entire block, if known.
 FIRST-INDENT: The indent of the first line, if known.

Returns values:
 a new vector of the indented lines with minimum indent removed
 the amount of the indent
 a boolean: did the indented block finish with a blank line or EOF?"
  (let* ((indent block-indent)
	 (first-indent (or first-indent block-indent))
	 (end (if first-indent (1+ start) start))
	 (last (length lines))
	 (blank-finish t))
    (loop
       (unless (< end last) (setf blank-finish t) (return))
       (let* ((line (aref lines end))
	      (line-indent (indent-level line)))
	 (cond
	   ((line-blank-p line)
	    (when until-blank (setf blank-finish t) (return)))
	   ((or (= line-indent 0)
		(and block-indent (< line-indent block-indent)))
	    ;;Line not indented or insufficiently indented.
	    ;;Block finished properly if the last indented line blank:
	    (setf blank-finish (and (> end start)
				    (line-blank-p (aref lines (1- end)))))
	    (return))
	   ((not block-indent)
	    (setf indent
		  (if indent
		      (min indent line-indent)
		      line-indent)))))
       (incf end))
    (let ((block (subseq lines start end)))
      (when (> (length block) 0)
	(when first-indent
	  (setf (aref block 0)
		(subseq (aref block 0)
			(min first-indent (length (aref block 0))))))
	(when (and indent strip-indent)
	  (dotimes(idx (length block))
	    (unless (and (= idx 0) first-indent)
	      (let ((s (aref block idx)))
		(setf (aref block idx) (subseq s (min indent (length s)))))))))
      (values block (or indent 0) blank-finish))))

(defvar *namespace* nil "Prefix namespace for ids")
(defvar *namespace-delimiter* "::" "Characters used as a delimiter for id namespace component")

(defgeneric namespace(id)
  (:method((id string))
  "Return the namespace component of an id or nil if none. Returns other id component as second value"
  (let ((p (search *namespace-delimiter* id)))
    (if p
      (values
       (subseq id 0 p)
       (subseq id (+ p (length *namespace-delimiter*))))
      (values nil id)))))

(defun make-name(name &key
                 (char-transform #'char-downcase)
                 (namespace *namespace*))
  (let ((last-wsp-p 0))
    (with-output-to-string(os)
      (when (and namespace (not (namespace name)))
        (write-string namespace os)
        (write-string *namespace-delimiter* os))
       (loop
          :for c :across name
          :do
          (cond
            ((wsp-char-p c) (unless (eql last-wsp-p 0) (setf last-wsp-p t)))
            (t
             (when (eql last-wsp-p t) (write-char #\space os))
             (write-char (funcall char-transform c) os)
             (setf last-wsp-p nil)))))))

(defun whitespace-normalise-name(name)
  "Return and whitespace-normalized name."
  (make-name name :char-transform #'identity :namespace nil))

(defun normalise-name(name)
  (make-name name :char-transform #'char-downcase :namespace *namespace*))

(defun make-id(string)
  "Make an ID from string that meets requirements of CSS and html 4.01"
  (with-output-to-string(os)
    (let ((start t)
          (last--p nil))
      (loop
         :for c :across (normalise-name string)
         :do
         (cond
           (start
            (when (alpha-char-p c)
              (setf start nil)
              (write-char c os)))
           ((alphanumericp c)
            (when last--p (write-char #\- os))
            (write-char c os)
            (setf last--p nil))
           ((setf last--p t)))))))

(defgeneric read-lines(entity)
  (:documentation "Read and return a vector of lines from an entity
for subsequent parsing"))

(defmethod read-lines((is stream))
  (let ((lines nil))
    (do ((line (read-line is nil) (read-line is nil)))
        ((not line))
      (push line lines))
    (make-array (length lines)
                :element-type 'string :initial-contents
                (nreverse lines))))

(defmethod read-lines((source pathname))
  (with-open-file(is source :direction :input)
    (read-lines is)))

(defmethod read-lines((source string))
  (split-lines source))

(defmethod read-lines((source vector))
  source)

(defparameter *length-units*
  '((:in . 1)
    (nil . 75)
    (:cm . 254/100)
    (:em . 72/10)
    (:ex . 10)
    (:px . 75) ;; assume 75 dpi
    (:% . 75/8) ;;  100% is 800 pixels
    (:pt . 72)
    (:pc . 12/72)
    (:mm . 254/10))
  "Conversion from various units to inches")

(defun length-unit(unit)
  (or (cdr (assoc unit *length-units*))
      (error "Unacceptable unit ~S - acceptable units are ~S"
             unit
             (mapcar #'car *length-units*))))

(defun convert-length-unit(size unit)
  (unless (consp size) (setf size (cons size :px)))
  (cons
   (* (/ (car size) (length-unit (cdr size))) (length-unit unit))
   unit))

(defmacro when-bind ((var expr) &body body)
  "Bind VAR to VALUE of expression, execute body if true"
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro while (test &body body)
  "Repeat body while test returns true"
  `(do ()
    ((not ,test))
    ,@body))

(declaim (inline is-prefix-p is-suffix-p))

(defun is-prefix-p(subseq seq &key (start 0) (test #'eql))
  "Return true if subseq is a prefix in seq"
  (let ((m (mismatch seq subseq :start1 start :test test)))
    (or (not m) (= m (length subseq)))))

(defun is-suffix-p(subseq seq &key (test #'eql))
  "Return true if subseq is a suffix in seq"
  (let ((m (mismatch seq subseq :from-end t :test test)))
    (or (not m)
        (= m (- (length seq) (length subseq))))))

(defun join-strings(strings  &optional (separator #\space))
  "Return a new string by joining together the STRINGS,
separating each string with a SEPARATOR character or string"
  (let ((first-p t))
    (with-output-to-string(os)
      (map 'nil
           #'(lambda(s)
               (if first-p
                   (setf first-p nil)
                   (write separator :stream os :escape nil))
               (write-string s os))
           strings))))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
      ((> ,var ,gstop))
      ,@body)))

(defun copy-stream(from to &optional count)
  "Copy from input stream FROM into output stream TO upto COUNT bytes,
or until end-of-file if COUNT is NIL"
  (let ((buf (make-array 8192 :element-type (stream-element-type from))))
    (do*((pos (read-sequence buf from)
              (read-sequence buf from))
         (n pos (+ n pos)))
        ((or (= 0 pos) (and count (> n count)))
         (when (> pos 0)
           (write-sequence buf to :end (- pos (- n count)))))
      (write-sequence buf to :end pos))	))

(defvar *search-path* nil "List of paths to search for dependencies in
addition to those specified in settings")

(defun find-file(pathname &key (search-path *search-path*))
  "Return the first complete pathname for an existing file found by
merging pathname with each item in a search path in turn. Returns nil
if not found"
  (flet ((ff(directory)
           (some #'probe-file
                 (directory (merge-pathnames pathname directory)))))
    (some
     #'(lambda(path)
         (etypecase path
           (list (some #'ff path))
           (string (some #'ff (split-string path :delimiter #\:)))
           (pathname (ff path))))
     search-path)))

(defclass line-wrap-stream(fundamental-character-output-stream
                           trivial-gray-stream-mixin)
  ((stream-line-column :initform 0 :reader stream-line-column)
   (indentation-level
    :initform 0 :reader indentation-level :initarg :indentation-level
    :documentation "Current indentation level")
   (indentation-character
    :initform #\space :reader indentation-character)
   (stream :initarg :stream :reader stream-of
           :documentation "Actual stream being written to - must be capable
of writing characters using write-char")
   (line-break-test :initform #'wsp-char-p :reader line-break-test
                    :documentation "Function returns true if character can be used as line break")
   (line-buffer :reader line-buffer-of))
  (:documentation "A simple line-wrapping stream filter"))

(defmethod initialize-instance :after((stream line-wrap-stream)
                                      &key (line-length 80) &allow-other-keys)
  (setf (slot-value stream 'line-buffer)
        (make-array (or line-length 4048)
                    :element-type 'character :fill-pointer 0)))

(defmethod close((stream line-wrap-stream) &key abort)
  (unless abort (finish-output stream)))

(defmethod stream-finish-output((stream line-wrap-stream))
  (write-string (line-buffer-of stream) (stream-of stream))
  (setf (fill-pointer (line-buffer-of stream)) 0)
  (finish-output (stream-of stream)))

(defmethod stream-force-output((stream line-wrap-stream))
  (write-string (line-buffer-of stream) (stream-of stream))
  (setf (fill-pointer (line-buffer-of stream)) 0)
  (force-output (stream-of stream)))

(defmethod stream-clear-output((stream line-wrap-stream))
  (setf (fill-pointer (line-buffer-of stream)) 0)
  (clear-output (stream-of stream)))

(defun stream-line-length(stream)
  (array-total-size (line-buffer-of stream)))

(defmethod stream-start-line-p((stream line-wrap-stream))
  (zerop (stream-line-column stream)))

(defmethod (setf indentation-level)((v integer) (stream line-wrap-stream))
  (assert (< v (stream-line-length stream))
          (v)
          "Indentation level must be less than ~D character line length."
          (stream-line-length stream))
  (setf (slot-value stream 'indentation-level) v))

(defmethod stream-write-char((stream line-wrap-stream) (c character))
 (let* ((buffer (line-buffer-of stream))
        (indent (indentation-level stream))
        (len (array-total-size buffer))
        (os (stream-of stream)))
   (with-slots(stream-line-column) stream
     (flet ((indent()
               (let ((c (indentation-character stream)))
                 (dotimes(x indent) (write-char c  os))
                 (setf stream-line-column indent))))
    (cond
      ((or (eql c #\newline)
           (and (>= stream-line-column len)
                (funcall (line-break-test stream) c)))
       (write-line buffer os)
       (setf (fill-pointer buffer) 0
             stream-line-column 0))
      (t
       (when (zerop stream-line-column)
             (indent))
       (vector-push c buffer)
       (incf stream-line-column)))
    (when (= (fill-pointer buffer) len)
      ;; buffer full
      (let ((p (position-if (line-break-test stream) buffer :from-end t)))
        (cond
          (p  ;; if there is a break character write up until it
           (write-line (subseq buffer 0 p) os)
           (setf stream-line-column 0)
           (do((i (1+ p) (1+ i))
               (j 0 (1+ j)))
              ((>= i len) (setf (fill-pointer buffer) j))
             (setf (aref buffer j) (aref buffer i))))
          (t ;; no break character - just empty out buffer
           (write-string buffer os)
           (setf (fill-pointer buffer) 0)))))))))

(defmacro with-block-indentation((n os) &body body)
  (let ((gn (gensym)))
    `(let ((,gn (indentation-level ,os)))
       (unwind-protect
            (progn
              (incf (indentation-level ,os) ,n)
              ,@body)
         (setf (indentation-level ,os) ,gn)))))

(defun last-char(stream)
  "Return last character written to the stream"
  (let* ((buffer (line-buffer-of stream))
         (l (fill-pointer buffer)))
    (when (> l 0) (aref buffer (1- l)))))

(defun unwrite-char(stream)
  "Removes last character from buffer and returns it if possible. If
buffer was empty returns nil."
  (let* ((buffer (line-buffer-of stream))
         (l (fill-pointer buffer)))
    (when (> l 0) (vector-pop buffer))))
