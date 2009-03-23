;; $Id: tables.lisp,v 1.4 2007/07/26 08:30:50 willijar Exp willijar $
;; Parse Restructured text Tables
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CL-docutils

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; This module defines table parser classes,which parse plaintext-graphic
;; tables and produce a well-formed data structure suitable for building a
;; CALS table.

;;; Code:

(in-package :docutils.parser.tables)

(define-condition table-condition(markup-condition)())

(defun parse-table(parser-class-name block)
  (let ((parser (make-instance parser-class-name
                               :block (copy-seq block))))
    (do-parse-table parser)
    (structure-from-cells parser)))

(defclass table-parser()
  ((text-block :initarg :block :reader text-block
               :documentation "Block of text to be parser")
   (separator-pattern :initarg :separator-pattern)
   (head-body-sep :reader head-body-sep :initform nil
                  :documentation "Index of head body separator")
   (done :type array :reader done))
  (:documentation "Abstract superclass for the common parts of the
syntax-specific parsers."))

(defun bottom(text-block) (1- (length text-block)))
(defun right(text-block) (1- (length (aref text-block 0))))

(defmethod initialize-instance :after ((parser table-parser)
                                       &key &allow-other-keys)
  (setf  (slot-value parser 'done)
         (make-array (length (aref (text-block parser) 0))
                     :element-type 'fixnum
                     :initial-element -1))
  (with-slots(text-block separator-pattern head-body-sep) parser
    (dotimes(i (length text-block))
      (let((line (aref text-block i)))
        (when (scan separator-pattern line)
          (if head-body-sep
              (error 'table-condition
                     :source text-block
                     :level :error
                     :message
                     (format nil "Multiple head/body row separators in table (at line offset ~d and ~d); only one allowed." head-body-sep i))
              (progn
                (setf head-body-sep  i)
                (setf (aref text-block i)
                      (substitute #\-  #\= line)))))))
    (when (and head-body-sep
               (or  (= head-body-sep 0)
                    (= head-body-sep (1- (length text-block)))))
      (error 'table-condition
             :source text-block
             :level :error
             :message "The head/body row separator may not be the first or last line of the table."))))

(defclass grid-table-parser(table-parser)
  ((cells :type list :initform nil :accessor cells)
   (colseps :accessor colseps :initform '(0))
   (rowseps :accessor rowseps :initform '(0)))
  (:default-initargs :separator-pattern "^\\+=[=+]+=\\+$")
  (:documentation
   "Parse a grid table using `parse()`.

    Here's an example of a grid table::

        +------------------------+------------+----------+----------+
        | Header row, column 1   | Header 2   | Header 3 | Header 4 |
        +========================+============+==========+==========+
        | body row 1, column 1   | column 2   | column 3 | column 4 |
        +------------------------+------------+----------+----------+
        | body row 2             | Cells may span columns.          |
        +------------------------+------------+---------------------+
        | body row 3             | Cells may  | - Table cells       |
        +------------------------+ span rows. | - contain           |
        | body row 4             |            | - body elements.    |
        +------------------------+------------+---------------------+

    Intersections use '+', row separators use '-' (except for one optional
    head/body row separator, which uses '='), and column separators use '|'.

    Passing the above table to the `parse()` method will result in the
    following data structure::


        ([24, 12, 10, 10],
         [[(0, 0, 1, ['Header row, column 1']),
           (0, 0, 1, ['Header 2']),
           (0, 0, 1, ['Header 3']),
           (0, 0, 1, ['Header 4'])]],
         [[(0, 0, 3, ['body row 1, column 1']),
           (0, 0, 3, ['column 2']),
           (0, 0, 3, ['column 3']),
           (0, 0, 3, ['column 4'])],
          [(0, 0, 5, ['body row 2']),
           (0, 2, 5, ['Cells may span columns.']),
           None,
           None],
          [(0, 0, 7, ['body row 3']),
           (1, 0, 7, ['Cells may', 'span rows.', '']),
           (1, 1, 7, ['- Table cells', '- contain', '- body elements.']),
           None],
          [(0, 0, 9, ['body row 4']), None, None, None]])


    The first item is a list containing column widths (colspecs). The second
    item is a list of head rows, and the third is a list of body rows. Each
    row contains a list of cells. Each cell is either None (for a cell unused
    because of another cell's span), or a tuple. A cell tuple contains four
    items: the number of extra rows used by the cell in a vertical span
    (morerows); the number of extra columns used by the cell in a horizontal
    span (morecols); the line offset of the first line of the cell contents;
    and the cell contents, a list of lines of text."))

(defun get-2d-block(text top left bottom right &key (strip-indent t))
  (let((result (make-array (- bottom top))))
    (when (> bottom top)
      (dotimes(i (length result))
        (setf (aref result i)
              (subseq (aref text (+ i top)) left right)))
      (when strip-indent
        (let ((indent (reduce #'min (map 'list #'indent-level result))))
          (when (< 0 indent right)
            (dotimes(i (length result))
              (setf (aref result i) (subseq (aref result i) indent))))))
      (dotimes(i (length result))
        (setf (aref result i) (rstrip (aref result i)))))
    result))

(defun do-parse-table(parser)
  (let ((bottom (bottom (text-block parser)))
        (right (right (text-block parser))))
    (let ((corners (list (list 0 0))))
      (while corners
        (let* ((corner (pop corners))
               (top (first corner))
               (left (second corner)))
          (unless (or (= top bottom) (= left right)
                      (<= top (aref (done parser) left)))
            (multiple-value-bind(bottom right rowseps colseps ok-p)
                (scan-cell parser top left)
              (when ok-p
                (setf (rowseps parser) (union rowseps (rowseps parser)))
                (setf (colseps parser) (union colseps (colseps parser)))
                (mark-done (done parser) top left bottom right)
                (push (list
                       top left bottom right
                       (get-2d-block
                        (text-block parser) (1+ top) (1+ left) bottom right))
                      (cells parser))
                (push (list top right) corners)
                (push (list bottom left) corners)
                (setf corners
                      (sort corners
                            #'(lambda(a b)
                                (or (< (first a) (first b))
                                    (and (= (first a) (first b))
                                         (< (second a) (second b)))))))))))))
    (unless (check-parse-complete parser)
      (error 'table-condition
             :source (text-block parser)
             :level :error
             :message "parse incomplete"))))

(defun mark-done(done top left bottom right)
  (let ((before (1- top))
        (after (1- bottom)))
    (do((col left (1+ col)))
       ((>= col right))
      (assert (= (aref done col) before))
      (setf (aref done col) after))))

(defun check-parse-complete(parser)
  (let ((last (1- (bottom (text-block parser))))
        (done (done parser)))
    (dotimes(col (right (text-block parser)))
      (unless (= (aref done col) last)
        (return-from check-parse-complete nil))))
  t)

(defun scan-cell(parser top left)
  (assert (eql (aref (aref (text-block parser) top) left) #\+))
  (scan-right (text-block parser) top left))

(defun scan-right(text-block top left)
  "Look for the top-right corner of the cell, and make note of all column
boundaries ('+')."
  (let ((colseps nil)
        (line (aref text-block top))
        (right (right text-block)))
    (do((i (1+ left) (1+ i)))
       ((> i right))
      (case (aref line i)
        (#\+
         (push i colseps)
         (multiple-value-bind(bottom rowseps newcolseps ok-p)
             (scan-down text-block top left i)
           (when ok-p
             (setf colseps (nunion newcolseps colseps))
             (return-from scan-right (values bottom i rowseps colseps t)))))
        (#\-)
        (t (return-from scan-right))))))

(defun scan-down(text-block top left right)
  "Look for the bottom-right corner of the cell, making note of all row
boundaries."
  (let ((rowseps nil)
        (bottom (bottom text-block)))
    (do((i (1+ top) (1+ i)))
       ((> i bottom))
      (case (aref (aref text-block i) right)
        (#\+
         (push i rowseps)
         (multiple-value-bind(newrowseps colseps ok-p)
             (scan-left text-block top left i right)
           (when ok-p
             (setf rowseps (nunion newrowseps rowseps))
             (return-from scan-down (values i rowseps colseps t)))))
        (#\|)
        (t (return-from scan-down))))))

(defun scan-left(text-block top left bottom right)
  "Noting column boundaries, look for the bottom-left corner of the cell.
   It must line up with the starting point."
  (let ((colseps nil)
        (line (aref text-block bottom)))
    (when (char= (aref line left) #\+)
      (do((i (1- right) (1- i)))
         ((<= i left))
        (case (aref line i)
          (#\+ (pushnew i colseps))
          (#\-)
          (t (return-from scan-left))))
      (multiple-value-bind(rowseps ok-p)
          (scan-up text-block top left bottom)
        (when ok-p
          (return-from scan-left (values rowseps colseps t)))))))

(defun scan-up(text-block top left bottom)
  "Noting row boundaries, see if we can return to the starting point."
  (let ((rowseps nil))
    (do((i (1- bottom) (1- i)))
       ((<= i top))
      (case (aref (aref text-block i) left)
        (#\+ (pushnew i rowseps))
        (#\|)
        (t (return-from scan-up))))
    (values rowseps t)))

(defun structure-from-cells(parser)
  (let* ((rowseps (sort (copy-list (rowseps parser)) #'<))
         (colseps (sort (copy-list (colseps parser)) #'<))
         (colspecs (map 'list #'(lambda(a b) (- a b 1))
                        (rest colseps) colseps))
         (rmax (1- (length rowseps)))
         (cmax (1- (length colseps)))
         (rows (make-array (list rmax cmax)
                           :initial-element nil))
         (remaining (* rmax cmax)))
    (flet((rowindex(row) (position row rowseps)) ;; row boundary -> index
          (colindex(col) (position col colseps))) ;; col boundary -> index
      (dolist(cell (cells parser))
        (destructuring-bind (top left bottom right block) cell
          (let ((rownum (rowindex top))
                (colnum (colindex left)))
            (when (aref rows rownum colnum)
              (report :error `("Cell (row ~D, column ~D) is already used"
                               ,(1+ rownum) ,(1+ colnum))))
            (let ((morerows (- (rowindex bottom) rownum 1))
                  (morecols (- (colindex right) colnum 1)))
              (decf remaining (* (1+ morerows) (1+ morecols)))
              (setf (aref rows rownum colnum)
                    (list morerows morecols (1+ top) block))))))
      (when (> remaining 0)
        (report :error "Unused cells remaining"))
      (if (head-body-sep parser)
          (let((h (rowindex (head-body-sep parser))))
            (list colspecs
                  (make-array (list h cmax)
                              :displaced-to rows
                              :displaced-index-offset 0)
                  (make-array (list (- rmax h) cmax)
                              :displaced-to rows
                              :displaced-index-offset (* h cmax))))
          (list colspecs nil rows)))))

