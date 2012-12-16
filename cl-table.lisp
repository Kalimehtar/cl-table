(in-package :cl-table)

(defclass table ()
  ((columns :accessor columns :type list)
   (rows :accessor rows :type list)
   (indexes :accessor indexes :type list)))

(defgeneric generic-lessp (x y)
  (:documentation "Order by numbers or strings")
  (:method ((x string) (y string))
    (string-lessp x y))
  (:method ((x string) y)
    (generic-lessp x (write-to-string y)))
  (:method (x (y string))
    (generic-lessp (write-to-string x) y))
  (:method ((x number) (y number))
    (< x y)))

(defun compare-rows (cols pred row1 row2)
  (when cols
    (labels ((%compare (%cols)
               (let ((f1 (field row1 (car %cols)))
                     (f2 (field row2 (car %cols))))
                 (if (equal f1 f2) (%compare (cdr %cols))
                     (funcall pred f1 f2)))))
      (%compare cols))))

(defun equal-rows (cols row1 row2)
  (if cols
      (let ((f1 (field row1 (car cols)))
            (f2 (field row2 (car cols))))
        (when (equal f1 f2) (equal-rows (cdr cols) row1 row2)))
      t))

(eval-when (:compile-toplevel :execute)
  (defun enable-sharpL-reader ()
    (set-dispatch-macro-character #\# #\L #'iterate::sharpL-reader))
  (setf *readtable* (copy-readtable *readtable*))
  (enable-sharpL-reader))


(defun sort! (table columns)
  (setf (rows table)
        (stable-sort (rows table)
                     #L(compare-rows columns #'generic-lessp 
                                     (cons table !1) (cons table !2)))))

;; (defun add-columns (sum-columns dst-row src-row)
;;   (mapc (lambda (column)
;;           (setf (field dst-row column)
;;                 (+ (field dst-row column)
;;                    (field src-row column))))
;;         sum-columns))

(defun sum-columns! (sum-columns dst-row src-row)
  "For each column in list SUM-COLUMNS put sum of fields
from dst and src rows to dst-row" 
  (assert (eq (car src-row) (car dst-row))) ; the same table for rows
  (let ((cols (columns (car src-row))))
    (mapc (lambda (column)
            (iter (for name in cols)
                  (for value in (cdr src-row))
                  (for place on (cdr dst-row))
                  (when (eq name column) 
                    (setf (car place) (+ (car place) value)))))
        sum-columns)))

(defun drop-columns! (table columns)
  (let ((old-columns (columns table)))
    (labels ((get-diff (row)
               (iter
                 (for col in old-columns)
                 (for field in row)
                 (unless (find col columns)
                   (collect field)))))            
      (iter
        (for row on (rows table))
        (setf (car row) (get-diff (car row))))
      (setf (columns table) (get-diff (columns table))))))
        

(defun wrap! (table group-columns sum-columns)
  (assert (null (intersection group-columns sum-columns)))
  (drop-columns! table 
                 (set-difference (columns table) 
                                 (union group-columns sum-columns)))
  (sort table group-columns)
  (let (res)
    (map-table (lambda (str)
                 (if (equal-rows group-columns (car res) str)
                     (sum-columns! sum-columns (car res) str)
                     (push str res))) table)
    (setf (rows table) (nreverse res))))


(defun field (str key)
  "Returns field of row STR with name symbol KEY
Assume (car str) = table & (cdr str) = current row"
  (iter (for name in (columns (car str)))
        (for value in (cdr str))
        (when (eq name key) (return value))))

(defsetf field (str key) (new-value)
  `(iter (for name in (columns (car ,str)))
         (for value on (cdr ,str))
         (when (eq name ,key) (setf (car value) ,new-value))))

(defun map-table (func table)
  (labels ((in-map (rest)
             (when rest 
               (funcall func (cons table (car rest)))
               (in-map (cdr rest)))))
    (in-map (rows table))))

(defmacro-clause (FOR var IN-TABLE table)
    "Rows of a table: row = (table field1 field2 ...)"
    (let ((tab (gensym))
          (row (gensym)))
      `(progn
         (with ,tab = ,table)
         (for ,row in ,(rows tab))
         (for ,var = (cons ,tab ,row)))))