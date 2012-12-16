(in-package :cl-table)

(defstruct row
  "Struct for representing row in table"
  (parent nil :type (or null row))
  (ref nil :type list)
  (children nil :type list)
  (table nil :type table)
  (num 0 :type fixnum)
  (data nil :type list))

(defstruct column
  (name nil :type (and symbol (not null)))
  (type t :type (or symbol list)))

(defclass table ()
  ((columns :accessor columns :type list)
   (rows :accessor rows :type list :initform nil
         :documentation 
         "List of lists = data in (car row), list of children rows in (cdr row)
Assert (length (car row)) == (length columns)")
   (indexes :accessor indexes :type list :initform nil)))

(defmethod shared-initialize :after ((table table) slot-names
                                     &key columns)
  (when (notevery #'column-p columns)
    (setf (columns table)
          (mapcar (lambda (x) (etypecase x
                                (symbol (make-column :name x))
                                (list (make-column :name (car x) 
                                                   :type (second x)))
                                (column x)))
                  columns))))
  

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


(defun sort! (table columns)
  (setf (rows table)
        (stable-sort (rows table)
                     (lambda (x y)
                       (compare-rows columns #'generic-lessp 
                                     (make-row :table table :data x) 
                                     (make-row :table table :data y))))))

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
  (sort! table group-columns)
  (let (res)
    (map-table (lambda (str)
                 (if (equal-rows group-columns (car res) str)
                     (sum-columns! sum-columns (car res) str)
                     (push str res))) table)
    (setf (rows table) (nreverse res))))


(defun field (str key)
  "Returns field of row STR with name symbol KEY"
  (iter (for column in (columns (row-table str)))
        (for value in (row-data str))
        (when (eq (column-name column) key) (return value))))

(defsetf field (str key) (new-value)
  (let ((column (gensym))
        (value (gensym)))
    `(iter (for ,column in (columns (row-table ,str)))
           (for ,value on (row-data ,str))
           (when (eq (column-name ,column) ,key) 
             (assert (typep ,new-value (column-type ,column)) (,new-value)
                     'type-error 
                     :datum ,new-value 
                     :expected-type (column-type ,column))
             (return (setf (car ,value) ,new-value))))))

(defun map-table (func table)
  (labels ((in-map (rows num)
             (when rows
               (funcall func (make-row :table table 
                                       :num num 
                                       :data (caar rows)
                                       :children (cdar rows)))
               (in-map (cdr rows) (+ 1 num)))))
    (in-map (rows table) 0)))

(defun map-table-row (func row)
  (labels ((in-table-row (rows num)
             (when rows
               (funcall func (make-row :table (row-table row)
                                       :num num
                                       :parent row
                                       :data (caar rows)
                                       :children (cdar rows)))
               (in-table-row (cdr rows) (+ 1 num)))))
    (in-table-row (row-children row) 0)))

(defmacro-clause (FOR var IN-TABLE table)
    "Rows of a table: row = (table field1 field2 ...)"
  (let ((tab (gensym))
        (row (gensym))
        (num (gensym)))
    `(progn
       (with ,tab = ,table)
       (for ,row in ,(rows tab))
       (for ,num from 0)
       (for ,var = (make-row :table ,tab :num ,num 
                             :data (car ,row) 
                             :children (cdr ,row))))))

(defmacro-clause (FOR var IN-TABLE-ROW table)
    "Rows of a table: row = (table field1 field2 ...)"
  (let ((tab (gensym))
        (row (gensym))
        (parent (gensym))
        (num (gensym)))
    `(progn
       (with ,parent = ,table)
       (with ,tab = ,(row-table table))
       (for ,row in (row-children ,tab))
       (for ,num from 0)
       (for ,var = (make-row :table ,tab :num ,num 
                             :data (car ,row)
                             :children (cdr ,row)
                             :parent ,table)))))


(defgeneric add (to-place))

(defmacro append-item (item list)
  `(setf ,list (append ,list (list ,item))))

(defmethod add ((table table))
  (let (res)
    (push nil res)
    (dotimes (i (length (columns table)))
      (push nil (car res)))
    (prog1
        (make-row :data (car res) :table table
                  :num (length (rows table)) :ref res)
      (append-item res (rows table)))))

(defmethod add ((row row))
  (let (res)
    (push nil res)
    (dotimes (i (length (columns (row-table row))))
      (push nil (car res)))
    (prog1
        (make-row :data (car res) :table (row-table row) :ref res
                  :num (length (row-children row)) :parent row)
      (append-item res (cdr (row-ref row))))))

(defgeneric path->row (table path))

(defmethod path->row :around (table (path fixnum))
  (call-next-method table (list path)))

(defmethod path->row ((table table) path)
  (when path
    (let* ((parent (path->row table (butlast path)))
           (num (car (last path)))
           (row (nth num (if parent 
                             (row-children parent)
                             (rows table)))))      
      (make-row :table table
                :num num 
                :parent parent
                :data (car row)
                :children (cdr row)))))
                