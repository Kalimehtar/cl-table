(in-package :cl-table)

(defstruct (iter-row (:include row))
  "Iterator element"
  (id 0 :type fixnum)
  (children-vector #() :type (vector iter-row)))

;; We need vector of top rows and vector of all rows (to have integer -> row)
;; And we have to store it with the table or else we have independent vars
;;   for a table

(defstruct iter-table
  (all #() :type (vector iter-row))
  (top #() :type (vector iter-row)))
 

(defun make-iterator (table)
  "Returns array of iter-row"
  (let (res visited (res-len -1))
    (declare (special visited))
    (labels ((to-vector (l)
               (coerce (nreverse l) 'vector))
             (visit-row (row)
               (declare (special visited))
               (let* ((children
                       (let (visited)
                         (declare (special visited))
                         (map-table-row #'visit-row row)
                         (to-vector visited)))
                      (new-row (make-iter-row 
                                :parent (row-parent row)
                                :table (row-table row)
                                :children-vector children
                                :children (row-children row)
                                :id (incf res-len)
                                :num (row-num row)
                                :data (row-data row))))
                 (push new-row res)
                 (push new-row visited))))
    (map-table #'visit-row table)
    (make-iter-table :all (to-vector res) :top (to-vector visited)))))

(defun aref* (array index)
  (when (< -1 index (array-dimension array 0))
    (aref array index)))

(defmethod path->row ((iter-table iter-table) path)
  (when path
    (path->row (aref* (iter-table-top iter-table) (car path)) (cdr path))))

(defmethod path->row ((iter-row iter-row) path)
  (if path
    (path->row (aref* (iter-row-children-vector iter-row) (car path))
               (cdr path))
    iter-row))
