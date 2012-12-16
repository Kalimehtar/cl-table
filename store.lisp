(in-package :cl-table)

(defparameter +db-table+ (cl-store:register-code 52 'table))


(cl-store:defstore-cl-store (obj table stream)
  (progn
    (cl-store:output-type-code +db-table+ stream)
    (cl-store:store-object (columns table) stream)
    (labels ((row-list (row)
               (list (
    (cl-store:store-object (mapcar #'row->list (rows table)) stream))))