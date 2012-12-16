(defpackage #:cl-table-test
  (:use #:cl #:cl-table))

(in-package #:cl-table-test)

(defun test ()
  (let ((tab (make-instance 'table :columns '(a b))))
    (let ((str (add tab)))
      (setf (field str 'a) "str1a"
            (field str 'b) "str1b"))
    (let ((str (add tab)))
      (setf (field str 'a) "str2a"
            (field str 'b) "str2b")
      (let ((str2 (add str)))
        (setf (field str2 'a) "str21a"
              (field str2 'b) "str21b")))
    (list (field (path->row tab '(0)) 'b)
          (field (path->row tab 1) 'a)
          (field (path->row tab '(1 0)) 'b))
    (path->row tab '(1 0))))
      


(let ((tab (make-instance 'table :columns '(a b))))
  (let ((str (add tab)))
    (setf (field str 'a) "str1a"
          (field str 'b) "str1b"))
  (let ((str (add tab)))
    (setf (field str 'a) "str2a"
          (field str 'b) "str2b")
    (let ((str2 (add str)))
      (setf (field str2 'a) "str21a"
            (field str2 'b) "str21b")))
      
  (assert (equalp '("str1b" "str2a" "str21b")
                  (list (field (path->row tab '(0)) 'b)
                        (field (path->row tab 1) 'a)
                        (field (path->row tab '(1 0)) 'b))))
  (let ((tab2 (make-iterator tab)))
    (assert (equalp '("str1b" "str2a" "str21b")
                  (list (field (path->row tab2 '(0)) 'b)
                        (field (path->row tab2 1) 'a)
                        (field (path->row tab2 '(1 0)) 'b))))))
  

  