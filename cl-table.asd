;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cl-table.asd -- Hierarchical tables in Lisp
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:cl-table-system
  (:use #:cl #:asdf))
(in-package #:cl-table-system)

(defsystem cl-table
  :description "Hierarchical tables in Lisp"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.9"
  :license "BSD"
  :depends-on (iterate)
  :serial t
  :components
  ((:file package)
   (:file table)
   (:file iterator)))
