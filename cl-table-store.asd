;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cl-table-store.asd -- Serialize cl-table with cl-store
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:cl-table-system
  (:use #:cl #:asdf))
(in-package #:cl-table-system)

(defsystem cl-table-store
  :description "CL-TABLE serialized with CL-STORE"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.9"
  :license "BSD"
  :depends-on (cl-table cl-store)
  :components
  ((:file store)))
