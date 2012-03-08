;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-38
  (:use)
  (:export
   :write-with-shared-structure
   :write/ss))

(defpackage :srfi-38.internal
  (:use :srfi-38 :cl :named-readtables :fiveam)
  (:shadow :lambda :member :map :assoc :write)
  (:shadowing-import-from :srfi-23 :error))
