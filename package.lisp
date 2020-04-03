;;;; package.lisp

(cl:in-package common-lisp-user)


(defpackage "https://github.com/g000001/srfi-38"
  (:use)
  (:export
   read-with-shared-structure
   read/ss
   write-with-shared-structure
   write/ss))


(defpackage "https://github.com/g000001/srfi-38#internals"
  (:use
   "https://github.com/g000001/srfi-38"
   "https://github.com/g000001/srfi-61"
   cl 
   fiveam)
  (:shadow lambda member map assoc write)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23" error)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-5" let)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-61" cond => else))


;;; *EOF*
