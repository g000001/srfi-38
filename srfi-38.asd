;;;; srfi-38.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-38
  :serial t
  :depends-on (:fiveam
               :srfi-23
               :named-readtables)
  :components ((:file "package")
               (:file "util")
               (:file "readtable")
               (:file "srfi-38")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-38))))
  (load-system :srfi-38)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-38.internal :srfi-38))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
