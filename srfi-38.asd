;;;; srfi-38.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-38
  :version "20200404"
  :description "SRFI 38 for CL: External Representation for Data With Shared Structure"
  :long-description "SRFI 38 for CL: External Representation for Data With Shared Structure
https://srfi.schemers.org/srfi-38"
  :author "Ray Dillinger"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :srfi-23
               :srfi-61
               :srfi-5)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-38")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-38))))
  (let ((name "https://github.com/g000001/srfi-38")
        (nickname :srfi-38))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-38))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-38#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-38)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
