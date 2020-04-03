(cl:in-package "https://github.com/g000001/srfi-38#internals")


(def-suite* srfi-38)


(test read/ss
  (is (equalp (with-input-from-string (in "'(#1=foo (z 8 #1#))")
                (read-with-shared-structure in))
              ''(FOO (Z 8 FOO)))))


;;; *EOF*
