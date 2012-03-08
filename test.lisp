(cl:in-package :srfi-38.internal)

(def-suite srfi-38)

(in-suite srfi-38)

(test read/ss
  (is (equalp (with-input-from-string (in "'(#1=foo (z 8 #1#))")
                (read-with-shared-structure in))
              ''(FOO (Z 8 FOO)))))
