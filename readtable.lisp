;;;; readtable.lisp

(cl:in-package :srfi-38.internal)
(in-readtable :common-lisp)

#|(defreadtable :srfi-38  (:merge :standard)
  (:macro-char char fctn opt...)
  (:syntax-from readtable to-char from-char)
  (:case :upcase))|#
