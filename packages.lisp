(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :com.search
    (:nicknames #:search #:cl-search)
    (:use :common-lisp)))
