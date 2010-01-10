;; -*- lisp -*-

(defpackage :com.search.system
  (:use :asdf :cl))

(in-package :com.search.system)

#+openmcl
(require "OBJC-SUPPORT")

(defsystem :search
    :name "search"
    :author "Vikram Bhandoh <vikram.bhandoh@gmail.com>"
    :version "0.2"
    :description "All code for the search"
    :depends-on (:cl-ppcre :md5 :cl-store)
    :components
    ((:file "packages")
     (:file "utils" :depends-on ("packages"))
     (:file "files" :depends-on ("packages"))
     (:file "words" :depends-on ("packages"))
     (:file "compress" :depends-on ("packages"))
     (:file "heap" :depends-on ("packages"))
     (:file "indexer" :depends-on ("packages" "heap" "compress" "words" "files"))
    ))
