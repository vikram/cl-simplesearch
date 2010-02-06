;; -*- lisp -*-

(defpackage :com.cl-simplesearch.system
  (:use :asdf :cl))

(in-package :com.cl-simplesearch.system)

#+openmcl
(require "OBJC-SUPPORT")

(defsystem :cl-simplesearch
    :name "cl-simplesearch"
    :author "Vikram Bhandoh <vikram.bhandoh@gmail.com>"
    :version "0.2"
    :description "All code for the cl-simplesearch"
    :depends-on (:cl-ppcre :md5 :cl-store)
    :components
    ((:file "packages")
     (:file "utils" :depends-on ("packages"))
     (:file "files" :depends-on ("packages"))
     (:file "words" :depends-on ("packages" "utils"))
     (:file "compress" :depends-on ("packages"))
     (:file "heap" :depends-on ("packages"))
     (:file "indexer" :depends-on ("packages" "utils" "heap" "compress" "words" "files"))
    ))
