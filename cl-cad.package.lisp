(in-package :common-lisp-user)

(defpackage cl-cad
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:cl-gtk2-cairo)
  (:export :main-window :run))