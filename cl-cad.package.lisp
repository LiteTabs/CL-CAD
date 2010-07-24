(in-package :common-lisp-user)

(defpackage cl-cad
  (:shadowing-import-from #:cairo #:scale #:rectangle #:pointer)
  (:use #:cl #:gtk #:gdk #:gobject #:cairo #:cl-gtk2-cairo)
  (:export :main-window :main-and-quit))