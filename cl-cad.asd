(in-package :common-lisp-user)

(defpackage cl-cad-system
  (:use :common-lisp :asdf))

(in-package :cl-cad-system)

(defsystem "cl-cad"
  :description "CAD program for GNU\Linux"
  :version "0.1"
  :author "Burdukov Denis <litetabs@gmail.com>"
  :licence "LGPL"
  :serial t
  :components ((:file "cl-cad.package")
	       (:file "cl-cad.config")
	       (:file "cl-cad.engine")
	       (:file "cl-cad.parameters")
	       (:file "cl-cad.parser")
	       (:file "cl-cad.osnaps")
	       (:file "cl-cad.utils")
	       (:file "cl-cad.layers")
	       (:file "cl-cad.input")
	       (:file "cl-cad.grid")
	       (:file "cl-cad.main"))
  :depends-on (:cl-gtk2-glib 
	       :cffi
	       :trivial-garbage
	       :iterate 
	       :cl-gtk2-gtk 
	       :cl-gtk2-cairo
	       :cl-cairo2))
