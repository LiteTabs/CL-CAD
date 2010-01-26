(defsystem :cl-cad
        :name :cl-cad
        :version "0.1"
	:author "Burdukov Denis <litetabs@gmail.com>"
        :licence "LLGPL"
        :serial t
        :components ((:file "cl-cad"))
        :depends-on (:cl-gtk2-glib :cffi :iterate :cl-gtk2-gdk :cl-gtk2-gtk :cl-cairo2 :cl-gtk2-cairo))
