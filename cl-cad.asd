(defsystem :cl-cad
        :name :cl-cad
        :author "Burdukov Denis <litetabs@gmail.com>"
        :version "0.1"
        :maintainer "Burdukov Denis <litetabs@gmail.com>"
        :licence "LLGPL"
        :serial t
        :components ((:file "cl-cad"))
        :depends-on (:cl-gtk2-glib :cffi :cl-gtk2-gdk :cl-gtk2-gtk :iterate :cl-cairo2))
