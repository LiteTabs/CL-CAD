(in-package :cl-cad)

(defun input-for-point ()
  (add-point
   *current-layer*
   (get-x)
   (get-y)
   0
   *current-color*
   *current-line-type*))

