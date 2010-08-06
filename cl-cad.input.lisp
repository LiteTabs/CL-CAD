(in-package :cl-cad)

(defparameter *clicked-mouse* nil)

(defun get-x ()
  (if (equal *clicked-mouse* t)
      *x*))

(defun input-for-point ()
  (add-point
   *current-layer*
   (get-x)
   (get-y)
   0
   *current-color*
   *current-line-type*))

