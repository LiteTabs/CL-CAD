(in-package :cl-cad)

(defvar *signal* nil)
(defvar *end* nil)

(defun draw-shadow ()
  (cond
    ((equal *signal* :line) (input-for-line))
    ((equal *signal* :circle) (input-for-circle))
    ;((equal *signal* :arc) (input-for-arc))
    ;((equal *signal* :continious) (input-for-continious))
    ;((equal *signal* :ray) (input-for-ray))
    ((equal *signal* :point) (input-for-point))
    ))

(defun input-for-line ()
  (set-source-rgb 1 0 0)
  (set-line-width 0.5)
  (move-to 
   (if (equal *x* 0) 
       *current-x*
       *x*)
   (if (equal *y* 0) 
       *current-y*
       *y*))
  (line-to *current-x* *current-y*)
  (stroke))
;  (if (equal *end* nil)
;   (progn 
;     (add-line "0" *x* *y* 0 *current-x* *current-y* 0 "continious" 1 1 1)
;     (setf *x* 0 *y* 0)
;     (setf *end* nil))))

(defun input-for-circle ()
  (set-source-rgb 1 0 0)
  (set-line-width 0.5)
  (arc (if (equal *x* 0) 
	   *current-x*
	   *x*)
       (if (equal *y* 0) 
	   *current-y*
	   *y*)
       (sqrt
	(+
	 (* (- *x* *current-x*)
	    (- *x* *current-x*))
	 (* (- *y* *current-y*)
	    (- *y* *current-y*))))
        0 (* 2 pi))
  (stroke))

;  (if (equal *end* t)
;      (progn
;	(add-circle "0" 
;		    *x* 
;		    *y* 
;		    0 
;		    (sqrt
;		     (+
;		      (* (- *x* *current-x*)
;			 (- *x* *current-x*))
;		      (* (- *y* *current-y*)
;			 (- *y* *current-y*))))
;		    1 1 1 1)
;	(setf *x* 0 *y* 0)
;	(setf *end* nil))))

(defun input-for-point ()
  (set-source-rgb 1 0 0)
  (set-line-width 0.5)
  (rectangle (- (/ *scroll-units* *current-x*) 0.5)
	     (- (/ *scroll-units* *current-y*) 0.5)
	     1 1)
  (fill-path)
  (restore))