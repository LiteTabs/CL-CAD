(in-package :cl-cad)

(defun coordinate-light (x y)
  (set-source-rgb (color-gtk-to-cairo (color-red (config-osnap-color *config*)))
		  (color-gtk-to-cairo (color-green (config-osnap-color *config*)))
		  (color-gtk-to-cairo (color-blue (config-osnap-color *config*))))
  (set-line-width 1)
  (move-to (- x 4) (- y 4))
  (line-to (- x 4) (+ y 4))
  (line-to (+ x 4) (+ y 4))
  (line-to (+ x 4) (- y 4))
  (line-to (- x 4) (- y 4))
  (stroke))

(defun get-snap-length (x y)
  (sqrt
   (+
    (* (- x *current-x*)
       (- x *current-x*))
    (* (- y *current-y*)
       (- y *current-y*)))))

(defun get-snap-coordinates ()
  (dolist (cd *current-draw*) 
    (if (equal *osnap-center* t) 
	(cond ((equal (getf cd :title) :circle)
	       (coordinate-light (* *scroll-units* (getf cd :x1)) (* *scroll-units* (getf cd :y1)))
	       (if (>= *osnap-area-delta* (get-snap-length (* *scroll-units* (getf cd :x1)) (* *scroll-units* (getf cd :y1))))
		   (progn
		     (move-to (* *scroll-units* (getf cd :x1)) (* *scroll-units* (getf cd :y1)))
		     (line-to *current-x* *current-y*)
		     (stroke))))))
    (if (equal *osnap-end* t) 
	(cond ((equal (getf cd :title) :line)
	       (coordinate-light (* *scroll-units* (getf cd :x1)) (* *scroll-units* (getf cd :y1)))
	       (coordinate-light (* *scroll-units* (getf cd :x2)) (* *scroll-units* (getf cd :y2))))))))
	       
	       
































;  (if (equal *osnap-insert* t) (dolist (cd *current-draw*) (cond ((equal (getf cd :title) "block")
;								  (print (cons (getf cd :x1) (getf cd :y1)))))))
;  (if (equal *osnap-intersection* t) (lambda ()))
;  (if (equal *osnap-midpoint* t) (dolist (cd *current-draw*) (cond ((equal (getf cd :title) "line")
;								    (print (cons (/ (+ (getf cd :x1) (getf cd :x2)) 2) (/ (+ (getf cd :y1) (getf cd :y2)) 2)))))))
;  (if (equal *osnap-nearest* t) (lambda ()))
;  (if (equal *osnap-point* t) (dolist (cd *current-draw*) (cond ((equal (getf cd :title) "point")
;								  (print (cons (getf cd :x1) (getf cd :y1))))))))
 ; (if (equal *osnap-perpendicular* t) (lambda ()))
 ; (if (equal *osnap-quadrant* t) (lambda ()))
 ; (if (equal *osnap-tangent* t) (lambda ()))
 ; (if (equal *osnap-track* t) (lambda ()))
 ; (if (equal *osnap-grid* t) (lambda ()))
 ; (if (equal *osnap-ortho* t) (lambda ())))

(defun osnap-window (parent-window)
  (within-main-loop
   (let ((window (make-instance 'gtk-window 
				:type :toplevel 
				:title "Osnap" 
				:window-position :center 
				:default-width 240 
				:default-height 320 
				:destroy-with-parent t
				:transient-for parent-window))
	 (v-box (make-instance 'v-box))
	 (h-box (make-instance 'h-box))
	 (osnap-table (make-instance 'table :n-rows 11 :n-columns 2 :homogeneous nil))
	 (button-oscenter (make-instance 'check-button :label "Center" :active (if (equal *osnap-center* t) (print t) (print nil))))
	 (center-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_cen.svg" *src-location*))))
	 (button-osend (make-instance 'check-button :label "End" :active (if (equal *osnap-end* t) (print t) (print nil))))
	 (end-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_end.svg" *src-location*))))
	 (button-osins (make-instance 'check-button :label "Insert":active (if (equal *osnap-insert* t) (print t) (print nil)) ))
	 (ins-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_ins.svg" *src-location*))))
	 (button-osint (make-instance 'check-button :label "Intersection" :active (if (equal *osnap-intersection* t) (print t) (print nil))))
	 (int-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_int.svg" *src-location*))))
	 (button-osmid (make-instance 'check-button :label "Midle" :active (if (equal *osnap-midpoint* t) (print t) (print nil))))
	 (mid-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_mid.svg" *src-location*))))
	 (button-osnea (make-instance 'check-button :label "Nearest" :active (if (equal *osnap-nearest* t) (print t) (print nil))))
	 (nea-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_nea.svg" *src-location*))))
	 (button-osnod (make-instance 'check-button :label "Points" :active (if (equal *osnap-point* t) (print t) (print nil))))
	 (nod-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_nod.svg" *src-location*))))
	 (button-osper (make-instance 'check-button :label "Perpendicular" :active (if (equal *osnap-perpendicular* t) (print t) (print nil))))
	 (per-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_per.svg" *src-location*))))
	 (button-osqua (make-instance 'check-button :label "Quadrant" :active (if (equal *osnap-quadrant* t) (print t) (print nil))))
	 (qua-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_qua.svg" *src-location*))))
	 (button-ostan (make-instance 'check-button :label "Tangent" :active (if (equal *osnap-tangent* t) (print t) (print nil))))
	 (tan-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_tan.svg" *src-location*))))
	 (button-ostrack (make-instance 'check-button :label "Track" :active (if (equal *osnap-track* t) (print t) (print nil))))
	 (track-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_track.svg" *src-location*))))
	 (button-osgrid (make-instance 'check-button :label "Grid" :active (if (equal *osnap-grid* t) (print t) (print nil))))
	 (grid-image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_grid.svg" *src-location*))))
	 (button-ok (make-instance 'button :label "Ok")))
     (container-add window v-box)
     (container-add v-box osnap-table)
     (table-attach osnap-table button-oscenter 0 1 0 1)
     (table-attach osnap-table center-image 1 2 0 1)
     (table-attach osnap-table button-osend 0 1 1 2)
     (table-attach osnap-table end-image 1 2 1 2)
     (table-attach osnap-table button-osins 0 1 2 3)
     (table-attach osnap-table ins-image 1 2 2 3)
     (table-attach osnap-table button-osint 0 1 3 4)
     (table-attach osnap-table int-image 1 2 3 4)
     (table-attach osnap-table button-osmid 0 1 4 5)
     (table-attach osnap-table mid-image 1 2 4 5)
     (table-attach osnap-table button-osnea 0 1 5 6)
     (table-attach osnap-table nea-image 1 2 5 6)
     (table-attach osnap-table button-osnod 0 1 6 7)
     (table-attach osnap-table nod-image 1 2 6 7)
     (table-attach osnap-table button-osper 0 1 7 8)
     (table-attach osnap-table per-image 1 2 7 8)
     (table-attach osnap-table button-osqua 0 1 8 9)
     (table-attach osnap-table qua-image 1 2 8 9)
     (table-attach osnap-table button-ostan 0 1 9 10)
     (table-attach osnap-table tan-image 1 2 9 10)
     (table-attach osnap-table button-ostrack 0 1 10 11)
     (table-attach osnap-table track-image 1 2 10 11)
     (table-attach osnap-table button-osgrid 0 1 11 12)
     (table-attach osnap-table grid-image 1 2 11 12)
     (box-pack-start v-box h-box :expand nil)
     (box-pack-start h-box button-ok :expand nil)
     (gobject:g-signal-connect window "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
     (gobject:g-signal-connect button-ok "clicked" (lambda (widget) (declare (ignore widget)) (object-destroy window)))
     (gobject:g-signal-connect button-oscenter "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-center* t) (setf *osnap-center* nil) (setf *osnap-center* t))))
     (gobject:g-signal-connect button-osend "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-end* t) (setf *osnap-end* nil) (setf *osnap-end* t))))
     (gobject:g-signal-connect button-osins "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-insert* t) (setf *osnap-insert* nil) (setf *osnap-insert* t))))
     (gobject:g-signal-connect button-osint "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-intersection* t) (setf *osnap-intersection* nil) (setf *osnap-intersection* t))))
     (gobject:g-signal-connect button-osmid "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-midpoint* t) (setf *osnap-midpoint* nil) (setf *osnap-midpoint* t))))
     (gobject:g-signal-connect button-osnea "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-nearest* t) (setf *osnap-nearest* nil) (setf *osnap-nearest* t))))
     (gobject:g-signal-connect button-osnod "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-point* t) (setf *osnap-point* nil) (setf *osnap-point* t))))
     (gobject:g-signal-connect button-osper "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-perpendicular* t) (setf *osnap-perpendicular* nil) (setf *osnap-perpendicular* t))))
     (gobject:g-signal-connect button-osqua "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-quadrant* t) (setf *osnap-quadrant* nil) (setf *osnap-quadrant* t))))
     (gobject:g-signal-connect button-ostan "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-tangent* t) (setf *osnap-tangent* nil) (setf *osnap-tangent* t))))
     (gobject:g-signal-connect button-ostrack "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-track* t) (setf *osnap-track* nil) (setf *osnap-track* t))))
     (gobject:g-signal-connect button-osgrid "clicked" (lambda (a) (declare (ignore a)) (if (equal *osnap-grid* t) (setf *osnap-grid* nil) (setf *osnap-grid* t))))
     (widget-show window))))