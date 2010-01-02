(defpackage #:cl-cad
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:cl-gtk2-cairo #:iter)
  (:export #:cl-cad))

(in-package #:cl-cad)

;open many files
(defmacro with-many-open-files ((var file-list) &body body)
  (let ((filespec (gensym)))
    `(let ((,var (loop :for ,filespec :in ,file-list
                       :collect (etypecase ,filespec
                                  ((or string pathname)
                                        (cons (pathname ,filespec)
                                              (open ,filespec)))
                                  (cons (cons (pathname (car ,filespec))
                                              (apply #'open ,filespec)))))))
       (unwind-protect
         (progn ,@body)
         (loop :for ,filespec :in ,var
               do (close (cdr ,filespec)))))))
;; filespec ::= "filename" | ("filename" &rest open-args)


(defvar *current-draw* nil)
(defvar *selection* nil)

(defun make-draw-current (draw)
  (setf *current-draw* draw))

(defun add-file-properties (file-name subject author keywords comments hyperlink created modified)
  (push (list :title "file-properties" :file-name file-name :subject subject :author author :keywords keywords :comments comments :hyperlink hyperlink :created created :modified modified) *current-draw*))

(defun add-sheet-properties (format hpole vpole background-color metrics space)
  (push (list :title "sheet-properties" :format format :hpole hpole :vpole vpole :background-color background-color :metrics metrics :space space) *current-draw*))

(defun add-layer (layer-name line-type color-line weight printable view)
  (push (list :title "layer" :layer-name layer-name :line-type line-type :color-line color-line :weight weight :printable printable :view view) *current-draw*))

(defun add-line (layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line weight)
  (push (list :title "line" :layer layer :x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2 :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-circle (layer x1 y1 z1 radius line-type zoom-line color-line weight)
  (push (list :title "circle" :layer layer :x1 x1 :y1 y1 :z1 z1 :radius radius :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-arc (layer x1 y1 z1 radius startangle endangle line-type zoom-line color-line weight)
  (push (list :title "arc" :layer layer :x1 x1 :y1 y1 :z1 z1 :radius radius :startangle startangle :endangle endangle :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-continious (layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line weight)
  (push (list :title "continious" :layer layer :x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2 :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-ray (layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line weight)
  (push (list :title "ray" :layer layer :x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2 :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-text (layer x1 y1 z1 count style annotate alignment height angle compression mirror mirror-left line-type zoom-line color-line weight)
  (push (list :title "text" :layer layer :x1 x1 :y1 y1 :z1 z1 :count count :style style :annotate annotate :alignment alignment :height height :angle angle :compression compression :mirror mirror :mirror-left mirror-left :line-type line-type :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-mtext (layer x1 y1 z1 count style annotate alignment height angle interval zoom-line color-line weight)
  (push (list :title "mtext" :layer layer :x1 x1 :y1 y1 :z1 z1 :count count :style style :annotate annotate :alignment alignment :height height :angle angle :interval interval :zoom-line zoom-line :color-line color-line :weight weight) *current-draw*))

(defun add-block (layer name-block x1 y1 z1 xscale yscale zscale count rotation)
  (push (list :title "block" :layer layer :name-block name-block :x1 x1 :y1 y1 :z1 z1 :xscale xscale :yscale yscale :zscale zscale :count count :rotation rotation) *current-draw*))

(defun add-point (layer x1 y1 z1 color-line style)
  (push (list :title "point" :layer layer :x1 x1 :y1 y1 :z1 z1 :color-line color-line :style style) *current-draw*))

(defun add-ellipse (layer x1 y1 z1 major-radius minor-radius radius-ratio start-angle end-angle color-line weight)
  (push (list :title "ellipse" :layer layer :x1 x1 :y1 y1 :z1 z1 :major-radius major-radius :minor-radius minor-radius :radius-ratio radius-ratio :start-angle start-angle :end-angle end-angle :color-line color-line :weight weight) *current-draw*))

(defun add-raster-image (layer x1 y1 z1 rotation-angle width height scale brightness contrast fade path show-image show-clipped transparency)
  (push (list :title "raster-image" :layer layer :x1 x1 :y1 y1 :z1 z1 :rotation-angle rotation-angle :width width :height height :scale scale :brightness brightness :contrast contrast :fade fade :path path :show-image show-image :show-clipped show-clipped :transparency transparency) *current-draw*))


(defun shutdown ()
  #+:clisp (ext:quit)
  #+:sbcl (sb-ext:quit)
  #+:ccl (ccl:quit))

(defun save-draw (filename)
	(with-open-file (out filename :direction :output :if-exists :supersede)
	(with-standard-io-syntax
		(print *current-draw* out))))

(defun open-draw (filename)
	(with-open-file (in filename)
	(with-standard-io-syntax
		(setf *current-draw* (read in)))))

(defun prompt-read (prompt)
	(format *query-io* "~a: " prompt)
	(force-output *query-io*)
	(read-line *query-io*))
	
(defun select (selector-fn)
  (remove-if-not selector-fn *current-draw*))

(defun delete-primitive (selector-fn)
  (setf *current-draw* (remove-if selector-fn *current-draw*)))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun update (selector-fn &key title layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line weight style)
           (setf *current-draw*
               (mapcar
                #'(lambda (row)
                    (when (funcall selector-fn row)
                      (if title (setf (getf row :title) title))
                      (if layer (setf (getf row :layer) layer))
                      (if x1 (setf (getf row :x1) x1))
                      (if y1 (setf (getf row :y1) y1))
                      (if z1 (setf (getf row :z1) z1))
                      (if x2 (setf (getf row :x2) x2))
                      (if y2 (setf (getf row :y2) y2))
                      (if z2 (setf (getf row :z2) z2))
                      (if line-type (setf (getf row :line-type) line-type))
                      (if zoom-line (setf (getf row :zoom-line) zoom-line))
                      (if color-line (setf (getf row :color-line) color-line))
                      (if weight (setf (getf row :weight) weight))
                      (if style (setf (getf row :style) style)))
                row) *current-draw*)))

(defun update-properties (selector-fn &key file-name subject author keywords comments hyperlink created modified format hpole vpole background-color metrics space)
           (setf *current-draw*
               (mapcar
                #'(lambda (row)
                    (when (funcall selector-fn row)
                      (if file-name (setf (getf row :file-name) file-name))
                      (if subject (setf (getf row :subject) subject))
                      (if author (setf (getf row :author) author))
                      (if keywords (setf (getf row :keywords) keywords))
                      (if comments (setf (getf row :comments) comments))
                      (if hyperlink (setf (getf row :hyperlink) hyperlink))
                      (if created (setf (getf row :created) created))
                      (if modified (setf (getf row :modified) modified))
                      (if format (setf (getf row :format) format))
                      (if hpole (setf (getf row :hpole) hpole))
                      (if vpole (setf (getf row :vpole) vpole))
                      (if background-color (setf (getf row :background-color) background-color))
                      (if metrics (setf (getf row :metrics) metrics))
                      (if space (setf (getf row :space) space)))
                row) *current-draw*)))
;sample
;(update (where :title "title") :x1 1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PARAMETERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is default parameters for drawing. Load ~.cl-cad/standard.conf for use your
;standards.
;P.S.: I don't now your drawwind standards. Make it and send it me - PROFIT!!!

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :cl-cad)))
(defparameter *default-format* "A4")
(defparameter *default-space* "2d")
(defparameter *default-background-color* (list 0 0 0))
(defparameter *default-metrics* "millimetters")
(defparameter *color-current-layer* nil)
(defparameter *default-line-width* 1)
(defparameter *default-font-size* 12)
(defvar *text-buffer-count*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;BEGIN USER INTERFACE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Drawwing functions

(defun draw-line (x1 y1 x2 y2)
  (set-line-width *default-line-width*)
  (move-to x1 y1)
  (line-to x2 y2)
  (set-source-rgb 0 0 0)
  (stroke))

(defun draw-background-color () ;todo make there coordinate indicator
  (set-source-rgb 1 1 1)
  (paint))

(defun draw-text (x1 y1 style text)
  (select-font-face style :normal :normal)
  (move-to x1 y1)
  (set-font-size *default-font-size*)
  (show-text text))

(defun draw-circle (x1 y1 radius)
  (arc x1 y1 radius 0 (* 2 pi))
  (set-source-rgb 1 1 1)
  (fill-path))

(defun draw-arc (x1 y1 radius startangle endangle)
  (arc x1 y1 radius (deg-to-rad startangle) (deg-to-rad endangle))
  (set-source-rgb 1 1 1)
  (fill-path))

;(defun draw-continious (x1 y1 x2 y2)
;  (set-line-width 0 0 0)
;  (move-to x1 y1)
;  (line-to x2 y2)
;  (set-source-rgb 1 0 0)
;  (stroke))

;(defun draw-ray (x1 y1 x2 y2)
;  (set-line-width 0 0 0)
;  (move-to x1 y1)
;  (line-to x2 y2)
;  (set-source-rgb 1 0 0)
;  (stroke))

;(defun draw-point (x1 y1 style)
;  (if (equal "simple" style)
;     (set-line-width 0 0 0)
;     (move-to x1 y1)
;     (line-to x1 y1)
;     (set-source-rgb 1 1 1)
;     (stroke)))

;(defun draw-ellipse ())
;make with curves

;(defun draw-image (x1 y1 path)
;  "Use only .png images"
;  (with-png-surface ((png-pathname-string path t) image)
;	  (let ((w (image-surface-get-width image))
;			(h (image-surface-get-height image)))
;		(rotate (* rotation-angle (/ pi 180)))
;		(set-source-surface image 0 0)
;		(paint))))


;BEGIN MAIN WINDOW

;TODO print window
;(defun print-properties-window ()
;  (within-main-loop
;   (let* ((window (make-instance 'gtk:gtk-window
;                                 :type :toplevel
;                                 :window-position :center
;                                 :title "Print properties"
;                                 :default-width 800
;                                 :default-height 600))
;TODO file properties
;(defun file-properties-window ()
;  (within-main-loop
;   (let* ((window (make-instance 'gtk:gtk-window
;                                 :type :toplevel
;                                 :window-position :center
;                                 :title "File properties"
;                                 :default-width 800
;                                 :default-height 600))
;;TODO  make-new-file window

;(defun make-new-file-window ()
;  (within-main-loop
;   (let* ((window (make-instance 'gtk:gtk-window
;                                 :type :toplevel
;                                 :window-position :center
;                                 :title "Make new file"
;                                 :default-width 800
;                                 :default-height 600))
;TODO draw properties window
;(defun draw-properties-window ()
;  (within-main-loop
;   (let* ((window (make-instance 'gtk:gtk-window
;                                 :type :toplevel
;                                 :window-position :center
;                                 :title "Draw properties"
;                                 :default-width 800
;                                 :default-height 600))
;;TODO wizzird window;
;
;TODO layers-window

(defun cl-cad ()
  (main-window)
  (join-main-thread))

(defun main-window ()
  (within-main-loop
   (let* ((window (make-instance 'gtk-window :title "CL-CAD" :request-width 1024 :request-height 600))
	  (v-box (make-instance 'v-box))
	  (h-box (make-instance 'h-box))
	  ;tools
	  (menu-notebook (make-instance 'notebook :enable-popup t))
	  (v-box-menu (make-instance 'v-box))
	  ;;;system
	  (system-expander (make-instance 'expander :expanded t :label "System"))
	  (system-table (make-instance 'table :n-rows 2 :n-columns 4 :homogeneous nil))
	  (button-save (make-instance 'button :label "Save"))
	  (button-save-as (make-instance 'button :label "Save as"))
	  (button-new (make-instance 'button :label "New"))
	  (button-open (make-instance 'button :label "Open"))
	  (button-about (make-instance 'button :label "About"))
	  (button-print (make-instance 'button :label "Print"))
	  (button-print-prop (make-instance 'button :label "Print properties"))
	  (button-file-prop (make-instance 'button :label "File properties"))
	  ;;;primitives
	  (primitives-expander (make-instance 'expander :expanded t :label "Primitives"))
	  (primitives-table (make-instance 'table :n-rows 5 :n-columns 4 :homogeneous nil))
	  (button-line (make-instance 'button :label "Line"))
	  (button-ray (make-instance 'button :label "Ray"))
	  (button-construction (make-instance 'button :label "Construction"))
	  (button-circle-radius (make-instance 'button :label "Circle-radius"))
	  (button-circle-2p (make-instance 'button :label "Circle-2p"))
	  (button-circle-3p (make-instance 'button :label "Circle-3p"))
	  (button-circle-diameter (make-instance 'button :label "Circle-diameter"))
	  (button-circle-ttr (make-instance 'button :label "Circle-ttr"))
	  (button-circle-ttt (make-instance 'button :label "Circle-ttt"))
	  (button-arc (make-instance 'button :label "Arc"))
	  (button-ellipse-center (make-instance 'button :label "Ellipse-center"))
	  (button-ellipse-axis (make-instance 'button :label "Ellipse-axis"))
	  (button-ellipse-arc (make-instance 'button :label "Ellipse-arc"))
	  (button-pline (make-instance 'button :label "Polyline"))
	  (button-polygon (make-instance 'button :label "Polygon"))
	  (button-point (make-instance 'button :label "Point"))
	  (button-rectangle (make-instance 'button :label "Rectangle"))
	  (button-spline (make-instance 'button :label "Spline"))
	  (button-text (make-instance 'button :label "Text"))
	  ;;;modify
	  (modify-expander (make-instance 'expander :expanded t :label "Modify"))
	  (modify-table (make-instance 'table :n-rows 4 :n-columns 4 :homogeneous nil))
	  (button-break (make-instance 'button :label "Break"))
	  (button-edit-pline (make-instance 'button :label "Edit pline"))
	  (button-edit-prop (make-instance 'button :label "Edit properties"))
	  (button-edit-spline (make-instance 'button :label "Edit spline"))
	  (button-edit-text (make-instance 'button :label "Edit text"))
	  (button-erase (make-instance 'button :label "Erase"))
	  (button-explode (make-instance 'button :label "Explode"))
	  (button-extend (make-instance 'button :label "Extend"))
	  (button-lengthen (make-instance 'button :label "Lengthen"))
	  (button-move (make-instance 'button :label "Move"))
	  (button-rotate (make-instance 'button :label "Rotate"))
	  (button-scale (make-instance 'button :label "Scale"))
	  (button-stretch (make-instance 'button :label "Stretch"))
	  (button-trim (make-instance 'button :label "Trim"))
	  ;;;osnap
	  (osnap-expander (make-instance 'expander :expanded t :label "Osnap"))
	  (osnap-table (make-instance 'table :n-rows 3 :n-columns 4 :homogeneous nil))
	  (button-oscenter (make-instance 'button :label "Center"))
	  (button-osend (make-instance 'button :label "Endpoint"))
	  (button-osins (make-instance 'button :label "InsPoint"))
	  (button-osint (make-instance 'button :label "Intersection"))
	  (button-osmid (make-instance 'button :label "Midpoint"))
	  (button-osnea (make-instance 'button :label "Nearest"))
	  (button-osnod (make-instance 'button :label "Nod"))
	  (button-osnone (make-instance 'button :label "None"))
	  (button-osper (make-instance 'button :label "Perpendicular"))
	  (button-osqua (make-instance 'button :label "Quadrant"))
	  (button-ostan (make-instance 'button :label "Tangens"))
	  (button-ostrack (make-instance 'button :label "Track"))
	  ;;;dimension
	  (dimension-expander (make-instance 'expander :expanded t :label "Dimension"))
	  (dimension-table (make-instance 'table :n-rows 3 :n-columns 4 :homogeneous nil))
	  (button-align (make-instance 'button :label "Align"))
	  (button-angular (make-instance 'button :label "Angular"))
	  (button-baseline (make-instance 'button :label "Baseline"))
	  (button-center (make-instance 'button :label "Center"))
	  (button-continue (make-instance 'button :label "Continue"))
	  (button-diameter (make-instance 'button :label "Diameter"))
	  (button-horiz (make-instance 'button :label "Horizontal"))
	  (button-leader (make-instance 'button :label "Leader"))
	  (button-vert (make-instance 'button :label "Vertical"))
	  ;;;zoom
	  (zoom-expander (make-instance 'expander :expanded t :label "Zoom"))
	  (zoom-table (make-instance 'table :n-rows 2 :n-columns 4 :homogeneous nil))
	  (button-zoom-all (make-instance 'button :label "All"))
	  (button-zoom-pan (make-instance 'button :label "Pan"))
	  (button-zoom-previous (make-instance 'button :label "Previous"))
	  (button-zoom-redraw (make-instance 'button :label "Redraw"))
	  (button-zoom-scale (make-instance 'button :label "Scale"))
	  (button-zoom-window (make-instance 'button :label "Window"))
	  (button-zoom-plus (make-instance 'button :label "Plus"))
	  (button-zoom-minus (make-instance 'button :label "Minus"))
	  ;draw
	  (draw-area (make-instance 'drawing-area))
	  ;terminal
	  (term-expander (make-instance 'expander :expanded t))
	  (term-notebook (make-instance 'notebook :enable-popup t)))

     ;;;pack
     (container-add window vbox)
     (box-pack-start v-box h-box)
     (box-pack-start v-box term-expander)
     (box-pack-start h-box menu-notebook)
     (box-pack-start h-box draw-area)
     (box-pack-start term-expander term-notebook)
     ;;menu-pack
     (box-pack-start menu-notebook v-box-menu)
     ;system
     (box-pack-start v-box-menu system-expander)
     (box-pack-start system-expander system-table)
     (table-attach system-table button-save 0 0 0 0)
     (table-attach system-table button-save-as 0 0 0 0)
     (table-attach system-table button-new 0 0 0 0)
     (table-attach system-table button-open 0 0 0 0)
     (table-attach system-table button-about 0 0 0 0)
     (table-attach system-table button-print 0 0 0 0)
     (table-attach system-table button-print-prop 0 0 0 0)
     (table-attach system-table button-file-prop 0 0 0 0)
     ;primitives
     (box-pack-start v-box-menu primitives-expander)
     (box-pack-start primitives-expander primitives-table)
     (table-attach primitives-table button-line 0 0 0 0)
     (table-attach primitives-table button-ray 0 0 0 0)
     (table-attach primitives-table button-construction 0 0 0 0)
     (table-attach primitives-table button-circle-radius 0 0 0 0)
     (table-attach primitives-table button-circle-2p 0 0 0 0)
     (table-attach primitives-table button-circle-3p 0 0 0 0)
     (table-attach primitives-table button-circle-diameter 0 0 0 0)
     (table-attach primitives-table button-circle-ttr 0 0 0 0)
     (table-attach primitives-table button-circle-ttt 0 0 0 0)
     (table-attach primitives-table button-arc 0 0 0 0)
     (table-attach primitives-table button-ellipse-center 0 0 0 0)
     (table-attach primitives-table button-ellipse-axis 0 0 0 0)
     (table-attach primitives-table button-ellipse-arc 0 0 0 0)
     (table-attach primitives-table button-pline 0 0 0 0)
     (table-attach primitives-table button-polygon 0 0 0 0)
     (table-attach primitives-table button-point 0 0 0 0)
     (table-attach primitives-table button-rectangle 0 0 0 0)
     (table-attach primitives-table button-spline 0 0 0 0)
     (table-attach primitives-table button-text 0 0 0 0)
     ;modify
     (box-pack-start v-box-menu modify-expander)
     (box-pack-start modify-expander modify-table)
     (table-attach modify-table button-break 0 0 0 0)
     (table-attach modify-table button-edit-pline 0 0 0 0)
     (table-attach modify-table button-edit-prop 0 0 0 0)
     (table-attach modify-table button-edit-spline 0 0 0 0)
     (table-attach modify-table button-edit-text 0 0 0 0)
     (table-attach modify-table button-erase 0 0 0 0)
     (table-attach modify-table button-explode 0 0 0 0)
     (table-attach modify-table button-extend 0 0 0 0)
     (table-attach modify-table button-lengthen 0 0 0 0)
     (table-attach modify-table button-move 0 0 0 0)
     (table-attach modify-table button-rotate 0 0 0 0)
     (table-attach modify-table button-scale 0 0 0 0)
     (table-attach modify-table button-stretch 0 0 0 0)
     (table-attach modify-table button-trim 0 0 0 0)
     ;osnap
     (box-pack-start v-box-menu osnap-expander)
     (box-pack-start osnap-expander osnap-table)
     (table-attach osnap-table button-oscenter 0 0 0 0)
     (table-attach osnap-table button-osend 0 0 0 0)
     (table-attach osnap-table button-osins 0 0 0 0)
     (table-attach osnap-table button-osint 0 0 0 0)
     (table-attach osnap-table button-osmid 0 0 0 0)
     (table-attach osnap-table button-osnea 0 0 0 0)
     (table-attach osnap-table button-osnod 0 0 0 0)
     (table-attach osnap-table button-osnone 0 0 0 0)
     (table-attach osnap-table button-osper 0 0 0 0)
     (table-attach osnap-table button-osqua 0 0 0 0)
     (table-attach osnap-table button-ostan 0 0 0 0)
     (table-attach osnap-table button-ostrack 0 0 0 0)
     ;dimension
     (box-pack-start v-box-menu dimension-expander)
     (box-pack-start dimension-expander dimension-table)
     (table-attach dimension-table button-align 0 0 0 0)
     (table-attach dimension-table button-angular 0 0 0 0)
     (table-attach dimension-table button-baseline 0 0 0 0)
     (table-attach dimension-table button-center 0 0 0 0)
     (table-attach dimension-table button-continue 0 0 0 0)
     (table-attach dimension-table button-diameter 0 0 0 0)
     (table-attach dimension-table button-horiz 0 0 0 0)
     (table-attach dimension-table button-leader 0 0 0 0)
     (table-attach dimension-table button-vert 0 0 0 0)
     ;zoom     
     (box-pack-start v-box-menu zoom-expander)
     (box-pack-start zoom-expander zoom-table)
     (table-attach zoom-table button-zoom-all 0 0 0 0)
     (table-attach zoom-table button-zoom-pan 0 0 0 0)
     (table-attach zoom-table button-zoom-previous 0 0 0 0)
     (table-attach zoom-table button-zoom-redraw 0 0 0 0)
     (table-attach zoom-table button-zoom-scale 0 0 0 0)
     (table-attach zoom-table button-zoom-window 0 0 0 0)
     (table-attach zoom-table button-zoom-plus 0 0 0 0)
     (table-attach zoom-table button-zoom-minus 0 0 0 0)
 
     ;;;g-signals
     (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
     (g-signal-connect window "delete-event" (lambda (widget event)
                                                (declare (ignore widget event))
                                                (let ((dlg (make-instance 'message-dialog
                                                                          :text "Are you sure?"
                                                                          :buttons :yes-no)))
                                                  (let ((response (dialog-run dlg)))
                                                    (object-destroy dlg)
                                                    (not (eq :yes response))))))
     (g-signal-connect button-save "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-save-as "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-new "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-open "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-about "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-print "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-print-prop "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-file-prop "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-line "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ray "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-construction "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-radius "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-2p "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-3p "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-diameter "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-ttr "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-circle-ttt "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-arc "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ellipse-center "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ellipse-axis "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ellipse-arc "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-pline "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-polygon "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-point "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-rectangle "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-spline "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-text "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-break "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-edit-pline "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-edit-prop "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-edit-spline "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-edit-text "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-erase "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-explode "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-extend "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-lengthen "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-move "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-rotate "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-scale "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-stretch "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-trim "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-oscenter "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osend "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osins "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osint "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osmid "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osnea "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osnod "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osnone "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osper "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-osqua "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ostan "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-ostrack "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-align "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-angular "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-baseline "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-center "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-continue "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-diameter "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-horiz "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-leader "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-vert "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-all "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-pan "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-previous "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-redraw "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-scale "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-window "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-plus "clicked" (lambda (w) (declare (ignore w)) ()))
     (g-signal-connect button-zoom-minus "clicked" (lambda (w) (declare (ignore w)) ()))
     
     (widget-show window))))