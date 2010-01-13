(defpackage #:cl-cad
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:iter)
  (:export #:main-window))

(in-package #:cl-cad)

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

;(defun draw-line ()
;  (set-line-width *default-line-width*)
;  (move-to 0 0)
;  (line-to 100 100)
;  (set-source-rgb 0 0 0)
;  (stroke))

;(defun draw-background-color () ;todo make there coordinate indicator
;  (set-source-rgb 1 1 1)
;  (paint))
;
;(defun draw-text (x1 y1 style text)
;  (select-font-face style :normal :normal)
;  (move-to x1 y1)
;  (set-font-size *default-font-size*)
;  (show-text text));

;(defun draw-circle (x1 y1 radius)
;  (arc x1 y1 radius 0 (* 2 pi))
;  (set-source-rgb 1 1 1)
;  (fill-path))
;
;(defun draw-arc (x1 y1 radius startangle endangle)
;  (arc x1 y1 radius (deg-to-rad startangle) (deg-to-rad endangle))
;  (set-source-rgb 1 1 1)
;  (fill-path))

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

;TODO print window
;(defun print-properties-window ()
;  (within-main-loop
;   (let* ((window (make-instance 'gtk:gtk-window
;                                 :type :toplevel
;                                 :window-position :center
;                                 :title "Print properties"
;                                 :default-width 800
;                                 :default-height 600))

;file properties
(defun file-properties-window ()
  (within-main-loop
   (let-ui
        (gtk-window
         :var w
         :type :toplevel
         :window-position :center
         :title "File properties"
         :default-width 450
         :default-height 256
         :border-width 5
         (v-box
          (table
           :n-rows 8
           :n-columns 2
           :homogeneous nil
           (label :label "Filename") :left 0 :right 1 :top 0 :bottom 1
           (entry :var file-entry) :left 1 :right 2 :top 0 :bottom 1
           (label :label "Subject") :left 0 :right 1 :top 1 :bottom 2
           (entry :var subject-entry) :left 1 :right 2 :top 1 :bottom 2
           (label :label "Author") :left 0 :right 1 :top 2 :bottom 3
           (entry :var author-entry) :left 1 :right 2 :top 2 :bottom 3
           (label :label "Keywords") :left 0 :right 1 :top 3 :bottom 4
           (entry :var keywords-entry) :left 1 :right 2 :top 3 :bottom 4
           (label :label "Comments") :left 0 :right 1 :top 4 :bottom 5
           (entry :var comments-entry) :left 1 :right 2 :top 4 :bottom 5
           (label :label "Hyperlink") :left 0 :right 1 :top 5 :bottom 6
           (entry :var hyperlink-entry) :left 1 :right 2 :top 5 :bottom 6
           (label :label "Created") :left 0 :right 1 :top 6 :bottom 7
           (entry :var created-entry) :left 1 :right 2 :top 6 :bottom 7
           (label :label "Modified") :left 0 :right 1 :top 7 :bottom 8
           (entry :var modified-entry) :left 1 :right 2 :top 7 :bottom 8)
          (h-box
           (button :label "gtk-ok" :use-stock t :var button-ok) :expand nil :pack-type :end
           (button :label "gtk-cancel" :use-stock t :var button-cancel) :expand nil :pack-type :end)))
     (gobject:g-signal-connect w "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
     (gobject:g-signal-connect w "delete-event" (lambda (widget event)
                                                (declare (ignore widget event))
                                                (let ((dlg (make-instance 'message-dialog
                                                                          :text "Are you sure?"
                                                                          :buttons :yes-no)))
                                                  (let ((response (dialog-run dlg)))
                                                    (object-destroy dlg)
                                                    (not (eq :yes response))))))
     (gobject:g-signal-connect button-cancel "clicked" (lambda (b) (declare (ignore b)) (object-destroy w)))
     (gobject:g-signal-connect button-ok "clicked" (lambda (b) (declare (ignore b)) (object-destroy w)))
     (widget-show w))))

;make-new-file window
(defun make-new-file-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Make new file" :type :toplevel :window-position :center :default-width 450 :default-height 260))
	  (v-box (make-instance 'v-box))
	  (h-box (make-instance 'h-box))
          (notebook (make-instance 'notebook :enable-popup t))
	  (button-new (make-instance 'button :label "Make new"))
	  (button-cancel (make-instance 'button :label "Cancel")))
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (gobject:g-signal-connect button-cancel "clicked" (lambda (b) (declare (ignore b)) (object-destroy window)))
      (box-pack-start h-box notebook)
      (box-pack-start h-box v-box :expand nil)
      (notebook-add-page notebook
       (make-instance 'v-box)
       (make-instance 'label :label "New documents"))
      (notebook-add-page notebook
       (make-instance 'v-box)
       (make-instance 'label :label "Templates"))
      (box-pack-start v-box button-new :expand nil)
      (box-pack-start v-box button-cancel :expand nil)
      (container-add window h-box)
      (widget-show window))))

;draw properties window
(defun draw-properties-window ()
  (within-main-loop
   (let ((window (make-instance 'gtk-window :title "Properties" :type :toplevel :window-position :center :default-width 600 :default-height 400))
	 (v-box (make-instance 'v-box))
	 (h-box (make-instance 'h-box))
	 (notebook (make-instance 'notebook :enable-popup t :tab-pos :left))
	 (button-new (make-instance 'button :label "Save"))
	 (button-cancel (make-instance 'button :label "Cancel")))
     (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
     (gobject:g-signal-connect button-cancel "clicked" (lambda (b) (declare (ignore b)) (object-destroy window)))
    ; (g-signal-connect button-save )
     (box-pack-start h-box notebook)
     (box-pack-start h-box v-box :expand nil)
     (notebook-add-page notebook
			(make-instance 'v-box)
			(make-instance 'label :label "System"))
     (notebook-add-page notebook
			(make-instance 'v-box)
			(make-instance 'label :label "New drawings"))
     (notebook-add-page notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Current screen"))
     (notebook-add-page notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Current draw"))
     (box-pack-start v-box button-new :expand nil)
     (box-pack-start v-box button-cancel :expand nil)
     (container-add window h-box)
     (widget-show window))))

;layers-window
(defstruct layer layer-name line-type color-line weight printable view)

(defun layers-window ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Layers"))
           (model (make-instance 'array-list-store))
           (scroll (make-instance 'scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
           (tv (make-instance 'tree-view :headers-visible t :width-request 100 :height-request 300 :rules-hint t))
           (h-box (make-instance 'h-box))
           (v-box (make-instance 'v-box))
           (layer-name-entry (make-instance 'entry))
           (line-type-entry (make-instance 'entry))
	   (color-line-entry (make-instance 'entry))
	   (weight-entry (make-instance 'entry))
	   (printable-entry (make-instance 'entry))
	   (view-entry (make-instance 'entry))
           (button (make-instance 'button :label "Add layer")))
      (store-add-column model "gchararray" #'layer-layer-name)
      (store-add-column model "gchararray" #'layer-line-type)
      (store-add-column model "gchararray" #'layer-color-line)
      (store-add-column model "gint" #'layer-weight)
      (store-add-column model "gchararray" #'layer-printable)
      (store-add-column model "gchararray" #'layer-view)
      (store-add-item model (make-layer :layer-name "0" :line-type "continious" :color-line "black" :weight 1 :printable "yes" :view "yes"))
      (setf (tree-view-model tv) model (tree-view-tooltip-column tv) 0)
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (gobject:g-signal-connect button "clicked" (lambda (b)
                                                   (declare (ignore b))
                                                   (store-add-item model (make-layer :layer-name (entry-text layer-name-entry)
										     :line-type (entry-text line-type-entry)
										     :color-line (entry-text color-line-entry)
										     :weight (or (parse-integer (entry-text weight-entry) 
                                                                                                             :junk-allowed t)
                                                                                              0)
										    :printable (or (entry-text printable-entry) "yes")
										    :view (or (entry-text view-entry) "yes")))))
      (gobject:g-signal-connect tv "row-activated" (lambda (tv path column)
						     (declare (ignore tv column))
						     (format t "You clicked on row ~A~%" (tree-path-indices path))))
      (container-add window v-box)
      (box-pack-start v-box h-box :expand nil)
      (box-pack-start h-box layer-name-entry :expand nil)
      (box-pack-start h-box line-type-entry :expand nil)
      (box-pack-start h-box color-line-entry :expand nil)
      (box-pack-start h-box weight-entry :expand nil)
      (box-pack-start h-box printable-entry :expand nil)
      (box-pack-start h-box view-entry :expand nil)
      (box-pack-start h-box button :expand nil)
      (box-pack-start v-box scroll)
      (container-add scroll tv)
      (let ((column (make-instance 'tree-view-column :title "Name" :sort-column-id 0))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 0)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Line type"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 1)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Color line"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 2)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Weight"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 3)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Printable"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 4)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "View"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 5)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (widget-show window))))

;entry
(defun entry-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel :title "Entry" :window-position :center))
	  (vbox (make-instance 'v-box));
	  (hbox (make-instance 'h-box))
	  (entry (make-instance 'entry))
	  (button (make-instance 'button :label "OK")))
      (box-pack-start vbox hbox :expand nil)
      (box-pack-start hbox entry :expand t)
      (box-pack-start hbox button :expand nil)
      (container-add window vbox)
      (gobject:g-signal-connect window "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
      (gobject:g-signal-connect button "clicked" (lambda (widget) (declare (ignore widget)) (object-destroy window)))
      (widget-show window))))


;main window
(defun main-window ()
  (within-main-loop
   (let ((w (make-instance 'gtk-window :title "CL-CAD" :type :toplevel :window-position :center :default-width 1024 :default-height 600))
	 (v-box (make-instance 'v-box))
	 (h-box (make-instance 'h-box))
	 (menu-hbox (make-instance 'h-box))
	 (menu-notebook (make-instance 'notebook :enable-popup t))
	 (draw-area (make-instance 'drawing-area))
	 ;terminal
	 (term-notebook (make-instance 'notebook :enable-popup t :tab-pos :left))
	 (term-vbox (make-instance 'v-box))
	 (term-hbox (make-instance 'h-box))
	 (tools-vbox (make-instance 'v-box))
	 (term-text-view (make-instance 'text-view))
	 (term-new (make-instance 'button :image (make-instance 'image :stock "gtk-new")))
	 (term-open (make-instance 'button :image (make-instance 'image :stock "gtk-open")))
	 (term-save (make-instance 'button :image (make-instance 'image :stock "gtk-save")))
	 (term-save-as (make-instance 'button :image (make-instance 'image :stock "gtk-save-as")))
	 (term-eval (make-instance 'button :image (make-instance 'image :stock "gtk-execute")))
         ;;;system
	 (button-save (make-instance 'button :image (make-instance 'image :stock "gtk-save")))
	 (button-save-as (make-instance 'button :image (make-instance 'image :stock "gtk-save-as")))
	 (button-new (make-instance 'button :image (make-instance 'image :stock "gtk-new")))
	 (button-open (make-instance 'button :image (make-instance 'image :stock "gtk-open")))
	 (button-about (make-instance 'button :image (make-instance 'image :stock "gtk-about")))
	 (button-print (make-instance 'button :image (make-instance 'image :stock "gtk-print")))
	 (button-print-prop (make-instance 'button :image (make-instance 'image :stock "gtk-page-setup")))
	 (button-system-properties (make-instance 'button :image (make-instance 'image :stock "gtk-properties")))
	 (button-layers (make-instance 'button :image (make-instance 'image :stock "gtk-justify-fill")))
	 (button-file-prop (make-instance 'button :image (make-instance 'image :stock "gtk-preferences")))
	 (button-color-selection (make-instance 'color-button :has-opacity-control t))
	 (button-select-font (make-instance 'font-button :font-name "Sans 10"))
	 (button-full (make-instance 'button :label "full"))
	 (button-unfull (make-instance 'button :label "unfull"))
         ;;;primitives
	 (primitives-expander (make-instance 'expander :expanded t :label "Primitives"))
	 (primitives-table (make-instance 'table :n-rows 5 :n-columns 4 :homogeneous nil))
	 (button-line (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/line.png" *src-location*)))))
	 (button-ray (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/ray.png" *src-location*)))))
	 (button-construction (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/construction.png" *src-location*)))))
	 (button-circle-radius (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circleradius.png" *src-location*)))))
	 (button-circle-2p (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circle2p.png" *src-location*)))))
	 (button-circle-3p (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circle3p.png" *src-location*)))))
	 (button-circle-diameter (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circlediameter.png" *src-location*)))))
	 (button-circle-ttr (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circlettr.png" *src-location*)))))
	 (button-circle-ttt (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/circlettt.png" *src-location*)))))
	 (button-arc (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/arc3p.png" *src-location*)))))
	 (button-ellipse-center (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/ellipsecenter.png" *src-location*)))))
	 (button-ellipse-axis (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/ellipseaxis.png" *src-location*)))))
	 (button-ellipse-arc (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/ellipsearc.png" *src-location*)))))
	 (button-pline (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/pline.png" *src-location*)))))
	 (button-polygon (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/polygon.png" *src-location*)))))
	 (button-point (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/point.png" *src-location*)))))
	 (button-rectangle (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/rectangle.png" *src-location*)))))
	 (button-spline (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/spline.png" *src-location*)))))
	 (button-text (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/objects/text.png" *src-location*)))))
         ;;;modify
	 (modify-expander (make-instance 'expander :expanded t :label "Modify"))
	 (modify-table (make-instance 'table :n-rows 3 :n-columns 4 :homogeneous nil))
	 (button-break (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_break.png" *src-location*)))))
	 (button-erase (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_erase.png" *src-location*)))))
	 (button-explode (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_explode.png" *src-location*)))))
	 (button-extend (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_extend.png" *src-location*)))))
	 (button-lengthen (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_lengthen.png" *src-location*)))))
	 (button-move (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_move.png" *src-location*)))))
	 (button-rotate (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_rotate.png" *src-location*)))))
	 (button-scale (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_scale.png" *src-location*)))))
	 (button-stretch (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_stretch.png" *src-location*)))))
	 (button-trim (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/modify/mod_trim.png" *src-location*)))))
         ;;;osnap
	 (osnap-expander (make-instance 'expander :expanded t :label "Osnap"))
	 (osnap-table (make-instance 'table :n-rows 3 :n-columns 4 :homogeneous nil))
	 (button-oscenter (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_cen.png" *src-location*)))))
	 (button-osend (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_end.png" *src-location*)))))
	 (button-osins (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_ins.png" *src-location*)))))
	 (button-osint (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_int.png" *src-location*)))))
	 (button-osmid (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_mid.png" *src-location*)))))
	 (button-osnea (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_nea.png" *src-location*)))))
	 (button-osnod (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_nod.png" *src-location*)))))
	 (button-osnone (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_none.png" *src-location*)))))
	 (button-osper (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_per.png" *src-location*)))))
	 (button-osqua (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_qua.png" *src-location*)))))
	 (button-ostan (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_tan.png" *src-location*)))))
	 (button-ostrack (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/osnap/osnap_track.png" *src-location*)))))
         ;;;dimension
	 (dimension-expander (make-instance 'expander :expanded t :label "Dimension"))
	 (dimension-table (make-instance 'table :n-rows 3 :n-columns 4 :homogeneous nil))
	 (button-align (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimalign.png" *src-location*)))))
	 (button-angular (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimangular.png" *src-location*)))))
	 (button-baseline (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimbaseline.png" *src-location*)))))
	 (button-center (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimcenter.png" *src-location*)))))
	 (button-continue (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimcontinue.png" *src-location*)))))
	 (button-diameter (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimdiameter.png" *src-location*)))))
	 (button-horiz (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimhoriz.png" *src-location*)))))
	 (button-leader (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimleader.png" *src-location*)))))
	 (button-vert (make-instance 'button :image (make-instance 'image :file (namestring (merge-pathnames "graphics/dimension/dimvert.png" *src-location*)))))
	 ;;;zoom
	 (zoom-expander (make-instance 'expander :expanded t :label "Zoom"))
	 (zoom-table (make-instance 'table :n-rows 2 :n-columns 4 :homogeneous nil))
	 (button-zoom-all (make-instance 'button :image (make-instance 'image :stock "gtk-zoom-100")))
	 (button-zoom-pan (make-instance 'button :image (make-instance 'image :stock "gtk-index")))
	 (button-zoom-previous (make-instance 'button :image (make-instance 'image :stock "gtk-go-back")))
	 (button-zoom-redraw (make-instance 'button :image (make-instance 'image :stock "gtk-refresh")))
	 (button-zoom-scale (make-instance 'button :image (make-instance 'image :stock "gtk-zoom-fit")))
	 (button-zoom-window (make-instance 'button :image (make-instance 'image :stock "gtk-fullscreen")))
	 (button-zoom-plus (make-instance 'button :image (make-instance 'image :stock "gtk-zoom-in")))
	 (button-zoom-minus (make-instance 'button :image (make-instance 'image :stock "gtk-zoom-out"))))

     ;;;pack
     (container-add w v-box)
     (box-pack-start v-box menu-hbox :expand nil)
     (box-pack-start v-box h-box)
     (box-pack-start h-box menu-notebook :expand nil)
     (box-pack-start h-box draw-area :expand t)
     (box-pack-start v-box term-notebook)
     (box-pack-start term-vbox term-hbox :expand nil)
     (box-pack-start term-vbox term-text-view :expand t)
     (box-pack-start term-hbox term-new :expand nil)
     (box-pack-start term-hbox term-open :expand nil)
     (box-pack-start term-hbox term-save :expand nil)
     (box-pack-start term-hbox term-save-as :expand nil)
     (box-pack-start term-hbox term-eval :expand nil)
     (notebook-add-page menu-notebook
			tools-vbox
			(make-instance 'label :label "Tools"))
     (notebook-add-page menu-notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Files"))
     (notebook-add-page menu-notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Statistic"))
     (notebook-add-page term-notebook 
			term-vbox
			(make-instance 'label :label "Terminal"))
     (notebook-add-page term-notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Palettes"))
     (notebook-add-page term-notebook
			(make-instance 'v-box)
			(make-instance 'label :label "Layers"))
     ;system 
     (box-pack-start menu-hbox button-save :expand nil)
     (box-pack-start menu-hbox button-save-as :expand nil)
     (box-pack-start menu-hbox button-new :expand nil)
     (box-pack-start menu-hbox button-open :expand nil)
     (box-pack-start menu-hbox button-about :expand nil)
     (box-pack-start menu-hbox button-print :expand nil)
     (box-pack-start menu-hbox button-print-prop :expand nil)
     (box-pack-start menu-hbox button-file-prop :expand nil)
     (box-pack-start menu-hbox button-layers :expand nil)
     (box-pack-start menu-hbox button-system-properties :expand nil)
     (box-pack-start menu-hbox button-color-selection :expand nil)
     (box-pack-start menu-hbox button-select-font :expand nil)
     (box-pack-start menu-hbox button-full :expand nil)
     (box-pack-start menu-hbox button-unfull :expand nil)
     ;primitives
     (box-pack-start tools-vbox primitives-expander :expand nil)
     (container-add primitives-expander primitives-table)
     (table-attach primitives-table button-line 0 1 0 1)
     (table-attach primitives-table button-ray 1 2 0 1)
     (table-attach primitives-table button-construction 2 3 0 1)
     (table-attach primitives-table button-circle-radius 3 4 0 1)
     (table-attach primitives-table button-circle-2p 0 1 1 2)
     (table-attach primitives-table button-circle-3p 1 2 1 2)
     (table-attach primitives-table button-circle-diameter 2 3 1 2)
     (table-attach primitives-table button-circle-ttr 3 4 1 2)
     (table-attach primitives-table button-circle-ttt 0 1 2 3)
     (table-attach primitives-table button-arc 1 2 2 3)
     (table-attach primitives-table button-ellipse-center 2 3 2 3)
     (table-attach primitives-table button-ellipse-axis 3 4 2 3)
     (table-attach primitives-table button-ellipse-arc 0 1 3 4)
     (table-attach primitives-table button-pline 1 2 3 4)
     (table-attach primitives-table button-polygon 2 3 3 4)
     (table-attach primitives-table button-point 3 4 3 4)
     (table-attach primitives-table button-rectangle 0 1 4 5)
     (table-attach primitives-table button-spline 1 2 4 5)
     (table-attach primitives-table button-text 2 3 4 5)
     ;modify
     (box-pack-start tools-vbox modify-expander :expand nil)
     (container-add modify-expander modify-table)
     (table-attach modify-table button-break 0 1 0 1)
     (table-attach modify-table button-erase 1 2 0 1)
     (table-attach modify-table button-explode 2 3 0 1)
     (table-attach modify-table button-extend 3 4 0 1)
     (table-attach modify-table button-lengthen 0 1 1 2)
     (table-attach modify-table button-move 1 2 1 2)
     (table-attach modify-table button-rotate 2 3 1 2)
     (table-attach modify-table button-scale 3 4 1 2)
     (table-attach modify-table button-stretch 0 1 2 3)
     (table-attach modify-table button-trim 1 2 2 3)
     ;osnap
     (box-pack-start tools-vbox osnap-expander :expand nil)
     (container-add osnap-expander osnap-table)
     (table-attach osnap-table button-oscenter 0 1 0 1)
     (table-attach osnap-table button-osend 1 2 0 1)
     (table-attach osnap-table button-osins 2 3 0 1)
     (table-attach osnap-table button-osint 3 4 0 1)
     (table-attach osnap-table button-osmid 0 1 1 2)
     (table-attach osnap-table button-osnea 1 2 1 2)
     (table-attach osnap-table button-osnod 2 3 1 2)
     (table-attach osnap-table button-osnone 3 4 1 2)
     (table-attach osnap-table button-osper 0 1 2 3)
     (table-attach osnap-table button-osqua 1 2 2 3)
     (table-attach osnap-table button-ostan 2 3 2 3)
     (table-attach osnap-table button-ostrack 3 4 2 3)
     ;dimension
     (box-pack-start tools-vbox dimension-expander :expand nil)
     (container-add dimension-expander dimension-table)
     (table-attach dimension-table button-align 0 1 0 1)
     (table-attach dimension-table button-angular 1 2 0 1)
     (table-attach dimension-table button-baseline 2 3 0 1)
     (table-attach dimension-table button-center 3 4 0 1)
     (table-attach dimension-table button-continue 0 1 1 2)
     (table-attach dimension-table button-diameter 1 2 1 2)
     (table-attach dimension-table button-horiz 2 3 1 2)
     (table-attach dimension-table button-leader 3 4 1 2)
     (table-attach dimension-table button-vert 0 1 2 3)
     ;zoom     
     (box-pack-start tools-vbox zoom-expander :expand nil)
     (container-add zoom-expander zoom-table)
     (table-attach zoom-table button-zoom-all 0 1 0 1)
     (table-attach zoom-table button-zoom-pan 1 2 0 1)
     (table-attach zoom-table button-zoom-previous 2 3 0 1)
     (table-attach zoom-table button-zoom-redraw 3 4 0 1)
     (table-attach zoom-table button-zoom-scale 0 1 1 2)
     (table-attach zoom-table button-zoom-window 1 2 1 2)
     (table-attach zoom-table button-zoom-plus 2 3 1 2)
     (table-attach zoom-table button-zoom-minus 3 4 1 2)

     ;;;g-signals
     (gobject:g-signal-connect w "destroy" (lambda (b) (declare (ignore b)) (leave-gtk-main)))
     (gobject:g-signal-connect w "delete-event" (lambda (widget event)
					  (declare (ignore widget event))
					  (let ((dlg (make-instance 'message-dialog
								    :text "Are you sure?"
								    :buttons :yes-no)))
					    (let ((response (dialog-run dlg)))
					      (object-destroy dlg)
					      (not (eq :yes response))))))
;     (gobject:g-signal-connect w "configure-event" (lambda (widget event)
;						     (declare (ignore event))
;						     (widget-queue-draw widget)))
 ;    (gobject:g-signal-connect w "expose-event" (lambda (widget event)
;						  (declare (ignore event))
;						  (cc-expose widget)))
;     (g-signal-connect button-save "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-save-as "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-new "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-open "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-about "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-print "clicked" (lambda (w) (declare (ignore w)) ()))
 ;    (gobject:g-signal-connect button-print-prop "clicked" (lambda (w) (declare (ignore w)) ()))
     (gobject:g-signal-connect button-file-prop "clicked" (lambda (w) (declare (ignore w)) (file-properties-window)))
     (gobject:g-signal-connect button-color-selection "color-changed" (lambda (s) (declare (ignore s)) (unless (color-selection-adjusting-p button-color-selection) (format t "color: ~A~%" (color-selection-current-color button-color-selection)))))
     (gobject:g-signal-connect button-select-font "font-set" (lambda (b) (declare (ignore b)) (format t "Chose font ~A~%" (font-button-font-name button-select-font))))
     (gobject:g-signal-connect button-system-properties "clicked" (lambda (w) (declare (ignore w)) (draw-properties-window)))
     (gobject:g-signal-connect button-layers "clicked" (lambda (w) (declare (ignore w)) (layers-window)))
     (gobject:g-signal-connect button-full "clicked" (lambda (b) (declare (ignore b)) (gtk-window-fullscreen w)))
     (gobject:g-signal-connect button-unfull "clicked" (lambda (b) (declare (ignore b)) (gtk-window-unfullscreen w)))
;     (gobject:g-signal-connect button-line "clicked" (lambda (widget) (declare (ignore widget))(with-2d-frame (f draw-area) draw-line)))
;     (g-signal-connect button-ray "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-construction "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-radius "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-2p "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-3p "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-diameter "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-ttr "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-circle-ttt "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-arc "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-ellipse-center "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-ellipse-axis "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-ellipse-arc "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-pline "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-polygon "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-point "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-rectangle "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-spline "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-text "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-break "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-erase "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-explode "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-extend "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-lengthen "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-move "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-rotate "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-scale "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-stretch "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-trim "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-oscenter "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osend "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osins "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osint "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osmid "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osnea "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osnod "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osnone "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osper "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-osqua "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-ostan "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-ostrack "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-align "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-angular "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-baseline "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-center "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-continue "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-diameter "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-horiz "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-leader "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-vert "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-all "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-pan "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-previous "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-redraw "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-scale "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-window "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-plus "clicked" (lambda (w) (declare (ignore w)) ()))
;     (g-signal-connect button-zoom-minus "clicked" (lambda (w) (declare (ignore w)) ()))
     
     (widget-show w))))

