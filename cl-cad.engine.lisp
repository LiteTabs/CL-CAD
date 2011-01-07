(in-package :cl-cad)

(defvar *current-draw* nil)

(defun add-file-properties (file-name subject author keywords comments hyperlink created modified)
  (push (list :title :file-properties 
	      :file-name file-name 
	      :subject subject 
	      :author author 
	      :keywords keywords 
	      :comments comments 
	      :hyperlink hyperlink 
	      :created created 
	      :modified modified) *current-draw*))

(defun add-layer (layer-name line-type color-line width printable view)
  (push (list :title :layer 
	      :layer-name layer-name 
	      :line-type line-type 
	      :color-line color-line 
	      :width width 
	      :printable printable 
	      :view view) *current-draw*))

(defun add-line (layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line width)
  (push (list :title :line 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :x2 x2
	      :y2 y2
	      :z2 z2
	      :line-type line-type
	      :zoom-line zoom-line 
	      :color-line color-line
	      :width width) *current-draw*))

(defun add-circle (layer x1 y1 z1 radius line-type zoom-line color-line width)
  (push (list :title :circle 
	      :layer layer
	      :x1 x1
	      :y1 y1 
	      :z1 z1
	      :radius radius
	      :line-type line-type
	      :zoom-line zoom-line
	      :color-line color-line
	      :width width) *current-draw*))

(defun add-arc (layer x1 y1 z1 radius startangle endangle line-type zoom-line color-line width)
  (push (list :title :arc
	      :layer layer
	      :x1 x1
	      :y1 y1
	      :z1 z1
	      :radius radius
	      :startangle startangle 
	      :endangle endangle 
	      :line-type line-type
	      :zoom-line zoom-line
	      :color-line color-line
	      :width width) *current-draw*))

(defun add-rectangle (layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line width)
  (push (list :title :rectangle 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :x2 x2 
	      :y2 y2 
	      :z2 z2 
	      :line-type line-type 
	      :zoom-line zoom-line 
	      :color-line color-line 
	      :width width) *current-draw*))

(defun add-continious (layer x1 y1 z1 x2 y2 z2)
  (push (list :title :continious 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :x2 x2 
	      :y2 y2 
	      :z2 z2) *current-draw*))

(defun add-ray (layer x1 y1 z1 x2 y2 z2)
  (push (list :title :ray 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :x2 x2 
	      :y2 y2 
	      :z2 z2) *current-draw*))

(defun add-text (layer x1 y1 z1 count style angle color-line)
  (push (list :title :text 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :count count 
	      :style style 
	      :angle angle 
	      :color-line color-line) *current-draw*))

;(defun add-mtext (layer x1 y1 z1 count style annotate alignment height angle interval zoom-line color-line width)
;  (push (list :title :mtext :layer layer :x1 x1 :y1 y1 :z1 z1 :count count :style style :annotate annotate :alignment alignment :height height :angle angle :interval interval :zoom-line zoom-line :color-line color-line :width width) *current-draw*))

(defun add-block (layer name-block x1 y1 z1 xscale yscale zscale count rotation)
  (push (list :title :block 
	      :layer layer 
	      :name-block name-block 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :xscale xscale 
	      :yscale yscale 
	      :zscale zscale 
	      :count count 
	      :rotation rotation) *current-draw*))

(defun add-point (layer x1 y1 z1 style)
  (push (list :title :point 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :style style) *current-draw*))

(defun add-ellipse (layer x1 y1 z1 major-radius minor-radius angle start-angle end-angle color-line width)
  (push (list :title :ellipse 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :major-radius major-radius 
	      :minor-radius minor-radius 
	      :angle angle 
	      :start-angle start-angle 
	      :end-angle end-angle 
	      :color-line color-line 
	      :width width) *current-draw*))

(defun add-raster-image (layer x1 y1 z1 rotation-angle width height scale brightness contrast fade path show-image show-clipped transparency)
  (push (list :title :raster-image 
	      :layer layer 
	      :x1 x1 
	      :y1 y1 
	      :z1 z1 
	      :rotation-angle rotation-angle 
	      :width width 
	      :height height 
	      :scale scale 
	      :brightness brightness 
	      :contrast contrast 
	      :fade fade 
	      :path path 
	      :show-image show-image 
	      :show-clipped show-clipped 
	      :transparency transparency) *current-draw*))

(defun add-hatch (layer x1 y1 z1 type)
  (push (list :title :hatch
	      :layer layer
	      :x1 x1 
	      :y1 y1 
	      :z1 z1
	      :type type) *current-draw*))

(defun save-data (filename)
	(with-open-file (out filename 
			     :direction 
			     :output 
			     :if-exists 
			     :supersede)
	(with-standard-io-syntax
		(print *current-draw* out))))

(defun open-file (filename)
	(with-open-file (in filename)
	(with-standard-io-syntax
		(setf *current-draw* (read in)))))
	
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

(defun update (selector-fn &key title layer x1 y1 z1 x2 y2 z2 line-type zoom-line color-line width style)
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
                      (if width (setf (getf row :width) width))
                      (if style (setf (getf row :style) style)))
                row) *current-draw*)))

(defun update-properties (selector-fn &key file-name subject author keywords comments hyperlink created modified)
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
                      (if modified (setf (getf row :modified) modified)))
                row) *current-draw*)))
;sample
;(update (where :title :title) :x1 1)
