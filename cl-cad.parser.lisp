(in-package :cl-cad)

(defgeneric draw-object (type object))

(defmethod draw-object ((type (eql :line)) line)
  (move-to (getf cd :x1) (getf cd :y1))
  (line-to (getf cd :x2) (getf cd :y2))
  (set-source-rgb 0 0 1)
  (set-line-width (getf cd :weight))
  (stroke))

(defmethod draw-object ((type (eql :circle)) circle)
  (arc (getf cd :x1) (getf cd :y1) (getf cd :radius) 0 (* 2 pi))
  (set-source-rgb 0 0 1)
  (set-line-width (getf cd :weight))
  (stroke))

(defmethod draw-object ((type (eql :arc)) arc)
  (arc (getf cd :x1) (getf cd :y1) (getf cd :radius) 0 (* 2 pi))
  (set-source-rgb 0 0 1)
  (set-line-width (getf cd :weight))
  (stroke))

(defmethod draw-object ((type (eql :continious)) continious)
  )

(defmethod draw-object ((type (eql :ray)) ray)
  )

(defmethod draw-object ((type (eql :text)) text)
  )

(defmethod draw-object ((type (eql :mtext)) mtext)
  )

(defmethod draw-object ((type (eql :block)) block)
  )

(defmethod draw-object ((type (eql :point)) boint)
  (move-to (getf cd :x1) (getf cd :y1))
  (line-to (getf cd :x1) (getf cd :y1))
  (set-source-rgb 0 0 1)
  (set-line-width 1)
  (stroke))

(defmethod draw-object ((type (eql :ellipse)) ellipse)
  )

(defmethod draw-object ((type (eql :raster-image)) raster-image)
  )

(defun draw-objects (object)
  (draw-object (getf object :title) object))

(defun screen-draw ()
  (dolist (cd *current-draw*)
    (draw-objects cd)))