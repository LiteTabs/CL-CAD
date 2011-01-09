(in-package :cl-cad)

(defstruct export-type type extention)
(defvar *export-type* :png)

(defun cb-export (parent-window)
  (let* ((model (make-instance 'array-list-store))
	 (combo (make-instance 'combo-box :model model))
	 (dlg (make-instance 'gtk:file-chooser-dialog
			     :action :save
			     :title "Export file"
			     :extra-widget combo
			     :window-position :center-on-parent
			     :transient-for (app-main-window parent-window))))
    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
    (setf (gtk:dialog-default-response dlg) :ok)
    (store-add-column model "gchararray" #'export-type-type)
    (store-add-column model "gchararray" #'export-type-extention)
    (store-add-item model (make-export-type :type "Portable network graphic" :extention ".png"))
    (store-add-item model (make-export-type :type "Scalable vector graphic" :extention ".svg"))
    (store-add-item model (make-export-type :type "PostScriptc" :extention ".ps"))
    (store-add-item model (make-export-type :type "Portable document format"  :extention ".pdf"))
    (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
      (cell-layout-pack-start combo renderer :expand t)
      (cell-layout-add-attribute combo renderer "text" 0))
    (let ((renderer (make-instance 'cell-renderer-text :text "A number")))
      (cell-layout-pack-start combo renderer :expand nil)
      (cell-layout-add-attribute combo renderer "text" 1))
    (gobject:g-signal-connect combo "changed" (lambda (c)
						(declare (ignore c))
						(export-type-select combo)))
    (when (std-dialog-run dlg)
      (export-action (gtk:file-chooser-filename dlg) 1024 768))))
	  
(defun export-type-select (combo)
  (cond 
    ((equal (combo-box-active combo) 0) (setf *export-type* :png))
    ((equal (combo-box-active combo) 1) (setf *export-type* :svg))
    ((equal (combo-box-active combo) 2) (setf *export-type* :ps))
    ((equal (combo-box-active combo) 3) (setf *export-type* :pdf))))

(defun export-action (filename w h)
  (cond
    ((equal *export-type* :png) (export-to-png filename w h))
    ((equal *export-type* :svg) (export-to-svg filename w h))
    ((equal *export-type* :ps) (export-to-ps filename w h))
    ((equal *export-type* :pdf) (export-to-pdf filename w h))))

(defun export-to-png (name-file w h)
  (with-png-file (name-file :rgb24 w h)
    (parser w h)))

(defparameter *surface-pdf* nil)

(defun export-to-pdf (name-file w h)
  (setf *surface-pdf* (create-pdf-surface name-file w h))
  (setf *context* (create-context *surface-pdf*))
  (destroy *surface-pdf*)
  (parser w h)
  (destroy *context*))

(defun export-to-ps (name-file w h)
  (setf *context* (create-ps-context name-file w h))
  (parser w h)
  (destroy *context*))

(defun export-to-svg (name-file w h)
  (setf *context* (create-svg-context name-file w h))
  (parser w h)
  (destroy *context*))