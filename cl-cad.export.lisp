(in-package :cl-cad)

(defun cb-save-export ( w h)
  (let ((dlg (make-instance 'gtk:file-chooser-dialog
                            :action :save
                            :title "Export file"
                            :window-position :center-on-parent)))
                           ; :transient-for (app-main-window app))))
    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
    (setf (gtk:dialog-default-response dlg) :ok)
    (when (eq (std-dialog-run dlg) :ok)
      (export-to-png (gtk:file-chooser-filename dlg) w h))))

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