(in-package :cl-cad)

(defun print-properties ()
  (let ((print-dlg (make-instance 'gtk:page-setup-unix-dialog)))
    (gtk:dialog-run print-dlg)
    (gtk:object-destroy print-dlg)))

(defun print-dialog ()
  (let ((print-dlg (make-instance 'gtk:print-unix-dialog
				  :title "Print")))
    (gtk:dialog-run print-dlg)
    (gtk:object-destroy print-dlg)))