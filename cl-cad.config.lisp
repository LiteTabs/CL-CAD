(in-package :cl-cad)

(defstruct config 
  background-color
  default-format
  default-units
  default-space
  author
  default-line-width
  default-font
  osnap-area-delta
  )

(defvar *config* nil)

(defun config-path ()
  (pathname (build-filename
	     (get-user-config-dir)
	     "cl-cad.conf")))

(defun load-config ()
  (with-open-stream (stream (open (config-path)
				  :direction :input
				  :if-does-not-exist nil))
    (setf *config*
	  (if stream
	      (read stream)
	      (make-config)))))

(defun save-config ()
  (with-open-stream (stream (open (config-path)
				  :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create))
    (format stream "~S" *config*)))

