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
  (merge-pathnames #p".cl-cad/cl-cad.conf"
		   (user-homedir-pathname))
  (ensure-directories-exist (merge-pathnames #p".cl-cad/cl-cad.conf"
                                             (user-homedir-pathname))))

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

