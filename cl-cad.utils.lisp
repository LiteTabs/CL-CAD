(in-package :cl-cad)

(defun ask (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :buttons :yes-no
			    :title "CL-CAD"
			    :message-type :question
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))
    (prog1
        (eql (gtk:dialog-run dlg) :yes)
      (gtk:object-destroy dlg))))

(defun ask-save (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :title "CL-CAD"
			    :message-type :warning
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))

    (gtk:dialog-add-button dlg "gtk-discard" :reject)
    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :reject :cancel))

    (prog1
        (gtk:dialog-run dlg)
      (gtk:object-destroy dlg))))

(defun say-error (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :buttons :ok
			    :title "CL-CAD"
			    :message-type :error
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:object-destroy dlg)))

(defun say-warning (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :buttons :ok
			    :title "CL-CAD"
			    :message-type :warning
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:object-destroy dlg)))

(defun make-std-dialog (parent-window title stock-icon content)
  (let ((dlg (make-instance 'gtk:dialog
			    :border-width 8
			    :modal t
			    :resizable t
			    :window-position :center-on-parent
			    :title title
			    :has-separator nil
			    :type-hint :dialog
			    :skip-taskbar_hint t
			    :skip-pager-hint t
			    :gravity :center
			    :transient-for parent-window)))

    (setf (gtk:gtk-window-icon dlg)
	  (gtk:widget-render-icon dlg stock-icon :dialog ""))

    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-ok" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))

    (setf (gtk:dialog-default-response dlg) :ok)

    (gtk:container-add (gtk:dialog-content-area dlg) content)

    dlg))

(defun std-dialog-run (dlg)
  (gtk:widget-show dlg :all t)
  (prog1
      (eql (gtk:dialog-run dlg) :ok)
    (gtk:widget-hide dlg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-text-file (file-name)
  (with-output-to-string (str)
    (with-open-file (file file-name)
      (loop
         for line = (read-line file nil nil)
         while line
         do (fresh-line str)
         do (write-string line str)))))

;coming-soon-window
(defun coming-soon-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window 
				 :title "Oops" 
				 :type :toplevel 
				 :window-position :center 
				 :default-width 170 
				 :default-height 40 ))
	  (label (make-instance 'label :label "Coming Soon =)")))
      (gobject:g-signal-connect window "destroy" (lambda (widget) 
						   (declare (ignore widget)) (leave-gtk-main)))
      (container-add window label)
      (widget-show window))))

(defun get-clear-time ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'od:~2,'0d, ~a, ~d/~2,'0d/~d (GMT~@d)"
	    hour
	    minute
	    second
	    (nth day-of-week *week-day-names*)
	    date
	    month
	    year
	    (- tz))))

;file properties
(defun file-properties-window (parent-window)
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
	 :destroy-with-parent t
	 :transient-for parent-window
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
           (entry :var hyperlink-entry) :left 1 :right 2 :top 5 :bottom 6)
          (h-box
           (button :label "gtk-ok" :use-stock t :var button-ok) :expand nil :pack-type :end
           (button :label "gtk-cancel" :use-stock t :var button-cancel) :expand nil :pack-type :end) :expand nil))
     (gobject:g-signal-connect w "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
     (gobject:g-signal-connect button-cancel "clicked" (lambda (b) (declare (ignore b)) (object-destroy w)))
     (gobject:g-signal-connect button-ok "clicked" (lambda (b)
						     (declare (ignore b))
						     (push (make-file-properties :file-name (or (entry-text file-entry) nil)
										 :subject (or (entry-text subject-entry) nil)
										 :author (or (entry-text author-entry) nil)
										 :keywords (or (entry-text keywords-entry) nil)
										 :comments (or (entry-text comments-entry) nil)
										 :hyperlink (or (entry-text hyperlink-entry) nil)
										 :created (get-clear-time)
										 :modified nil) *current-draw*)
						     (object-destroy w)))
     (widget-show w))))

;make-new-file window
(defun make-new-file-window (parent-window)
  (within-main-loop
    (let* ((window (make-instance 'gtk-window 
				  :title "Make new file" 
				  :type :toplevel 
				  :window-position :center 
				  :default-width 450 
				  :default-height 260 
				  :destroy-with-parent t
				  :transient-for parent-window))
	   (v-box (make-instance 'v-box))
	   (h-box (make-instance 'h-box))
	   (notebook (make-instance 'notebook :enable-popup t))
	   (new-vbox (make-instance 'v-box))
	   (detal-button (make-instance 'radio-button :label "Detal"))
	   (collect-button (make-instance 'radio-button :label "Collect" :group detal-button))
	   (specification-button (make-instance 'radio-button :label "Specification" :group detal-button))
	   (button-new (make-instance 'button :label "Make new"))
	   (button-cancel (make-instance 'button :label "Cancel")))
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (gobject:g-signal-connect button-cancel "clicked" (lambda (b) (declare (ignore b)) (object-destroy window)))
      (box-pack-start v-box notebook)
      (box-pack-start v-box h-box :expand nil)
      (notebook-add-page notebook
       new-vbox
       (make-instance 'label :label "New documents"))
      (notebook-add-page notebook
       (make-instance 'v-box)
       (make-instance 'label :label "Templates"))
      (box-pack-start new-vbox detal-button)
      (box-pack-start new-vbox collect-button)
      (box-pack-start new-vbox specification-button)
      (box-pack-start h-box button-new :expand nil)
      (box-pack-start h-box button-cancel :expand nil)
      (container-add window v-box)
      (widget-show window))))



;entry
(defun entry-window (parent-window)
  (within-main-loop
    (let ((window (make-instance 'gtk-window 
				 :type :toplevel 
				 :title "Entry" 
				 :window-position :center 
				 :destroy-with-parent t
				 :transient-for parent-window))
	  (vbox (make-instance 'v-box))
	  (hbox (make-instance 'h-box))
	  (entry (make-instance 'entry))
	  (button (make-instance 'button :label "OK")))
      (box-pack-start vbox hbox :expand nil)
      (box-pack-start hbox entry :expand t)
      (box-pack-start hbox button :expand nil)
      (container-add window vbox)
      (gobject:g-signal-connect window "destroy" 
				(lambda (widget) 
				  (declare (ignore widget)) (leave-gtk-main)))
      (gobject:g-signal-connect button "clicked" 
				(lambda (widget) 
				  (declare (ignore widget)) (object-destroy window)))
      (widget-show window))))

