(in-package :cl-cad)

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

(defun file-properties-window (parent-window)
  (within-main-loop
   (let-ui
        (gtk-window
         :var w
         :type :toplevel
         :window-position :center
         :title "File properties"
         :default-width 400
         :default-height 250
         :border-width 5
	 :destroy-with-parent t
	 :transient-for parent-window
         (v-box
          (table
           :n-rows 8
           :n-columns 2
           :homogeneous nil
           (label :label "Filename") :left 0 :right 1 :top 0 :bottom 1
           (entry :var file-entry :text (getf (car (select  (where :title :file-properties))) :file-name)) :left 1 :right 2 :top 0 :bottom 1
           (label :label "Subject") :left 0 :right 1 :top 1 :bottom 2
           (entry :var subject-entry :text (getf (car (select  (where :title :file-properties))) :subject)) :left 1 :right 2 :top 1 :bottom 2
           (label :label "Author") :left 0 :right 1 :top 2 :bottom 3
           (entry :var author-entry :text (getf (car (select  (where :title :file-properties))) :author)) :left 1 :right 2 :top 2 :bottom 3
           (label :label "Keywords") :left 0 :right 1 :top 3 :bottom 4
           (entry :var keywords-entry :text (getf (car (select  (where :title :file-properties))) :keywords)) :left 1 :right 2 :top 3 :bottom 4
           (label :label "Comments") :left 0 :right 1 :top 4 :bottom 5
           (entry :var comments-entry :text (getf (car (select  (where :title :file-properties))) :comments)) :left 1 :right 2 :top 4 :bottom 5
           (label :label "Hyperlink") :left 0 :right 1 :top 5 :bottom 6
           (entry :var hyperlink-entry :text (getf (car (select  (where :title :file-properties))) :hyperlink)) :left 1 :right 2 :top 5 :bottom 6
	   ;created
	   ;modified
	   )
          (h-box
           (button :label "gtk-ok" :use-stock t :var button-ok) :expand nil :pack-type :end) :expand nil))
     (gobject:g-signal-connect w "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
     (gobject:g-signal-connect button-ok "clicked" (lambda (b)
						     (declare (ignore b))
						     (if (equal (select (where :title :file-properties))
								nil)
							 (push (add-file-properties 
								:file-name (or (entry-text file-entry) "")
								:subject (or (entry-text subject-entry) "")
								:author (or (entry-text author-entry) "")
								:keywords (or (entry-text keywords-entry) "")
								:comments (or (entry-text comments-entry) "")
								:hyperlink (or (entry-text hyperlink-entry) "")
								:created (get-clear-time)
								:modified nil) *current-draw*)
							 (update-properties (where :title :file-properties)
									    :file-name (or (entry-text file-entry) "")
									    :subject (or (entry-text subject-entry) "")
									    :author (or (entry-text author-entry) "")
									    :keywords (or (entry-text keywords-entry) "")
									    :comments (or (entry-text comments-entry) "")
									    :hyperlink (or (entry-text hyperlink-entry) "")
									    :modified (get-clear-time)))
						     (object-destroy w)))
     (widget-show w))))
