;;;; falcon.lisp

(in-package #:falcon)

(defparameter *master-list*
  (loop :for i :from 0 :to 1000
        :collecting (format nil "DUMMY ~a" i)))

(defparameter *flags* nil)
(defparameter *buttons-across* 10)


(defun notify (message title)
   (message-box message title "ok" "info"))

(defun populate-from-directory (&optional (program-prefix-string ""))
  (let* ((d (choose-directory :title "Choose a directory to populate files from"))
	 (files (directory (make-pathname :directory 
					   `(:absolute ,d)
					  :name :wild :type :wild))))
    (setf *master-list*
	  (loop :for i :in files :collecting
	     (list (file-namestring i) (format nil "~a ~a" program-prefix-string i))))))

(defun start-gui ()
  (with-ltk ()
    (wm-title *tk* "Falcon Button Tray")
    (set-geometry *tk* 640 480 0 0)


    (populate-from-directory)
    (let* ((menu (make-instance 'menubar :master *tk*))
	   (file-menu (make-instance 'menu :master menu :name "file" :text "File"))
	   (file-menu-open-directory
	    (make-instance 'menubutton
			   :master file-menu
			   :text "Open Directory"))
	   (file-menu-quit
	    (make-instance 'menubutton
			   :master file-menu
			   :text "Quit"))
	   (about-menu (make-instance 'menu :master menu :name "about" :text "Aboot"))
	   (nb (make-instance 'notebook :master *tk* :padding 8))
	   (pw (make-instance 'scrolled-frame :master nb :name "scrolled_frame"))
	   (pw2 (make-instance 'scrolled-frame :master nb :name "scrolled_frame2"))
	   (row 0)
	   (column 0))

      (pack nb :fill "both" :expand t)
      (notebook-enable-traversal nb)
      (notebook-add nb pw :text "Output")
      (notebook-add nb pw2 :text "Favorites")
      
      (loop :for (name command) :in *master-list* :do
	   (let ((name name)
		 (command command))
	     (grid (make-instance 'button
				:master (interior pw)
				:text name
				:command (lambda() (asdf::run-program command)))
				row column :sticky "EW" :padx 1 :pady 1))
	   (cond ((>= column (1- *buttons-across*))
		  (setf column 0)
		  (incf row))
		 (t
		  (incf column)))))))

(defun read-piped-file ()
 (let* ((strings
	 (loop :for str = (read *standard-input* nil :eof)
	    :until (eq str :eof)
	    :collect str))
	(descs
	 (loop :for (button-name command-string) :on strings :by #'cddr
	    :collect (list button-name command-string))))
   (setf *master-list* descs)))

(defun main ()
; (format *standard-output* "Recieved Arguments: ~a" sb-ext:*posix-argv*)
 ;; This doesn't QUITE work because it'll read stdin all the time.
 ;; you prolly need a command line argument like '--' or something to indicate
 ;; it is ok to read stdin--otherwise it would be ignored.

; (setf *flags* (apply-argv:parse-argv sb-ext:*posix-argv*))
 (setf *master-list*
       (loop :for (a b) :on *master-list* :by #'cddr collect (list a b)))
; (format t "~a" *flags*)
;
 
 (start-gui))


