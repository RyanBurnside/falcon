;;;; falcon.lisp

;; TODO List
;; Multiple folders
;; Prefix out of master list and in to single field
;; Nasty OS dependent filepath fixes (spaces, seperators, escape characters YUCK!)
;; Call with directory(s) and prefix
;; Favorites tab populated
;; Regex filtering list
;; arrg my foot is bleeding

;; BUG Windows version of Tk puts / marks in filepaths from file chooser

(in-package #:falcon)

(defparameter *master-list* nil)
(defparameter *flags* nil)
(defparameter *buttons-across* 7)

(defun notify (message title)
  (message-box message title "ok" "info"))

(defun populate-from-directory (&optional (program-prefix-string ""))
  (let* ((d (choose-directory :title "Choose a directory to populate files from"))
	 (is-windows (numberp (search "WIN" (string-upcase (software-type)))))
	 (files nil))

    (format *standard-output* "Directory is: ~a" d)
    
    (when is-windows
      (setf d (substitute #\\ #\/ d)))

    (setf files (uiop:directory-files (make-pathname :directory 
					  `(:absolute ,d)
					  :name :wild :type :wild)))
    (uiop:directory-files "./")
    (format *standard-output* "Directory is now: ~a" d)
    
    (setf *master-list*
	  (loop :for i :in files :collecting
	     (list (file-namestring i)
		   (format nil "~a \"~a\"" program-prefix-string i))))
    
    (print files)
    (print *master-list*)
    (finish-output)))

(defparameter *light-color* "#fff7c5")
(defparameter *middle-color* "#d48626")
(defparameter *dark-color* "#6d2317")
(defparameter *very-dark-color* "#1a0500")

(defun style-setup ()
  "Send wish some direct commands since ltk doesn't work with styles"
  
  (send-wish "ttk::style theme use default")
  ;;(send-wish (format nil "ttk::style configure TButton -background \"~a\" -foreground \"~a\" -borderwidth 3" *very-dark-color* *middle-color*))
  ;;(send-wish "ttk::style configure TMenu -background \"#d48626\" -foreground \"#fff7c5\"")
  ;;(send-wish "ttk::style configure TFrame -background \"#fff7c5\"")
  
  )


(defun start-gui ()
  (with-ltk ()
    (wm-title *tk* "Falcon Button Tray")
    (set-geometry *tk* 640 480 0 0)
    (style-setup)    (populate-from-directory)
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
	   (nb (make-instance 'notebook :master *tk*))
	   (tab-frame (make-instance 'frame :master nb :name "tab_frame"))
	   (prefix-hull (make-instance 'frame :master tab-frame))
	   (prefix-label (make-instance 'label :master prefix-hull :text "Call with prefix: "))
	   (prefix-field (make-instance 'entry :master prefix-hull :text ""))
	   (prefix-button (make-instance 'button :master prefix-hull :text "..." :width 3))

	   (path-hull (make-instance 'frame :master tab-frame))
	   (path-label (make-instance 'label :master path-hull :text "Populate From: "))
	   (path-field (make-instance 'entry :master path-hull :text "."))
	   (path-button (make-instance 'button :master path-hull :text "..." :width 3 :command #'populate-from-directory))
	   
	   (run-parallel-hull (make-instance 'frame :master tab-frame))
	   (run-parallel-label (make-instance 'label :master run-parallel-hull :text "Spawn New Process: "))
	   (run-parallel-check (make-instance 'check-button :master run-parallel-hull))

	   (refresh-button (make-instance 'button :master tab-frame :text "Refresh" :command #'populate-from-directory))	   
	   (pw (make-instance 'scrolled-frame :master tab-frame :name "scrolled_frame"))
	   (pw2 (make-instance 'scrolled-frame :master nb :name "scrolled_frame2"))
	   (row 0)
	   (column 0))

      (pack nb :fill "both" :expand t)
      (pack prefix-hull :anchor "NW" :side "top")
      (pack prefix-label :side "left")
      (pack prefix-field :side "left")
      (pack prefix-button :side "left")

      (pack path-hull :anchor "NW" :side "top")
      (pack path-label :side "left")
      (pack path-field :side "left")
      (pack path-button :side "left")
      
      (pack run-parallel-hull :anchor "NW" :side "top")
      (pack run-parallel-label :side "left")
      (pack run-parallel-check :side "left")

      (pack refresh-button :anchor "NW" :side "top")
      
      (pack pw :fill "both" :anchor "NW" :side "top" :expand t)
      (notebook-enable-traversal nb)
      (notebook-add nb tab-frame :text "Output")
      (notebook-add nb pw2 :text "Favorites")


      
      (loop :for (name command) :in *master-list* :do
	 (let ((name name)
	       (command command))
	   (grid (make-instance 'button
				:master (interior pw)
				:text name
				:command (lambda() (asdf::run-program
						    (concatenate 'string
								 (text prefix-field)
								 " "
								 command))))
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
  ;; (format *standard-output* "Recieved Arguments: ~a" sb-ext:*posix-argv*)
  ;; This doesn't QUITE work because it'll read stdin all the time.
  ;; you prolly need a command line argument like '--' or something to indicate
  ;; it is ok to read stdin--otherwise it would be ignored.
  ;; (setf *flags* (apply-argv:parse-argv sb-ext:*posix-argv*))
  ;; (setf *master-list*
  ;; (loop :for (a b) :on *master-list* :by #'cddr collect (list a b)))
  ;; (format t "~a" *flags*)
  
  (start-gui))


