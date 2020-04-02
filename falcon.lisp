;;;; falcon.lisp

(in-package #:falcon)

(defparameter *master-list*
  (list
   "Nothing Mapped" ""
   ))

(setf *master-list*
      (loop for (a b) on *master-list* by #'cddr collect (list a b)))
(defparameter *buttons-across* 3)

(defun start-gui ()
  (with-ltk ()
    (wm-title *tk* "Falcon Button Tray")
    ;;(set-geometry *tk* 640 480 0 0)

    (let ((image (make-image)))
      (image-load image "./FALCON.GIF")
      (let ((canvas (make-instance 'canvas)))
	(create-image canvas 6 6 :image image)
	(create-text canvas 40 12 "(C) Falcon Technologies 1997")
        (configure canvas :height 48)
        (pack canvas :fill "x" :padx 0 :pady 0 :ipadx 0 :ipady 0 :fill "x")))
    
    (let* ((pw (make-instance 'scrolled-frame :master *tk*))
	   (row 0)
	   (column 0))
      (loop for (name command) in *master-list* do
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
		  (incf column))))
      (pack pw :fill "both" :expand t))))


(defun main ()
 (format *standard-output* "Recieved Arguments: ~a" sb-ext:*posix-argv*)
 ;; This doesn't QUITE work because it'll read stdin all the time.
 ;; you prolly need a command line argument like '--' or something to indicate
 ;; it is ok to read stdin--otherwise it would be ignored.
 (let* ((strings
         (loop :for str = (read *standard-input* nil :eof)
               :until (eq str :eof)
               :collect str))
        (descs
         (loop :for (button-name command-string) :on strings :by #'cddr
	    :collect (list button-name command-string))))
   (setf *master-list* descs)
   (format t "Read: ~S~%" descs))

 (start-gui))

