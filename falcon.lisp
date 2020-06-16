;;;; falcon.lisp

;; TODO List
;; Favorites tab populated
;; Regex filtering list
;; Bind program selector result to prefix field

;; BUG Windows version of Tk puts / marks in filepaths from file chooser

(in-package #:falcon)

(defparameter *master-list* nil)
(defparameter *flags* nil)
(defparameter *buttons-across* 3)

;; The following are used to manipulate the button tray outside Tk
(defparameter *button-hull* nil)
(defparameter *prefix-field* nil)

(defun tk-parent (w)
  "Returns a Wish understood representation of the CLOS object parent"
  (let* ((old-path (widget-path w))
         (parent-path (subseq old-path 0 (search "." old-path :from-end t))))
    parent-path))

(defun tk-destroy-widget (w)
  "Given a CLOS object destroys it on the Wish side"
  (send-wish (concatenate 'string "destroy " (widget-path w)))
  (setf w NIL))

(defun tk-destroy-children (w)
  "Given a CLOS object destroys the children on the Wish side"
  (let ((command-string
         "foreach w [winfo children \"~a\"] { destroy $w }"))
    (send-wish (format nil command-string (widget-path w)))))

(defun notify (message title)
  (message-box message title "ok" "info"))

(defun populate-from-directory (&optional (program-prefix-string ""))
  (unless *button-hull*
    (tk-destroy-children *button-hull*))

  (let* ((d (choose-directory :title "Choose a directory to populate files from"))
         (files nil))

    (setf *master-list* (uiop:directory-files (make-pathname :directory
                                                             (list :absolute d))))

    (refresh-tray)))

(defparameter *light-color* "#fff7c5")
(defparameter *middle-color* "#d48626")
(defparameter *dark-color* "#6d2317")
(defparameter *very-dark-color* "#1a0500")

(defun style-setup () ;; Messing with color a bit, I hope for sickly medical pastels eventually
  "Send wish some direct commands since ltk doesn't work with styles"

  (send-wish "ttk::style theme use default")
  ;;(send-wish (format nil "ttk::style configure TButton -background \"~a\" -foreground \"~a\" -borderwidth 3" *very-dark-color* *middle-color*))
  ;;(send-wish "ttk::style configure TMenu -background \"#d48626\" -foreground \"#fff7c5\"")
  ;;(send-wish "ttk::style configure TFrame -background \"#fff7c5\"")
  (send-wish "ttk::style configure TButton -background \"#fff7c5\""))


;; TODO MAKE THIS SIMPLY GENERATE AN INSTANCE OF THE TRAY!
;; MAKE A FUNCTION TO DELETE THE ORIGINAL AND REBIND INSTEAD!!!
(defun refresh-tray ()
  "Destroys then repopulates file launch buttons"
  (if (or (null *button-hull*)
          (null *prefix-field*))
      (return-from refresh-tray))

  (let ((row 0)
        (column 0))
    (loop :for path :in *master-list* :do
       (let ((path path)) ; We force eval of path when binding to the button's action
         (grid (make-instance 'button
                              :master *button-hull*
                              :text (uiop:native-namestring (file-namestring path))
                              :command (lambda() (uiop:launch-program
                                                  (format nil "\"~a\" \"~a\"" ;Windows/Linux hack "command" "full path with spaces"
                                                          (text *prefix-field*)
                                                          (uiop:native-namestring path)))))
               row column :sticky "EW" :padx 1 :pady 1))
       (cond ((>= column (1- *buttons-across*))
              (setf column 0)
              (incf row))
             (t
              (incf column))))))


(defun start-gui ()
  (with-ltk ()
    (wm-title *tk* "Falcon Button Tray")
    (set-geometry *tk* 640 480 0 0)
    (style-setup)
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
           (path-label (make-instance 'label :master path-hull :text "Add From Directory: "))
           (path-button (make-instance 'button :master path-hull :text "+" :width 3 :command #'populate-from-directory))

           (run-parallel-hull (make-instance 'frame :master tab-frame))
           (run-parallel-label (make-instance 'label :master run-parallel-hull :text "Spawn New Process: "))
           (run-parallel-check (make-instance 'check-button :master run-parallel-hull))

           (refresh-button (make-instance 'button :master tab-frame :text "Clear All" :command (lambda () (tk-destroy-children *button-hull*))))
           (pw (make-instance 'scrolled-frame :master tab-frame :name "scrolled_frame"))
           (pw2 (make-instance 'scrolled-frame :master nb :name "scrolled_frame2")))

      (setf *prefix-field* prefix-field)
      (setf *button-hull* (interior pw))

      (pack nb :fill "both" :expand t)
      (pack prefix-hull :anchor "NW" :side "top")
      (pack prefix-label :side "left")
      (pack prefix-field :side "left")
      (pack prefix-button :side "left")

      (pack path-hull :anchor "NW" :side "top")
      (pack path-label :side "left")
      (pack path-button :side "left")

      (pack run-parallel-hull :anchor "NW" :side "top")
      (pack run-parallel-label :side "left")
      (pack run-parallel-check :side "left")

      (pack refresh-button :anchor "NW" :side "top")

      (pack pw :fill "both" :anchor "NW" :side "top" :expand t)
      (notebook-enable-traversal nb)
      (notebook-add nb tab-frame :text "Output")
      (notebook-add nb pw2 :text "Favorites")
      (refresh-tray)
      )))


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
