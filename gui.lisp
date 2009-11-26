(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defvar *gui*)

;;; define basic gui

(defclass gui (widget)
  ((filelist :accessor filelist)
   (display :accessor display)
   (stats :accessor stats)
   (repl :accessor repl)
   (image :accessor image)
   (statusbar :accessor statusbar)
   ;; frames
   (frame-left :accessor frame-left)
   (frame-right :accessor frame-right)
   (frame-top :accessor frame-top)
   (frame-bottom :accessor frame-bottom)
   (frame-filelist :accessor frame-filelist)
   (frame-display :accessor frame-display)
   (frame-stats :accessor frame-stats)
   (frame-repl :accessor frame-repl)
   ;; options
   (files-kern :accessor files-kern)
   (filelist-width :accessor filelist-width :initarg :filelist-width)
   (frame-font :accessor frame-font :initarg :frame-font)
   (stats-height :accessor stats-height :initarg :stats-height)
   (pad :accessor pad :initarg :pad)))

(defmethod listbox-popup ((gui gui))
  (let1 mp (make-menu nil "Popup")
    (make-menubutton mp "close score"
                     (lambda ()
                       (loop for s in (filelist-get-selection gui) do
                             (filelist-delete-item gui s))))
    (make-menubutton mp "foo2" (lambda () (print :bar)))
    (filelist-configure gui :selectmode :single)
    (bind (get-filelist gui) "<ButtonPress-1>"
          (lambda (event)
            (declare (ignore event))
            (let1 score (get-score (first (filelist-get-selection gui)))
              (score-to-ps score)
              (display-insert-image gui (%output-pathname score "ps")))))
    
    (bind (get-filelist gui) "<ButtonPress-3>"
          (lambda (event)
            (declare (ignore event))
            (if (filelist-get-selection gui)
                (popup mp
                       (+ 3 (screen-mouse-x (get-filelist gui)))
                       (+ 3 (screen-mouse-y (get-filelist gui)))))))))

(defmethod initialize-instance :after ((obj gui) &key)
  ;; menu
  (menu obj)
  ;; basic frames
  (setf (frame-top obj) (make-instance 'frame))
  (setf (frame-bottom obj) (make-instance 'frame))
  (setf (frame-left obj)
        (make-instance 'frame
                       :padx (pad obj) :pady (pad obj) :width (filelist-width obj)
                       :master (frame-top obj)))
  (setf (frame-right obj)
        (make-instance 'frame :padx (pad obj) :pady (pad obj) :master (frame-top obj)))

  ;; filelist
  (setf (frame-filelist obj)
        (make-instance 'labelframe
                       :text "Files" :padx (pad obj) :pady (pad obj)
                       :width (filelist-width obj) :font (frame-font obj)
                       :master (frame-left obj)))
  (setf (filelist obj)
        (make-instance 'scrolled-listbox :background :white :master (frame-filelist obj)))
  (listbox-popup obj)
  ;; display
  (setf (frame-display obj)
        (make-instance 'labelframe
                       :text "Display" :padx (pad obj) :pady (pad obj)
                       :font (frame-font obj) :master (frame-right obj)))
  (setf (display obj)
        (make-instance 'canvas :master (frame-display obj) :background :white))
  (setf (image obj) (make-instance 'photo-image))
  ;; stats
  (setf (frame-stats obj)
        (make-instance 'labelframe
                       :text "Stats" :padx (pad obj) :pady (pad obj)
                       :font (frame-font obj) :height (stats-height obj)
                       :master (frame-right obj)))
  (setf (stats obj)
        (make-instance 'text
                       :background :white :height (stats-height obj)
                       :width 50 :master (frame-stats obj)))
  ;; repl
  (setf (frame-repl obj)
        (make-instance 'labelframe
                       :text "Listener" :padx (pad obj) :pady (pad obj)
                       :font (frame-font obj) :height (stats-height obj)
                       :master (frame-right obj)))
  (setf (repl obj)
        (make-instance 'text
                       :background :white :height (stats-height obj)
                       :master (frame-repl obj)))
  (bind (repl obj) "<Return>" (lambda (event)
                                (declare (ignore event))
                                (print (get-cursor-pos (repl obj)))
                                (append-text (repl obj) "> ")))
  (setf (text (repl obj)) "> ")
  ;; statusbar
  (setf (statusbar obj)
        (make-instance 'label
                       :name "statusbar" :relief :sunken :anchor :w
                       :master (frame-bottom obj))))

(defmethod get-cursor-pos ((text text))
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (ltk::read-data))

(defmethod pack ((obj gui) &key &allow-other-keys)
  (pack (frame-top obj) :side :top :expand 1 :fill :both)
  (pack (frame-bottom obj) :side :bottom :fill :x)
  (pack (frame-left obj) :fill :y :side :left)
  (pack (frame-right obj) :side :right :expand 1 :fill :both)
  (pack (frame-filelist obj) :fill :y :side :left)
  (pack (filelist obj) :expand 1 :fill :y :side :bottom)
  (pack (frame-display obj) :expand 1 :fill :both :side :top)
  (pack (display obj) :expand 1 :fill :both)
  (pack (frame-stats obj) :fill :x :side :left)
  (pack (stats obj) :expand 1 :fill :x)
  (pack (frame-repl obj) :fill :x :side :right)
  (pack (repl obj) :expand 1 :fill :x)
  (pack (statusbar obj) :expand 1 :fill :x))

(defun menu (w)
  (make-easy-menu (("File"
                    ("Open file" (menu-open-file w) 1)
                    ("Open folder" (menu-open-folder w) 1)
                    ("Open collection" (menu-open-collection w) 1)
                    -----------------
                    ("Quit" (quit) 1))
                   ("Options"
                    ("Open file" (menu-open-file w) 1))
                   ("Collections"
                    ("Make collection" (menu-open-file w) 1)
                    ("Open collection" (menu-open-folder w) 1))
                   ("Analysis"
                    ("Open file" (menu-open-folder w) 1))
                   ("Help"
                    ("Tutorial" (menu-open-folder w) 1)
                    ("about" (menu-open-folder w) 1)))))

;;; extend ltk

(defun choose-directory (&key (initialdir (namestring *default-pathname-defaults*))
			      parent title mustexist)
  (format-wish "senddatastring [tk_chooseDirectory ~@[ -initialdir {~a}~]~@[ -parent ~a ~]~@[ -title {~a}~]~@[ -mustexist ~a~]]" initialdir (and parent (widget-path parent)) title (and mustexist 1))
  (ltk::read-data))

(defgeneric listbox-delete (l start &optional end))
(defmethod listbox-delete ((l listbox) start &optional end)
  (format-wish "~a delete ~a ~@[~a~]" (widget-path l) start end)
  l)

(defgeneric listbox-activate (l index))
(defmethod listbox-activate ((l listbox) index)
  (format-wish "~a activate ~a" (widget-path l) index)
  l)

;;; methods on gui

(defmethod get-filelist ((gui gui))
  (listbox (filelist gui)))

(defmethod filelist-append-item ((gui gui) thing)
  (listbox-append (get-filelist gui) thing))

(defmethod filelist-delete-item ((gui gui) start &optional end)
  (listbox-delete (get-filelist gui) start end))

(defmethod filelist-get-selection ((gui gui))
  (listbox-get-selection (get-filelist gui)))

(defmethod filelist-clear ((gui gui))
  (listbox-clear (get-filelist gui)))

(defmethod filelist-nearest ((gui gui) y)
  (listbox-nearest (get-filelist gui) y))

(defmethod filelist-activate ((gui gui) index)
  (listbox-nearest (get-filelist gui) index))

(defmethod filelist-configure ((gui gui) option value)
  (configure (get-filelist gui) option value))

(defmethod stats-append-item ((gui gui) thing)
  (append-text (stats gui) thing))

(defmethod stats-append-newline ((gui gui))
  (append-newline (stats gui)))

(defmethod stats-clear ((gui gui))
  (clear-text (stats gui)))

(defmethod display-insert-image ((gui gui) image)
  (image-load (image gui) image)
  (create-image (display gui) 0 0 :image (image gui)))

;;; operations

(defun statusbar-update (text)
 (format-wish "set text_statusbar {  ~a}" text))

(defun statusbar-clean ()
 (format-wish "set text_statusbar {}"))

(defun menu-open-file (w)
  ;; FIXME to work with slime and CLI (villa-dev-dir)
  (let1 file (get-open-file :filetypes '(("Humdrum files" "*.krn"))
                            :initialdir (villa-dev-dir))
    (setf (files-kern w) file)
    ;;(setf (image w) (parse-humdrum-file file))
    (filelist-append-item w (pathname-name file))
    (statusbar-update (strcat "file " (pathname-name file) " openend"))
    ;;(setf (text (statusbar w)) "foo")
    ;;(stats-append-item w "foo")
    ;;(display-insert-image w "/tmp/bar.ps")
    ))

(defun menu-open-folder (w)
  (let1 dir (choose-directory :initialdir (villa-dev-dir))
    (statusbar-update "Parsing files ...")
    (open-folder (pathname-as-directory dir))
    (statusbar-update "Files opened ...")
    (filelist-append-item w (mapcar #'cdr (files *options*)))))

(defun menu-open-collection (w)
  (declare (ignore w))
  (print :foo))

(defun menu-make-collection (w)
  (declare (ignore w))
  (print :foo))

(defun menu-option-analysis (w)
  (declare (ignore w))
  (print :foo))

(defun menu-help-about (w)
  (declare (ignore w))
  (print :foo))

(defun menu-help-tutorial (w)
  (declare (ignore w))
  (print :foo))

(defun gui ()
  ;;(start-wish)
  (with-ltk ()
   (send-wish "package require Img")
  ;;(send-wish "option add *background gray100")
   (wm-title *tk* "Villa-lobos")
   (setf *gui* (make-instance 'gui
                              :frame-font "Monospace-10" :pad 10
                              :filelist-width 20 :stats-height 8))
   (pack *gui*))
  ;;(mainloop)
  )
