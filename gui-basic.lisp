(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defun menu (w)
  (make-easy-menu (("File"
                    ("Open file" (menu-open-file w) 1)
                    ("Open folder" (menu-open-folder w) 1)
                    ("Open collection" (menu-open-folder w) 1)
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

(defclass gui ()
  ((filelist :accessor filelist)
   (display :accessor display)
   (stats :accessor stats)
   (image :accessor image)
   ;; frames
   (frame-left :accessor frame-left)
   (frame-right :accessor frame-right)
   (frame-filelist :accessor frame-filelist)
   (frame-display :accessor frame-display)
   (frame-stats :accessor frame-stats)
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
    (make-menubutton mp "foo3" (lambda () (print :bar)))
    (make-menubutton mp "foo4" (lambda () (print :bar)))
    (bind (filelist gui) "<ButtonPress-3>"
          (lambda (event)
            (declare (ignore event))
            (popup mp
                   (+ 3 (screen-mouse-x (filelist gui)))
                   (+ 3 (screen-mouse-y (filelist gui))))))))

(defmethod initialize-instance :after ((obj gui) &key)
  ;; menu
  (menu obj)
  ;; basic frames
  (setf (frame-left obj)
        (make-instance 'frame
                       :padx (pad obj) :pady (pad obj) :width (filelist-width obj)))
  (setf (frame-right obj)
        (make-instance 'frame :padx (pad obj) :pady (pad obj)))
  ;; filelist
  (setf (frame-filelist obj)
        (make-instance 'labelframe
                       :text "Files" :padx (pad obj) :pady (pad obj)
                       :width (filelist-width obj) :font (frame-font obj)
                       :master (frame-left obj)))
  (setf (filelist obj)
        (make-instance 'listbox :background :white :master (frame-filelist obj)))
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
                       :master (frame-stats obj))))

(defmethod pack ((obj gui) &key &allow-other-keys)
  (pack (frame-left obj) :fill :y :side :left)
  (pack (frame-right obj) :side :right :expand 1 :fill :both)
  (pack (frame-filelist obj) :fill :y :side :left)
  (pack (filelist obj) :expand 1 :fill :y :side :bottom)
  (pack (frame-display obj) :expand 1 :fill :both :side :top)
  (pack (display obj) :expand 1 :fill :both)
  (pack (frame-stats obj) :fill :x :side :bottom)
  (pack (stats obj) :expand 1 :fill :x))


(defgeneric listbox-delete (l start &optional end))
(defmethod listbox-delete ((l listbox) start &optional end)
  (format-wish "~a delete ~a ~@[~a~]" (widget-path l) start end)
  l)

(defmethod filelist-append-item ((gui gui) thing)
  (listbox-append (filelist gui) thing))

(defmethod filelist-delete-item ((gui gui) start &optional end)
  (listbox-delete (filelist gui) start end))

(defmethod filelist-get-selection ((gui gui))
  (listbox-get-selection (filelist gui)))

(defmethod filelist-clear ((gui gui))
  (listbox-clear (filelist gui)))

(defmethod filelist-nearest ((gui gui) y)
  (listbox-nearest (filelist gui) y))

(defmethod stats-append-item ((gui gui) thing)
  (append-text (stats gui) thing))

(defmethod stats-append-newline ((gui gui))
  (append-newline (stats gui)))

(defmethod stats-clear ((gui gui))
  (clear-text (stats gui)))

(defmethod display-insert-image ((gui gui) image)
  (image-load (image gui) image)
  (create-image (display gui) 0 0 :image (image gui)))
