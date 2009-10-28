(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defclass gui ()
  ((filelist :accessor filelist)
   (display :accessor display)
   (stats :accessor stats)
   (score :accessor score)
   ;; frames
   (frame-left :accessor frame-left)
   (frame-right :accessor frame-right)
   (frame-filelist :accessor frame-filelist)
   (frame-display :accessor frame-display)
   (frame-stats :accessor frame-stats)
   ;; options
   (filelist-width :accessor filelist-width :initarg :filelist-width)
   (frame-font :accessor frame-font :initarg :frame-font)
   (stats-height :accessor stats-height :initarg :stats-height)
   (pad :accessor pad :initarg :pad)))

(defmethod initialize-instance :after ((obj gui) &key)
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
  ;; display
  (setf (frame-display obj)
        (make-instance 'labelframe
                       :text "Display" :padx (pad obj) :pady (pad obj)
                       :font (frame-font obj) :master (frame-right obj)))
  (setf (display obj)
        (make-instance 'canvas :master (frame-display obj) :background :white))
  (setf (score obj) (make-instance 'photo-image))
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
