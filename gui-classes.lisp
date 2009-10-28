(in-package :villa-lobos)

(declaim (optimize (debug 3)))

;;; define classes

(defclass listboxframe (listbox)
  ((width :accessor width :initarg :width)
   (frame :accessor frame)
   (listbox :accessor listbox)
   (master :accessor master :initarg :master)
   (frame-text :accessor frame-text :initarg :frame-text)
   (frame-font :accessor frame-font :initarg :frame-font)))

(defclass canvasframe (canvas)
  ((frame :accessor frame)
   (canvas :accessor canvas)
   (score :accessor score)
   (master :accessor master :initarg :master)
   (frame-text :accessor frame-text :initarg :frame-text)
   (frame-font :accessor frame-font :initarg :frame-font)))

(defclass textframe (text)
  ((frame :accessor frame)
   (text :accessor text)
   (height :accessor height :initarg :height)
   (master :accessor master :initarg :master)
   (frame-text :accessor frame-text :initarg :frame-text)
   (frame-font :accessor frame-font :initarg :frame-font)))

(defclass main-gui (widget)
  ((frame-font :accessor frame-font :initarg :frame-font)
   (file-list-width :accessor file-list-width :initarg :file-list-width :initform 20)
   (frame-left :accessor frame-left :initarg :frame-left)
   (frame-right :accessor frame-right :initarg :frame-right)
   (file-list :accessor file-list :initarg :file-list)
   (music-display :accessor music-display :initarg music-display)
   (stats :accessor stats :initarg stats)
   (pad :accessor pad :initarg :pad :initform 10)))

;;; initialize-instance

(defmethod initialize-instance :after ((obj listboxframe) &key)
  (setf (frame obj)
        (make-instance 'labelframe :text (frame-text obj) :padx 10 :pady 10
                       :width (width obj) :font (frame-font obj) :master (master obj)))
  (setf (listbox obj)
        (make-instance 'listbox :background :white :master (frame obj))))

(defmethod initialize-instance :after ((obj canvasframe) &key)
  (setf (frame obj)
        (make-instance 'labelframe :text (frame-text obj) :padx 10 :pady 10
                       :font (frame-font obj) :master (master obj)))
  (setf (canvas obj)
        (make-instance 'canvas :master (frame obj) :background :white))
  (canvas (canvas obj))
  (setf (score obj)
        (make-instance 'photo-image)))

(defmethod initialize-instance :after ((obj textframe) &key)
  (setf (frame obj)
        (make-instance 'labelframe :text (frame-text obj)
                       :padx 10 :pady 10 :font (frame-font obj)
                        :master (master obj) :height (height obj)))
  (setf (text obj)
        (make-instance 'text :master (frame obj) :background :white
                       :height (height obj))))

(defmethod initialize-instance :after ((obj main-gui) &key)
  (menu)
  (setf (frame-left obj)
        (make-instance 'frame :padx (pad obj) :pady (pad obj)
                       :width (file-list-width obj)))
  (setf (frame-right obj)
        (make-instance 'frame :padx (pad obj) :pady (pad obj)))
  (setf (file-list obj)
        (make-instance 'listboxframe
                       :master (frame-left obj)
                       :width (file-list-width obj)
                       :frame-font (frame-font obj)
                       :frame-text "Files"))
  (setf (music-display obj)
        (make-instance 'canvasframe
                       :master (frame-right obj)
                       :frame-font (frame-font obj)
                       :frame-text "Display"))
  (setf (stats obj)
        (make-instance 'textframe
                       :master (frame-right obj)
                       :height 8
                       :frame-font (frame-font obj)
                       :frame-text "Stats")))

;;; pack

(defmethod pack ((obj listboxframe) &key &allow-other-keys)
  (pack (frame obj) :fill :y :side :left)
  (pack (listbox obj) :expand 1 :fill :y :side :bottom))

(defmethod pack ((obj canvasframe) &key &allow-other-keys)
  (pack (frame obj) :expand 1 :fill :both :side :top)
  (pack (canvas obj) :expand 1 :fill :both))

(defmethod pack ((obj textframe) &key &allow-other-keys)
  (pack (frame obj) :fill :x :side :bottom)
  (pack (text obj) :expand 1 :fill :x))

(defmethod pack ((obj main-gui) &key &allow-other-keys)
  (pack (frame-left obj) :fill :y :side :left)
  (pack (frame-right obj) :side :right :expand 1 :fill :both)
  (pack (file-list obj))
  (pack (music-display obj))
  (pack (stats obj)))
