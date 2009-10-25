(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defun menu-open-file ()
  (print :foo))

(defun menu-open-folder ()
  (print :foo))

(defun menu-open-collection ()
  (print :foo))

(defun menu-make-collection ()
  (print :foo))

(defun menu-option-analysis ()
  (print :foo))

(defun menu-help-about ()
  (print :foo))

(defun menu-help-tutorial ()
  (print :foo))

(defun menu ()
  (let* ((menubar (make-menubar))
         (file (make-menu menubar "File"))
         (options (make-menu menubar "Options"))
         (collections (make-menu menubar "Collections"))
         (help (make-menu menubar "Help")))
    (make-menubutton file "Open File" #'menu-open-file :underline 1)
    (make-menubutton file "Open Folder" #'menu-open-folder :underline 1)
    (make-menubutton file "Open Collection" #'menu-open-collection :underline 1)
    (add-separator file)
    (make-menubutton file "Quit" #'quit :underline 1)
    (make-menubutton collections "Make Collection" #'menu-make-collection :underline 1)
    (make-menubutton collections "Open Collection" #'menu-open-collection :underline 1)
    (make-menubutton options "Analysis" #'menu-option-analysis :underline 1)
    (make-menubutton help "Tutorial" #'menu-help-tutorial :underline 1)
    (make-menubutton help "About" #'menu-help-about :underline 1)
    ))

(defun gui ()
  (with-ltk ()
    (let* ((file-width 20)
           (stats-height 8)
           (fr-base-left (make-instance 'frame :padx 10 :pady 10 :width file-width))
           (fr-base-right (make-instance 'frame :padx 10 :pady 10))
           (fr-left (make-instance 'labelframe :text "Files"
                                   :master fr-base-left :width file-width
                                   :padx 10 :pady 10 :font "Monospace-20"))
           (fr-right-top (make-instance 'labelframe :master fr-base-right :text "Display"
                                        :padx 10 :pady 10 :font "Monospace-20"))
           (fr-right-bottom (make-instance 'labelframe :master fr-base-right
                                           :height stats-height :text "Stats"
                                           :padx 10 :pady 10 :font "Monospace-20"))
           (lb1 (make-instance 'listbox :master fr-left :background :white
                               :width file-width))
           (sc2 (make-instance 'canvas :master fr-right-top :background :white))
           (display (canvas sc2))
           (stats (make-instance 'text :name "stats" :master fr-right-bottom
                                 :background :white :height stats-height))
           (image (make-instance 'photo-image)))
      (send-wish "package require Img")
      (menu)
      (pack fr-base-left :fill :y :side :left)
      (pack fr-base-right :side :right :expand 1 :fill :both)
      (pack fr-left :fill :y :side :left)
      (pack fr-right-top :expand 1 :fill :both :side :top)
      (pack fr-right-bottom :fill :x :side :bottom)
      ;;;
      (pack lb1 :expand 1 :fill :y :side :bottom)
      (pack sc2 :expand 1 :fill :both)
      (pack stats :expand 1 :fill :x)

      (listbox-append lb1 '("foo" "bar"))

      (image-load image "Out.ps")
      (create-image sc2 0 0 :image image)
      )))
