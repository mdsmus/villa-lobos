(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defun menu-open-file ()
  (print :foo))

(defun menu-open-folder ()
  (choose-directory))

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
  (make-easy-menu (("File"
                    ("Open file" #'choose-directory 1)
                    ("Open folder" #'menu-open-folder 1)
                    ("Open collection" #'menu-open-folder 1)
                    -----------------
                    ("Quit" #'quit 1))
                   ("Options"
                    ("Open file" #'choose-directory 1))
                   ("Collections"
                    ("Make collection" #'choose-directory 1)
                    ("Open collection" #'menu-open-folder 1))
                   ("Analysis"
                    ("Open file" #'choose-directory 1))
                   ("Help"
                    ("Tutorial" #'choose-directory 1)
                    ("about" #'menu-open-folder 1)))))

(defun gui ()
  (with-ltk ()
    (send-wish "package require Img")
    (wm-title *tk* "Villa-lobos")
    (let ((w (make-instance 'main-gui)))
      (pack w))))
