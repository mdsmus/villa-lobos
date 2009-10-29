(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defun menu-open-file (w)
  ;; FIXME to work with slime and CLI (villa-dev-dir)
  (let1 file (get-open-file :filetypes '(("Humdrum files" "*.krn"))
                            :initialdir (villa-dev-dir))
    (setf (files-kern w) file)
    ;;(setf (image w) (parse-humdrum-file file))
    (filelist-append-item w (pathname-name file))
    (stats-append-item w "foo")
    (display-insert-image w "Out.ps")
    ))

(defun menu-open-folder (w)
  (print (files-kern w))
  )

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
  (with-ltk ()
    (send-wish "package require Img")
    (wm-title *tk* "Villa-lobos")
    (pack (make-instance 'gui
                         :frame-font "Monospace-10" :pad 10
                         :filelist-width 20 :stats-height 8))))
