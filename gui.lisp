(in-package :villa-lobos)

(defun print-text ()
  (format t "Hello World!~&"))

(defun hello ()
  (with-ltk ()
    (let ((b (make-instance 'button 
                            :master nil
                            :text "Press Me"
                            :command #'print-text)))
      (pack b))))
