(in-package :villa-lobos)

(defun print-text ()
  (format t "Hello World 3!~&"))

(defun hello ()
  (with-ltk ()
    (let ((b (make-instance 'button 
                            :master nil
                            :text "Press Me"
                            :command #'print-text)))
      (pack b))))

(defun main ()
  ;; hack to deal with this bug:
  ;; https://bugs.launchpad.net/sbcl/+bug/444427
  (setf swank:*log-output* nil)
  (swank:create-server :dont-close t :coding-system "utf-8-unix")
  (hello)
  0)
