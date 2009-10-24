(in-package :villa-lobos)

(defun main ()
  (hello))

(defun run ()
  (setf ltk:*wish-pathname*
       (join-strings (namestring (main-dir)) "bin/wish"))
  ;; hack to deal with this bug:
  ;; https://bugs.launchpad.net/sbcl/+bug/444427
  (setf swank:*log-output* nil)
  (swank:create-server :dont-close t :port 1717 :coding-system "utf-8-unix")
  (main)
  0)
