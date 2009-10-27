(in-package :villa-lobos)

(defun main ()
  (setf ltk:*wish-pathname*
        (namestring (merge-pathnames "bin/wish" (villa-dev-dir))))
  (gui))

(defun run ()
  (setf ltk:*wish-pathname*
       (join-strings (namestring (main-dir)) "bin/wish"))
  ;; hack to deal with this bug:
  ;; https://bugs.launchpad.net/sbcl/+bug/444427
  (setf swank:*log-output* nil)
  (swank:create-server :dont-close t :port 1717 :coding-system "utf-8-unix")
  (main)
  0)
