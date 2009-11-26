(in-package :villa-lobos)

(defun main ()
  (open-user-configuration)
  (setf ltk:*wish-pathname*
        (namestring (merge-pathnames "bin/wish" (villa-dev-dir))))
  (gui))

(defun run ()
  (open-user-configuration)
  ;; hack to deal with this bug:
  ;; https://bugs.launchpad.net/sbcl/+bug/444427
  (setf swank:*log-output* nil)
  (swank:create-server :dont-close t :port 1717 :coding-system "utf-8-unix")
  (if (equal (first (argv)) "--nogui")
      (loop (print (eval (read))))
      (progn
        (setf ltk:*wish-pathname*
              (join-strings (namestring (main-dir)) "bin/wish"))
        (gui)))
  0)
