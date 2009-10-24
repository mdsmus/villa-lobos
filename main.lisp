(in-package :villa-lobos)

;; (defun main-dir ()
;;   ;; hack to know the true directory of a file in case the binary is
;;   ;; as symlink
;;   (directory-namestring (truename (merge-pathnames (first sb-ext:*posix-argv*)
;;                                                    (default-directory)))))

(defun main ()
  (hello))

(defun main-binary ()
  ;;(setf ltk:*wish-pathname*
  ;;      (join-strings (namestring (main-dir)) "bin/wish"))
  ;; hack to deal with this bug:
  ;; https://bugs.launchpad.net/sbcl/+bug/444427
  (setf swank:*log-output* nil)
  (swank:create-server :dont-close t :port 1717 :coding-system "utf-8-unix")
  (main)
  0)
