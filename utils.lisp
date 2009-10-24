(in-package :villa-lobos)

(defun app-name ()
  (first #+sbcl sb-ext:*posix-argv*
         #+ccl ccl:*command-line-argument-list*))