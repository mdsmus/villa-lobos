(in-package :villa-lobos)

(defun app-name ()
  (first #+sbcl sb-ext:*posix-argv*
         #+ccl ccl:*command-line-argument-list*
         #+ecl (ext:command-args)))

(defun main-dir ()
  ;; hack to know the true directory of a file in case the binary is
  ;; as symlink. It relies in truename following a symbolic link and
  ;; returning the full name.
  (directory-namestring (truename (merge-pathnames (app-name)
                                                   (default-directory)))))
