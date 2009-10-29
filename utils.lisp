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

(defun villa-dev-dir ()
  (asdf:component-pathname (asdf:find-system :villa-lobos)))

(defmacro make-easy-menu (list)
  (let ((menubar (gensym)))
    `(let ((,menubar (make-menubar)))
       (progn
         ,@(mapcar (lambda (menu)
                     (destructuring-bind (name &rest rest) menu
                       (let ((i (gensym)))
                         `(let ((,i (make-menu ,menubar ,name)))
                            (progn
                              ,@(mapcar (lambda (item)
                                          (if (listp item)
                                              (destructuring-bind (n (fn &rest args) un) item
                                                `(make-menubutton ,i
                                                                  ,n
                                                                  (lambda () (,fn ,@args))
                                                                  :underline ,un))
                                              `(add-separator ,i)))
                                        rest))))))
                   list)))))
