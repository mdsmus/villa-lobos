(in-package :villa-lobos)

(declaim (optimize (debug 3)))

;;; Classes

(defclass options ()
  ((files :accessor files :initform nil)
   (scores :accessor scores :initform nil)
   (analysis :accessor analysis :initform nil)
   (user-options :accessor user-options :initform nil)
   (collections :accessor collections :initform nil)))

(defclass analysis ()
  ((name :accessor name)
   (spine :accessor spine)
   (arguments :accessor arguments)))

;;; Special variables

(defvar *options* (make-instance 'options))

(defun user-config-file ()
  (merge-pathnames ".villarc" (user-homedir-pathname)))

(defun user-config-dir ()
  (merge-pathnames ".villa/" (user-homedir-pathname)))

(defun wish-binary ()
  (merge-pathnames "deps/bin/wish" (villa-dev-dir)))

(defun abcm2ps-binary ()
  (merge-pathnames "deps/bin/abcm2ps" (villa-dev-dir)))

(defun open-user-configuration ()
  (ensure-directories-exist (user-config-dir))
  (when (file-exists-p (user-config-file))
    (with-open-file (in (user-config-file))
      (with-standard-io-syntax
        (let1 config (read in)
          (setf (collections *options*) (rest (assoc :collections config)))
          (setf (user-options *options*) (rest (assoc :user-options config))))))))

(defun list-collections ()
    (iter (for collection in (collections *options*))
          (for i from 1)
          (destructuring-bind (name &rest files) collection
            (format t "~a: ~a [~a files]~%" i name (length files)))))

(defun %open-files (list-of-files)
  (setf (files *options*) list-of-files)
  (setf (scores *options*)
        (iter (for (file . tag) in (files *options*))
              (collect (parse-humdrum-file file)))))

(defun open-collection (n)
  "Open the nth collection in collection list, where list is 1
  indexed."
  (%open-files (rest (nth (1- n) (collections *options*)))))

(defun open-folder (dir)
  ;; the directory list must have a cons inside (same as collections)
  (let1 files (directory (merge-pathnames "*.krn" (pathname-as-directory dir)))
    (%open-files (mapcar (lambda (x) (cons x (pathname-name x))) files))))

(defun open-file (file)
  (%open-files (list (cons file (pathname-name file)))))

(defun get-score (n)
  (nth n (scores *options*)))

(defun list-files ()
  (iter (for (file . tag) in (files *options*))
        (for i from 1)
        (format t "~a: ~a~%" i tag)))

(defun abc-to-ps (abc-file ps-file)
  ;; FIXME full pathname
  (run-prog (namestring (abcm2ps-binary))
            :args (list "-m" ".5cm" "-r" "-J" "0" "-O" (namestring ps-file) (namestring abc-file))))

(defun %output-pathname (score type)
  (concatenate 'string)
  (make-pathname :directory (pathname-directory (user-config-dir))
                 :name (pathname-name (get-filename score))
                 :type type))

(defun score-to-abc (score)
  ;; we don't export to 2 staves now because of bug on export-abc
  (export-abc (%output-pathname score "abc") score t))

(defun score-to-ps (score)
  (score-to-abc score)
  (abc-to-ps (%output-pathname score "abc") (%output-pathname score "ps")))
