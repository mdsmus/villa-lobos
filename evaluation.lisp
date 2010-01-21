(in-package :villa-lobos)

(defun %get-kern (key)
  (parse-humdrum-file (format nil "/home/kroger/Documents/code/villa-lobos/chorales/~a.krn" key) t))

(defun evaluate-chorales (algorithm)
  (iter (for (key answer) in *answer-sheets*)
        (unless (equal key "150")
          (for result = (compare-chords-agreement (harmonic-analysis algorithm (%get-kern key))
                                                  (parse-chords answer)))
          (format t "~a: ~a~%" key result))))
