(in-package :villa-lobos)

(defun %get-kern (key)
  (parse-humdrum-file (format nil "/home/kroger/Documents/code/villa-lobos/chorales/~a.krn" key) t))

(defun %get-old-chorale (key)
  (let* ((dir "/home/kroger/Documents/code/villa-lobos/rameau-chorales/~a.lisp")
         (list (with-open-file (s (format nil dir key))
                 (read s))))
    (iter (for segment in list)
          (collect (iter (for (note octave dur) in segment)
                         (collect (make-note note octave dur (string-to-code note :base40) :base40)))))))

(defun evaluate-chorales (algorithm)
  ;; ok means they have the same size
  (sort (iter (for (key answer) in *answer-sheets*)
              (for analysis = (harmonic-analysis algorithm (%get-old-chorale key)))
              (for result = (compare-chords-agreement analysis (parse-chords answer)))
              (for size-analysis = (length analysis))
              (for size-answer = (length answer))
              (collect (list key
                             result
                             (if (= size-analysis size-answer)
                                 :ok
                                 (list :not size-analysis size-answer)))))
        #'> :key #'second))

(defun print-evaluate-chorales (algorithm)
  (let1 results (evaluate-chorales algorithm)
    (iter (for (name number ok) in results)
          (format t "~a: ~,2f ~a~%" name number ok))))
