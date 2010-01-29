(in-package :villa-lobos)

(defun %get-kern (key)
  (parse-humdrum-file (format nil "/home/kroger/Documents/code/villa-lobos/chorales/~a.krn" key) t))

(defun get-old-chorale (key)
  (let* ((dir "/home/kroger/Documents/villa-lobos/rameau-chorales/~a.lisp")
         (list (with-open-file (s (format nil dir key))
                 (read s))))
    (iter (for segment in list)
          (collect (iter (for (note octave dur) in segment)
                         (collect (make-note note octave dur (string-to-code note :base40) :base40)))))))

(defun bad-chorale-p (key)
  (member key
          '("131" "142" "150" "253" "357" "359" "361" "366" "367" "368" "369" "370" "371")
          :test #'equal))

(defparameter *corrected-chorales*
  '("001" "002" "003" "004" "005" "006" "007" "008" "010" "012" "014"
    "017" "018" "019" "020" "021" "022" "023" "024" "025" "026" "027"
    "028" "029" "030" "031" "032" "033" "036" "040" "042" "044" "046"
    "048" "050" "053" "054" "055" "056" "057" "059" "060" "062" "065"
    "072" "074" "075" "076" "078" "080" "090" "093" "094" "095" "129"
    "130" "131" "132" "133" "134" "135" "136" "137" "138" "139" "140"
    "141" "142" "143" "144" "145" "146" "147" "148" "149" "150" "151"
    "152" "153" "162"))

(defun evaluate-chorales (algorithm)
  ;; ok means they have the same size
  (sort (iter (for (key answer) in *answer-sheets*)
              (unless (bad-chorale-p key)
                (for analysis = (harmonic-analysis algorithm (get-old-chorale key)))
                (for result = (compare-chords-agreement analysis (parse-chords answer)))
                (for size-analysis = (length analysis))
                (for size-answer = (length answer))
                (collect (list key
                               result
                               (if (= size-analysis size-answer)
                                   :ok
                                   (list :not size-analysis size-answer))))))
        #'> :key #'second))

(defun print-evaluate-chorales (algorithm)
  (let* ((results (evaluate-chorales algorithm))
         (sum (reduce #'+ (mapcar #'second results)))
         (size (length results))
         (total (/ sum size)))
    (iter (for (name number ok) in results)
          (format t "~a: ~,2f ~a~%" name number ok))
    (format t "~%total: ~,2f~%" total)))

