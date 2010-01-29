(in-package :villa-lobos)

(declaim (optimize (debug 3)))

(defun svm-model-file ()
  (merge-pathnames "svm.model" (user-config-dir)))

(defun svm-train (targets inputs)
  (save-model (train (make-problem targets inputs) (make-parameter :gamma 20))
              (svm-model-file)))

(defun svm-predict (input)
  (predict (load-model (svm-model-file)) input))

(defun notes-to-svm (list)
  "Receive a list of notes and output a vector in the format expected
by libsvm. Example: notes-to-svm '(3 15 26)) =>
  #((3 . 1) (15 . 1) (8 . 1))"
  (let ((hash (make-hash-table))
        (vector (make-array 1 :fill-pointer 0 :adjustable t)))
    (iter (for item in list)
          (if (gethash item hash)
              (incf (gethash item hash))
              (setf (gethash item hash) 1)))
    (iter (for (k v) in-hashtable hash)
          (vector-push-extend (cons k v) vector))
    vector))

(defun notes-to-svm-no-duplicate (list)
  "Receive a list of notes and output a vector in the format expected
by libsvm. Example: notes-to-svm '(3 15 26)) =>
  #((3 . 1) (15 . 1) (8 . 1))"
  (map 'vector (lambda (x) (cons x 1)) list))

(defun get-answer-sheet (key)
  (second (assoc key *answer-sheets* :test #'equal)))

(defun answer-sheet-to-svm (list)
  (iter (for item in list)
        (aristoxenus::parse-chords-label item)))

(defun get-old-chorale-notes (key)
  (let* ((dir "/home/kroger/Documents/villa-lobos/rameau-chorales/~a.lisp")
         (list (with-open-file (s (format nil dir key)) (read s))))
    (mapcar (lambda (segment)
              (sort (mapcar (lambda (list) (string-to-code (first list) :base40)) segment)
                    #'<))
            list)))

(defun transpose-to-c (list)
  (let1 interval (- (first list) 3)
   (mapcar (lambda (x) (mod40 (- x interval))) list)))

(defun train-list (list-of-segments list-of-answers)
  (iter (for segment in list-of-segments)
        (for answer in list-of-answers)
        (for chord = (parse-chords-label answer))
        ;; (collect (list (notes-to-svm segment)
        ;;                (if (aristoxenus::chord-label-p chord)
        ;;                    (aristoxenus::chord-label-root-code chord)
        ;;                    200)))
        (collect (list (notes-to-svm-no-duplicate (remove-duplicates segment))
                       (if (aristoxenus::chord-label-p chord)
                           (aristoxenus::chord-label-root-code chord)
                           200)))))

(defun svm-train-music (key)
  (let* ((list (train-list (get-old-chorale-notes key) (get-answer-sheet key)))
         (uniq-list (remove-duplicates list :test #'equalp :key #'first))
         (sorted-list (sort uniq-list #'< :key #'second)))
    (print sorted-list)
    (svm-train (map 'vector #'second sorted-list)
               (map 'vector #'first sorted-list))))

(defun svm-generate-python (key)
  (let* ((list (train-list (get-old-chorale-notes key) (get-answer-sheet key)))
         (uniq-list (remove-duplicates list :test #'equalp :key #'first))
         (sorted-list (sort uniq-list #'< :key #'second)))
    (format t "[~{~a~^,~}]~%" (mapcar #'second sorted-list))
    (iter (for seg in (mapcar #'first sorted-list))
          (format t "{")
          (iter (for (a . b) in-vector seg)
                (format t "~a:~a," a b))
          (format t "},"))))


;; (svm-train #(10 20 1)
;;              #(#((1 . 1) (5 . 1) (8 . 1))
;;                #((1 . 1) (4 . 1) (8 . 1))
;;                #((1 . 1) (3 . 1) (8 . 1))))

;; (svm-predict #((3 . 1) (15 . 1) (26 . 1)))

(defun test-svm (key)
  ;;(svm-train-music key)
  (iter (for seg in (get-old-chorale-notes key))
        (print (svm-predict (notes-to-svm-no-duplicate (remove-duplicates seg))))))
