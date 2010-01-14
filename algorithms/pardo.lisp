(in-package :villa-lobos)

(declaim (optimize (debug 3)))

;;; weighted-notes is a list in the format '((0 1) (4 1) (7 1)) where
;;; the first element of each list is a note and the second is the
;;; number of times the note occours in a sonority.

(defun mostn (fn list)
  "mostn takes a function and a list and returns a list of all the
elements for which the function yields the highest score (along with
the score itself)."
  (when list
    (let ((max-value (funcall fn (first list)))
          (list-max nil))
      (dolist (el list)
        (cond ((> (funcall fn el) max-value)
               (setf max-value (funcall fn el))
               (setf list-max (list el)))
              ((= (funcall fn el) max-value)
               (push el list-max))))
      list-max)))

(defparameter *pardo-templates* 
  '(("M" (0 4 7))
    ("M7" (0 4 7 10))
    ("m" (0 3 7))
    ("°7" (0 3 6 9))
    ("ø7" (0 3 6 10))
    ("°" (0 3 6))))

(defun calculate-score (template weighted-notes)
  ;; (calculate-score '(4 0 7) '((0 1) (4 1) (7 1))) => 6
  (let ((sum-weights (sum (mapcar #'second weighted-notes)))
        (number-notes (length template)))
   (iter (for (note weight) in weighted-notes)
         (with score = (- 0 sum-weights number-notes))
         (with match = 0)
         (when (member note template)
           (setf score (+ 1 score (* 2 weight)))
           (incf match))
         (finally (return (+ score match))))))

(defstruct pardo-grade root result answer weighted-notes)

(defun set-transpose (set index)
  (mapcar (lambda (n) (+ n index)) set))

(defun get-most-results (template-notes weighted-notes)
  (mostn #'pardo-grade-result
         (iter (for note in aristoxenus::*35-notes*)
               (for transposed-template = (set-transpose template-notes note))
               (collect (make-pardo-grade :root note
                                          :result (calculate-score transposed-template weighted-notes)
                                          :weighted-notes weighted-notes)))))

(defun score-sonority (template weighted-notes)
  (destructuring-bind (template-name template-notes) template
    (iter (with results = (get-most-results template-notes weighted-notes))
          (for result in results)
          (setf (pardo-grade-answer result)
                (cons (pardo-grade-root result) template-name))
          (finally (return results)))))

(defun weight-notes (sonority)
  "Return a list of weighted notes."
  (iter (for item in (mapcar #'get-code sonority))
        (with hash = (make-hash-table))
        (if (gethash item hash)
            (incf (gethash item hash))
            (setf (gethash item hash) 1))
        (finally (return (iter (for (k v) in-hashtable hash)
                               (collect (list k v)))))))

(defun pardo (sonority)
  ;; sonority is a list of #<note>s
  (mostn #'pardo-grade-result
         (mapcan (lambda (template) (score-sonority template (weight-notes sonority)))
                 *pardo-templates*)))

(defun segments (score)
  (let1 hash (get-minimal-segments score)
    (iter (for item in-vector (gethash 1 hash))
          (for x from 0)
          (collect (iter (for (key val) in-hashtable hash)
                         (collect (aref (gethash key hash) x)))))))

(defmethod harmonic-analysis (algorithm (score score))
  (when (eql algorithm :pardo)
    (iter (for segment in (segments score))
          (when (every #'music-p segment)
            (collect (pardo segment))))))
