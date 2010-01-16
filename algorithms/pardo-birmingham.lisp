(in-package :villa-lobos)

(declaim (optimize (debug 3)))

;;; weighted-notes is a list in the format '((0 1) (4 1) (7 1)) where
;;; the first element of each list is a note and the second is the
;;; number of times the note occours in a sonority.
;;; FIXME: 35 is 7M hard-coded (base40 is hardcoded)
;;; FIXME: root for dim chords are often wrong
;;; (calculate-score '(4 0 7) '((0 1) (4 1) (7 1))) => 6

(defparameter *pardo-templates* 
  '(("M"  (3 15 26))
    ("M7" (3 15 26 37))
    ("m"  (3 14 26))
    ("°7" (3 14 21 32))
    ("ø7" (3 14 21 37))
    ("°"  (3 14 21))))

(defstruct pardo-grade root result answer weighted-notes)

(defun calculate-score (template weighted-notes)
  (let ((sum-weights (sum (mapcar #'second weighted-notes)))
        (number-notes (length template)))
   (iter (for (note weight) in weighted-notes)
         (with score = (- 0 sum-weights number-notes))
         (with match = 0)
         (when (member note template)
           (setf score (+ 1 score (* 2 weight)))
           (incf match))
         (finally (return (+ score match))))))

(defun mod40 (n)
  (mod n 40))

(defun set-transpose (set index)
  (mapcar (lambda (n) (mod40 (+ n index))) set))

(defun get-most-results (template-notes weighted-notes)
  (mostn #'pardo-grade-result
         (iter (for note in aristoxenus::*35-notes*)
               (for transp = (set-transpose template-notes (mod40 (- note 3))))
               (collect (make-pardo-grade :root note
                                          :result (calculate-score transp weighted-notes)
                                          :weighted-notes weighted-notes)))))

(defun score-sonority (template weighted-notes)
  (destructuring-bind (template-name template-notes) template
    (iter (with results = (get-most-results template-notes weighted-notes))
          (for result in results)
          (setf (pardo-grade-answer result)
                (cons (pardo-grade-root result) template-name))
          (finally (return results)))))

(defun weight-notes (sonority)
  "Return a list of weighted notes (sonority is a list of notes)."
  (iter (for item in (mapcar #'get-code sonority))
        (with hash = (make-hash-table))
        (if (gethash item hash)
            (incf (gethash item hash))
            (setf (gethash item hash) 1))
        (finally (return (iter (for (k v) in-hashtable hash)
                               (collect (list k v)))))))

(defun pardo (sonority)
  ;; sonority is a list of #<note>s, we disregard a list that was
  ;; non-music stuff for now
  (mostn #'pardo-grade-result
         (mapcan (lambda (template) (score-sonority template (weight-notes sonority)))
                 *pardo-templates*)))

(defun root-weight (grade)
  (or (second (assoc (pardo-grade-root grade)
                     (pardo-grade-weighted-notes grade)))
      0))

(defun template-probablity (grade)
  (length (member-if (lambda (x) (equal (car x) (rest (pardo-grade-answer grade))))
                     *pardo-templates*)))

(defun dim7-p (grades)
  (every (lambda (x) (search "°" (cdr (pardo-grade-answer x)) :test #'equal)) grades))

(defun resolution-dim7 (grade next)
  (let1 note (mod40 (+ (pardo-grade-root next) 35))
    (or (find-if (lambda (x) (= note (pardo-grade-root x))) grade)
        (first grade))))

(defun tie-break (grade rest)
  (let* ((next (first rest))
         (max-root (mostn #'root-weight grade)))
    (cons (if (= 1 (length max-root))
              (car max-root)
              (let1 temp-prob (mostn #'template-probablity max-root)
                (if (= 1 (length temp-prob))
                    (car temp-prob)
                    (if (dim7-p grade)
                        (resolution-dim7 grade next)
                        (car max-root)))))
          rest)))

(defun grade-to-chord (grade)
  (destructuring-bind (note . type) (pardo-grade-answer grade)
    (format nil "~:(~a~)~a" (code-to-string note :base40) (remove #\M type))))

(defmethod harmonic-analysis (algorithm (score score))
  (when (eql algorithm :pardo)
    (let ((result (mapcar #'grade-to-chord
                          (reduce #'tie-break
                                  (mapcar #'pardo (remove-if-not #'every-note-p (segments score)))
                                  :from-end t :initial-value nil)))
          (analysis-hash (get-analysis score)))
      (setf (gethash algorithm analysis-hash) result))))
