(defclass harmonic-analysis-algorithm ()
  ((name :accessor name :initarg :name)
   (description :accessor description :initarg :description)))

(defclass pardo (algorithm) (harmonic-analysis-algorithm))


;; (defmethod harmonic-analysis (segments (algorithm pardo))
;;   (pardo-classify segments algorithm))
