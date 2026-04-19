;;;; Chapter 13 - Problem 13-4
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defstruct stud
    (size '2X4)
    (length 8)
    (strength 'medium))

(defstruct (oak-stud (:include stud (strength 'high))))

(defparameter *stud-standard* (make-stud))
(defparameter *oak-example* (make-oak-stud))
(describe *stud-standard*)
(describe *oak-example*)