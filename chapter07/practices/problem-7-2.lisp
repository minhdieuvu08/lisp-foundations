;;;; ============================================================
;;;; Chapter 7 - Problem 7-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defparameter *freezing* 27)
(defparameter *boiling* 100)

(defun outlyer (x)
    (or (> x *boiling*) (< x *freezing*)))

(defun count-outlyers-with-count-if (lst)
    (count-if #'outlyer lst))
(format t "Number of outlyers: ~A~%" (count-outlyers-with-count-if '(18 20 31 101 270)))
