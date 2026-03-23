;;;; ============================================================
;;;; Chapter 7 - Problem 7-4
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defparameter *freezing* 27)
(defparameter *boiling* 100)

(defun list-outlyers-counts (lst)
    (let ((a 0) (b 0))
        (dolist (element lst (list a b))
            (cond ((> element *boiling*) (setf a (+ a 1)))
                ((< element *freezing*) (setf b (+ b 1)))))))

(format t "Count1 (Below, Above): ~A~%" (list-outlyers-counts '()))
(format t "Count2 (Below, Above): ~A~%" (list-outlyers-counts '(10 20 31 180 280 500)))
(format t "Count3 (Below, Above): ~A~%" (list-outlyers-counts '(30 50 60 70)))
(format t "Count4 (Below, Above): ~A~%" (list-outlyers-counts '(0 -5 10)))
