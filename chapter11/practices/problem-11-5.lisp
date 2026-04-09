;;;; ============================================================
;;;; Chapter 11 - Problem 11-5
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun distance (c1 c2)
    (let* ((p1 (get c1 'position))
            (p2 (get c2 'position))
            (dx (- (car p1) (car p2)))
            (dy (- (cadr p1) (cadr p2))))
        (sqrt (+ (expt dx 2) (expt dy 2)))))

(setf (get 'hanoi 'position) '(1 0))
(setf (get 'haiphong 'position) '(3 4))
(format t "Distance between HaNoi and HaiPhong: ~A~%" (distance 'hanoi 'haiphong))