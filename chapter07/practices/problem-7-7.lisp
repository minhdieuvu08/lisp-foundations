;;;; ============================================================
;;;; Chapter 7 - Problem 7-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================
(defun do-factorial (n)
    (do ((temp n (- temp 1))
        (res 1 (* res temp)))
        ((zerop temp) res)))
        
(format t "~A~%" (do-factorial 3))
(format t "~A~%" (do-factorial 5))