;;;; ============================================================
;;;; Chapter 7 - Problem 7-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================
(defun dotimes-factorial (n)
    (let ((res 1))
        (dotimes (i n res)
            (setf res (* res (+ i 1))))))

(format t "~A~%" (dotimes-factorial 0))
(format t "~A~%" (dotimes-factorial 3))
(format t "~A~%" (dotimes-factorial 5))

;; Recursive approach
(defun recursive-factorial (n)
    (cond ((= n 0) 1) 
        (t (* n (recursive-factorial(- n 1))))))

(format t "~A~%" (recursive-factorial 0))
(format t "~A~%" (recursive-factorial 3))
(format t "~A~%" (recursive-factorial 5))