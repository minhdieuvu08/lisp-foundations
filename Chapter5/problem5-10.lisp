;;;; ============================================================
;;;; Chapter 5 - Problem 5-10
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun fib (n)
    (let* ((phi (/ (+ 1 (sqrt 5)) 2))
        (psi (/ (- 1 (sqrt 5)) 2))
        (res (/ (- (expt phi n) (expt psi n)) (sqrt 5))))
    (round res)
    )
)

(defun fib-recursion (n)
    (if (< n 2)
        n
        (+ (fib-recursion (- n 1)) (fib-recursion (- n 2)))
    )
)

(format t "n=2: ~A~%" (fib 2))
(format t "n=5: ~A~%" (fib 5))
(format t "n=10: ~A~%" (fib 10))
(format t "n=20: ~A~%" (fib 20))

(format t "Fibonacci Recursion ~%")
(format t "n=2: ~A~%" (fib-recursion 2))
(format t "n=5: ~A~%" (fib-recursion 5))
(format t "n=10: ~A~%" (fib-recursion 10))
(format t "n=20: ~A~%" (fib-recursion 20))