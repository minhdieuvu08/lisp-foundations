;;;; ============================================================
;;;; Chapter 5 - Problem 5-12
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun fib-recursion (n &optional (curr 0) (a 0) (b 1)) 
    (if (= curr n)
        a
        (fib-recursion n (+ curr 1) b (+ a b))
    )
)

(format t "~A~%" (fib-recursion 2))
(format t "~A~%" (fib-recursion 5))
(format t "~A~%" (fib-recursion 10))