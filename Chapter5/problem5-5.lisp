;;;; ============================================================
;;;; Chapter 5 - Problem 5-5
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun tower-of-hanoi (n)
    (if (null n) 
        0
    ;; Move n-1 disk to C -> T(n-1) steps
    ;; Move the biggest disk to B -> 1 step
    ;; Move n-1 disk from C to B -> T(n-1) steps
        (+ 1 (* 2 (tower-of-hanoi(rest n)))))
)

(format t "~A~%" (tower-of-hanoi '(1)))
(format t "~A~%" (tower-of-hanoi '(2 1)))
(format t "~A~%" (tower-of-hanoi '(3 2 1)))
(format t "~A~%" (tower-of-hanoi '(4 3 2 1)))
(format t "~A~%" (tower-of-hanoi '(5 4 3 2 1)))
(format t "~A~%" (tower-of-hanoi '(10 9 8 7 6 5 4 3 2 1)))