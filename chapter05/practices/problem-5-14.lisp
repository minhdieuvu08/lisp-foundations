;;;; ============================================================
;;;; Chapter 5 - Problem 5-14
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

; The idea is that we use 'res' to store the first element of list 'lst'
; then recursive on the rest list until 'lst' is null.

(defun tail-recursive-reverse (lst &optional res)
    (if (null lst)
        res
    (tail-recursive-reverse (rest lst) (cons (first lst) res))))

(format t "Empty list: ~A~%" (tail-recursive-reverse '())) 
(format t "Single element: ~A~%" (tail-recursive-reverse '(A))) 
(format t "Standard list: ~A~%" (tail-recursive-reverse '(A C B D E))) 
(format t "Nested list: ~A~%" (tail-recursive-reverse '(A (B C) D)))