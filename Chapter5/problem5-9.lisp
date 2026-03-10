;;;; ============================================================
;;;; Chapter 5 - Problem 5-9
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun squash (lst)
    (cond ((null lst) nil)
        ((atom lst) (list lst)) 
        (t (append (squash (first lst)) (squash (rest lst)))))
)
(format t "~A~%" (squash '(a (a (a (a b))) (((a b) b) b))))
(format t "~A~%" (squash '(a (b (c (a (a b))) (((a b) b) c) (c (b a))))))