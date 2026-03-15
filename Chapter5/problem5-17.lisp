;;;; ============================================================
;;;; Chapter 5 - Problem 5-17
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun user-defined-nthcdr (n lst)
    (if (zerop n) 
        lst
        (user-defined-nthcdr (- n 1) (rest lst))))

(format t "~A~%" (user-defined-nthcdr 3 '(M O R N I N G)))
(format t "~A~%" (user-defined-nthcdr 4 '(N O O N)))