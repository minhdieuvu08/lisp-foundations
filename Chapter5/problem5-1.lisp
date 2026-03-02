;;;; ============================================================
;;;; Chapter 5 - Problem 5-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun skip-first-n (x lst)
    (if (or (zerop x) (endp lst)) 
    lst
    (skip-first-n (- x 1) (rest lst)))
)

(format t "~A~%" (skip-first-n 3 '(a b c d e f g h i)))
 