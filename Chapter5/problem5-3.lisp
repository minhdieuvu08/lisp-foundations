;;;; ============================================================
;;;; Chapter 5 - Problem 5-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun keep-first-n-cleverly-aux (n lst result)
    (if (or (zerop n) (endp lst))
        (reverse result)
        (keep-first-n-cleverly-aux (- n 1) (rest lst) (cons (first lst) result))))

(defun keep-first-n-cleverly (n lst) 
    (keep-first-n-cleverly-aux n lst nil))

(format t "~A~%" (keep-first-n-cleverly 3 '(a b c d e f g h)))
(format t "~A~%" (keep-first-n-cleverly 2 '(a b)))