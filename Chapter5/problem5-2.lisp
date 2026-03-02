;;;; ============================================================
;;;; Chapter 5 - Problem 5-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun keep-first-n (x lst)
    (if (zerop x)
        nil
        (cons (first lst)
            (keep-first-n (- x 1) (rest lst))))
)

(format t "~A~%" (keep-first-n 3 '(a b c d e f g h i)))
 