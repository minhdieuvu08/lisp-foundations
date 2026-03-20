;;;; ============================================================
;;;; Chapter 5 - Problem 5-13
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

; (defun recursive-expt (m n)
;     if (zerop n)
;     1
;     (* m (recursive-expt m (- n 1)))
; )

(defun tail-recursive-expt (m n &optional (temp 1))
    (if (zerop n)
        temp
        (tail-recursive-expt m (- n 1) (* m temp))
    )
)

(format t "~A~%" (tail-recursive-expt 2 4))
(format t "~A~%" (tail-recursive-expt 10 2))
(format t "~A~%" (tail-recursive-expt 5 3))
