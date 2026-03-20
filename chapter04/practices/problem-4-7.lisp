;;;; ============================================================
;;;; Chapter 4 - Problem 4-7
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun check-temperature (temp)
    (cond ((> temp 100) 'ridiculously-hot)
        ((< temp 0) 'ridiculously-cold)
        (t 'ok)
    )
)
(format t "~A~%" (check-temperature 125))
(format t "~A~%" (check-temperature -15))
(format t "~A~%" (check-temperature 20))