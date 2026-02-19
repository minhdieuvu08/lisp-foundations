;;;; ============================================================
;;;; Chapter 4 - Problem 4-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun DIVISIBLE-BY-THREE(x)
    (=(rem x 3)0))

(format t "Is 6 divisible by 3? ~A~%" (DIVISIBLE-BY-THREE 6))
(format t "Is 7 divisible by 3? ~A~%" (DIVISIBLE-BY-THREE 7))
(format t "Is 8 divisible by 3? ~A~%" (DIVISIBLE-BY-THREE 8))
