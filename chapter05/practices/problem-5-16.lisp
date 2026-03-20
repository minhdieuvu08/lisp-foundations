;;;; ============================================================
;;;; Chapter 5 - Problem 5-16
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun user-defined-list (&rest x) x)

(format t "~A~%" (user-defined-list 'A))
(format t "~A~%" (user-defined-list 'H 'E 'L 'L 'O))
(format t "~A~%" (user-defined-list 1 '(2 3) "hi"))