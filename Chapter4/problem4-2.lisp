;;;; ============================================================
;;;; Chapter 4 - Problem 4-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun PALINDROMEP(lst)
    (equal (reverse lst) lst))

(format t "Check if the list (a b c b a) is a palindrome: ~A~%" (PALINDROMEP '(a b c b a)))
(format t "Check if the list (a b c c b a) is a palindrome: ~A~%" (PALINDROMEP '(a b c c b a)))
(format t "Check if the list (a b c c c a) is a palindrome: ~A~%" (PALINDROMEP '(a b c c c a)))

