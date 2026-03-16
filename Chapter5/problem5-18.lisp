;;;; ============================================================
;;;; Chapter 5 - Problem 5-18
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun user-defined-last (lst)
    ;; If rest of list is empty then at that time list only has 1 element
    (if (endp (rest lst)) 
        lst ; return last element

        ;; else: continue find last element in the rest of list
        (user-defined-last (rest lst))))

(format t "~A~%" (user-defined-last '()))
(format t "~A~%" (user-defined-last '(A B C)))
(format t "~A~%" (user-defined-last '((A B) (C D))))