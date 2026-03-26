;;;; ============================================================
;;;; Chapter 7 - Problem 7-9
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun do-reverse (lst)
    (do ((temp lst (rest temp))
        (res nil)) ;; Initialize result list 
        ((endp temp) res) ;; If temp is empty then return res
        (push (first temp) res))) ;; in the body part: push the first element of temp to res 

(format t "~A~%" (do-reverse '(A B C D)))  
(format t "~A~%" (do-reverse '(M O R N I N G)))  