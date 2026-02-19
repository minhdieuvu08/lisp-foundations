;;;; =================================================================
;;;; Chapter 4 - Problem 4-4
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; =================================================================


(format t "~A~%"(and (listp pi) (defparameter result 'set-in-first-and)))
;; NIL
;; this means that at least one of the expressions evaluated to NIL

;; Check '(listp pi)'
(format t "~A~%" (listp pi)) ;; NIL
;; Since "pi" is a number and not a list, '(listp pi)' evaluates to NIL.
(format t "~A~%"(and (numberp pi) (defparameter result 'set-in-second-and)))
;; RESULT
;; Since "pi" is a number, (numberp pi) evaluates to T, 
;; allowing the second expression to run 
;; so the "result" is assigned a new value.

