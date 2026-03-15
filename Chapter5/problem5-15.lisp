;;;; ============================================================
;;;; Chapter 5 - Problem 5-15
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

; (defun count-atoms (list)
;     ; If list is null then number of atoms is 0
;     (cond ((null list) 0)
;         ; If it's a single atom then return 1
;         ((atom list) 1) 

;         (t (+ (count-atoms (first list)) ; Recursively for first element in list
;             (count-atoms (rest list)))) ; then add them to the count of the rest of the list
;     )
; )

;;; Tail Recursion
(defun clever-count-atoms (list &optional (count 0))
    (format t "State: ~8S | count = ~A~%" list count)

    (cond ((null list) count)
        ((atom list) (+ count 1))
        (t (clever-count-atoms (rest list) (clever-count-atoms (first list) count)))))

(format t "~A~%" (clever-count-atoms '((A) B)))
(format t "~A~%" (clever-count-atoms '((A B) C)))
(format t "~A~%" (clever-count-atoms '((A B) (C D))))