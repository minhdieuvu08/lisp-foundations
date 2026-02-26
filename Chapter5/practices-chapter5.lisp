;;; ============================================================
;;; Chapter 5 - Procedure Abstraction and Recursion
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; ============================================================

;; Procedure Abstraction
(defun both-ends (whole-list)
    (combine-elements
        (extract-first-element whole-list)
        (extract-last-element whole-list)
    )
)

(defun combine-elements (e1 e2) (list e1 e2))
(defun extract-first-element (lst) (first lst))
(defun extract-last-element (lst) (first (last lst)))

(defun combine-elements (e1 e2) (cons e1 (cons e2 nil)))
(defun extract-first-element (lst) (car lst))
(defun extract-last-element (lst) (first (reverse lst)))

(defun higher-level-expt (m n)
    (if (zerop n)
        1
        (* m (lower-level-expt m (- n 1)))
    )
)
(defun lower-level-expt (m n)
    (higher-level-expt m n))
    
;; Recursion
(defun recursive-expt (m n)
    (if (zerop n)
        1
        (* m (recursive-expt m (- n 1)))
    )
)
(format t "~A~%" (recursive-expt 2 3))
(format t "~A~%" (recursive-expt 10 0))

(defun fibonacci (n)
    (if (or (= n 0) (= n 1))
        1
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
    )
)
(format t "~A~%" (fibonacci 4))

(defun count-elements (lst)
    (if (endp lst)
        0
        (+ 1 (count-elements (rest lst)))
    )
)
(format t "~A~%" (count-elements '(fast computers are nice)))
(format t "~A~%" (count-elements '(dog cat cow)))
(format t "~A~%" (count-elements '()))

(defun count-elements-cleverly-aux (lst result)
    (if (endp lst)
        result
        (count-elements-cleverly-aux (rest lst) (+ 1 result))))
(format t "~A~%" (count-elements-cleverly-aux '(a (b c)) 0))

(defun count-elements-cleverly (lst) 
    (count-elements-cleverly-aux lst 0))
(format t "~A~%" (count-elements-cleverly '()))
(format t "~A~%" (count-elements-cleverly '(lisp is very cool)))
(format t "~A~%" (count-elements-cleverly '(a (b c) d)))

(defun count-elements-buffer (lst result)
    (count-elements-indirectly lst result))


(defun count-elements-indirectly (lst result)
    (if (endp lst)
        result
        (count-elements-buffer (rest lst) (+ 1 result))
    )
)
(defun count-elements-mutually (lst)
    (count-elements-indirectly lst 0))
(format t "~A~%" (count-elements-mutually '()))
(format t "~A~%" (count-elements-mutually '(computer mouse keyboard)))

(defun count-atoms (list)
    (cond ((null list) 0)
        ((atom list) 1)
        (t (+ (count-atoms (first list))
            (count-atoms (rest list))))
    )
)
(format t "~A~%" (count-atoms '(sqrt (expt x 2) (expt y 2))))
