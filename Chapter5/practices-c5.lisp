;;; =========================================================================
;;; Chapter 5 - Procedure Abstraction and Recursion
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

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

;;;; =========================================================================
;;;; Optional Parameters
;;;; =========================================================================
(format t "~A~%" (root 9))
(format t "~A~%" (root 9 2))
(format t "~A~%" (root 27 3))

;; Notes:
; &OPTIONAL: Parameter that don't require an argument.
; If it not provided, the program default to NIL or a specified value.
(defun root (x &optional n)
    (if n
        ; If n is not NIL
        (expt x (/ 1 n)) 
        ; If n is NIL
        (sqrt x))
)
(format t "~A~%" (root 9))
(format t "~A~%" (root 9 2))
(format t "~A~%" (root 27 3))

(defun root (x &optional (n 2))
    (expt x (/ 1 n))
)
(format t "~A~%" (root 9))
(format t "~A~%" (root 9 2))
(format t "~A~%" (root 27 3))

(defun count-with-optional-parameter (lst &optional (result 0))
    (if (endp lst)
        ; Base case: if list is empty, return the accumulated total.
        result
        ; 1. Move to the next sub-list
        ; 2. Increase 'result' by 1 
        ; Continue recursion with updated values 
        (count-with-optional-parameter (rest lst) (+ 1 result))
    )
)

(format t "~A~%" (raise 2))
(format t "~A~%" (raise 2 3))
(format t "~A~%" (raise 2 3 5))

;;;; =========================================================================
;;;; Rest Parameters
;;;; =========================================================================
;; Notes:
; &REST: Collects all remaining arugments into a single list.
(defun raise-aux (result number-list)
    "Raises result to each exponent in number-list using tail recursion."
    (if (endp number-list)
        result
        (raise-aux (expt result (first number-list))
                    (rest number-list)
        )
    )
)

(defun raise (x &rest numbers)
    (raise-aux x numbers)
)
(format t "~A~%" (rotate-list '(a b c d e)))
(format t "~A~%" (rotate-list '(a b c d e) :direction 'left))
(format t "~A~%" (rotate-list '(a b c d e) :distance 2))
(format t "~A~%" (rotate-list '(a b c d e) :distance 'left :distance 2))

(defun rotate-list-right (l n)
    (if (zerop n)
        l
        (rotate-list-right (append (last l) (butlast l)) (-n 1))
    )
)

(defun rotate-list-left (l n)
    (if (zerop n)
        l
        (rotate-list-left (append (rest l) (list (first l))) (- n 1))
    )
)
;;;; =========================================================================
;;;; Key Parameters
;;;;==========================================================================
;; Notes:
; '&key' defines named, optional parameters with default values.
; ':direction' is a symbol to decide the logic branch (left/right).
; ':distance' is a number specifying how many times to rotate (defaults to 1). 
(defun rotate-list (l &key direction distance)
    (if (eq direction 'left)
        ; Call left rotation if ':direction' is 'left'
        (rotate-list-left l (if distance distance 1))
        ; Otherwise, call right rotation
        (rotate-list-right l (if distance distance 1))
    )
)

(format t "~A~%" (rotate-list '(a b c d e) :direction 'left :distance 2))
(format t "~A~%" (rotate-list '(a b c d e) :distance 2 :direction 'left))

; Rewrite
(defun rotate-list (l &key direction (distance 1))
    (if (eq direction 'left)
        (rotate-list-left l distance)
        (rotate-list-right l distance)
    )
)

(defun both-ends-with-let (whole-list)
    (let* ((element (first whole-list))
            (trailer (last whole-list)))
        (cons element trailer)
    )
)

;;;; =========================================================================
;;;; Aux Parameters
;;;;==========================================================================
;; Notes:
; &aux used to bind values to variable for use inside the function body,
; similar to 'let*'

(defun both-ends-with-aux
    (whole-list &aux
        (element (first whole-list))
        (trailer (last whole-list))
    )
    (cons element trailer)
)

(defun user-defined-length (l)
    (if (endp l)
        0
        (+ 1 (user-defined-length (rest l)))
    )
)

(defun user-defined-append2 (l1 l2)
    (if (endp l1)
        l2
        (cons (first l1)
            (user-defined-append2 (rest l1) l2)
        )
    )
)

(defun append-aux (lists)
    (if (endp lists)
        nil
        (user-defined-append2 (first lists)
                            (append-aux (rest lists))
        )
    )
)

(defun user-defined-append (&rest lists)
    (append-aux lists)
)