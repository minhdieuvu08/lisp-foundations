;;; ==========================================================
;;; Chapter 3 - Procedure Definition and Binding
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; ==========================================================


(format t "Hello world~%")
(+ 1 2)

;; Notes: (defun <function-name> (<parameters>) <body>)
(defun both-ends
    (whole-list)
    (cons (first whole-list)
        (last whole-list))
)

(setf meals '(breakfast lunch tea dinner))
(cons (first meals) (last meals))
(setf route2 '(boston cambridge lincoln concord))
(cons (first route2) (last route2))
(both-ends meals)
(both-ends route2)

(defun both-ends-with-special-variable ()
    (setf whole-list
        (cons (first whole-list) (last whole-list))
    )
)

(setf whole-list '(monday tuesday wednesday thursday friday))
(both-ends-with-special-variable)

;;;; ============================================================
;;;; Chapter 3 - Problem 3-1
;;;; ============================================================
;; Swaps the first two elements of a two-element list.
(defun exchange(x)
    (list (second x) (first x))
)

;;;; ============================================================
;;;; Chapter 3 - Problem 3-3
;;;; ============================================================
;; Rotates the list to the left (e.g., (a b c) -> (b c a))
(defun rotate-left(x)
    (list (first (cons)))
)