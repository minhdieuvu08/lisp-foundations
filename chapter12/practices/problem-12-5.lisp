;;;; ============================================================
;;;; Chapter 12 - Problem 12-5
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

;; The problem is that they want transform 'define' into 'defun' 
;; For example: (define (square x) (+ x x))
;; to: (defun square (x) (+ x x))

;; Breakdown:
;; (name &rest params): Extract the function name and the rest of the list as parameters.
;; then using '&body' to capture the forms as the function's body
(defmacro define ((name &rest params) &body body)
    `(defun ,name ,params ,@body))

(format t "~A~%" (define (square x) (+ x x)))