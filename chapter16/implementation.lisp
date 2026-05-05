; ;;; ============================================================
; ;;; Chapter 16 - Special Variable
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defvar *lst*)
(defun first-of-first1 (*lst*)
    (first (auxiliary1 *lst*)))

(defun auxiliary1 (*lst*)
    (first *lst*))

(defun first-of-first2 (*lst*)
    (first (auxiliary2)))

(defun auxiliary2 ()
    (first *lst*))

; (format t "~A~%" (first-of-first2 '((a b) (c d)))) 
; ERROR

(format t "~A~%" (first-of-first2 '((a b) (c d))))

(defvar *x*)
(defun inside (*x* e)
    (inside-aux e))

(defun inside-aux (e)
    (cond ((atom e) (eq *x* e))
        (t (or (inside-aux (first e))
                (inside-aux (rest e))))))

(format t "~A~%" (inside 'a '(a b)))

(defun contains (e *x*)
    (inside-aux e))

(defun inside (*x* e)
    (inside-aux e))

(defun inside-aux (e)
    (cond ((atom e) (eq *x* e))
        (t (or (inside-aux (first e))
            (inside-aux (rest e))))))

;; Example 1
(defvar *interest-rate* .07)
(format t "Interest rate 1: ~A~%" *interest-rate*)

;; In example 2, I changed the 'interest-rate' from 0.07 to 0.08 
;; but the result is still 0.07 
;; This demonstrates that 'defvar' only assigns a value to a variable if it is currently none.
(defvar *interest-rate* .08)
(format t "Interest rate 2: ~A~%" *interest-rate*)

;; In this example, I don't 'defvar *mpg*'
;; but in first function, 'mpg' is a special variable
;; so that why the function won't work
; (defun compute-distance (gallons)
;     (* mpg gallons))

; (defun time-custom (mpg gallons speed)
;     (/ (compute-distance gallons) speed))

; (format t "~A~%" (time-custom 25 10 50)) ;; ERROR

(defvar *mpg*)
(defun compute-distance (gallons)
    (* *mpg* gallons))

(defun time-custom (*mpg* gallons speed)
    (/ (compute-distance gallons) speed))

(format t "Time: ~A~%" (time-custom 25 10 50))