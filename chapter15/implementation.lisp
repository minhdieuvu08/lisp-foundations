; ;;; ============================================================
; ;;; Chapter 15 - Lexical Variables Generators and Encapsulation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;; Example
(defun both-ends-with-let (whole-list)
    (let ((leading (first whole-list))
            (trailing (first (last whole-list)))
        (list leading trailing))))

(format t "List (2 4 5 9 12): ~a" (both-ends-with-let '(2 4 5 9 12)))
(defun both-ends-with-funny-let (whole-list)
    (let (leading trailing)
        (setf leading (first whole-list))
        (setf trailing (first (last whole-list)))
        (list leading trailing)))

(defun both-ends-with-helpers (whole-list)
    (list (front-end whole-list) (back-end whole-list)))

(defun front-end (m) (first m))
(defun back-end (n) (first (last n)))

(defun first-of-first-with-labels (lst)
    (labels ((aux (m) (first m)))
        (first (aux lst))))

(format t "~A~%" (first-of-first-with-labels '((a b) (c d))))

(defun first-of-first-with-funny-labels (lst)
    (labels ((aux () (first lst)))
        (first (aux))))

(defun inside (x e)
    (labels 
        ((inside-aux (e)
            (cond ((atom e) (eq x e))
                    ((endp e) nil)
                    (t (or (inside-aux (first e))
                            (inside-aux (rest e)))))))
        (insdie-aux e)))

(defparameter previous-power-of-two 1)
(defun previous-power-of-two ()
    (setf previous-power-of-two (* previous-power-of-two 2)))
(format t "~A~%" (power-of-two))