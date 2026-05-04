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
