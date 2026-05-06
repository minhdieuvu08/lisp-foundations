; ;;; ============================================================
; ;;; Chapter 17 - List Storage, Surgery and Reclamation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defparameter *abc* '(a b c))
(defparameter *xyz* '(x y z))
(defparameter *bc* (rest *abc*))
(defparameter *yz* (rest *xyz*))
(defparameter *abcxyz* (append *abc* *xyz*))
(format t "abc: ~A~%" *abc*)
(format t "xyz: ~A~%" *xyz*)
(format t "bc: ~A~%" *bc*)
(format t "yz: ~A~%" *yz*)

;; DELETE
(defparameter tosses '(heads tails tails heads tails))
(format t "Result after delete heads: ~A~%" (delete 'heads tosses))
(format t "tosses: ~A~%" tosses)

;; REMOVE
(setf tosses '(heads tails tails heads tails))
(format t "Result after remove heads: ~A~%" (remove 'heads tosses))
(format t "tosses: ~A~%" tosses)
