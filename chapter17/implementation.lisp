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

(format t "~A~%" (delete '(heads tails) '((alpha omega) (heads tails) (zenith nadir))))
(format t "~A~%" (delete '(heads tails) '((alpha omega) (heads tails) (zenith nadir)) :test #'equal))
(format t "~A~%" (remove '(heads tails) '((alpha omega) (heads tails) (zenith nadir))))

(format t "~A~%" (remove '(heads tails) 
                            '((alpha omega) (heads tails) (zenith nadir)) 
                            :test #'equal))

(defparameter *fact1* '(big computers are nice))
(setf (first *fact1*) 'fast)
(format t "~A~%" *fact1*)

(defparameter *fact2* '(apple are good for you))
(setf (rest *fact2*) '(were bad for adam))
(format t "fact2 ~A~%" *fact2*)

;;(defparameter *fact3* '(circle))
;;(setf (rest *fact3*) *fact3*)
;;(format t "fact3 ~A~%" *fact3*)

(defparameter *l1* (list 'a 'b 'c))
(defparameter *l2* (list 'a 'b 'c))
(defparameter *l3* *l2*)
