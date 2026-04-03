;;; =========================================================================
;;; Chapter 11 - Properties and Arrays
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;;; =========================================================================
;;;; GET, DEFPARAMETER and SETF
;;;; =========================================================================
(get 'patrick 'parent)
(format t "~A~%" (setf (get 'patrick 'parents) '(robert dorothy)))
(format t "~A~%" (setf (get 'bag 'contents) '(bread butter)))
(format t "~A~%" (get 'bag 'contents))
(format t "~A~%" (remprop 'bag 'contents))

(defparameter *part-bins* (make-array 4))
(defparameter *checker-board* (make-array '(8 8)))

; (setf *part-bins* (make-array '(4)))
; (setf *checker-board* (make-array '(8 8)))
(format t "~A~%" (setf *part-bins* (make-array 4)))
(format t "~A~%" (setf *part-bins* (make-array 4 :initial-element 'e)))
(format t "~A~%" (setf checker-board (make-array '(8 8)
                                                :initial-contents
                                                '((X B X B X B X B)
                                                (B X B X B X B X)
                                                (X B X B X B X B)
                                                (E X E X E X E X)
                                                (X E X E X E X E)
                                                (W X W X W X W X)
                                                (X W X W X W X W)
                                                (W X W X W X W X)))))
(format t "~A~%" (aref part-bins 0))
(format t "~A~%" (aref part-bins 3))