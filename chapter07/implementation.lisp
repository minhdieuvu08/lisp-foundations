;;; =========================================================================
;;; Chapter 7 - Iteration on Numbers and Lists
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;;; =========================================================================
;;;; DOTIMES
;;;;==========================================================================
;; Notes:
;; (dotimes <(count parameter> <upper-bound form> <result form> <body>)
(defun dotimes-expt (m n)
    (let ((res 1))
    (dotimes (count n res)
        (setf res (* m res)))))

(format t "~A~%" (dotimes-expt 10 0))
(format t "~A~%" (dotimes-expt 2 1))
(format t "~A~%" (dotimes-expt 2 3))