;;; =========================================================================
;;; Chapter 7 - Iteration on Numbers and Lists
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;;; =========================================================================
;;;; DOTIMES
;;;;==========================================================================
;; Notes:
;; (dotimes (<count parameter> <upper-bound form> <result form>) <body>)
;; It is similar to 'for (i=0; i<n; i++)' in C/C++ but it always increments by 1.

(defun dotimes-expt (m n)
    (let ((res 1))
    (dotimes (count n res)
        (setf res (* m res)))))

(format t "~A~%" (dotimes-expt 10 0))
(format t "~A~%" (dotimes-expt 2 1))
(format t "~A~%" (dotimes-expt 2 3))

;;;; =========================================================================
;;;; DOLIST
;;;;==========================================================================
;; Notes:
;; (dolist (<element parameter> <list form> <result form>) <body>)

; Example
(defparameter *freezing* 32)
(defparameter *boiling* 212)

(defun count-outlyers (list-of-elements)
    (let ((result 0))
        (dolist (element list-of-elements result)
            (when (or (> element *boiling*) (< element *freezing*))
                (setf result (+ result 1))))))
(format t "Number of outlyers: ~A~%" (count-outlyers '(18 75 31 180 270 52)))

(defun first-n-outlyers (n list-of-elements)
    (let ((result 0) (outlyers nil))
        (dolist (element list-of-elements outlyers)
            (cond ((or (> element *boiling*) (< element *freezing*))
                    (setf result (+ result 1))
                    (push element outlyers))
                    ((= n result) (return outlyers))))))
(format t "Outlyers (n=3): ~A~%" (first-n-outlyers 3 '(18 75 31 180 270 52)))
(format t "Outlyers (n=2): ~A~%" (first-n-outlyers 2 '(35 64 88 100)))
(format t "Outlyers (n=10): ~A~%" (first-n-outlyers 10 '(15 28 64 101)))

(defun do-expt (m n)
    (do* ((result 1) (exponent n)) ((zerop exponent) result)
    (setf result (* m result))
    (setf exponent (- exponent 1))))

(defun do-expt (m n)
    (do* ((result m (* m result)) (exponent n (- exponent 1))
        (counter (- exponent 1) (- exponent 1)))
        ((zerop counter) result)))

(defun do-expt (m n)
    (do* ((result 1 (* m result)) (exponent n (- exponent 1)))
        ((zerop exponent) result)))

(defun do-expt (m n)
    (do* ((result m (* m result))
    (counter (- exponent 1) (- exponent 1)))
    ((zerop counter) result)))

(defun do-expt (m n)
    (do* ((result m (* m result))
        (exponent n (- exponent 1))
        (counter (- n 1) (- counter 1)))
        ((zerop counter) result)))
;;;; =========================================================================
;;;; LOOP
;;;;==========================================================================
(setf cheers '(cheer cheer cheer))
(setf loop-count 0)
(loop 
    (when (endp cheers) (return loop-count))
    (setf cheers (rest cheers))
    (setf loop-count (+ loop-count 1)))

;;;; =========================================================================
;;;; PROG1 and PROGN
;;;;==========================================================================
(format t "~A~%" (prog1 (setf a 'x) (setf b 'y) (setf c 'z)))
(format t "~A~%" (progn (setf a 'x) (setf b 'y) (setf c 'z)))