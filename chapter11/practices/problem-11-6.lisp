;;;; ============================================================
;;;; Chapter 11 - Problem 11-6
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun static-value (checker-board)
    (let ((result 0))
        (dotimes (i (array-dimension checker-board 0)) ;; For rows
            (dotimes (j (array-dimension checker-board 1)) ;; For columns
                (let ((piece (aref checker-board i j)))
                    (cond ((eq piece 'w) (setf result (+ result 1)))
                        ((eq piece 'w-king) (setf result (+ result 2)))
                        ((eq piece 'b) (setf result (- result 1)))
                        ((eq piece 'b-king) (setf result (- result 2)))))))
    result))

(defparameter *board01* (make-array '(8 8)
                                 :initial-contents
                                 '((X B X B X B X B)
                                   (B X B X B X B X)
                                   (X B X B X B X B)
                                   (E X W-KING X E X E X)
                                   (X E X E X E X E)
                                   (W X W X W X W X)
                                   (X W X W X B-KING X W)
                                   (W X W X W X W X))))

(format t "~A~%" (static-value *board01*))

(defparameter *board02* (make-array '(8 8)
                                 :initial-contents
                                 '((X B X B X B X B)
                                   (B X B X B X B X)
                                   (X B X B X B X B)
                                   (E X E X E X E X)
                                   (X E X E X E X E)
                                   (W X W X W X W X)
                                   (X W X W X W X W)
                                   (W X W X W X W X))))
(format t "~A~%" (static-value *board02*))
