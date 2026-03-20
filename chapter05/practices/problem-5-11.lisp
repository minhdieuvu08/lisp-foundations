;;;; ============================================================
;;;; Chapter 5 - Problem 5-11
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

; (defun punctuate (lst) (append lst '(period)))
; (defun punctuate (lst mark) (append lst (list mark)))

(defun punctuate (lst &optional (mark 'period))
    (append lst (list mark)))

(format t "~A~%" (punctuate '(this is an example)))
(format t "~A~%" (punctuate '(is this an example) 'question-mark))