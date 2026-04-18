;;;; ============================================================
;;;; Chapter 12 - Problem 12-6
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun punctuate (lst &rest symbols)
    (append lst symbols))

(format t "~A~%" (punctuate '(this is an example) 'so 'to 'speak))