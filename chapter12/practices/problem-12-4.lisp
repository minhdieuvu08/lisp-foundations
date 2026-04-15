;;;; ============================================================
;;;; Chapter 12 - Problem 12-4
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defmacro letq (var-value &body body)
    ;; Use 'mapcar' to transform each pair into (variable 'value)
    `(let ,(mapcar #'(lambda (pair) 
                        (list (first pair) (list 'quote (second pair)))) var-value)
        
        ,@body))

(format t "~A~%" (letq ((x (+ 6 12))) (print x)))
(format t "~A~%" (letq ((y "Book")) (print y)))