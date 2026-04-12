;;;; ============================================================
;;;; Chapter 12 - Problem 12-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================
(defmacro put (symbol value property)
    `(setf (get ,symbol ,property) ,value))

(format t "~A~%" (put 'thing 'blue 'color))