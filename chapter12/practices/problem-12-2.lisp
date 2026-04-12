;;;; ============================================================
;;;; Chapter 12 - Problem 12-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defmacro getq (symbol property)
    `(get ',symbol ',property))

(defmacro putq (symbol property value)
    `(setf (get ',symbol ',property) ,value))

(putq apple color 'red)
(format t "~A~%" (getq apple color))