;;;; ============================================================
;;;; Chapter 4 - Problem 4-6
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================
(defparameter u nil)
(defparameter x nil)
(defparameter y 'dog)
(defparameter z 'cat)
(defparameter a t)
(defparameter b t)
(defparameter c 'hello)
(format t "~A~%" (cond (u nil) (t t)))
(format t "~A~%" (cond (x x) (y y) (t z)))
(format t "~A~%" (cond (a (cond (b c)))))