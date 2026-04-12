;;;; ============================================================
;;;; Chapter 12 - Problem 12-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defmacro when-nil (trigger result)
    `(when (not ,trigger) ,result))

(defparameter pressure -10)
(format t "~A~%" (when-nil (plusp pressure) (print 'low-pressure)))