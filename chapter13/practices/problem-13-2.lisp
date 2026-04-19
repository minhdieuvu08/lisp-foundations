;;;; Chapter 13 - Problem 13-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defstruct rock 
    (color 'gray)
    (size 'pebble)
    (worth nil))

(defparameter *high-hopes* (make-rock :color 'gold :worth 'high))

(format t "~A~%" *high-hopes*)