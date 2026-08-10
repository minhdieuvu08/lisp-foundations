;;;; ============================================================
;;;; Chapter 17 - Problem 17-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defmacro enqueue (item queue)
    `(if (null ,queue)
        (setf ,queue (list ,item))
        (progn 
            (setf (rest (last ,queue)) (list ,item))
            ,queue)))

(defmacro dequeue (queue)
    `(if (null ,queue)
        (setf ,queue (list ,item))
        (progn
            (setf (rest (last ,queue)) (list ,item))
            ,queue)))