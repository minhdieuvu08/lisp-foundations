;;;; ============================================================
;;;; Chapter 17 - Problem 17-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun user-defined-nreverse (lst)
    (if (endp lst)
        nil
        (let ((prev nil) (curr lst) (next nil))
            (loop
                (when (null curr) ( return prev))

                (setf next (cdr curr))       
                (setf (cdr curr) prev)
                
                (setf prev curr)
                (setf curr next)))
        ))

(defparameter *lst01* '(h e l l o))
(format t "~A~%" (user-defined-nreverse *lst01*))