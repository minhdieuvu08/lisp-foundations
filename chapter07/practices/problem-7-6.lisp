;;;; ============================================================
;;;; Chapter 7 - Problem 7-6
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun dolist-reverse (lst)
    (let ((res nil))
        (dolist (element lst res)
            (push element res))))
(format t "Reverse of (15 30 45 60): ~A~%" (dolist-reverse '(15 30 45 60)))