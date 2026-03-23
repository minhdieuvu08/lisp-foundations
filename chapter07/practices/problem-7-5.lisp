;;;; ============================================================
;;;; Chapter 7 - Problem 7-5
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun recursive-member (item lst)
    (cond ((endp lst) nil)
        ((eql item (first lst)) t)
        (t (recursive-member item (rest lst)))))

(defun dolist-member (item lst)
    (dolist (element lst nil)
        (when (eql item element) (return t))))

(format t "Find 8 in (10 20 8 50): ~A~%" (recursive-member 8 '(10 20 8 50)))
(format t "Find 8 in (10 20 8 50): ~A~%" (dolist-member 8 '(10 20 8 50)))