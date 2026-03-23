;;;; ============================================================
;;;; Chapter 7 - Problem 7-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defparameter *freezing* 27)
(defparameter *boiling* 100)

(defun outlyer (x)
    (or (> x *boiling*) (< x *freezing*)))

; (defun list-outlyers (lst)
;     (remove-if-not #'outlyer lst))

(defun list-outlyers (lst)
    (let ((res nil))
        (dolist (element lst res)
            (when (outlyer element)
                (setf res (append res (list element)))))))
(format t "Outlyers: ~A~%" (list-outlyers '(10 20 30 120 200)))
(format t "Outlyers: ~A~%" (list-outlyers '(28 30 40 80)))