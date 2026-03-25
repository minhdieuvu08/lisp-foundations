;;;; ============================================================
;;;; Chapter 7 - Problem 7-8
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

; (defun recursive-member (item lst)
;     (cond ((endp lst) nil)
;         ((eq1 item (first lst)) lst)
;         (t (recursive-member item (rest lst)))))

(defun do-member (item lst)
    (do* ((sublist lst (rest sublist)))
        ((endp sublist) nil)
        (when (eql item (first sublist))
            (return sublist))))
            
(format t "~A~%" (do-member 2 '(1 2 3 4 5)))