;;;; ============================================================
;;;; Chapter 11 - Problem 11-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun adam (name)
    (cond ((get name 'father) (adam (get name 'father)))
        (t name)))

(setf (get 'alice 'father) 'patrick)
(setf (get 'patrick 'father) 'alex)
(setf (get 'alex 'father) 'david)

(setf (get 'a 'father) 'b)
(setf (get 'b 'father) 'a)

(format t "The oldest ancestor of Alice is: ~A~%" (adam 'alice))
;(format t "The oldest ancestor of A is: ~A~%" (adam 'a)) ;; Stack overflow

(defun adam-fix (name &optional visited)
    (let ((father (get name 'father)))
        (cond ((null father) name)
            ((member father visited) name)
            (t (adam-fix father (cons name visited))))))
(format t "The oldest ancestor of Alice is: ~A~%" (adam-fix 'alice))
(format t "The oldest ancestor of A is: ~A~%" (adam-fix 'a))
