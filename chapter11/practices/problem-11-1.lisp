;;;; ============================================================
;;;; Chapter 11 - Problem 11-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun grandfather (name)
    (get (get name 'father) 'father))

(setf (get 'patrick 'father) 'robert)
(setf (get 'robert 'father) 'thomas)
(setf (get 'stanley 'father) 'albert)

(format t "Test 1 (Grandfather is known): ~A~%" (grandfather 'patrick))
(format t "Test 2 (Father is known, Grandfather is unknown): ~A~%" (grandfather 'stanley))
(format t "Test 3 (Father is unknown): ~A~%" (grandfather 'abc))