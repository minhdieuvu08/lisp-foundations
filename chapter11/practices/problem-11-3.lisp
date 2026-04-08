;;;; ============================================================
;;;; Chapter 11 - Problem 11-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun ancestors (person)
    (if (null person) nil
        (let ((father (get person 'father)) 
                (mother (get person 'mother)))
            (append (list person) (ancestors father) (ancestors mother)))))

(setf (get 'john 'father) 'david)
(setf (get 'john 'mother) 'mary)
(setf (get 'alice 'father) 'john)
(setf (get 'alice 'mother) 'mia)

(format t "Ancestors of John: ~A~%" (ancestors 'john))
(format t "Ancestors of Alice: ~A~%" (ancestors 'alice))