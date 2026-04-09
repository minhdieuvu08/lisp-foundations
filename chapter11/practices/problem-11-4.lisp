;;;; ============================================================
;;;; Chapter 11 - Problem 11-4
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

;; Use 'member' to check if a connection already exists.
;; if not, add each city into the other's neighbors using 'cons'.
(defun connect (c1 c2)
    (cond ((member c2 (get c1 'neighbors)) nil)
        (t (setf (get c1 'neighbors) (cons c2 (get c1 'neighbors)))
            (setf (get c2 'neighbors) (cons c1 (get c2 'neighbors)))
            t)))

(format t "Test 1: ~A~%" (connect 'hanoi 'saigon))
(format t "Hanoi neighbors: ~A~%" (get 'hanoi 'neighbors))
(format t "Saigon neighbors: ~A~%" (get 'saigon 'neighbors))

(format t "Test 2: ~A~%" (connect 'saigon 'quynhon))
(format t "Saigon neighbors: ~A~%" (get 'saigon 'neighbors))
(format t "Quynhon neighbors: ~A~%" (get 'quynhon 'neighbors))

(format t "Test 3:~%")
(format t "Hanoi neighbors: ~A~%" (get 'hanoi 'neighbors))
(format t "Quynhon neighbors: ~A~%" (get 'quynhon 'neighbors))
