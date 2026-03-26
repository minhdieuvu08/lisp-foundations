;;;; ============================================================
;;;; Chapter 9 - Problem 9-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun tower-of-hanoi (disks from to spare)
    (if (endp disks)
        nil ; base case
        (progn 
            ; Move n-1 disks from "from" to "sparse"
            (tower-of-hanoi (rest disks) from spare to)

            ; Move the current largest disk to "to"
            (format t "Move ~A from ~A to ~A ~%" (first disks) from to)
            
            ; Move n-1 disk back to "to"
            (tower-of-hanoi (rest disks) spare to from))))

(format t "~A~%" (tower-of-hanoi '(3 2 1) 'a 'b 'c))