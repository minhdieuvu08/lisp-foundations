;;;; =================================================================
;;;; Chapter 4 - Problem 4-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; =================================================================


(defun RIGHTP(a b c)
    ; 'let' is similar to 'defvar', 
    ; but it establishes local bindings only within the scope of the function.
    (let (h a)
        ; Calculate the sum of the squares of all sides
        (s (+ (* a a) (* b b ) (* c c)))) 

    ; Find the longest side
    ; if b is greater than h, set h to b
    (if (> b h) (setf h b)) 
    ; if c is greater than h, set h to c
    (if (> c h) (setf h c)) 

    ; Calculate the square of the hypotenuse
    (let ((h-2 (* h h)) 
        (sum-h (- s (*h h))))) ; The sum of the squares of the other two sides

)