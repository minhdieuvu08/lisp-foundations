; ;;; ============================================================
; ;;; Chapter 19 - Classes and Generic Functions
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(setf (get 's 'neighbors) '(a d)
        (get 'a 'neighbors) '(s b d)
        (get 'b 'neighbors) '(a c e)
        (get 'c 'neighbors) '(b)
        (get 'd 'neighbors) '(s a e)
        (get 'e 'neighbors) '(b d f)
        (get 'f 'neigbors) '(e))

(defun extend (path)
    (print (reverse path))
    (mapcar #'(lambda (new-node) (cons new-node path))
            (remove-if #'(lambda (neighbor) (member neighbor path))
                        (get (first path) 'neighbors))))

(defun depth-first (start finish &optional (queue (list (list start))))
    (cond ((endp queue) nil)
            ((eq finish (first (first queue)))
            (reverse (first queue)))
            (t (depth-first 
                    start 
                    finish
                    (append (extend (first queue))
                            (rest queue))))))

(format t "~a~%" (depth-first 's 'f))
