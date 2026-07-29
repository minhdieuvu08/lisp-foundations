; ;;; ============================================================
; ;;; Chapter 19 - Examples involving Search
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defun extend (path)
    (print (reverse path))
    (mapcar #'(lambda (new-node) (cons new-node path))
            (remove-if #'(lambda (neighbor) (member neighbor path))
                        (get (first path) 'neighbors))))

(defun depth-first (start finish &optional (queue (list (list start))))
    (do* ((temp-q queue (append (extend (first temp-q)) (rest temp-q))))
        ((endp temp-q) nil)
        (when (eq finish (first (first temp-q)))
            (return (reverse (first temp-q))))))

(setf (get 's 'neighbors) '(a d)
        (get 'a 'neighbors) '(s b d)
        (get 'b 'neighbors) '(a c e)
        (get 'c 'neighbors) '(b)
        (get 'd 'neighbors) '(s a e)
        (get 'e 'neighbors) '(b d f)
        (get 'f 'neighbors) '(e))

(format t "~a~%" (depth-first 's 'f))