; ;;; ============================================================
; ;;; Chapter 19 - Problem 19.2
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defun extend (path)
    (print (reverse path))
    (mapcar #'(lambda (new-node) (cons new-node path))
            (remove-if #'(lambda (neighbor) (member neighbor path))
                        (get (first path) 'neighbors))))

(defun depth-first (start finish &optional (path (list start)))
    (cond ((eq finish (first (first path))) 
            (reverse (first path))) ;
            (t (depth-first 
                    start 
                    finish
                    (append (extend (first queue))
                            (rest queue))))))

(format t "~a~%" (depth-first 's 'f))