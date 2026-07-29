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
        (get 'f 'neighbors) '(e))

(defun extend (path)
    (print (reverse path))
    (mapcar #'(lambda (new-node) (cons new-node path))
            (remove-if #'(lambda (neighbor) (member neighbor path))
                        (get (first path) 'neighbors))))

(defun depth-first (start finish &optional (queue (list (list start))))
    (cond ((endp queue) nil) ;; Check if queue is empty.
            ((eq finish (first (first queue))) ;; If the finish node is found,
            (reverse (first queue))) ;; then return the path.
            (t (depth-first ;; Otherwise, expand the path and recurse.
                    start 
                    finish
                    (append (extend (first queue))
                            (rest queue))))))

(format t "~a~%" (depth-first 's 'f))

(defun breadth-first (start finish &optional (queue (list (list start))))
        (cond ((endp queue) nil)
                ((eq finish (first (first queue)))
                        (reverse (first queue)))
                (t (breadth-first start finish (append (rest queue) (extend (first queue)))))))

(format t "BFS result: ~A~%" (breadth-first 's 'f))

(format t "Sort (3 1 4 1 5 9): ~A~%" (sort '(3 1 4 1 5 9) #'<))
(setf pi-front '(3 1 4 1 5 6))
(format t "Sort pi-front: ~A~%" (sort pi-front #'<))

(setf pi-front '(3 1 4 1 5 6))
(format t "Sort pi-front (copy list): ~A~%" (sort (copy-list pi-front) #'<))

(defun best-first (start finish &optional (queue (list (list start))))
        (cond ((endp queue) nil)
                ((eq finish (first (first queue)))
                        (reverse (first queue)))
                (t (best-first start finish (sort (append (extend (first queue)) (rest queue))
                                                        #'(lambda (p1 p2) (closerp p1 p2 finish)))))))

(setf (get 's 'coordinates) '(0 3)
        (get 'a 'coordinates) '(4 6)
        (get 'b 'coordinates) '(7 6)
        (get 'c 'coordinates) '(11 6)
        (get 'd 'coordinates) '(3 0)
        (get 'e 'coordinates) '(6 0)
        (get 'f 'coordinates) '(11 3))

(defun straight-line-distance (node-1 node-2)
        (let ((coordinates-1 (get node-1 'coordinates))
                (coordinates-2 (get node-2 'coordinates)))
                (sqrt (+ (expt (- (first coordinates-1) (first coordinates-2)) 2)
                        (expt (- (second coordinates-1) (second coordinates-2)) 2)))))

(defun closerp (path-1 path-2 target-node)
        (< (straight-line-distance (first path-1) target-node)
                (straight-line-distance (first path-2) target-node)))

(format t "~A~%" (best-first 's 'f))
(format t "~A~%" (merge 'list '(1 3 5 7 9) '(0 2 4 6 8) #'<))
(format t "~A~%" (merge 'list "13579" '(#\0 #\2 #\4 #\6 #\8) #'char<))
(format t "~A~%" (merge 'string "13579" '(#\0 #\2 #\4 #\6 #\8) #'char<))