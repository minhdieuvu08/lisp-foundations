;;;; ============================================================
;;;; Chapter 6 - Problem 6-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(defun make-book (title author classification)
    (list (list 'title title)
        (list 'author author)
        (list 'classification classification)))

(defparameter books 
    (list 
        (make-book '(artificial intelligence)
                   '(patrick henry winston)
                   '(technical ai))
        (make-book '(common lisp)
                   '(guy l steele)
                   '(technical lisp))
        (make-book '(moby dick)
                   '(herman melville)
                   '(fiction))
        (make-book '(tom sawyer)
                   '(mark twain)
                   '(fiction))
        (make-book '(tom sawyer)
                   '(mark twain)
                   '(fiction))
        (make-book '(the black orchid)
                   '(rex stout)
                   '(fiction mystery))))

(defun book-title (book)
    (second (assoc 'title book)))

(defun find-book-by-title-words (words books) 
    (cond ((endp books) nil)
        ((subsetp words (book-title (first books))) (first books))
        (t (find-book-by-title-words words (rest books))))
)

(format t "~A~%" (find-book-by-title-words '(orchid) books))
(format t "~A~%" (find-book-by-title-words '(black orchid) books))
(format t "~A~%" (find-book-by-title-words '(orchid black) books))
(format t "~A~%" (find-book-by-title-words '(blue orchid) books))