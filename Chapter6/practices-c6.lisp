;;; =========================================================================
;;; Chapter 6 - Data Abstraction and Mapping
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

(defun book-author (book) (second book))

(defparameter book-example-1
    '((Aritficial Intelligence) ;; Title
    (Patrick Henry Winston) ;; Author
    (Technical AI) ;; Classification
    )
)
(format t "~A~%" (book-author book-example-1))
(format t "~A~%" (second book-example-1))

(defparameter book-example-2 
    '((title (Artificial Intelligence))
    (author (Patrick Henry Winston))
    (classification (Technical AI))))

(format t "~A~%" (assoc 'author book-example-1))
(format t "~A~%" (second (assoc 'author book-example-2)))

(defun book-author (book) 
    (second (assoc 'author (first book)))
)

(defparameter book-example-3 
    '(((title (Artificial Intelligence))
    (author (Patrick Henry Winston))
    (classification (Technical AI)))
    ((loaned-to (Karen Predergast))
    (loaned-on (26 May 88)))))
(format t "~A~%" (book-author book-example-3))

(defun make-book (title author classification)
    (list (list 'title title)
        (list 'author author)
        (list 'classification classification)))

(defparameter book-example-4 
    (make-book '(Common Lisp)
               '(Guy Steele)
               '(Technical Lisp)))
(defun book-title (book)
    (second (assoc 'title book)))
(defun book-author (book)
    (second (assoc 'author book)))
(defun book-classification (book)
    (second (assoc 'classification book)))

(defun book-author-writer (book author)
    (cons (list 'author author) book))

(setf book-example-4
    (book-author-writer book-example-4 '(Guy L Steeve))
)

(format t "~A~%" (book-author book-example-4))
(defun book-author-writer (book author)
    (if (eql 'author (first (first book)))
        (cons (list 'author author) (rest book))
        (cons (first book)
              (book-author-writer(rest book) author))))
(defparameter book-example-4 
                (book-author-writer book-example-4 '(Guy L Steele)))
(format t "~A~%" (book-author book-example-4))

(defun book-author-writer (book author)
    (cond ((null book) nil)
        ((eql 'author (first (first book)))
        (cons (list 'author author) (rest book)))
        (t (cons (first book) 
            (book-author-writer (rest book) author)))))

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

(defun list-authors (books)
    (if (endp books)
        nil
        (cons (book-author (first books))
              (list-authors (rest books)))))
(format t "~A~%" (list-authors books))

(defun fictionp (book)
    (member 'fiction (book-classification book)))
(format t "~A~%" (fictionp '((title (tom sawyer))
                            (author (mark twain))
                            (classification (fiction)))))
(format t "~A~%" (fictionp '((title (:common-lisp))
                            (author (guy l steele))
                            (classification (technical lisp)))))

(defun list-fiction-books (books)
    (cond ((endp books) nil)
        ((fictionp (first books))
        (cons (first books)
              (list-fiction-books (rest books))))
        (t (list-fiction-books (rest books)))))

(format t "~A~%" (list-fiction-books books))

(format t "~A~%" (length (list-fiction-books books)))
(format t "~A~%" (first (list-fiction-books books)))

(defun count-fiction-books (books)
    (cond ((endp books) 0)
        (fictionp (first books))
        (+ 1 (count-fiction-books (rest books))))
        (t (count-fiction-books (rest books))))

(defun find-first-fiction-book (books)
    (cond ((endp books) nil)
        ((fictionp (first books))
        (first books))
        (t (find-first-fiction-book (rest books)))))