;;; =========================================================================
;;; Chapter 6 - Data Abstraction and Mapping
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;;; =========================================================================
;;;; Data Abstraction Facilitates Progress
;;;;==========================================================================

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

;;;; =========================================================================
;;;; CONSTRUCTORS
;;;;==========================================================================

(defun make-book (title author classification)
    (list (list 'title title)
        (list 'author author)
        (list 'classification classification)))

(defparameter book-example-4 
    (make-book '(Common Lisp)
               '(Guy Steele)
               '(Technical Lisp)))

;;;; =========================================================================
;;;; READERS
;;;;==========================================================================

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

;; Recursive 
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

;;;; =========================================================================
;;;; DATABASE INITIALIZATION
;;;;==========================================================================

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

;;;; =========================================================================
;;;; TRANSFORMATION
;;;;==========================================================================
(defun list-authors (books)
    (if (endp books)
        nil
        (cons (book-author (first books))
              (list-authors (rest books)))))
(format t "~A~%" (list-authors books))

;;;; =========================================================================
;;;; FILTER
;;;;==========================================================================
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

;;;; =========================================================================
;;;; COUNT
;;;;==========================================================================
; Examples
(format t "~A~%" (length (list-fiction-books books)))
(format t "~A~%" (first (list-fiction-books books)))

; Efficient solutions that don't construct a new list
(defun count-fiction-books (books)
    (cond ((endp books) 0)
        ((fictionp (first books))
        (+ 1 (count-fiction-books (rest books))))
        (t (count-fiction-books (rest books)))))

(defun find-first-fiction-book (books)
    (cond ((endp books) nil)
        ((fictionp (first books))
        (first books))
        (t (find-first-fiction-book (rest books)))))

;;;; =========================================================================
;;;; PROGRAMMING CLICHES
;;;;==========================================================================

;; 1. Transforming template (mapping)
;; To create a new list where every element is transformed.
; (defun <transforming procedure> (input-list)
;     (if (endp input-list)
;         nil
;         (cons (<element transformer> (first input-list))
;             (<transforming procedure> (rest input-list)))))

;; 2. Filter template 
;; To create a new list containing only elements that pass a test.
; (defun <filtering procedure> (input-list)
;     (count ((endp input-list) nil)
;         ((<element tester> (first input-list))
;         (cons (first input-list)
;             (<filtering procedure> (rest input-list))))
;         (t (<filtering procedure>) (rest input-list))))

;; 3. Count template
;; To return a number representing how many elements pass a test.
; (defun <counting procedure> (input-list)
;     (cond ((endp input-list) 0)
;         ((<element tester> (first input-list))
;         (+ 1 (<counting procedure> (rest input-list))))
;         (t (<counting procedure> (rest input-list)))))

;; 4. Find template 
;; To return the first element (or nil) that pass a test.
; (defun <finding procedure> (input-list)
;     (cond ((endp input-list) nil)
;         ((<element tester> (first input-list))
;         (first input-list))
;         (t (<finding procedure> (rest input-list)))))

(format t "~A~%" (mapcar #'oddp '(1 2 3)))
(format t "~A~%" (mapcar #'= '(1 2 3) '(3 2 1)))
(format t "~A~%" (mapcar #'book-author books))
(format t "~A~%" (remove-if-not #'fictionp books))
(format t "~A~%" (remove-if #'fictionp books))
(format t "~A~%" (count-if #'fictionp books))
(format t "~A~%" (find-if #'fictionp books))

(funcall #'first '(e1 e2 e3))
(first '(e1 e2 e3))
(funcall #'append '(a b) '(x y))
(first '(e1 e2 e3))
(funcall #'append '(a b) '(x y))
(append '(a b) '(x y))

(defun toss (item procedure) (funcall procedure item))
(format t "~A~%" (toss '(victim of attack) #'first))
(format t "~A~%" (toss '(victim of attack) #'rest))
(apply #'append '((a b) (x y)))
(append '(a b) '(x y))