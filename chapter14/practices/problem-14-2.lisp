;;;; ============================================================
;;;; Chapter 14 - Problem 14-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

;; Base classes
(defclass article ()
  ((title :accessor article-title :initarg :title)
   (author :accessor article-author :initarg :author)))

(defclass friend ()
  ((name :accessor friend-name :initarg :name)))

;; Subclasses
(defclass business-article (article) ())
(defclass political-article (article) ())
(defclass music-article (article) ()) 

(defclass philosopher-friend (friend) ())
(defclass philo-friend (philosopher-friend) ()) 
(defclass hacker-friend (friend) ())
(defclass entrepreneur-friend (friend) ())

(defun print-notification (article friend)
  (format t "Tell ~a about \"~a.\"~%"
            (friend-name friend)
            (article-title article)) t)

(defmethod process ((friend friend) (article music-article))
  (print-notification article friend))
  
(defmethod process ((friend friend) (article article))
  nil)

(defparameter *articles*
    (list (make-instance 'business-article
                            :title "Memory Prices Down")
            (make-instance 'political-article
                            :title "Memory Impugned")
            (make-instance 'music-article :title "Shostakovich Waltz No. 2")))

(defparameter *friends* 
    (list (make-instance 'philo-friend :name 'Dan)
            (make-instance 'hacker-friend :name 'Gerry)
            (make-instance 'entrepreneur-friend :name 'Philip)
            (make-instance 'philo-friend :name 'David)))

(dolist (f *friends*)
  (dolist (a *articles*)
    (process f a)))