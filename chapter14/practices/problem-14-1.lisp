;;;; ============================================================
;;;; Chapter 14 - Problem 14-1
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

;; Base classes
(defclass article ()
  ((title :accessor article-title :initarg :title)
   (author :accessor article-author :initarg :author)))

(defclass friend ()
  ((name :accessor friend-name :initarg :name)))

;; Subclasses
(defclass computer-article (article) ())
(defclass business-article (article) ())
(defclass political-article (article) ())
(defclass math-article (article) ())

(defclass philosopher-friend (friend) ())
(defclass hacker-friend (friend) ())
(defclass entrepreneur-friend (friend) ())

;; Specialized classes for the problem logic
(defclass philo-friend (philosopher-friend) ())
(defclass comp-article (computer-article) ())

(defun print-notification (article friend)
  (format t "Tell ~a about \"~a.\"~%"
            (friend-name friend)
            (article-title article)) t)

;; Default method send notices about articles for all friend
(defmethod process ((friend friend) (article article))
  (print-notification article friend))

;; Method prevent philosopher friend from rêciving notices about computer article
(defmethod process ((friend philo-friend)
                    (article comp-article))
    nil)

(defparameter *articles*
    (list (make-instance 'business-article
                            :title "Memory Prices Down")
            (make-instance 'computer-article 
                            :title "Memory Speed Up")
            (make-instance 'political-article
                            :title "Memory Impugned")))

(defparameter *friends* 
    (list (make-instance 'philo-friend :name 'Dan)
            (make-instance 'hacker-friend :name 'Gerry)
            (make-instance 'entrepreneur-friend :name 'Philip)
            (make-instance 'philo-friend :name 'David)))

(defparameter *jack* (make-instance 'philo-friend :name 'Jack))
(defparameter *comp-article* (make-instance 'comp-article :title "Machine Learning"))
(defparameter *math-article* (make-instance 'math-article :title "Calculus II"))

(format t "Test 1: Jack with Computer Article: ~A~%" (process *jack* *comp-article*))
(format t "Test 2: Jack with Math Article: ~%")
(process *jack* *math-article*)