; ;;; ============================================================
; ;;; Chapter 17 - List Storage, Surgery and Reclamation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defparameter *abc* '(a b c))
(defparameter *xyz* '(x y z))
(defparameter *bc* (rest *abc*))
(defparameter *yz* (rest *xyz*))
(defparameter *abcxyz* (append *abc* *xyz*))
(format t "abc: ~A~%" *abc*)
(format t "xyz: ~A~%" *xyz*)
(format t "bc: ~A~%" *bc*)
(format t "yz: ~A~%" *yz*)

;; DELETE
(defparameter tosses '(heads tails tails heads tails))
(format t "Result after delete heads: ~A~%" (delete 'heads tosses))
(format t "tosses: ~A~%" tosses)

;; REMOVE
(setf tosses '(heads tails tails heads tails))
(format t "Result after remove heads: ~A~%" (remove 'heads tosses))
(format t "tosses: ~A~%" tosses)

(format t "~A~%" (delete '(heads tails) '((alpha omega) (heads tails) (zenith nadir))))
(format t "~A~%" (delete '(heads tails) '((alpha omega) (heads tails) (zenith nadir)) :test #'equal))
(format t "~A~%" (remove '(heads tails) '((alpha omega) (heads tails) (zenith nadir))))

(format t "~A~%" (remove '(heads tails) 
                            '((alpha omega) (heads tails) (zenith nadir)) 
                            :test #'equal))

(defparameter *fact1* '(big computers are nice))
(setf (first *fact1*) 'fast)
(format t "~A~%" *fact1*)

(defparameter *fact2* '(apple are good for you))
(setf (rest *fact2*) '(were bad for adam))
(format t "fact2 ~A~%" *fact2*)

;;(defparameter *fact3* '(circle))
;;(setf (rest *fact3*) *fact3*)
;;(format t "fact3 ~A~%" *fact3*)

(defparameter *l1* (list 'a 'b 'c))
(defparameter *l2* (list 'a 'b 'c))
(defparameter *l3* *l2*)

(defparameter example2 (list 'a 'b 'c))
(defparameter example2 (list 'x 'y 'z))

(defun user-reverse (lst)
    (if (endp lst)
        nil
        (append (user-reverse (rest lst))
                (list (first lst)))))

(defun user-reverse (lst &optional result)
    (if (endp lst)
        result
        (user-reverse (rest lst)
                        (cons (first lst) result))))

(defun liberate1 (s)
    (cond ((eq s 'man) 'person)
            ((atom s) s)
            (t (cons (liberate1 (first s))
                    (liberate1 (rest s))))))

(defun liberate2 (s)
    (cond ((eq s 'man) 'person)
            ((atom s) s)
            ((eq (first s) 'man)
            (setf (first s) 'person)
            (liberate2 (rest s)))
            (t (liberate2 (first s))
                (liberate2 (rest s))
                s)))

(defparameter *test* '(chair man))
(format t "liberate1 example: ~A~%" (liberate1 *test*))
(format t "test: ~A~%" *test*)

(format t "liberate2 example: ~A~%" (liberate2 *test*))
(format t "test: ~A~%" *test*)

(defparameter *memory* (make-array 40 :initial-contents
    '(free     unmarked nil      4   ;Elements 0-3.
      free     unmarked nil      8   ;Elements 4-7.
      free     unmarked nil      12  ;Elements 8-11.
      free     unmarked nil      16  ;Elements 12-15.
      free     unmarked nil      20  ;Elements 16-19.
      free     unmarked nil      24  ;Elements 20-23.
      free     unmarked nil      28  ;Elements 24-27.
      free     unmarked nil      32  ;Elements 28-31.
      free     unmarked nil      36  ;Elements 32-35.
      free     unmarked nil      nil ;Elements 36-39.\
    )))

;; 'next-free-chunk' is the address of the first element of the first free chunk.
;; 'last-free-chunk' is the address of the first element of the last free chunk.
(defparameter *next-free-chunk* 0)
(defparameter *last-free-chunk* 36)

(format t "memory: ~A~%" *memory*)
(format t "next free chunk: ~A~%" *next-free-chunk*)
(format t "last free chunk: ~A~%" *last-free-chunk*)

(setf (aref *memory* 0) 'symbol)
(setf (aref *memory* 1) 'unmarked)
(setf (aref *memory* 2) 'x)
(setf *next-free-chunk* 4)
(format t "~%memory: ~A~%" *memory*)

;; Mark
(defun place-marks (index)
    (when (numberp index)
        (unless (eq 'marked (aref *memory* (+ 1 index)))
            (setf (aref *memory* (+ 1 index)) 'marked)
            (when (eq 'box (aref *memory* index))
                (place-marks (aref *memory* (+ 2 index)))
                (place-marks (aref *memory* (+ 3 index)))))))

(defun mark ()
    (do ((index 0 (+ 4 index)))
        ((= index (array-dimension *memory* 0)))
        (when (and (eq 'symbol (aref *memory* index))
                    (not (eq 'unbound (aref *memory* (+ 3 index)))))
            (setf (aref *memory* (+ 1 index)) 'marked)
            (place-marks (aref *memory* (+ 3 index))))))

(mark)
(format t "~%memory after mark: ~A~%" *memory*)

;;  SWEEP
(defun sweep ()
    (do ((index 0 (+ 4 index)))
        ;; Stop casconditione: When the index reaches the end of the memory array.
        ((= index (array-dimension *memory* 0)))

        ;; If the box is not free and not marked,
        (unless (or (eq 'free (aref *memory* index))
                    (eq 'marked (aref *memory* (+ 1 index))))
            
            ;; First, set the box to free and clear its pointers (car/cdr)
            (setf (aref *memory* index) 'free)
            (setf (aref *memory* (+ 2 index)) nil)
            (setf (aref *memory* (+ 3 index)) nil)

            ;; Second, update the free list
            (setf (aref *memory* (+ 3 *last-free-chunk*)) index)
            (setf *last-free-chunk* index))
        
        ;; Finally, set the status of all index to unmarked
        (setf (aref *memory* (+ 1 index)) 'unmarked)))
(sweep)
(format t "~%memory after sweep: ~A~%" *memory*)
