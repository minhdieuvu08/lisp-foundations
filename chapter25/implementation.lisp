; ;;; ============================================================
; ;;; Chapter 25 - Streams and Delayed Evaluation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;; ------------------------------------------------------------
;;; 1. Eager streams: representation and operations
;;; ------------------------------------------------------------

(defun stream-endp (stream)
    "Return T when STREAM is the EMPTY-STREAM sentinel, NIL otherwise." (eq stream 'empty-stream))
(defun stream-first (stream)
    "Return the first element of a nonempty STREAM." (first stream))
(defun stream-rest (stream)
    "Return the stored tail of a nonempty eager STREAM." (second stream))
(defun stream-cons (object stream)
    "Return an eager stream node containing OBJECT and tail STREAM.
  Both arguments are evaluated before this function is called." (list object stream))

(defun stream-append (stream1 stream2)
    "Append STREAM2 to STREAM1, copying the nodes of STREAM1.
    With the eager stream constructor used here, recursion builds the
    entire copied prefix before returning. An empty STREAM1 returns STREAM2."
    (if (stream-endp stream1)
        stream2
        (stream-cons (stream-first stream1)
                     (stream-append (stream-rest stream1) stream2))))

(defun stream-concatenate (streams)
    "Flatten STREAMS, a stream of streams, into one stream in order.
    Empty member streams are skipped. An empty STREAMS returns EMPTY-STREAM."
    (if (stream-endp streams) 'empty-stream
        (if (stream-endp (stream-first streams))
            (stream-concatenate (stream-rest streams))
            (stream-cons (stream-first (stream-first streams))
                         (stream-concatenate
                            (stream-cons (stream-rest (stream-first streams))
                                         (stream-rest streams)))))))

(defun stream-transform (procedure stream)
    "Return a stream of the results of calling PROCEDURE on each element.
    Element order is preserved; an empty STREAM returns EMPTY-STREAM."
    (if (stream-endp stream)
        'empty-stream
        (stream-cons (funcall procedure (stream-first stream))
                     (stream-transform procedure (stream-rest stream)))))

(defun stream-member (object stream)
    "Return T if OBJECT is EQUAL to an element of STREAM, NIL otherwise.
    Search stops at the first match or at EMPTY-STREAM."
    (cond ((stream-endp stream) nil)
          ((equal object (stream-first stream)) t)
          (t (stream-member object (stream-rest stream
          )))))

(defmacro stream-remember (object variable)
    "Append OBJECT to the stream stored in VARIABLE unless already present.
    Membership is tested with EQUAL. When absent, update VARIABLE with SETF
    and return OBJECT; otherwise return NIL. The expansion may evaluate
    OBJECT and VARIABLE more than once."
    `(unless (stream-member ,object ,variable)
        (setf ,variable
              (stream-append ,variable 
                            (stream-cons ,object
                                         `empty-stream)))
        ,object))

;;; ------------------------------------------------------------
;;; 2. Eager stream examples: append, concatenate, transform, and remember
;;; ------------------------------------------------------------

(defparameter *stream1*
    (stream-cons 'object-a
                 (stream-cons 'object-b 'empty-stream)))

(defparameter *stream2*
    (stream-cons 'object-x
                 (stream-cons 'object-y 'empty-stream)))

(format t "Stream 1: ~A~%" *stream1*)
(format t "Stream 2: ~A~%" *stream2*)
(format t "Appended: ~A~%" (stream-append *stream1* *stream2*))

(defparameter *stream-of-streams*  
                (stream-cons *stream1*
                             (stream-cons *stream2* 'empty-stream)))
(format t "Stream-of-streams: ~A~%" *stream-of-streams*)
(format t "Concatenate: ~A~%" (stream-concatenate *stream-of-streams*))

(defparameter *number-stream*
                (stream-cons 2 (stream-cons 3 'empty-stream)))

(format t "Transform: ~A~%" (stream-transform #'(lambda (number) (expt 2 number)) *number-stream*))

(defparameter *long-stream* (stream-append *stream1* *stream2*))
(format t "Long stream: ~A~%" *long-stream*)
(format t "Stream remember: ~A~%" (stream-remember 'last-object *long-stream*))
(format t "Long stream: ~A~%" *long-stream*)
(format t "Stream remember: ~A~%" (stream-remember 'last-object *long-stream*))

;;; ------------------------------------------------------------
;;; 3. Delaying computation with lexical closures
;;; ------------------------------------------------------------

; ;; ERROR
; (defun skimmer (n)
;     `(expt n 2))

; ;;ERROR
; (defun thinker (form)
;     (eval form))

; (format t "Skimmer (data form): ~A~%" (skimmer 2))
; (format t "Thinker (evaluated form): ~A~%" (thinker (skimmer 2)))

(defun skimmer (n)
    "Return a zero-argument closure that computes the square of N.
    The closure captures N and delays the computation until it is called."
    #'(lambda () (expt n 2)))

(defun thinker (procedure)
    "Call the zero-argument PROCEDURE and return its result."
    (funcall procedure))

(format t "Skimmer (closure): ~A~%" (skimmer 2))
(format t "Thinker (invoking closure): ~A~%" (thinker (skimmer 2)))

;;; ------------------------------------------------------------
;;; 4. Delayed stream tails: construction and manual evaluation
;;; ------------------------------------------------------------

(format t "~%--- SECTION 2: TRUE STREAMS WITH DELAYED EVALUATION ---~%")
(defmacro stream-cons (object stream)
    "Construct a stream node with OBJECT and a delayed tail expression STREAM.
    OBJECT is evaluated immediately; STREAM is enclosed in a zero-argument
    closure and evaluated each time that closure is called."
    `(list ,object #'(lambda () ,stream)))
   
(defparameter *power-of-two-stream* 
              (stream-cons (expt 2 2)
                           (stream-cons (expt 2 3)
                                        'empty-stream)))
(format t "Raw stream structure: ~A~%" *power-of-two-stream*) 
(format t "Unpacking 2nd node manually: ~A~%" 
        (funcall (second *power-of-two-stream*)))
(format t "Unpacking 3rd node manually: ~A~%" 
        (funcall (second (funcall (second *power-of-two-stream*)))))

;;; ------------------------------------------------------------
;;; 5. Accessing delayed tails and advancing through a stream
;;; ------------------------------------------------------------

(defun stream-rest (stream)
    "Evaluate and return the delayed tail of a nonempty STREAM.
  The tail closure runs on every call; its result is not cached."
  (funcall (second stream)))

;; 1. Read while the stream is still at the first node (before SETF).
(format t "First element: ~A~%" (stream-first *power-of-two-stream*))
(format t "Second element: ~A~%" (stream-first (stream-rest *power-of-two-stream*)))

;; 2. Advance one step to the node containing 8.
(setf *power-of-two-stream* (stream-rest *power-of-two-stream*))
(format t "After advancing 1 step (current first): ~A~%" (stream-first *power-of-two-stream*))

;; 3. Advance a second step to reach the empty tail (EMPTY-STREAM).
(setf *power-of-two-stream* (stream-rest *power-of-two-stream*))
(format t "After advancing 2 steps (tail reached): ~A~%" *power-of-two-stream*)
(format t "Is stream ended? ~A~%" (stream-endp *power-of-two-stream*))

;;; ------------------------------------------------------------
;;; 6. Encapsulate and expose: an abstraction for delayed evaluation
;;; ------------------------------------------------------------

(defun stream-endp (stream)
    "Return T when STREAM is the EMPTY-STREAM sentinel, NIL otherwise." (eq stream 'empty-stream))
(defun stream-first (stream)
    "Return the first element of a nonempty STREAM." (first stream))
(defmacro encapsulate (form)
    "Wrap FORM in a zero-argument closure without evaluating it yet.
  The closure retains lexical bindings and evaluates FORM each time called."
  `#'(lambda () ,form))

(defun expose (procedure)
    "Call an encapsulated zero-argument PROCEDURE and return its result.
  Repeated calls repeat the computation rather than returning a cached value."
  (funcall procedure))
(defun stream-rest (stream)
    "Expose and return the delayed tail of a nonempty STREAM.
  Each call evaluates the tail again; no result is cached." (expose (second stream)))
(defmacro stream-cons (object stream)
    "Construct a stream node with OBJECT and an encapsulated tail STREAM.
    OBJECT is evaluated immediately; ENCAPSULATE delays evaluation of STREAM
    until the tail is exposed. Tail results are not cached."
    `(list ,object (encapsulate ,stream)))

;;; ------------------------------------------------------------
;;; 7. Example using encapsulate and expose
;;; ------------------------------------------------------------

(defparameter *clean-stream*
  (stream-cons 100 (stream-cons 200 'empty-stream)))

(format t "Clean stream first: ~A~%" (stream-first *clean-stream*))
(format t "Clean stream second: ~A~%" (stream-first (stream-rest *clean-stream*)))
