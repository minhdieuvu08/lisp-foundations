; ;;; ============================================================
; ;;; Chapter 25 - Streams and Delayed Evaluation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defun stream-endp (stream) (eq stream 'empty-stream))
(defun stream-first (stream) (first stream))
(defun stream-rest (stream) (second stream))
(defun stream-cons (object stream) (list object stream))

(defun stream-append (stream1 stream2)
    (if (stream-endp stream1)
        stream2
        (stream-cons (stream-first stream1)
                     (stream-append (stream-rest stream1) stream2))))

(defun stream-concatenate (streams)
    (if (stream-endp streams) 'empty-stream
        (if (stream-endp (stream-first streams))
            (stream-concatenate (stream-rest streams))
            (stream-cons (stream-first (stream-first streams))
                         (stream-concatenate
                            (stream-cons (stream-rest (stream-first streams))
                                         (stream-rest streams)))))))

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