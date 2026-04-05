;;;; ============================================================
;;;; Chapter 9 - Problem 9-2
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

(format t "Enter the path to your file: ~%")
(finish-output)

(let ((file-path (read-line)))
    (with-open-file (patient-stream file-path :direction :input)
        (do ((record (read patient-stream nil 'eof) ;; Initialize -> Read first form
                    (read patient-stream nil 'eof))) ;; Read next forms
            ((eq record 'eof) (format t "Done! This is the end of the file. ~%"))
            (format t "~A~%" record))))