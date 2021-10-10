;;;;matrix.lisp

(in-package :seria)

;;;;------------------------------------------------------------------------
;;;;Tone-row Matrices
;;;;-----------------------------------------------------------------------

(defclass tone-matrix ()
  ((prime-row :initarg :prime-row
	      :accessor prime-row)
   (rows      :initarg :rows
	      :accessor rows)))

(defmethod print-object ((obj tone-matrix) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((prime-row prime-row)
		     (rows rows))
	obj
      (format stream "~%Prime:~%~a~%~%Matrix:~%~{~a~%~}" (tone-list prime-row) rows))))
;~%~{~a~%~}
(defun prime-order (row)
  "Generates a prime-order for building the matrix (y axis of matrix)."
  (mapcar #'(lambda (n)
	      (if (zerop n) 0
		  (- 12 n)))
	  (loop for n in (tone-list row)
		collect (interval (first (tone-list row)) n))))

(defun build-matrix (row)
  (make-instance 'tone-matrix :prime-row row
			      :rows (mapcar #'(lambda (n)
						(transpose-row (tone-list row) n))
					    (prime-order row))))

  



						 
