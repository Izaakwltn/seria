;;;;standardization.lisp

(in-package :seria)

;;;;------------------------------------------------------------------------
;;;;Standardization
;;;;------------------------------------------------------------------------
(defun standard-row (row)
    "Conforms the row to traditional '(0 1 2 3 4 5 6 7 8 9 t e) notation."
    (cond ((null row) nil)
          ((equal (first row) 11) (cons 't (standard-row (rest row))))
          ((equal (first row) 12) (cons 'e (standard-row (rest row))))
          (t (cons (transpose-tone (first row) -1)
                   (standard-row (rest row))))))

(defun standard-matrix (row)
  (make-instance 'tone-matrix :prime-row row
			      :rows (mapcar #'standard-row (mapcar #'(lambda (n)
						(transpose-row (tone-list row) n))
					    (prime-order row)))))
