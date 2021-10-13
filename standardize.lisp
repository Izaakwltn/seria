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

;;;;------------------------------------------------------------------------
;;;;Create a matrix using traditional note-names
;;;;------------------------------------------------------------------------

(alexandria:define-constant num-note '((1 C)
		                      (2 C#/Db)
		                      (3 D)
		                      (4 D#/Eb)
		                      (5 E)
		                      (6 F)
		                      (7 F#/Gb)
		                      (8 G)
		                      (9 G#/Ab)
		                      (10 A)
		                      (11 A#/Bb)
		                      (12 B)) :test 'equal)

(defun note-row (row)
 (make-instance 'row :state 'Prime
		      :root (second (assoc (first (tone-list row)) num-note))
		      :tone-list (mapcar #'(lambda (ton)
					     (second (assoc ton num-note)))
					 (tone-list row))))

(defun note-tone-list (row-tones)
  (mapcar #'(lambda (ton)
	      (second (assoc ton num-note)))
        row-tones))

(defun note-matrix (row)
  (make-instance 'tone-matrix :prime-row (note-tone-list (tone-list row))
		              :rows (loop for r in (rows (build-matrix row))
					  collect (note-tone-list r) into note-rows
					  finally (return note-rows))))
