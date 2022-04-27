;;;;standardization.lisp

(in-package :seria)

;;;;------------------------------------------------------------------------
;;;;Standardization
;;;;------------------------------------------------------------------------
(defmethod standard-tone (tone)
  (cond ((equal tone 11) 't)
	((equal tone 12) 'e)
	(t (transpose-tone tone -1))))

(defmethod standardize ((row row))
    "Conforms the row to traditional '(0 1 2 3 4 5 6 7 8 9 t e) notation."
  (loop :for i :in (tone-list row)
	:collect (standard-tone i)))

(defmethod standard-matrix ((row row))
  (make-instance 'tone-matrix :prime-row row
			      :rows (mapcar #'standardize (mapcar #'(lambda (n)
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
