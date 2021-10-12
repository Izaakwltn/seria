;;;;row-manipulation.lisp

(in-package :seria)

;;;;------------------------------------------------------------------------
;;;;Transposition Functions
;;;;------------------------------------------------------------------------

(defun interval (tone1 tone2)
    "Determines the interval in between two tones"
    (cond ((> 0 (- tone2 tone1)) (+ 12 (- tone2 tone1)))
          (t (- tone2 tone1))))

(defun transpose-tone (tone interval)
    "Transposes a tone by a specified interval"
    (cond ((> (+ tone interval) 12) 
           (- (+ tone interval) 12))
          ((< (+ tone interval) 0)
           (+ (+ tone interval) 12))
          (t (+ tone interval))))

(defun transpose-row (row interval)
    "Transposes an entire tone-row by a specified interval."
    (cond ((null row) nil)
          (t (cons (transpose-tone (first row) interval) 
		   (transpose-row (rest row) interval)))))

(defun interval-list (row)
  (loop for n in (tone-list row)
		collect (interval (first (tone-list row)) n)))

;;;;------------------------------------------------------------------------
;;;;Manipulations- Prime, Retrograde, Inverse, Inverse-retrograde
;;;;
;;;;------------------------------------------------------------------------

(defun prime (n row)
  "Takes root and row, returns transposed P-n prime."
  (make-row 'prime (transpose-row (tone-list row) (interval (first (tone-list row)) n))))

(defun retrograde (n row)
  "Takes root and row, returns R-n retrograde."
  (make-row 'retrograde (reverse (tone-list (prime n row)))))

(defun find-nth-interval (n row)
        "Finds n in tone-row, returns numeric spot."
        (cond ((equal n (first row)) (- 12 (length row)))
              (t (find-nth-interval n (rest row)))))

(defun inverse (n row)
  "Takes root and row, returns I-n inverse."
  (make-row 'inverse (mapcar #'(lambda (matrix-row)
               (nth (find-nth-interval n (tone-list row))
                    matrix-row))
	  (rows (build-matrix row)))))

(defun retrograde-inverse (n row)
  "Takes root and row, returns RI-n Retrograde Inverse"
  (make-row 'retrograde-inverse (reverse (tone-list (inverse n row)))))


	     
