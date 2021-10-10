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

;;;;------------------------------------------------------------------------
;;;;Manipulations- Prime, Retrograde, Inverse, Inverse-retrograde
;;;;
;;;;------------------------------------------------------------------------

(defun prime (n row)
  "Takes root and row, returns transposed P-n prime."
  (make-row (transpose-row row (interval (first row) n))))

(defun retrograde (state n row)
  "Takes root and row, returns R-n retrograde."
  (make-row state (reverse (transpose-row row (interval (nth (- (length row) 1) row) n)))))


	     
