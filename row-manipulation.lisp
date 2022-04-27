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

(defgeneric transpose (object interval)
  (:documentation "Transposes an object by a given interval"))

(defmethod transpose ((row row) interval)
  "Transposes an entire tone-row by a specified interval."
  (loop :for i :in (tone-list row)
	:collect (transpose-tone i interval)))

(defmethod interval-list ((row row))
  (loop :for n :in (tone-list row)
        :collect (interval (first (tone-list row)) n)))

;;;;------------------------------------------------------------------------
;;;;Manipulations- Prime, Retrograde, Inverse, Inverse-retrograde
;;;;
;;;;------------------------------------------------------------------------

(defmethod prime ((row row) root)
  "Takes root and row, returns transposed P-n prime."
  (make-row 'prime (transpose row (interval (first (tone-list row)) root))))

(defmethod retrograde ((row row) root)
  "Takes root and row, returns R-n retrograde."
  (make-row 'retrograde (reverse (tone-list (prime row root)))))

(defmethod find-nth-interval ((row row) root)
  "Finds n in tone-row, returns numeric spot."
  (if (equal n (first row))
      (- 12 (length row)))
      (find-nth-interval n (rest row)))

(defmethod inverse ((row row) root)
  "Takes root and row, returns I-n inverse."
  (make-row 'inverse (mapcar #'(lambda (matrix-row)
               (nth (find-nth-interval n (tone-list row))
                    matrix-row))
	  (rows (build-matrix row)))))

(defmethod retrograde-inverse ((row row) root)
  "Takes root and row, returns RI-n Retrograde Inverse"
  (make-row 'retrograde-inverse (reverse (tone-list (inverse n row)))))


	     
