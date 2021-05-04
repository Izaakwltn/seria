;;;--------------------------------------------------------------------------
;;Seria.lisp
;;;--------------------------------------------------------------------------
;;;;A Module for Exploring Twelve-Tone Serialism in Common Lisp
;;;--------------------------------------------------------------------------
;;;--------------------------------------------------------------------------

    ;;1. Randomized Tone Row Generator
    ;;2. Matrix Building
    ;;3. Matrix Operations (P, I, R, RI)
    ;;4. Transposition functions
    ;;5. Standardization procedures
    ;;6. Print-friendly procedures

(setf tones '(1 2 3 4 5 6 7 8 9 10 11 12))

;;;--------------------------------------------------------------------------
;;;1. Tone Row Generator
;;;--------------------------------------------------------------------------

(defun row-generator ()
    "Random tone-row generator."
    (setf sample-row '())
    (row-generation sample-row tones))
    
(defun random-tone (tone-list)
    "Generates one random tone out of a list of tones."
    (elt tone-list (random (length tone-list))))

(defun row-generation (sample-row tone-list)
    "The backend of row-generator, builds a row without duplicates."
    (cond ((equal (length sample-row) (length tone-list)) sample-row)
          (t (push (random-tone (set-difference tone-list sample-row)) sample-row)
             (row-generation sample-row tone-list))))

(defun test-row () 
    "Makes a random row and assigns it to variable TEST-ROW."
    (setf test-row (row-generator)))

;;;--------------------------------------------------------------------------
;;;2. Matrix Building
;;;--------------------------------------------------------------------------

(defun tone-matrix (row)
    "Builds a matrix from a specified row."
    (mapcar #'(lambda (n)
                   (transpose-row row n)) (prime-order row)))

(defun prime-order (row)
    "Generates a prime-order for building the matrix (y axis of matrix)."
          (mapcar #'(lambda (n)
                (cond ((zerop n) 0 )
                      ((- 12 n))))(interval-list (first row) row)))

(defun interval-list (row)
    "Generates list of intervals for the whole row, starting with 0
    (X axis of matrix)."
    (interval-list-backend (first row) row))

(defun interval-list-backend (tone row)
    (cond ((null row) nil)
          (t (cons (interval tone (first row)) 
                   (interval-list tone (rest row))))))

;;;--------------------------------------------------------------------------
;;;3. Matrix Operations- Prime, Retrograde, Inverse, Inverse-retrograde
;;;--------------------------------------------------------------------------

(defun prime (n row)
    "Takes interval, row, returns P-n prime"
    (transpose-row row n))

(defun retrograde (n row)
    "Takes interval and row, returns R-n retrograde"
    (reverse (prime n row)))

(defun inverse (n row)
    "Takes interval and row, returns I-n inverse"
    (mapcar #'(lambda (matrix-row)
               (nth (find-nth-interval n (interval-list (first test-row) test-row))
                     matrix-row)) (tone-matrix row)))

    (defun find-nth-interval (n row)
        "Finds n in interval-list, returns numeric spot."
        (cond ((equal n (first row)) (- 12 (length row)))
              (t (find-nth-value n (rest row))))) 

(defun r-inverse (n row)
    "Takes interval and row, returns RI-n Retrograde Inverse"
    (reverse (inverse n row)))
;;;--------------------------------------------------------------------------
;;;4. Transposition Functions
;;;-------------------------------------------------------------------------- 

(defun transpose-row (row interval)
    "Transposes an entire tone-row by a specified interval."
    (cond ((null row) nil)
          (t (cons (transpose-tone (first row) interval) 
             (transpose-row (rest row) interval)))))

(defun transpose-tone (tone interval)
    "Transposes a tone by a specified interval"
    (cond ((> (+ tone interval) 12) 
           (- (+ tone interval) 12))
          ((< (+ tone interval) 0)
           (+ (+ tone interval) 12))
          (t (+ tone interval))))

(defun interval (tone1 tone2)
    "Determines the interval in between two tones"
    (cond ((> 0 (- tone2 tone1)) (+ 12 (- tone2 tone1)))
          (t (- tone2 tone1))))

;;;--------------------------------------------------------------------------
;;;5. Standardization Procedures
;;;-------------------------------------------------------------------------- 

(defun stand-row (row)
    "Conforms the row to traditional '(0 1 2 3 4 5 6 7 8 9 t e) notation."
    (cond ((null row) nil)
          ((equal (first row) 11) (cons 't (stand-row (rest row))))
          ((equal (first row) 12) (cons 'e (stand-row (rest row))))
          (t (cons (transpose-tone (first row) -1)
                   (stand-row (rest row))))))

(defun stand-matrix (matrix)
    "Conforms the entire matrix to traditional notation."
    (mapcar #'stand-row (matrix)))

(defun de-stand (row)
    "Converts a standard row into 1-12 notation."
    (cond ((null row) nil)
          ((equal (first row) 't) (cons 11 (de-stand (rest row))))
          ((equal (first row) 'e) (cons 12 (de-stand (rest row))))
          (t (cons (transpose-tone (first row) 1)
                   (de-stand (rest row))))))

(defun stest-row () 
    "Makes a random row and assigns it to variable TEST-ROW."
    (setf test-row (stand-row (row-generator))))

(defun smatrix (row)
    "Creates a traditional matrix for a given row"
    (mapcar #'standard-row (matrix-builder row)))
    
;;;--------------------------------------------------------------------------
;;;6. Print-Friendly Procedures- coming soon
;;;--------------------------------------------------------------------------
;;basically the same functions pmatrix, prow, pprime, etc, but they print better

