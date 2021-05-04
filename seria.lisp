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
                      ((- 12 n))))(interval-list row)))

(defun interval-list (row)
    "Generates list of intervals for the whole row, starting with 0
    (X axis of matrix)."
    (interval-list-backend (first row) row))

(defun interval-list-backend (tone row)
    (cond ((null row) nil)
          (t (cons (interval tone (first row)) 
                   (interval-list-backend tone (rest row))))))

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
              (t (find-nth-interval n (rest row))))) 

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
;;;6. Print-Friendly Procedures- in progress
;;;--------------------------------------------------------------------------
;;basically the same functions pmatrix, prow, pprime, etc, but they print better

(defun pprime (n row)
    "Aesthetically polite prime-row printer."
    (format t "Prime-~S: ~S" n (prime n row)))

(defun pmatrix (row)
    "Potentially pleasing matrix printer."
    (format t "~&")
    (format t "Matrix ~S:~%" row)
    (pmatrix-row (tone-matrix row)))

(defun pmatrix-row (matrix)
    "Backend for pmatrix."
    (cond ((null matrix) nil)
          (t (format t "~&~S" (first matrix)) (matrix-row (rest matrix))))

(defun pmatrix-complete (row)
    "Matrix, pretty printed with axis notations."
    (format t "~&Matrix ~S:~%~%" row)
    (format t "  ~S~S~%~S~%" (inverse-line-printer row) 
                       (pmatrix-printer (prime-order row) (tone-matrix row))
                       (r-i-line-printer row)))

(defun pmatrix-printer (prime-list matrix)
    "Backend for pmatrix-complete."
    (cond ((null prime-list) (format t "~&"))
    (t (format t "~&P~S ~S R~S "
                (first prime-list) (first matrix) (first prime-list))
       (pmatrix-printer (rest prime-list) (rest matrix))))))

(defun inverse-line-printer (row)
    "Prints inverse values for pmatrix-complete."
    (inverse-line (interval-list row)))

(defun inverse-line (inverse-row)
    "Backend for inverse-line-printer."
    (cond ((null inverse-row) (format t "~&"))
          (t (format t "I~S " (first inverse-row)) 
             (inverse-line (rest inverse-row)))))

(defun r-i-line-printer (row)
    "Prints retrograde inverse values for pmatrix-complete."
    (retro-inverse-line (interval-list row)))

(defun retro-inverse-line (inverse-row)
    "Backend for r-i-line-printer."
    (cond ((null inverse-row) (format t "~&"))
          (t (format t "RI~S " (first inverse-row)) 
             (retro-inverse-line (rest inverse-row)))))

