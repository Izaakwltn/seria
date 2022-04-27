;;;;row.lisp
;;;;

(in-package :seria)

;;;;------------------------------------------------------------------------
;;;;Tone Specifications
;;;;------------------------------------------------------------------------

(alexandria:define-constant tones
    '(1 2 3 4 5 6 7 8 9 10 11 12) :test 'equal)

(alexandria:define-constant standard-tones
    '(0 1 2 3 4 5 6 7 8 9 't 'e) :test 'equal)

;;;;------------------------------------------------------------------------
;;;;Row Definition
;;;;------------------------------------------------------------------------

(defclass row ()
  ((state     :initarg :state
	      :initform 'Prime 
	      :accessor state)
   (root      :initarg :root
	      :initform 'original
              :accessor root)
   (tone-list :initarg :tone-list
	      :accessor tone-list)))

(defmethod print-object ((obj row) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((state state)
		     (root root)
		     (tone-list tone-list))
	obj
      (format stream "~a-~a: ~%~%~a" state root tone-list))))

(defun make-row (state twelve-tone-list)
  "Builds a tone-row from a given set of 12 tones."
  (make-instance 'row :state state
		      :root (first twelve-tone-list)
		      :tone-list twelve-tone-list))
;;;;------------------------------------------------------------------------
;;;;Random Row Generation
;;;;------------------------------------------------------------------------

(defvar test-row nil)

(defun random-tone (tone-list)
    "Generates one random tone out of a list of tones."
    (elt tone-list (random (length tone-list))))

(defun row-generation (sample-row tone-list)
    "The backend of row-generator, builds a row without duplicates."
    (cond ((equal (length sample-row) (length tone-list)) sample-row)
          (t (push (random-tone (set-difference tone-list sample-row)) sample-row)
             (row-generation sample-row tone-list))))

(defvar sample-row nil)

(defun row-generator ()
    "Random tone-row generator."
    (setq sample-row '())
    (row-generation sample-row tones))

(defun test-row () 
    "Makes a random row and assigns it to variable TEST-ROW."
  (setq test-row (make-row 'Prime (row-generator))))

  

   
