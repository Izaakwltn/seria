;;;;package.lisp
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(defpackage #:seria
  (:documentation "12 Tone Serialism Module")
  (:use #:cl #:alexandria)
  
  ;;;;row.lisp
  (:export
   #:make-row
   #:test-row)

  ;;;;row-manipulation.lisp
  (:export
   #:interval
   #:transpose-row
   #:prime
   #:retrograde
   #:inverse
   #:retrograde-inverse)
   
   
  ;;;;matrix.lisp
  (:export
   #:build-matrix)

  ;;;;standardize.lisp
  (:export
   #:standard-row
   #:standard-matrix
   #:note-row
   #:note-matrix)
  )
