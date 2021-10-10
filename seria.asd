;;;;seria.asd
;;;;
;;;;Copyright (c) 2021 Izaak Walton

(asdf:defsystem #:seria
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "A module for building 12 tone tone-rows and matrices."
  :depends-on (:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "row")
	       (:file "row-manipulation")
	       (:file "matrix")
	       (:file "standardize")))
