;;;; falcon.asd

(asdf:defsystem #:falcon
  :description "A utility to procedurally bind commands to buttons"
  :author "Ryan Burnside"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:ltk)
  :components ((:file "package")
               (:file "falcon")))


