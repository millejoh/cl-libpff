(in-package :cl-user)

(asdf:defsystem #:cl-libpff
  :serial t
  :description ""
  :author "John M. Miller"
  :license "MIT"
  :version "0.1"
  :pathname "src"
  :depends-on ("cffi" "cl-autowrap" "cl-plus-c")
  :components ((:file "package")
               (:file "library")
               (:module #:autospec
                :pathname "autospec"
                :components ((:static-file "interface.h")))))
