(in-package :cl-user)

(asdf:defsystem :cl-libpff-test
  :serial t
  :pathname ""
  :depends-on ("cl-libpff")
  :components ((:file "test")
               (:file "test-cffi")
               (:file "test-sb-alien")))
