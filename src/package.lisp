(in-package :cl-user)

(defpackage #:libpff-ffi
  (:use))

(defpackage #:libpff
  (:use #:cl #:autowrap.minimal #:plus-c #:libpff-ffi))
