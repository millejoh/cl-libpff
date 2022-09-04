(in-package :libpff-ffi)

(cffi:define-foreign-library libpff
 (:unix (:or "libpff.so" "libpff.so.1" "/usr/local/lib/libpff.so"))
 (t (:default "libpff")))

(autowrap:c-include '(cl-libpff autospec "interface.h")
           :spec-path '(cl-libpff autospec)
           :exclude-definitions ("remove"))

(cffi:use-foreign-library libpff)
