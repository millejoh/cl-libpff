(defpackage :cl-libpff-test-cffi
  (:use #:cl #:cffi))

(in-package :cl-libpff-test-cffi)

(defparameter *test-pst-file* "/mnt/c/Users/mille/Documents/Outlook Files/Outlook1.pst")

(define-foreign-library libpff
  (:unix (:or "libpff.so" "libpff.so.1" "/usr/local/lib/libpff.so"))
  (t (:default "libpff")))

(use-foreign-library libpff)

(defcfun "libpff_get_version" :string)

(defun test-cffi-get-version ()
  (libpff-get-version))

(defcfun "libpff_check_file_signature" :int
  (filename :string)
  (err :pointer))

(defun test-check-file-signture ()
  (let ((fname "/mnt/c/Users/mille/Documents/Outlook Files/Outlook1.pst")
        (invalid-fname "/home/millejoh/Dropbox/20170627 Scarlett DS11_Complete.pdf"))
    (if (= 1 (libpff-check-file-signature fname (null-pointer)))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))
    (if (= 1 (libpff-check-file-signature invalid-fname (null-pointer)))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))))


(defcfun "libpff_file_initialize" :int
  (file :pointer)
  (error :pointer))

(defcfun "libpff_file_free" :int
  (file :pointer)
  (error :pointer))

(defun test-file-initialize ()
  (let ((f (foreign-alloc :pointer :initial-element (null-pointer))))
    (unless (= 1 (libpff-file-initialize f (null-pointer)))
      (warn "Something went wrong initializing a file object."))
    (unless (null-pointer-p f)
      (libpff-file-free f (null-pointer)))
    (foreign-free f)))

(defcfun "libpff_file_open" :int
  (file :pointer)
  (filename :string)
  (access-flags :int)
  (error :pointer))

(defcfun "libpff_file_close" :int
  (file :pointer)
  (error :pointer))

(defun test-file-open (&optional (path *test-pst-file*))
  (unless (probe-file path)
    (warn "File ~A does not exist.~%" path))
  (let ((f (foreign-alloc :pointer :initial-element (null-pointer))))
    (format t "handle: ~A (~A)~%" f (mem-aref f :pointer 0))
    (unless (= 1 (libpff-file-initialize f (null-pointer)))
      (warn "Something went wrong initializing a file object."))
    (format t "handle: ~A (~A)~%" f (mem-aref f :pointer 0))
    (unless (= 1 (libpff-file-open (mem-aref f :pointer 0) path 1 (null-pointer)))
      (warn "Something went wrong opening file ~A.~%" path))
    (unless (= 1 (libpff-file-close (mem-aref f :pointer 0) (null-pointer)))
      (warn "Something went wrong closing file ~A.~%" path))
    (libpff-file-free f (null-pointer))
    (foreign-free f)))
