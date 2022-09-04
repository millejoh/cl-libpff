(defpackage :cl-libpff-test-sb-alien
  (:use #:cl #:sb-alien))

(in-package :cl-libpff-test-sb-alien)

(load-shared-object "/usr/local/lib/libpff.so")


(define-alien-routine "libpff_get_version" c-string)

(defun test-get-version ()
  (libpff-get-version))


(define-alien-routine "libpff_check_file_signature" int
  (filename c-string)
  (error (* t)))

(defun test-file-signature ()
  (let ((fname "/mnt/c/Users/mille/Documents/Outlook Files/Outlook1.pst")
        (invalid-fname "/home/millejoh/Dropbox/20170627 Scarlett DS11_Complete.pdf"))
    (if (= 1 (libpff-check-file-signature fname nil))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))
    (if (= 1 (libpff-check-file-signature invalid-fname nil))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))))

(define-alien-routine "libpff_file_initialize" int
  (fhandle (* t))
  (error (* t)))

(defun test-file-initialize ()
  )
