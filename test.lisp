(defpackage :cl-libpff-test
  (:use #:cl #:libpff-ffi #:plus-c))

(in-package :cl-libpff-test)

(defparameter *test-pst-file-1* "/mnt/c/Users/mille_9b1cm14/OneDrive/Documents/Outlook Files/test.pst")
(defparameter *test-pst-file-2* "/mnt/c/Users/mille_9b1cm14/AppData/Local/Microsoft/Outlook/johnpantagruel@gmail.com.ost")

(defun test-version ()
  (libpff-get-version))

(defun test-file-signature ()
  (let ((fname "/mnt/c/Users/mille/Documents/Outlook Files/Outlook1.pst"
               )
        (invalid-fname "/home/millejoh/Dropbox/20170627 Scarlett DS11_Complete.pdf"))
    (if (= 1 (libpff-check-file-signature fname (cffi:null-pointer)))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))
    (if (= 1 (libpff-check-file-signature invalid-fname (cffi:null-pointer)))
        (format t "~A is a valid PFF datafile.~%" fname)
        (warn "Something isn't right with signature of ~A.~%" fname))))

(defun test-initialize-file ()
  (let ((f (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer))))
    (unless (= 1 (libpff-file-initialize f (cffi:null-pointer)))
      (warn "Something went wrong initializing a file object."))
    (unless (cffi:null-pointer-p f)
      (libpff-file-free f (cffi:null-pointer)))
    (cffi:foreign-free f)))

(defun make-file-handle ()
  (let ((f (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer))))
    (unless (= 1 (libpff-file-initialize f (cffi:null-pointer)))
      (warn "Something went wrong initializing a file object."))
    f))

(defun free-file-handle (handle)
  (unless (cffi:null-pointer-p handle)
    (libpff-file-free handle (cffi:null-pointer))))

(defun test-open-pst (&optional (file-name *test-pst-file-1*))
  (let* ((f (make-file-handle))
         (errptr (cffi:null-pointer)))
    (c-let ((handle :pointer :ptr f)
            (size :int))
      (print (libpff-ffi::libpff-file-open handle file-name 1 errptr))
      (print (libpff-ffi::libpff-file-get-size handle (size &) errptr))
      (print size)
      (free-file-handle f))))

(defparameter +default-error+ (cffi:null-pointer))

(defun make-t-pointer ()
  (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer)))

(defclass pst-object ()
  ((handle :accessor pst-handle :initform (make-t-pointer))))

(defclass pst-store (pst-object)
  ((source :accessor pst-store-source :initarg :source)
   (type :reader pst-store-type :initform 0)
   (root-folder :reader pst-store-root-folder :initform (make-t-pointer))
   (message-store :reader pst-store-message-store :initform (make-t-pointer))
   (size :reader pst-store-size :initform 0)))

(defclass item (pst-object)
  ((id :reader pst-item-id :initform 0)
   (subitem-count :reader pst-)))


(defmacro define-libpff-accessor (object-type accessor return-type)
  (let ((ffi-accessor-fn (intern (format nil "LIBPFF-~A-GET-~A" object-type accessor)))
        (accessor-fn (intern (format nil "_GET-~S-~S" object-type accessor))))
    `(defun ,accessor-fn (ptr)
       (c-let ((rval ,return-type))

         (let ((result (,ffi-accessor-fn ptr (rval &) +default-error+)))

           (values (rval *) result))))))

(define-libpff-accessor item identifier :unsigned-int)
(define-libpff-accessor item number-of-entries :unsigned-int)
(define-libpff-accessor item number-of-sub-items :unsigned-int)
(define-libpff-accessor item type :unsigned-char)
(define-libpff-accessor file size :int)
(define-libpff-accessor file type :unsigned-char)
(define-libpff-accessor file content-type :unsigned-char)
(define-libpff-accessor folder type ::unsigned-char)
(define-libpff-accessor folder utf8-name-size :int)
(define-libpff-accessor folder utf16-name-size :int)
(define-libpff-accessor folder number-of-sub-folders :int)

(defmethod initialize-instance :after ((store pst-store) &key &allow-other-keys)
  (with-slots (handle source type size root-folder message-store) store
    (format t  "~A" source)
    (libpff-file-initialize handle (cffi:null-pointer))
    (when (probe-file source)
      (c-let ((handle :pointer :ptr handle)
              (rtype :unsigned-char)
              (rsize :int))
        (when (libpff-file-open handle source 1 +default-error+)
          (libpff-file-get-content-type handle (rtype &) +default-error+)
          (libpff-file-get-size handle (rsize &) +default-error+)
          (libpff-file-get-root-folder handle root-folder +default-error+)
          (libpff-file-get-message-store handle message-store +default-error+)
          (setf type rtype
                size rsize))))))

(defun new-test-store (&optional (filename *test-pst-file-1*))
  (unless (probe-file filename)
    (error "File does not exist: ~A." filename))
  (make-instance 'pst-store :source *test-pst-file-1*))
