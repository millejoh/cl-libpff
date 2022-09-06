(defpackage :cl-libpff-test-cffi
  (:use #:cl #:cffi))

(in-package :cl-libpff-test-cffi)

(defparameter *test-pst-file-1* "/mnt/c/Users/mille_9b1cm14/OneDrive/Documents/Outlook Files/test.pst")
(defparameter *test-pst-file-2* "/mnt/c/Users/mille_9b1cm14/AppData/Local/Microsoft/Outlook/johnpantagruel@gmail.com.ost")

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
      (libpff-file-f))
    (foreign-free f)))

(defcfun "libpff_file_open" :int
  (file :pointer)
  (filename :string)
  (access-flags :int)
  (error :pointer))

(defcfun "libpff_file_close" :int
  (file :pointer)
  (error :pointer))

(defcfun "libpff_file_get_size" :int
  (file :pointer)
  (size (:pointer :int64))
  (err :pointer))

(defcfun "libpff_file_get_content_type" :int
  (file :pointer)
  (content-type (:pointer :uint8))
  (err :pointer))

(defcfun "libpff_file_get_type" :int
  (file :pointer)
  (type (:pointer :uint8))
  (err :pointer))

(defcfun "libpff_file_get_message_store" :int
  (file :pointer)
  (message-store :pointer)
  (err :pointer))

(defcfun "libpff_file_get_root_item" :int
  (handle :pointer)
  (root-item :pointer)
  (err :pointer))

(defcfun "libpff_file_get_root_folder" :int
  (handle :pointer)
  (root-folder :pointer)
  (err :pointer))

(defun test-file-open (&optional (path *test-pst-file-1*))
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
       (with-foreign-object (rval ,return-type)
         (let ((result (,ffi-accessor-fn ptr rval +default-error+)))
           (values (mem-ref rval ,return-type) result))))))

(define-libpff-accessor file size :long)
(define-libpff-accessor file type :uint8)
(define-libpff-accessor file content-type :uint8)
;; (define-libpff-accessor item identifier :unsigned-int)
;; (define-libpff-accessor item number-of-entries :unsigned-int)
;; (define-libpff-accessor item number-of-sub-items :unsigned-int)
;; (define-libpff-accessor item type :unsigned-char)
;; (define-libpff-accessor folder type ::unsigned-char)
;; (define-libpff-accessor folder utf8-name-size :int)
;; (define-libpff-accessor folder utf16-name-size :int)
;; (define-libpff-accessor folder number-of-sub-folders :int)

(defmethod initialize-instance :after ((store pst-store) &key &allow-other-keys)
  (with-slots (handle source type size root-folder message-store) store
    (format t  "~A" source)
    (libpff-file-initialize handle (cffi:null-pointer))
    (when (probe-file source)
      (let ((rtype (foreign-alloc :uint8 :initial-element 0))
            (rsize (foreign-alloc :int :initial-element 0)))
        (when (libpff-file-open handle source 1 +default-error+)
          (libpff-file-get-content-type handle rtype +default-error+)
          (libpff-file-get-size handle rsize +default-error+)
          (libpff-file-get-root-folder handle root-folder +default-error+)
          (libpff-file-get-message-store handle message-store +default-error+)
          (setf type (mem-aref rtype :uint8)
                size (mem-aref rsize :int))
          (foreign-free rtype)
          (foreign-free rsize))))))

(defun new-test-store (&optional (filename *test-pst-file-1*))
  (unless (probe-file filename)
    (error "File does not exist: ~A." filename))
  (make-instance 'pst-store :source *test-pst-file-1*))
