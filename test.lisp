(defpackage :cl-libpff-test
  (:use #:cl #:libpff-ffi #:plus-c))

(in-package :cl-libpff-test)

(defparameter *test-pst-file-1* "/mnt/c/Users/mille_9b1cm14/OneDrive/Documents/Outlook Files/test.pst")
(defparameter *test-pst-file-2* "/mnt/c/Users/mille/AppData/Local/Microsoft/Outlook/johnpantagruel@gmail.com.ost")

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
   (size :reader pst-store-size :initform 0)))

(defclass item (pst-object)
  ((id :reader pst-item-id :initform 0)
   (subitem-count :reader pst-subitem-count)))

(defclass folder (item)
  ((name :reader folder-name :initform nil)))

(defmethod initialize-instance :after ((store pst-store) &key &allow-other-keys)
  (with-slots (handle source type size root-folder message-store) store
    (format t  "~A" source)
    (libpff-file-initialize handle (cffi:null-pointer))
    (c-let ((handle :pointer :ptr handle))
      (unless (= 1 (libpff-file-open handle source +libpff-open-read+ +default-error+))
        (error "libpff: Could not open file ~A." source)))))

(defmethod get-type ((folder folder))
  (with-slots (handle) folder
    (c-let ((handle :pointer :ptr handle)
            (type :int))
      (let ((ret (libpff-folder-get-type handle (type &) +default-error+)))
        (values type ret)))))

(defmethod get-name ((folder folder))
  (with-slots (handle) folder
    (c-let ((handle :pointer :ptr handle)
            (string-size :int)
            (name :pointer))
      (let* ((ret1 (libpff-folder-get-utf8-name-size handle (string-size &) +default-error+))
             (ret2 (libpff-folder-get-utf8-name handle (name &) string-size +default-error+)))
        (if (= 1 ret1 ret2)
            (cffi:foreign-string-to-lisp (name &) :count (1- string-size))
            (warn "Could not get folder's name."))))))

(defmethod get-root-folder ((store pst-store))
  (with-slots (handle) store
    (c-let ((handle :pointer :ptr handle))
      (let* ((folder (make-instance 'folder))
             (rtval (libpff-file-get-root-folder handle (pst-handle folder) +default-error+)))
        (values folder rtval)))))

(defmethod subitem-count ((f folder))
  (with-slots (handle) f
    (c-let ((handle :pointer :ptr handle)
            (icnt :int))
      (let ((ret (libpff-folder-get-number-of-sub-folders handle (icnt &) +default-error+)))
        (values icnt ret)))))

(defmethod get-subitem-by-index ((f folder) idx)
  (with-slots (handle) f
    (c-let ((handle :pointer :ptr handle))
      (let* ((subfolder (make-instance 'folder))
             (ret (libpff-folder-get-sub-folder handle idx (pst-handle subfolder) +default-error+)))
        (values subfolder ret)))))

(defun new-test-store (&optional (filename *test-pst-file-1*))
  (unless (probe-file filename)
    (error "File does not exist: ~A." filename))
  (make-instance 'pst-store :source filename))
