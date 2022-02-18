#|
 This file is a part of cl-gltf
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gltf)

(defgeneric normalize-type (spec))
(defgeneric element-reader (element-type))
(defgeneric element-byte-stride (element-type))
(defgeneric element-writer (element-type))
(defgeneric lisp-element-type (element-type))
(defgeneric element-count (type))

(defmethod normalize-type ((type string))
  (intern (string-upcase type) "KEYWORD"))

(defmethod element-reader ((type integer))
  (element-reader (normalize-type type)))

(defmethod element-byte-stride ((type integer))
  (element-byte-stride (normalize-type type)))

(defmethod element-writer ((type integer))
  (element-writer (normalize-type type)))

(defmethod lisp-element-type ((type integer))
  (lisp-element-type (normalize-type type)))

(defmethod element-count ((type string))
  (element-count (normalize-type type)))

(defmacro define-element-accessor ((type size) &rest alts)
  `(progn
     (defmethod element-reader ((type (eql ,type)))
       (lambda (ptr)
         (declare (type cffi:foreign-pointer ptr))
         (values (cffi:mem-ref ptr ,type)
                 (cffi:inc-pointer ptr ,size))))

     (defmethod element-byte-stride ((type (eql ,type)))
       ,size)
     
     (defmethod element-writer ((type (eql ,type)))
       (lambda (value ptr)
         (declare (type cffi:foreign-pointer ptr))
         (values (setf (cffi:mem-ref ptr ,type) value)
                 (cffi:inc-pointer ptr ,size))))

     ,@(loop for alt in alts
             collect `(defmethod normalize-type ((type (eql ,alt))) ,type))))

(define-element-accessor (:int8 1) 5120)
(define-element-accessor (:uint8 1) 5121)
(define-element-accessor (:int16 2) 5122)
(define-element-accessor (:uint16 2) 5123)
(define-element-accessor (:int32 4) 5124)
(define-element-accessor (:uint32 4) 5125)
(define-element-accessor (:int64 8) 5134)
(define-element-accessor (:uint64 8) 5135)
(define-element-accessor (:float 4) 5126)
(define-element-accessor (:double 8) 5130)

(defmethod lisp-element-type ((type (eql :int8))) '(signed-byte 8))
(defmethod lisp-element-type ((type (eql :uint8))) '(unsigned-byte 8))
(defmethod lisp-element-type ((type (eql :int16))) '(signed-byte 16))
(defmethod lisp-element-type ((type (eql :uint16))) '(unsigned-byte 16))
(defmethod lisp-element-type ((type (eql :int32))) '(signed-byte 32))
(defmethod lisp-element-type ((type (eql :uint32))) '(unsigned-byte 32))
(defmethod lisp-element-type ((type (eql :int64))) '(signed-byte 64))
(defmethod lisp-element-type ((type (eql :uint64))) '(unsigned-byte 64))
(defmethod lisp-element-type ((type (eql :float))) 'single-float)
(defmethod lisp-element-type ((type (eql :double))) 'double-float)

(defmethod element-count ((type (eql :scalar))) 1)
(defmethod element-count ((type (eql :vec2))) 2)
(defmethod element-count ((type (eql :vec3))) 3)
(defmethod element-count ((type (eql :vec4))) 4)
(defmethod element-count ((type (eql :mat2))) 4)
(defmethod element-count ((type (eql :mat3))) 9)
(defmethod element-count ((type (eql :mat4))) 16)

(defmethod construct-element-reader (element-type component-type)
  (let* ((count (element-count element-type))
         (reader (element-reader component-type))
         (buffer (make-array count :element-type (lisp-element-type component-type))))
    (lambda (ptr)
      (dotimes (i count buffer)
        (multiple-value-bind (value next-ptr) (funcall reader ptr)
          (setf (aref buffer i) value)
          (setf ptr next-ptr))))))

(defmethod construct-element-writer (element-type component-type)
  (let ((count (element-count element-type))
        (writer (element-writer component-type)))
    (lambda (value ptr)
      (dotimes (i count value)
        (multiple-value-bind (value next-ptr) (funcall writer (aref value i) ptr)
          (declare (ignore value))
          (setf ptr next-ptr))))))

(define-element buffer (uri-element named-element sequences:sequence)
  ((start :name null :reader start)
   (byte-length :reader sequences:length :accessor byte-length)))

(defmethod initialize-instance :after ((buffer buffer) &key)
  (cond ((and (< (length "data:") (length (uri buffer)))
              (string= "data:" (uri buffer) :end2 (length "data:")))
         (change-class buffer 'uri-buffer))
        (T
         (change-class buffer 'mmap-buffer))))

(defmethod sequences:elt ((buffer buffer) i)
  (cffi:mem-aref (start buffer) :uint8 i))

(defmethod (setf sequences:elt) (value (buffer buffer) i)
  (setf (cffi:mem-aref (start buffer) :uint8 i) value))

(define-element uri-buffer (buffer)
  ())

(defmethod initialize-instance :after ((buffer uri-buffer) &key)
  (let* ((string (uri buffer))
         (start (1+ (position #\, string)))
         (memory (cffi:foreign-alloc :uint8 :count (* 3 (floor (- (length string) start) 4)))))
    (setf (slot-value buffer 'start) memory)
    (with-input-from-string (in string :start start)
      (let* ((decoder (qbase64:make-decoder))
             (string (make-string 4))
             (bytes (make-array 5))
             (read-string t)
             (buffered nil)
             (eof nil))
        (loop while (or buffered (not eof))
              for end1 = (when read-string (read-sequence string in))
              do (when (and read-string (< end1 (length string)))
                   (setf eof T))
                 (multiple-value-bind (end2 pending)
                     (if read-string
                         (qbase64:decode decoder string bytes :end1 end1)
                         (qbase64:decode decoder "" bytes))
                   (dotimes (i end2)
                     (setf (cffi:mem-aref memory (incf start)) (aref bytes i)))
                   (setf buffered pending
                         read-string (or (not pending) (zerop end2)))))))))

(defmethod close ((buffer uri-buffer) &key abort)
  (declare (ignore abort))
  (when (slot-boundp buffer 'start)
    (cffi:foreign-free (start buffer))
    (slot-makunbound buffer 'start)))

(define-element mmap-buffer (buffer)
  ((mmap :name null :reader mmap)))

(defmethod initialize-instance :after ((buffer mmap-buffer) &key)
  (unless (slot-boundp buffer 'mmap)
    (multiple-value-bind (start fd size) (mmap:mmap (path buffer))
      (setf (slot-value buffer 'mmap) (list start fd size))
      (setf (slot-value buffer 'start) start))))

(defmethod close ((buffer mmap-buffer) &key abort)
  (declare (ignore abort))
  (when (slot-boundp buffer 'mmap)
    (apply #'mmap:munmap (mmap buffer))
    (slot-makunbound buffer 'mmap)
    (slot-makunbound buffer 'start)))

(define-element buffer-view (named-element sequences:sequence)
  ((buffer :ref buffers)
   (start :name null :reader start)
   (byte-offset :initform 0)
   (byte-length :reader sequences:length :accessor byte-length)
   (byte-stride :initform 1)
   target))

(defmethod initialize-instance :after ((view buffer-view) &key)
  (unless (slot-boundp view 'start)
    (setf (slot-value view 'start) (cffi:inc-pointer (start (buffer view)) (byte-offset view)))))

(defmethod sequences:elt ((view buffer-view) i)
  (cffi:mem-aref (start view) :uint8 (* i (byte-stride view))))

(defmethod (setf sequences:elt) (value (view buffer-view) i)
  (setf (cffi:mem-aref (start view) :uint8 (* i (byte-stride view))) value))

(define-element accessor (sequences:sequence named-element)
  ((buffer-view :ref buffer-views)
   (byte-offset :initform 0)
   (component-type :name "componentType" :initform :float :parse element-type)
   (element-type :name "type" :parse element-type)
   (size :name "count" :accessor size :reader sequences:length)
   (normalized :initform NIL)
   (maximum :name "max")
   (minimum :name "min")
   (start :name null :reader start)
   (byte-stride :name null :reader byte-stride)
   (element-reader :name null :initarg :element-reader :accessor element-reader)
   (element-writer :name null :initarg :element-writer :accessor element-writer)))

(defmethod initialize-instance :after ((accessor accessor) &key)
  (unless (slot-boundp accessor 'start)
    (setf (slot-value accessor 'start) (cffi:inc-pointer (start (buffer-view accessor)) (byte-offset accessor))))
  (unless (slot-boundp accessor 'byte-stride)
    (setf (slot-value accessor 'byte-stride) (byte-stride (buffer-view accessor))))
  (unless (slot-boundp accessor 'element-reader)
    (setf (slot-value accessor 'element-reader) (construct-element-reader (element-type accessor) (component-type accessor))))
  (unless (slot-boundp accessor 'element-writer)
    (setf (slot-value accessor 'element-writer) (construct-element-writer (element-type accessor) (component-type accessor)))))

(defmethod sequences:elt ((accessor accessor) i)
  (funcall (element-reader accessor) (cffi:inc-pointer (start accessor) (* (byte-stride accessor) i))))

(defmethod (setf sequences:elt) (value (accessor accessor) i)
  (funcall (element-writer accessor) value (cffi:inc-pointer (start accessor) (* (byte-stride accessor) i))))

(define-element sparse-accessor (accessor)
  (index-count
   index-start
   index-byte-stride
   index-reader))
