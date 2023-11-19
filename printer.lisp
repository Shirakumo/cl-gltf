(in-package #:org.shirakumo.fraf.gltf)

;; Special destructuring
(defmethod to-json ((type perspective-camera) writer))
(defmethod to-json ((type orthographic-camera) writer))
(defmethod to-json ((type box-shape) writer))
(defmethod to-json ((type capsule-shape) writer))
(defmethod to-json ((type convex-shape) writer))
(defmethod to-json ((type cylinder-shape) writer))
(defmethod to-json ((type sphere-shape) writer))
(defmethod to-json ((type trimesh-shape) writer))

(defun merge-buffers (gltf)
  (when (< 1 (length (buffers gltf)))
    (let* ((data (static-vectors:make-static-vector
                 (loop for buffer across (buffers gltf)
                       maximize (byte-length buffer))))
           (ptr (static-vectors:static-vector-pointer data))
           (new-buffer (make-instance 'static-buffer :idx 0 :gltf gltf :buffer data :byte-length (length data))))
      ;; Copy all the buffer data into one array and "normalize" each buffer into an
      ;; offset into the one data array.
      (loop for buffer across (buffers gltf)
            for start = 0
            do (etypecase buffer
                 (static-buffer
                  (static-vectors:replace-foreign-memory
                   (cffi:inc-pointer ptr start)
                   (cffi:inc-pointer (static-vectors:static-vector-pointer (buffer buffer)) (start buffer))
                   (byte-length buffer)))
                 (lisp-buffer
                  (replace data (buffer buffer) :start1 start :start2 (start buffer)))
                 (buffer
                  (static-vectors:replace-foreign-memory
                   (cffi:inc-pointer ptr start)
                   (start buffer)
                   (byte-length buffer))))
               (change-class buffer 'buffer)
               (setf (slot-value buffer 'start) (cffi:inc-pointer ptr start))
               (incf start (byte-length buffer)))
      ;; Now update all buffer views to point to the new buffer
      (loop for view across (buffer-views gltf)
            for old-buffer = (buffer view)
            for offset = (- (cffi:pointer-address (start old-buffer)) (cffi:pointer-address ptr))
            do (setf (buffer view) new-buffer)
               (setf (byte-offset view) (+ offset (byte-offset view)))
               (setf (slot-value view 'start) (cffi:inc-pointer ptr (byte-offset view))))
      ;; And update the buffer array
      (setf (buffers gltf) (vector new-buffer))))
  gltf)

(defun normalize-buffers (gltf)
  (loop for buffer across (buffers gltf)
        for i from 0
        do (etypecase buffer
             (uri-buffer
              (setf (uri buffer) NIL)
              (change-class buffer 'static-buffer))
             (mmap-buffer
              (let ((data (static-vectors:make-static-vector (byte-length buffer))))
                (static-vectors:replace-foreign-memory
                 (static-vectors:static-vector-pointer data)
                 (start buffer) (byte-length buffer))
                (change-class buffer 'static-buffer :buffer data :start 0)))
             (buffer))
           (unless (uri buffer)
             (setf (uri buffer) (format NIL "buf-~4,'0d.dat" i))))
  gltf)

(defun urlify-buffers (gltf)
  (loop for buffer across (buffers gltf)
        do (etypecase buffer
             (uri-buffer)
             (lisp-buffer
              (change-class buffer 'uri-buffer)
              (setf (uri buffer) (format NIL "data:;base64,~a" (qbase64:encode-bytes (buffer buffer)))))
             (buffer
              (let ((data (static-vectors:make-static-vector (byte-length buffer))))
                (replace data buffer)
                (change-class buffer 'uri-buffer)
                (setf (uri buffer) (format NIL "data:;base64,~a" (qbase64:encode-bytes data)))))))
  gltf)

(defun serialize (gltf file &key (if-exists :supersede))
  (etypecase file
    (string
     (serialize gltf (pathname file)))
    (pathname
     (cond ((string-equal "glb" (pathname-type file))
            (with-open-file (stream file :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists if-exists)
              (serialize gltf stream)))
           (T
            (with-open-file (stream file :direction :output
                                         :element-type 'character
                                         :if-exists if-exists)
              (serialize gltf stream)))))
    (stream
     (cond ((equal '(unsigned-byte 8) (stream-element-type file))
            (merge-buffers gltf)
            ;; Header
            (nibbles:write-ub32/le #x46546C67 file)
            (nibbles:write-ub32/le 2 file)
            ;; JSON block
            (let ((str (babel:string-to-octets (to-json gltf NIL))))
              (nibbles:write-ub32/le (length str) file)
              (nibbles:write-ub32/le #x4E4F534A file)
              (write-sequence str file))
            ;; BIN block
            (let ((buf (aref (buffers gltf) 0)))
              (nibbles:write-ub32/le (byte-length buf) file)
              (nibbles:write-ub32/le #x004E4942 file)
              (etypecase buf
                (lisp-buffer
                 (write-sequence (buffer buf) file))
                (buffer
                 ;; FFI? How to get the stream FD?
                 (write-sequence buf file)))))
           ((equal 'character (stream-element-type file))
            (dolist (buffer (buffers gltf))
              (etypecase buffer
                (uri-buffer)
                (lisp-buffer
                 (with-open-file (stream (merge-pathnames (uri buffer) file)
                                         :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists if-exists)
                   (write-sequence (buffer buffer) stream)))
                (buffer
                 ;; FFI?
                 (with-open-file (stream (merge-pathnames (uri buffer) file)
                                         :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists if-exists)
                   (write-sequence buffer stream)))))
            (to-json gltf file))
           (T
            (error "Can't write to a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8)."))))))
