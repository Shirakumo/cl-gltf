(in-package #:org.shirakumo.fraf.gltf)

(defun update-asset-generator (gltf)
  (setf (generator (asset gltf))
        (format NIL "cl-gltf v~a" #.(asdf:component-version (asdf:find-system :cl-gltf)))))

(defun merge-buffers (gltf)
  (when (< 1 (length (buffers gltf)))
    (let* ((data (static-vectors:make-static-vector
                  (loop for buffer across (buffers gltf)
                        sum (byte-length buffer))))
           (ptr (static-vectors:static-vector-pointer data))
           (new-buffer (make-instance 'static-buffer :idx 0 :gltf gltf :buffer data :start 0 :byte-length (length data))))
      ;; Copy all the buffer data into one array and "normalize" each buffer into an
      ;; offset into the one data array.
      (loop with start = 0
            for buffer across (buffers gltf)
            do (etypecase buffer
                 (static-buffer
                  (static-vectors:replace-foreign-memory
                   (cffi:inc-pointer ptr start)
                   (start buffer)
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

(defun to-json (thing output)
  (com.inuoe.jzon:stringify (to-table thing NIL) :stream output))

(defun write-buffer (buffer stream)
  (typecase stream
    #+sbcl
    (file-stream
     (finish-output stream)
     (sb-posix:write (sb-posix:file-descriptor stream) (start buffer) (byte-length buffer)))
    (T
     (write-sequence buffer stream))))

(defun serialize (gltf file &rest args &key (if-exists :supersede) (update-asset-generator T))
  (etypecase file
    (string
     (apply #'serialize gltf (pathname file) args))
    (pathname
     (cond ((string-equal "glb" (pathname-type file))
            (with-open-file (stream file :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists if-exists)
              (apply #'serialize gltf stream args)))
           (T
            (with-open-file (stream file :direction :output
                                         :element-type 'character
                                         :if-exists if-exists)
              (apply #'serialize gltf stream args)))))
    (stream
     (when update-asset-generator
       (setf (uri gltf) (pathname file))
       (update-asset-generator gltf))
     (cond ((equal '(unsigned-byte 8) (stream-element-type file))
            (merge-buffers gltf)
            (let ((buf (aref (buffers gltf) 0))
                  (str (babel:string-to-octets (to-json gltf NIL))))
              ;; Header
              (nibbles:write-ub32/le #x46546C67 file)
              (nibbles:write-ub32/le 2 file)
              (nibbles:write-ub32/le (+ (+ 4 4 4) ; header
                                        (+ 4 4 (length str)) ; str block
                                        (+ 4 4 (length buf))) ; buf block
                                     file)
              ;; JSON block
              (let ((start (start buf)))
                (setf (slot-value buf 'start) NIL)
                (nibbles:write-ub32/le (length str) file)
                (nibbles:write-ub32/le #x4E4F534A file)
                (write-sequence str file)
                (setf (slot-value buf 'start) start))
              ;; BIN block
              (nibbles:write-ub32/le (byte-length buf) file)
              (nibbles:write-ub32/le #x004E4942 file)
              (etypecase buf
                (lisp-buffer
                 (write-sequence (buffer buf) file))
                (buffer
                 (write-buffer buf file)))))
           ((equal 'character (stream-element-type file))
            (loop for buffer across (buffers gltf)
                  do (etypecase buffer
                       (uri-buffer)
                       (lisp-buffer
                        (with-open-file (stream (merge-pathnames (uri buffer) file)
                                                :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :if-exists if-exists)
                          (write-sequence (buffer buffer) stream)))
                       (buffer
                        (with-open-file (stream (merge-pathnames (uri buffer) file)
                                                :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :if-exists if-exists)
                          (write-buffer buffer stream)))))
            (to-json gltf file))
           (T
            (error "Can't write to a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8).")))))
  file)
