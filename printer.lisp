(in-package #:org.shirakumo.fraf.gltf)

(defun total-buffer-length (buffers)
  (loop for buffer across buffers
        unless (typep buffer 'uri-buffer)
        maximize (+ (start buffer) (byte-length buffer))))

(defun merge-buffers (gltf)
  )

(defun normalize-buffers (gltf)
  )

(defun urlify-buffers (gltf)
  )

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
                (mmap-buffer
                 ;; FFI? How to get the stream FD?
                 (write-sequence buffer stream)))))
           ((equal 'character (stream-element-type file))
            (dolist (buffer (buffers gltf))
              (etypecase buffer
                (uri-buffer)
                (mmap-buffer
                 (error "Cannot serialise with MMAP buffers present.
Please call URLIFY-BUFFERS or NORMALIZE-BUFFERS first."))
                (buffer
                 (with-open-file (stream (merge-pathnames (uri buffer) file)
                                         :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists if-exists)
                   (write-sequence (buffer buffer) stream)))))
            (to-json gltf file))
           (T
            (error "Can't write to a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8)."))))))
