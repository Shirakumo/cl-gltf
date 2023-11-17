(in-package #:org.shirakumo.fraf.gltf)

(defun to-json (gltf)
  )

(defun serialize-buffers (buffers stream)
  ;; FIXME: I don't think this is how this works....
  (loop with start = 0
        for buffer across (sort (copy-seq buffers) #'< :key #'start)
        do (when (/= start (start buffer))
             (file-position stream (start buffer))
             (setf start (start buffer)))
           (typecase buffer
             (lisp-buffer (write-sequence (buffer buffer) stream))
             (mmap-buffer (error "FIXME"))
             (buffer (write-sequence buffer stream)))
           (incf start (byte-length buffer))))

(defun total-buffer-length (buffers)
  (loop for buffer across buffers
        unless (typep buffer 'uri-buffer)
        maximize (+ (start buffer) (byte-length buffer))))

(defun serialize (gltf file)
  (etypecase file
    (string
     (serialize gltf (pathname file)))
    (pathname
     (cond ((string-equal "glb" (pathname-type file))
            (with-open-file (stream file :element-type '(unsigned-byte 8))
              (serialize gltf stream)))
           (T
            (with-open-file (stream file :element-type 'character)
              (serialize gltf stream)))))
    (stream
     (let ((json (to-json gltf))
           (buffers (buffers gltf)))
       (cond ((equal '(unsigned-byte 8) (stream-element-type file))
              ;; Header
              (nibbles:write-ub32/le #x46546C67 file)
              (nibbles:write-ub32/le 2 file)
              ;; JSON block
              (let ((str (babel:string-to-octets (com.inuoe.jzon:stringify json))))
                (nibbles:write-ub32/le (length str) file)
                (nibbles:write-ub32/le #x4E4F534A file)
                (write-sequence str file))
              ;; BIN block
              (nibbles:write-ub32/le (total-buffer-length buffers) file)
              (nibbles:write-ub32/le #x004E4942 file)
              (serialize-buffers buffers file))
             ((equal 'character (stream-element-type file))
              (let ((buffer-file (make-pathname :type "dat" :defaults (pathname file))))
                (with-open-file (stream buffer-file :element-type '(unsigned-byte 8))
                  (serialize-buffers buffers stream))
                (com.inuoe.jzon:stringify json :stream file)))
             (T
              (error "Can't read from a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8).")))))))
