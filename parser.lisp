(in-package #:org.shirakumo.fraf.gltf)

(defun gethash* (table &rest keys)
  (if (and table keys)
      (apply #'gethash* (gethash (first keys) table) (rest keys))
      table))

(defmethod parse-from (json (type gltf) gltf)
  (flet ((val (slot source type)
           (let ((source (if (listp source) (apply #'gethash* json source) (gethash source json))))
             (when (and source (= 0 (length (slot-value gltf slot))))
               (let ((result (parse-from source type gltf)))
                 (setf (slot-value gltf slot) result)
                 (when (typep result 'vector)
                   (loop for i from 0 below (length result)
                         for element = (aref result i)
                         do (when (typep element 'indexed-element)
                              (setf (idx element) i)))))))))
    ;; FIXME: this *sucks* can we put this into define-element somehow instead?
    (val 'buffers "buffers" 'buffer)
    (val 'buffer-views "bufferViews" 'buffer-view)
    (val 'accessors "accessors" 'accessor)
    (val 'cameras "cameras" 'camera)
    (val 'asset "asset" 'asset)
    (val 'images "images" 'image)
    (val 'samplers "samplers" 'sampler)
    (val 'textures "textures" 'texture)
    (val 'materials "materials" 'material)
    (val 'meshes "meshes" 'mesh)
    (val 'lights '("extensions" "KHR_lights_punctual") 'light)
    (val 'image-lights '("extensions" "EXT_lights_image_based") 'image-light)
    (val 'articulations '("extensions" "AGI_articulations") 'articulation)
    (val 'shapes '("extensions" "KHR_collision_shapes" "shapes") 'shape)
    (val 'physics-materials '("extensions" "KHR_rigid_bodies" "physicsMaterials") 'physics-material)
    (val 'physics-joint-limits '("extensions" "KHR_rigid_bodies" "physicsJointLimits") 'physics-joint-limit)
    (val 'collision-filters '("extensions" "KHR_rigid_bodies" "collisionFilters") 'collision-filter)
    (val 'nodes "nodes" 'node)
    (val 'skins "skins" 'skin)
    (val 'animations "animations" 'animation)
    (val 'scenes "scenes" 'scene)
    (setf (scene gltf) (resolve (gethash "scene" json) 'scenes gltf))
    (setf (extensions-used gltf) (gethash "extensionsUsed" json))
    ;; Tie up crap.
    (loop for node across (nodes gltf)
          for children = (children node)
          do (loop for i from 0 below (length children)
                   for child = (aref (nodes gltf) (aref children i))
                   do (setf (parent child) node)
                      (setf (aref children i) child))
             (when (skin node)
               (setf (skin node) (resolve (skin node) 'skins gltf))))
    type))

(defun parse-glb-stream (stream &optional (gltf (make-instance 'gltf :uri stream)))
  (assert (= (nibbles:read-ub32/le stream) #x46546C67))
  (assert (= (nibbles:read-ub32/le stream) 2))
  (nibbles:read-ub32/le stream)
  (let ((skip (if (typep stream 'file-stream)
                  (lambda (i) (file-position stream (+ (file-position stream) i)))
                  (lambda (i) (loop repeat i do (read-byte stream)))))
        json)
    (setf (buffers gltf) (make-array 1 :initial-element NIL))
    (loop for length = (handler-case (nibbles:read-ub32/le stream)
                         (end-of-file () NIL))
          while length
          do (let ((type (nibbles:read-ub32/le stream)))
               (case type
                 (#x4E4F534A           ; JSON
                  (when json (warn "Multiple JSON blocks, overwriting."))
                  (setf json (com.inuoe.jzon:parse stream :max-string-length NIL :allow-multiple-content T)))
                 (#x004E4942           ; BIN
                  (when (svref (buffers gltf) 0) (warn "Multiple BIN blocks, overwriting."))
                  (let ((buffer (static-vectors:make-static-vector length)))
                    (read-sequence buffer stream)
                    (setf (svref (buffers gltf) 0) (make-instance 'static-buffer :gltf gltf :idx 0 :start 0 :buffer buffer))))
                 (#x00000000           ; EOF
                  (return))
                 (T
                  (warn "Unknown glb block type ~8,'0x" type)
                  (funcall skip length)))))
    (unless json (error "No JSON block present in GLB file."))
    (parse-from json gltf gltf)
    gltf))

(defun parse-glb-memory (ptr start end &optional (gltf (make-instance 'gltf :uri NIL)))
  (let ((i start)
        json)
    (setf (buffers gltf) (make-array 1 :initial-element NIL))
    (flet ((read-ub32 ()
             (prog1 (cffi:mem-ref (cffi:inc-pointer ptr i) :uint32)
               (incf i 4))))
      (assert (= (read-ub32) #x46546C67))
      (assert (= (read-ub32) 2))
      (read-ub32)
      (loop while (< i end)
            do (let ((length (read-ub32))
                     (type (read-ub32)))
                 (case type
                   (#x4E4F534A        ; JSON
                    (let ((text (cffi:foreign-string-to-lisp (cffi:inc-pointer ptr i) :count length)))
                      (when json (warn "Multiple JSON blocks, overwriting."))
                      (setf json (com.inuoe.jzon:parse text :max-string-length NIL))))
                   (#x004E4942        ; BIN
                    (when (svref (buffers gltf) 0) (warn "Multiple BIN blocks, overwriting."))
                    (setf (svref (buffers gltf) 0) (make-instance 'buffer :gltf gltf :idx 0 :start (cffi:inc-pointer ptr i) :byte-length length)))
                   (#x00000000        ; EOF
                    (return))
                   (T
                    (warn "Unknown glb block type ~8,'0x" type)))
                 (incf i length)))
      (unless json (error "No JSON block present in GLB file."))
      (parse-from json gltf gltf)
      gltf)))

(defun parse-glb-vector (vector start end &optional (gltf (make-instance 'gltf :uri NIL)))
  (let ((i start)
        json)
    (setf (buffers gltf) (make-array 1 :initial-element NIL))
    (flet ((read-ub32 ()
             (prog1 (nibbles:ub32ref/le vector i)
               (incf i 4))))
      (assert (= (read-ub32) #x46546C67))
      (assert (= (read-ub32) 2))
      (read-ub32)
      (loop while end
            do (let ((length (read-ub32))
                     (type (read-ub32)))
                 (case type
                   (#x4E4F534A        ; JSON
                    ;; FIXME: this sucks
                    (when json (warn "Multiple JSON blocks, overwriting."))
                    (setf json (com.inuoe.jzon:parse (make-array length :displaced-to vector :displaced-index-offset i)
                                                     :max-string-length NIL)))
                   (#x004E4942        ; BIN
                    (when (svref (buffers gltf) 0) (warn "Multiple BIN blocks, overwriting."))
                    (setf (svref (buffers gltf) 0) (make-instance 'lisp-buffer :gltf gltf :idx 0 :buffer vector :start i)))
                   (#x00000000        ; EOF
                    (return))
                   (T
                    (warn "Unknown glb block type ~8,'0x" type)))
                 (incf i length)))
      (unless json (error "No JSON block present in GLB file."))
      (parse-from json gltf gltf)
      gltf)))

(defun parse (file &key (start 0) (end most-positive-fixnum) (mmap T))
  (etypecase file
    (pathname
     (cond ((string-equal "glb" (pathname-type file))
            (if mmap
                (multiple-value-bind (addr fd size) (mmap:mmap file)
                  (parse-glb-memory addr 0 size (make-instance 'gltf :uri file :%mmap (list addr fd size))))
                (with-open-file (stream file :element-type '(unsigned-byte 8))
                  (parse stream))))
           (T
            (with-open-file (stream file :element-type 'character)
              (parse stream)))))
    (string
     (with-input-from-string (stream file)
       (parse stream)))
    (cffi:foreign-pointer
     (parse-glb-memory file start end))
    ((vector (unsigned-byte 8))
     (parse-glb-vector file start end))
    (stream
     (cond ((equal '(unsigned-byte 8) (stream-element-type file))
            (parse-glb-stream file))
           ((equal 'character (stream-element-type file))
            (let ((json (com.inuoe.jzon:parse file :max-string-length NIL))
                  (gltf (make-instance 'gltf :uri file)))
              (parse-from json gltf gltf)))
           (T
            (error "Can't read from a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8)."))))))

(defmacro with-gltf ((gltf file) &body body)
  `(let ((,gltf (parse ,file)))
     (unwind-protect
          (let ((,gltf ,gltf))
            ,@body)
       (close ,gltf))))
