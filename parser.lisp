(in-package #:org.shirakumo.fraf.gltf)

(defmethod resolve ((index integer) slot gltf)
  (svref (slot-value gltf slot) index))

(defmethod unresolve ((element indexed-element))
  (idx element))

(defmethod resolve ((array vector) slot gltf)
  (let ((base (slot-value gltf slot)))
    (map 'vector (lambda (index) (svref base index)) array)))

(defmethod unresolve ((array vector))
  (map 'vector #'unresolve array))

(defmethod resolve ((null null) slot gltf)
  null)

(defmethod unresolve ((null null))
  null)

(defgeneric parse-from (json type gltf))
(defgeneric serialize-to (type value))

(defmethod parse-from ((null null) type gltf)
  null)

(defmethod parse-from ((array vector) type gltf)
  (map 'vector (lambda (json) (parse-from json type gltf)) array))

(defmethod serialize-to (type (array vector))
  (map 'vector (lambda (value) (serialize-to type value)) array))

(defmethod parse-from ((string string) type gltf)
  string)

(defmethod parse-from (json (type symbol) gltf)
  (parse-from json (c2mop:class-prototype (c2mop:ensure-finalized (find-class type))) gltf))

(defmethod parse-from (json (type gltf-element) gltf)
  (apply #'make-instance (type-of type) :gltf gltf (initargs type json gltf)))

(defmethod serialize-to ((target hash-table) (value gltf-element))
  (to-table value target))

(defmethod serialize-to (target (value gltf-element))
  (to-table value NIL))

(defmethod serialize-to (target (value pathname))
  (uiop:native-namestring value))

(defmethod serialize-to (target (value null))
  value)

(defun %parse-from (json type gltf)
  (apply #'make-instance type :gltf gltf (initargs (c2mop:class-prototype (c2mop:ensure-finalized (find-class type))) json gltf)))

(defmethod parse-from (json (type camera) gltf)
  (cond ((gethash "perspective" json)
         (%parse-from json 'perspective-camera gltf))
        ((gethash "orthographic" json)
         (%parse-from json 'orthographic-camera gltf))
        (T
         (call-next-method))))

(defmethod parse-from (json (type shape) gltf)
  (loop for (field type) in '(("box" box-shape)
                              ("capsule" capsule-shape)
                              ("convex" convex-shape)
                              ("cylinder" cylinder-shape)
                              ("sphere" sphere-shape)
                              ("trimesh" trimesh-shape))
        for value = (gethash field json)
        thereis (when value (%parse-from json type gltf))
        finally (return (call-next-method))))

(defmethod parse-from (json (type (eql 'filter)) gltf)
  (ecase json
    (9728 :nearest)
    (9729 :linear)
    (9984 :nearest-mipmap-nearest)
    (9985 :linear-mipmap-nearest)
    (9986 :nearest-mipmap-linear)
    (9987 :linear-mipmap-linear)))

(defmethod serialize-to ((type (eql 'filter)) value)
  (ecase value
    (:nearest 9728)
    (:linear 9729)
    (:nearest-mipmap-nearest 9984)
    (:linear-mipmap-nearest 9985)
    (:nearest-mipmap-linear 9986)
    (:linear-mipmap-linear 9987)))

(defmethod parse-from (json (type (eql 'wrapping)) gltf)
  (ecase json
    (33071 :clamp-to-edge)
    (33648 :mirrored-repeat)
    (10497 :repeat)))

(defmethod serialize-to ((type (eql 'wrapping)) value)
  (ecase value
    (:clamp-to-edge 33071)
    (:mirrored-repeat 33648)
    (:repeat 10497)))

(defmethod parse-from (json (type (eql 'primitive-mode)) gltf)
  (ecase json
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)))

(defmethod serialize-to ((type (eql 'primitive-mode)) value)
  (ecase value
    (:points 0)
    (:lines 1)
    (:line-loop 2)
    (:line-strip 3)
    (:triangles 4)
    (:triangle-strip 5)
    (:triangle-fan 6)))

(defmethod parse-from (json (type (eql 'element-type)) gltf)
  (normalize-type json))

(defmethod parse-from ((json string) (type (eql 'element-type)) gltf)
  (normalize-type json))

(defmethod serialize-to ((type (eql 'element-type)) value)
  (case value
    (:int8 5120)
    (:uint8 5121)
    (:int16 5122)
    (:uint16 5123)
    (:int32 5124)
    (:uint32 5125)
    (:int64 5134)
    (:uint64 5135)
    (:float 5126)
    (:double 5130)
    (T (string-downcase value))))

(defmethod parse-from ((json string) (type (eql 'keyword)) gltf)
  (intern (string-upcase json) "KEYWORD"))

(defmethod serialize-to ((type (eql 'keyword)) value)
  (string-downcase value))

(defmethod parse-from (json (type (eql 'mesh-attributes)) gltf)
  (let ((table (make-hash-table :test 'eql)))
    (maphash (lambda (k v)
               (setf (gethash (intern (string-upcase k) "KEYWORD") table)
                     (resolve v 'accessors gltf)))
             json)
    table))

(defmethod serialize-to ((type (eql 'mesh-attributes)) value)
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash (string-downcase k) table) (idx v)))
             value)
    table))

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
