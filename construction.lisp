(in-package #:org.shirakumo.fraf.gltf)

(defun vertex-attribute-element-type (attribute)
  (ecase attribute
    (:position :vec3)
    (:normal :vec3)
    (:tangent :vec3)
    ((:texcoord_0 :texcoord_1 :texcoord_2 :texcoord_3) :vec2)
    ((:joints_0 :joints_1 :joints_2 :joints_3) :vec4)
    ((:weights_0 :weights_1 :weights_2 :weights_3) :vec4)))

(defun element-type-component-type (element-type)
  (cond ((equal element-type '(unsigned-byte 8)) :uint8)
        ((equal element-type '(signed-byte 8)) :int8)
        ((equal element-type '(unsigned-byte 16)) :uint16)
        ((equal element-type '(signed-byte 16)) :int16)
        ((equal element-type '(unsigned-byte 32)) :uint32)
        ((equal element-type '(signed-byte 32)) :int32)
        ((equal element-type '(unsigned-byte 64)) :uint64)
        ((equal element-type '(signed-byte 64)) :int64)
        ((equal element-type 'single-float) :float)
        ((equal element-type 'double-float) :double)
        (T (error "Element-type ~s is not suitable for a buffer." element-type))))

(defun component-type-bytes (component-type)
  (ecase component-type
    ((:uint8 :int8) 1)
    ((:uint16 :int16) 2)
    ((:uint32 :int32) 4)
    ((:uint64 :int64) 8)
    ((:float) 4)
    ((:double) 8)))

(defun type-slot (type)
  (ecase type
    ((buffer lisp-buffer static-buffer uri-buffer mmap-buffer) 'buffers)
    (buffer-view 'buffer-views)
    (accessor 'accessors)
    ((camera orthographic-camera perspective-camera) 'cameras)
    (mesh 'meshes)
    (image 'images)
    (sampler 'samplers)
    (material 'materials)
    (skin 'skins)
    (node 'nodes)
    (animation 'animations)
    (scene 'scenes)
    (light 'lights)
    (image-light 'image-lights)
    (articulation 'articulations)
    ((shape box-shape sphere-shape cylinder-shape capsule-shape trimesh-shape convex-shape) 'shapes)
    (physics-material 'physics-materials)
    (physics-joint-limit 'physics-joint-limits)
    (collision-filter 'collision-filters)))

(defun push* (element sequence)
  (etypecase sequence
    (list (list* element sequence))
    (vector (cond ((adjustable-array-p sequence)
                   (vector-push-extend element sequence)
                   sequence)
                  (T
                   (let ((arr (make-array (1+ (length sequence)) :adjustable T :fill-pointer T)))
                     (replace arr sequence)
                     (setf (aref arr (length sequence)) element)
                     arr))))))

(defun push-child (child node)
  (setf (parent child) node)
  (setf (children node) (push* child (children node)))
  node)

(defun make-indexed (type element &rest initargs)
  (let* ((gltf (gltf element))
         (obj (apply #'make-instance type :gltf gltf :idx (length (slot-value gltf (type-slot type))) initargs)))
    (setf (slot-value gltf (type-slot type)) (push* obj (slot-value gltf (type-slot type))))
    obj))

(defun make-simple-view (gltf buffer-data &rest initargs)
  (let* ((data (static-vectors:make-static-vector (* (length buffer-data) (component-type-bytes (element-type-component-type (array-element-type buffer-data))))))
         (buffer (make-indexed 'static-buffer gltf :buffer data :byte-length (length data))))
    (cffi:with-pointer-to-vector-data (ptr buffer-data)
      (static-vectors:replace-foreign-memory 
       (static-vectors:static-vector-pointer data)
       ptr
       (length data)))
    (apply #'make-indexed 'buffer-view gltf :buffer buffer :byte-length (byte-length buffer) initargs)))

(defun make-mesh-primitive (gltf vertex-data face-indices vertex-attributes &rest args)
  (let* ((vertex-buffer (make-simple-view gltf vertex-data))
         (face-buffer (when face-indices (make-simple-view gltf face-indices)))
         (attributes (make-hash-table :test 'eql))
         (component-type (element-type-component-type (array-element-type vertex-data)))
         (components-per-vertex (loop for attribute in vertex-attributes sum (element-count (vertex-attribute-element-type attribute))))
         (vertex-count (/ (length vertex-data) components-per-vertex)))
    (loop for attribute in vertex-attributes
          for start = 0 then (+ start (* (element-count (vertex-attribute-element-type attribute))
                                         (component-type-bytes component-type)))
          do (setf (gethash attribute attributes)
                   (make-indexed 'accessor gltf :buffer-view vertex-buffer
                                                :component-type component-type
                                                :element-type (vertex-attribute-element-type attribute)
                                                :byte-offset start :size vertex-count)))
    (apply #'make-instance 'mesh-primitive
           :attributes attributes
           :indices (when face-indices
                      (make-indexed 'accessor gltf :buffer-view face-buffer
                                                   :component-type (element-type-component-type (array-element-type face-indices))
                                                   :element-type :scalar
                                                   :size (length face-indices)))
           :gltf (gltf gltf)
           args)))
