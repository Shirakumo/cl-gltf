(in-package #:org.shirakumo.fraf.gltf)

(defun type-slot (type)
  (ecase type
    (buffer 'buffers)
    (buffer-view 'buffer-views)
    (accessor 'accessors)
    (camera 'cameras)
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
    (shape 'shapes)
    (physics-material 'physics-materials)
    (physics-joint-limit 'physics-joint-limits)
    (collision-filter 'collision-filters)))

(defun push* (element sequence)
  (etypecase sequence
    (list (list* element sequence))
    (vector (if (adjustable-array-p sequence)
                (vector-push-extend element sequence)
                (let ((arr (make-array (1+ (length sequence)) :adjustable T :fill-pointer T)))
                  (replace arr sequence)
                  (setf (aref arr (length sequence)) element)
                  arr)))))

(defun make-indexed (type element &rest initargs)
  (let* ((gltf (gltf element))
         (obj (apply #'make-instance type :gltf gltf :idx (length (slot-value gltf (type-slot type))) initargs)))
    (setf (slot-value gltf (type-slot type)) (push* obj (slot-value gltf (type-slot type))))
    obj))

(defun make-simple-view (gltf buffer-data &rest initargs)
  (let ((buffer (make-indexed 'lisp-buffer gltf :buffer buffer-data :byte-length (length buffer-data))))
    (apply #'make-indexed 'buffer-view gltf :buffer buffer :byte-length (byte-length buffer) initargs)))

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

(defun make-mesh-primitive (gltf vertex-data face-indices vertex-attributes &rest args)
  (let* ((vertex-buffer (make-simple-view gltf vertex-data))
         (face-buffer (when face-indices (make-simple-view gltf face-indices)))
         (attributes (make-hash-table :test 'eql))
         (components-per-vertex (loop for attribute in vertex-attributes sum (element-count (vertex-attribute-element-type attribute))))
         (vertex-count (/ (length vertex-data) components-per-vertex)))
    (loop for attribute in vertex-attributes
          do (setf (gethash attribute attributes)
                   (make-indexed 'accessor gltf :buffer-view vertex-buffer
                                                :component-type (element-type-component-type (array-element-type vertex-data))
                                                :element-type (vertex-attribute-element-type attribute)
                                                :size vertex-count)))
    (apply #'make-instance 'mesh-primitive 
           :attributes attributes
           :indices (when face-indices
                      (make-indexed 'accessor gltf :buffer-view face-buffer
                                                   :component-type (element-type-component-type (array-element-type face-indices))
                                                   :element-type :scalar
                                                   :size (length face-buffer)))
           :gltf (gltf gltf)
           args)))
