(in-package #:org.shirakumo.fraf.gltf)

(defgeneric parse-from (json type gltf))
(defgeneric serialize-to (type value))
(defgeneric resolve (object/s slot gltf))
(defgeneric unresolve (object/s))

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

(defmethod parse-from ((null null) type gltf)
  null)

(defmethod parse-from ((array vector) type gltf)
  (map 'vector (lambda (json) (parse-from json type gltf)) array))

(defmethod serialize-to (type (array vector))
  (if (stringp array)
      array
      (map 'vector (lambda (value) (serialize-to type value)) array)))

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
    (T (string value))))

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
               (setf (gethash (string k) table) (idx v)))
             value)
    table))
