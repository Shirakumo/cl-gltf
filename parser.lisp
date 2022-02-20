#|
 This file is a part of cl-gltf
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gltf)

(defmethod resolve ((index integer) slot gltf)
  (svref (slot-value gltf slot) index))

(defmethod resolve ((array vector) slot gltf)
  (let ((base (slot-value gltf slot)))
    (map 'vector (lambda (index) (svref base index)) array)))

(defmethod resolve ((null null) slot gltf)
  null)

(defgeneric parse-from (json type gltf))

(defmethod parse-from ((null null) type gltf)
  null)

(defmethod parse-from ((array vector) type gltf)
  (map 'vector (lambda (json) (parse-from json type gltf)) array))

(defmethod parse-from ((string string) type gltf)
  string)

(defmethod parse-from (json (type symbol) gltf)
  (parse-from json (c2mop:class-prototype (c2mop:ensure-finalized (find-class type))) gltf))

(defmethod parse-from (json (type gltf-element) gltf)
  (apply #'make-instance (type-of type) :gltf gltf (initargs type json gltf)))

(defmethod parse-from (json (type camera) gltf)
  (cond ((gethash "perspective" json)
         (apply #'make-instance 'perspective-camera :gltf gltf
                (append (initargs 'perspective-camera (gethash "perspective" json) gltf) (initargs type json gltf))))
        ((gethash "orthographic" json)
         (apply #'make-instance 'orthographic-camera :gltf gltf
                (append (initargs 'orthographic-camera (gethash "orthographic" json) gltf) (initargs type json gltf))))
        (T (apply #'make-instance (type-of type) :gltf gltf (initargs type json gltf)))))

(defmethod parse-from (json (type (eql 'filter)) gltf)
  (ecase json
    (9728 :nearest)
    (9729 :linear)
    (9984 :nearest-mipmap-nearest)
    (9985 :linear-mipmap-nearest)
    (9986 :nearest-mipmap-linear)
    (9987 :linear-mipmap-linear)))

(defmethod parse-from (json (type (eql 'wrapping)) gltf)
  (ecase json
    (33071 :clamp-to-edge)
    (33648 :mirrored-repeat)
    (10497 :repeat)))

(defmethod parse-from (json (type (eql 'element-type)) gltf)
  (normalize-type json))

(defmethod parse-from ((json string) (type (eql 'element-type)) gltf)
  (normalize-type json))

(defmethod parse-from ((json string) (type (eql 'keyword)) gltf)
  (intern (string-upcase json) "KEYWORD"))

(defmethod parse-from (json (type (eql 'mesh-attributes)) gltf)
  (let ((table (make-hash-table :test 'eql)))
    (maphash (lambda (k v)
               (setf (gethash (intern (string-upcase k) "KEYWORD") table)
                     (resolve v 'accessors gltf)))
             json)
    table))

(defmethod parse-from (json (type gltf) gltf)
  (flet ((val (slot source type)
           (let ((source (gethash source json)))
             (when source
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

(defun parse-glb-stream (stream)
  (assert (= (nibbles:read-ub32/le stream) #x46546C67))
  (assert (= (nibbles:read-ub32/le stream) 2))
  (nibbles:read-ub32/le stream)
  (let ((skip (if (typep stream 'file-stream)
                  (lambda (i) (file-position stream (+ (file-position stream) i)))
                  (lambda (i) (loop repeat i do (read-byte stream)))))
        (gltf (make-instance 'gltf :uri NIL)))
    (loop for length = (nibbles:read-ub32/le stream)
          for type = (nibbles:read-ub32/le stream)
          do (case type
               (#x4E4F534A                ; JSON
                (parse-from (shasht:read-json stream T NIL T) gltf gltf))
               (#x004E4942                ; BIN
                (let ((buffer (static-vectors:make-static-vector length)))
                  (read-sequence buffer stream)
                  (change-class (svref (buffers gltf) 0) 'static-buffer :buffer buffer)))
               (T
                (funcall skip length))))))

(defun parse (file)
  (etypecase file
    (pathname
     (cond ((string-equal "glb" (pathname-type file))
            (with-open-file (stream file :element-type '(unsigned-byte 8))
              (parse stream)))
           (T
            (with-open-file (stream file :element-type 'character)
              (parse stream)))))
    (string
     (with-input-from-string (stream file)
       (parse stream)))
    (cffi:foreign-pointer
     (error "Implement GLB parsing from memory"))
    ((vector (unsigned-byte 8))
     (error "Implement GLB parsing from memory"))
    (stream
     (cond ((equal '(unsigned-byte 8) (stream-element-type file))
            (parse-glb-stream file))
           ((equal 'character (stream-element-type file))
            (let ((json (shasht:read-json file))
                  (gltf (make-instance 'gltf :uri file)))
              (parse-from json gltf gltf)))
           (T
            (error "Can't read from a stream with element type other than CHARACTER or (UNSIGNED-BYTE 8)."))))))

(defmacro with-gltf ((gltf file) &body body)
  `(let ((,gltf (parse ,file)))
     (unwind-protect
          (progn
            ,@body)
       (close ,gltf))))
