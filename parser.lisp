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

(defmethod parse-from ((array vector) type gltf)
  (map 'vector (lambda (json) (parse-from json type gltf)) array))

(defmethod parse-from ((string string) type gltf)
  string)

(defmethod parse-from (json (type symbol) gltf)
  (parse-from json (c2mop:class-prototype (c2mop:ensure-finalized (find-class type))) gltf))

(defmethod parse-from (json (type gltf-element) gltf)
  (apply #'make-instance (type-of type) :gltf gltf (initargs type json gltf)))

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

(defmethod parse-from (json (type (eql 'keyword)) gltf)
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
               (setf (slot-value gltf slot) (parse-from source type gltf))))))
    (val 'buffers "buffers" 'buffer)
    (val 'buffer-views "bufferViews" 'buffer-view)
    (val 'accessors "accessors" 'accessor)
    (val 'asset "asset" 'asset)
    (val 'meshes "meshes" 'mesh)
    (val 'images "images" 'image)
    (val 'samplers "samplers" 'sampler)
    (val 'textures "textures" 'texture)
    (val 'materials "materials" 'material)
    (val 'skins "skins" 'skin)
    (val 'nodes "nodes" 'node)
    (val 'animations "animations" 'animation)
    (val 'scenes "scenes" 'scene)
    (loop for node across (nodes gltf)
          for children = (children node)
          do (loop for i from 0 below (length children)
                   for child = (aref (nodes gltf) i)
                   do (setf (parent child) node)
                      (setf (aref children i) child)))
    type))

(defun parse (file)
  (with-open-file (stream file)
    (let ((json (shasht:read-json stream))
          (gltf (make-instance 'gltf :uri file)))
      (parse-from json gltf gltf))))

(defmacro with-gltf ((gltf file) &body body)
  `(let ((,gltf (parse ,file)))
     (unwind-protect
          (progn
            ,@body)
       (close ,gltf))))
