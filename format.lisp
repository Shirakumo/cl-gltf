#|
 This file is a part of cl-gltf
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gltf)

(define-element gltf-element ()
  (extensions
   extras
   (gltf :name null :initarg :gltf :initform NIL :reader gltf)))

(define-element named-element (gltf-element)
  (name))

(define-element uri-element (gltf-element)
  (uri))

(defmethod path ((element uri-element))
  (merge-pathnames (uri element) (uri (gltf element))))

(define-element gltf (gltf-element)
  (uri
   (buffers :parse buffer)
   (buffer-views :parse buffer-view)
   (accessors :parse accessor)
   (asset :parse asset)
   (cameras :parse camera)
   (meshes :parse mesh)
   (images :parse image)
   (samplers :parse sampler)
   (textures :parse texture)
   (materials :parse material)
   (skins :parse skin)
   (nodes :parse node)
   (animations :parse animation)
   (scenes :parse scene)))

(defmethod initialize-instance :after ((gltf gltf) &key)
  (setf (slot-value gltf 'gltf) gltf))

(defmethod close ((gltf gltf) &key abort)
  (loop for buffer across (buffers gltf)
        do (close buffer)))

(define-element asset (gltf-element)
  (copyright
   generator
   version
   min-version))

(define-element scene (named-element)
  ((nodes :ref nodes)))

(define-element node (named-element)
  ((camera :ref cameras)
   (parent :name null)
   (children :initform #())
   (skin :ref skins)
   (mesh :ref meshes)
   matrix
   rotation
   scale
   translation
   weights))

(define-element camera (named-element)
  ())

(define-element orthographic-camera (camera)
  (xmag
   ymag
   zfar
   znear))

(define-element perspective-camera (camera)
  (aspect-ratio
   fov
   zfar
   znear))

(define-element mesh (named-element)
  ((primitives :parse mesh-primitive)
   weights))

(define-element mesh-primitive (gltf-element)
  ((attributes :parse mesh-attributes)
   (indices :ref accessors)
   (material :ref materials)
   (mode :initform 4)
   targets))

(define-element material (named-element)
  ((pbr :parse pbr :name "pbrMetallicRoughness")
   (normal-texture :parse texture-info)
   (occlusion-texture :parse texture-info)
   (emissive-texture :parse texture-info)
   (emissive-factor :initform #(0.0 0.0 0.0))
   (alpha-mode :initform :opaque :parse keyword)
   (alpha-cutoff :initform 0.5)
   (double-sided-p :initform NIL :name "doubleSided")))

(define-element animation (named-element)
  ((channels :parse animation-channel)
   (samplers :parse animation-sampler)))

(define-element animation-channel (gltf-element)
  (sampler
   (target :parse animation-channel-target)))

(define-element animation-channel-target (gltf-element)
  ((node :ref nodes)
   (path :parse keyword)))

(define-element animation-sampler (gltf-element)
  ((input :ref accessors)
   (output :ref accessors)
   (interpolation :parse keyword)))

(define-element image (uri-element named-element)
  (mime-type
   (buffer-view :ref buffer-views)))

(define-element sampler (named-element)
  ((mag-filter :initform :linear :parse filter)
   (min-filter :initform :linear :parse filter)
   (wrap-s :initform :repeat :parse wrapping)
   (wrap-t :initform :repeat :parse wrapping)))

(define-element skin (named-element)
  ((inverse-bind-matrices :ref accessors)
   (skeleton :ref nodes)
   (joints :ref nodes)))

(define-element texture (named-element)
  ((sampler :ref samplers)
   (source :ref images)))

(define-element texture-info (gltf-element)
  ((texture :name "index" :ref textures)
   (tex-coord :initform 0)
   (scale :initform 1.0)
   (strength :initform 1.0)))

(define-element pbr (gltf-element)
  ((albedo :parse texture-info :name "baseColorTexture")
   (albedo-factor :initform #(1.0 1.0 1.0 1.0) :name "baseColorFactor")
   (metallic-factor :initform 1.0)
   (roughness-factor :initform 1.0)
   (matallic-roughness :parse texture-info :name "metallicRoughnessTexture")))
