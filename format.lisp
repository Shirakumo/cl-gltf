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

(define-element indexed-element (gltf-element)
  ((idx :name NIL :accessor idx)))

(define-element gltf (gltf-element)
  (uri
   (buffers :initform #() :parse buffer)
   (buffer-views :initform #() :parse buffer-view)
   (accessors :initform #() :parse accessor)
   (asset :parse asset)
   (cameras :initform #() :parse camera)
   (meshes :initform #() :parse mesh)
   (images :initform #() :parse image)
   (samplers :initform #() :parse sampler)
   (textures :initform #() :parse texture)
   (materials :initform #() :parse material)
   (skins :initform #() :parse skin)
   (nodes :initform #() :parse node)
   (animations :initform #() :parse animation)
   (scenes :initform #() :parse scene)))

(defmethod initialize-instance :after ((gltf gltf) &key)
  (setf (slot-value gltf 'gltf) gltf))

(defmethod close ((gltf gltf) &key abort)
  (declare (ignore abort))
  (loop for buffer across (buffers gltf)
        do (close buffer)))

(define-element asset (gltf-element)
  (copyright
   generator
   version
   min-version))

(define-element scene (indexed-element named-element)
  ((nodes :initform #() :ref nodes)))

(define-element node (indexed-element named-element)
  ((camera :ref cameras)
   (parent :name null :initform NIL :accessor parent)
   (children :initform #())
   skin
   (mesh :ref meshes)
   matrix
   rotation
   scale
   translation
   weights))

(define-element camera (indexed-element named-element)
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

(define-element mesh (indexed-element named-element)
  ((primitives :initform #() :parse mesh-primitive)
   weights))

(define-element mesh-primitive (gltf-element)
  ((attributes :initform #() :parse mesh-attributes)
   (indices :initform #() :ref accessors)
   (material :ref materials)
   (mode :initform :triangles :parse primitive-mode)
   targets))

(define-element material (indexed-element named-element)
  ((pbr :parse pbr :name "pbrMetallicRoughness")
   (normal-texture :parse texture-info)
   (occlusion-texture :parse texture-info)
   (emissive-texture :parse texture-info)
   (emissive-factor :initform #(0.0 0.0 0.0))
   (alpha-mode :initform :opaque :parse keyword)
   (alpha-cutoff :initform 0.5)
   (double-sided-p :initform NIL :name "doubleSided")))

(define-element animation (indexed-element named-element)
  ((channels :initform #() :parse animation-channel)
   (samplers :initform #() :parse animation-sampler)))

(define-element animation-channel (gltf-element)
  (sampler
   (target :parse animation-channel-target)))

(define-element animation-channel-target (gltf-element)
  ((node :ref nodes)
   (path :parse keyword)))

(define-element animation-sampler (gltf-element)
  ((input :ref accessors)
   (output :ref accessors)
   (interpolation :initform :linear :parse keyword)))

(define-element image (indexed-element uri-element named-element)
  (mime-type
   (buffer-view :ref buffer-views)))

(define-element sampler (indexed-element named-element)
  ((mag-filter :initform :linear :parse filter)
   (min-filter :initform :linear :parse filter)
   (wrap-s :initform :repeat :parse wrapping)
   (wrap-t :initform :repeat :parse wrapping)))

(define-element skin (indexed-element named-element)
  ((inverse-bind-matrices :ref accessors)
   (skeleton :ref nodes)
   (joints :initform #() :ref nodes)))

(define-element texture (indexed-element named-element)
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
