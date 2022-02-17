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
   parent
   (children :ref nodes)
   (skin :ref skins)
   matrix
   (mesh :ref meshes)
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
  (attributes
   (indices :ref accessors)
   (material :ref materials)
   mode
   targets))

(define-element material (named-element)
  ((pbr :parse pbr :name "pbrMetallicRoughness")
   (normal :parse texture-info)
   (occlusion :parse texture-info)
   (emission :parse texture-info)
   emission-factor
   (alpha-mode :parse alpha-mode)
   alpha-cutoff
   double-sided-p))

(define-element animation (named-element)
  ((channels :parse animation-channel)
   (samplers :parse animation-sampler)))

(define-element animation-channel (gltf-element)
  (sampler
   (target :parse animation-channel-target)))

(define-element animation-channel-target (gltf-element)
  ((node :ref nodes)
   path))

(define-element animation-sampler (gltf-element)
  ((input :ref accessors)
   (output :ref accessors)
   (interpolation :parse interpolation)))

(define-element image (uri-element named-element)
  (mime-type
   (buffer-view :parse buffer-views)))

(define-element sampler (named-element)
  (mag-filter
   min-filter
   wrap-s
   wrap-t))

(define-element skin (named-element)
  ((inverse-bind-matrices :ref accessors)
   (skeleton :ref nodes)
   (joints :ref nodes)))

(define-element texture (named-element)
  ((sampler :ref samplers)
   (source :ref images)))

(define-element texture-info (gltf-element)
  ((texture :ref textures)
   tex-coord
   scale
   strength))

(define-element pbr (gltf-element)
  ((albedo :parse texture-info :name "baseColorTexture")
   (albedo-factor :name "baseColorFactor")
   metallic-factor
   roughness-factor
   (matallic-roughness :parse texture-info :name "metallicRoughnessTexture")))
