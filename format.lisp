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
   (scenes :initform #() :parse scene)
   (lights :initform #() :parse light :name ("extensions" "KHR_lights_punctual"))
   (image-lights :initform #() :parse image-light :name ("extensions" "EXT_lights_image_based"))
   (articulations :initform #() :parse articulation :name ("extensions" "AGI_articulations"))
   (shapes :initform #() :parse shape :name ("extensions" "KHR_collision_shapes" "shapes"))
   (physics-materials :initform #() :parse physics-material :name ("extensions" "KHR_rigid_bodies" "physicsMaterials"))
   (physics-joint-limits :initform #() :parse physics-joint-limit :name ("extensions" "KHR_rigid_bodies" "physicsJointLimits"))
   (collision-filters :initform #() :parse collision-filter :name ("extensions" "KHR_rigid_bodies" "collisionFilters"))
   (%mmap :initform NIL)))

(defmethod initialize-instance :after ((gltf gltf) &key)
  (setf (slot-value gltf 'gltf) gltf))

(defmethod close ((gltf gltf) &key abort)
  (declare (ignore abort))
  (loop for buffer across (buffers gltf)
        do (close buffer))
  (when (%mmap gltf)
    (apply #'mmap:munmap (%mmap gltf))
    (setf (%mmap gltf) NIL)))

(define-element asset (gltf-element)
  (copyright
   generator
   version
   min-version))

(define-element scene (indexed-element named-element)
  ((nodes :initform #() :ref nodes)
   (light :initform NIL :ref image-lights :name ("extensions" "EXT_lights_image_based" "light"))))

(define-element node (indexed-element named-element)
  ((camera :ref cameras)
   (parent :name null :initform NIL :accessor parent)
   (children :initform #())
   (lights :initform #() :ref lights :name ("extensions" "KHR_lights_punctual"))
   (articulations :initform #() :ref articulations :name ("extensions" "AGI_articulations" "articulationName"))
   (lods :initform #() :ref nodes :name ("extensions" "MSFT_lod" "ids"))
   (lod-screen-coverage :initform #() :name ("extras" "MSFT_screencoverage"))
   (collider :initform NIL :parse collider :name ("extensions" "KHR_rigid_bodies" "collider"))
   (rigidbody :initform NIL :parse rigidbody :name ("extensions" "KHR_rigid_bodies" "motion"))
   (trigger :initform NIL :parse trigger :name ("extensions" "KHR_rigid_bodies" "trigger"))
   (physics-joint :initform NIL :parse physics-joint :name ("extensions" "KHR_rigid_bodies" "joint"))
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
   (double-sided-p :initform NIL :name "doubleSided")
   (occlusion-metalness-roughness-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "occlusionRoughnessMetallicTexture"))
   (roughness-metallic-occlusion-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "roughnessMetallicOcclusionTexture"))
   (2d-normal-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "normalTexture"))
   (lods :initform #() :ref nodes :name ("extensions" "MSFT_lod" "ids"))
   (lod-screen-coverage :initform #() :name ("extras" "MSFT_screencoverage"))))

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
   (metallic-roughness :parse texture-info :name "metallicRoughnessTexture")))

(define-element light (indexed-element named-element gltf-element)
  ((kind :parse :keyword :name "type")
   (color :initform #(1.0 1.0 1.0))
   (intensity :initform 1.0)
   (range :initform NIL)
   (inner-angle :initform 0.0 :name ("spot" "innerConeAngle"))
   (outer-angle :initform (/ PI 4.0) :name ("spot" "outerConeAngle"))))

(define-element image-light (indexed-element named-element)
  ((rotation :initform #(0.0 0.0 0.0 1.0))
   (intensity :initform 1.0)
   irradiance-coefficients
   specular-image-size
   (specular-images :ref images)))

(define-element articulation (named-element gltf-element)
  ((stages :initform #() :parse articulation-stage)
   pointing-vector))

(define-element articulation-stage ()
  (name
   (kind :parse :keyword :name "type")
   minimum-value
   maximum-value
   initial-value))

(define-element shape ()
  ())

(define-element box-shape (shape)
  ((size :initform #(1.0 1.0 1.0))))

(define-element capsule-shape (shape)
  ((height :initform 0.5)
   (radius :initform 0.25)))

(define-element convex-shape (shape)
  ((mesh :ref meshes)))

(define-element cylinder-shape (shape)
  ((height :initform 0.5)
   (radius :initform 0.25)))

(define-element sphere-shape (shape)
  ((radius :initform 0.5)))

(define-element trimesh-shape (shape)
  ((mesh :ref meshes)))

(define-element collider ()
  ((shape :ref shapes)
   (physics-material :ref physics-materials)
   (collision-filter :ref collision-filters)))

(define-element rigidbody ()
  ((kinematic-p :name "isKinematic")
   (mass :initform 1.0)
   (center-of-mass :initform #(0.0 0.0 0.0))
   (inertia-orientation :initform #(0.0 0.0 0.0 0.0))
   (inertia-diagonal :initform #(1.0 1.0 1.0))
   (linear-velocity :initform #(0.0 0.0 0.0))
   (angular-velocity :initform #(0.0 0.0 0.0))
   (gravity-factor :initform 1.0)))

(define-element trigger ()
  ((shape :ref shapes)
   (collision-filter :ref collision-filters)))

(define-element physics-material ()
  ((static-friction :initform 0.6)
   (dynamic-friction :initform 0.6)
   (restitution :initform 0.0)
   (friction-combine :parse :keyword)
   (restitution-combine :parse :keyword)))

(define-element physics-joint ()
  ((connected-node :ref nodes)
   (joint-limits :ref physics-joint-limits)
   (collision-enabled-p :name "enableCollision")))

(define-element physics-joint-limit ()
  ((minimum-value :name "min")
   (maximum-value :name "max")
   spring-constant
   spring-damping
   linear-axes
   angular-axes))

(define-element collision-filter ()
  (collision-systems
   not-collide-with-systems
   collide-with-systems))
