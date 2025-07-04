(in-package #:org.shirakumo.fraf.gltf)

(define-element gltf-element ()
  (extensions
   extras
   (gltf :name null :initarg :gltf :initform NIL :reader gltf)))

(define-element named-element (gltf-element)
  (name))

(defmethod print-object ((element named-element) stream)
  (print-unreadable-object (element stream :type T)
    (format stream "~s" (name element))))

(define-element uri-element (gltf-element)
  (uri))

(defmethod path ((element uri-element))
  (merge-pathnames (uri element) (uri (gltf element))))

(define-element indexed-element (gltf-element)
  ((idx :name null :initarg :idx :accessor idx)))

(defmethod print-object ((element indexed-element) stream)
  (print-unreadable-object (element stream :type T)
    (format stream "#~d" (idx element))))

(define-element gltf (gltf-element)
  (uri
   (buffers :initform #() :parse buffer)
   (buffer-views :initform #() :parse buffer-view)
   (accessors :initform #() :parse accessor)
   (asset :initform (make-instance 'asset) :parse asset)
   (cameras :initform #() :parse camera)
   (meshes :initform #() :parse mesh)
   (images :initform #() :parse image)
   (samplers :initform #() :parse sampler)
   (textures :initform #() :parse texture)
   (materials :initform #() :parse material)
   (skins :initform #() :parse skin)
   (shapes :initform #() :parse shape :name ("extensions" "KHR_implicit_shapes" "shapes"))
   (nodes :initform #() :parse node)
   (animations :initform #() :parse animation)
   (scenes :initform #() :parse scene)
   (scene :initform NIL :ref scenes)
   (extensions-used :initform #())
   (extensions-required :initform #())
   (lights :initform #() :parse light :name ("extensions" "KHR_lights_punctual" "lights"))
   (image-lights :initform #() :parse image-light :name ("extensions" "EXT_lights_image_based"))
   (articulations :initform #() :parse articulation :name ("extensions" "AGI_articulations"))
   (physics-materials :initform #() :parse physics-material :name ("extensions" "KHR_physics_rigid_bodies" "physicsMaterials"))
   (physics-joint-limits :initform #() :parse physics-joint-limit :name ("extensions" "KHR_physics_rigid_bodies" "physicsJointLimits"))
   (collision-filters :initform #() :parse collision-filter :name ("extensions" "KHR_physics_rigid_bodies" "collisionFilters"))
   (material-variants :initform #() :parse material-variant :name ("extensions" "KHR_materials_variants" "variants"))
   (%mmap :initarg :%mmap :name null :accessor %mmap :initform NIL)))

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

(define-element scene (named-element indexed-element)
  ((nodes :initform #() :ref nodes)
   (light :initform NIL :ref image-lights :name ("extensions" "EXT_lights_image_based" "light"))
   (envmap :initform NIL :parse shirakumo-envmap :name ("extensions" "SHIRAKUMO_trial" "envmap"))))

(define-element node (named-element indexed-element)
  ((camera :ref cameras)
   (parent :name null :initform NIL :accessor parent)
   (children :initform #() :ref* nodes)
   (light :initform NIL :ref lights :name ("extensions" "KHR_lights_punctual" "light"))
   (articulations :initform #() :ref articulations :name ("extensions" "AGI_articulations" "articulationName"))
   (lods :initform #() :ref nodes :name ("extensions" "MSFT_lod" "ids"))
   (lod-screen-coverage :initform #() :name ("extras" "MSFT_screencoverage"))
   (collider :initform NIL :parse collider :name ("extensions" "KHR_physics_rigid_bodies" "collider"))
   (rigidbody :initform NIL :parse rigidbody :name ("extensions" "KHR_physics_rigid_bodies" "motion"))
   (trigger :initform NIL :parse trigger :name ("extensions" "KHR_physics_rigid_bodies" "trigger"))
   (shirakumo-trigger-data :initform NIL :parse shirakumo-trigger-data :name ("extensions" "SHIRAKUMO_trial"))
   (physics-joint :initform NIL :parse physics-joint :name ("extensions" "KHR_physics_rigid_bodies" "joint"))
   (virtual-p :initform NIL :name ("extensions" "SHIRAKUMO_trial" "virtual"))
   (prop-p :initform NIL :name ("extensions" "SHIRAKUMO_trial" "prop"))
   (instance-of :initform NIL :name ("extensions" "SHIRAKUMO_trial" "instanceOf"))
   (shirakumo-interactable :initform NIL :parse shirakumo-interactable :name ("extensions" "SHIRAKUMO_trial" "interactable"))
   (skin :ref* skins)
   (mesh :ref meshes)
   (instancing :initform NIL :parse gpu-instancing :name ("extensions" "EXT_mesh_gpu_instancing"))
   matrix
   rotation
   scale
   translation
   weights))

(define-element camera (named-element indexed-element)
  ((kind :parse keyword :name "type")))

(define-element orthographic-camera (camera)
  ((xmag :name ("orthographic" "xmag"))
   (ymag :name ("orthographic" "ymag"))
   (zfar :name ("orthographic" "zfar"))
   (znear :name ("orthographic" "znear"))))

(define-element perspective-camera (camera)
  ((aspect-ratio :name ("perspective" "aspectRatio"))
   (fov :name ("perspective" "yfov"))
   (zfar :name ("perspective" "zfar"))
   (znear :name ("perspective" "znear"))))

(define-element mesh (named-element indexed-element)
  ((primitives :initform #() :parse mesh-primitive)
   (target-names :initform #() :name ("extras" "targetNames"))
   weights))

(define-element mesh-primitive (gltf-element)
  ((attributes :initform (make-hash-table :test 'eql) :parse mesh-attributes)
   (indices :initform NIL :ref accessors)
   (material :ref materials)
   (mode :initform :triangles :parse primitive-mode)
   (targets :initform #() :parse mesh-attributes)
   (matrix :name ("extensions" "SHIRAKUMO_trial" "matrix"))
   (material-variants :parse material-variant-mapping :name ("extensions" "KHR_materials_variants" "mappings"))))

(define-element material (named-element indexed-element)
  ((pbr :parse pbr :name "pbrMetallicRoughness")
   (unlit-p :initform NIL :parse present :name ("extensions" "KHR_materials_unlit"))
   (normal-texture :parse texture-info)
   (occlusion-texture :parse texture-info)
   (emissive-texture :parse texture-info)
   (emissive-factor :initform #(0.0 0.0 0.0))
   (emissive-strength :initform 1.0 :name ("extensions" "KHR_materials_emissive_strength" "emissiveStrength"))
   (alpha-mode :initform :opaque :parse keyword)
   (alpha-cutoff :initform 0.5)
   (double-sided-p :initform NIL :name "doubleSided")
   (occlusion-metalness-roughness-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "occlusionRoughnessMetallicTexture"))
   (roughness-metallic-occlusion-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "roughnessMetallicOcclusionTexture"))
   (2d-normal-texture :parse texture-info :name ("extensions" "MSFT_packing_occlusionRoughnessMetallic" "normalTexture"))
   (lods :initform #() :ref nodes :name ("extensions" "MSFT_lod" "ids"))
   (lod-screen-coverage :initform #() :name ("extras" "MSFT_screencoverage"))))

(define-element animation (named-element indexed-element)
  ((channels :initform #() :parse animation-channel)
   (samplers :initform #() :parse animation-sampler)
   (kind :initform :default :name ("extensions" "SHIRAKUMO_trial" "type") :parse keyword)
   (velocity-scale :initform 1.0 :name ("extensions" "SHIRAKUMO_trial" "velocityScale"))
   (loop-p :initform T :name ("extensions" "SHIRAKUMO_trial" "loop"))
   (next :initform NIL :name ("extensions" "SHIRAKUMO_trial" "next"))
   (blend-duration :initform 0.2 :name ("extensions" "SHIRAKUMO_trial" "blendDuration"))
   (effects :initform #() :name ("extensions" "SHIRAKUMO_trial" "effects") :parse shirakumo-effect)))

(define-element animation-channel (gltf-element)
  (sampler
   (target :parse animation-channel-target)))

(define-element animation-channel-target (gltf-element)
  ((node :ref nodes)
   (path :parse keyword)
   (pointer :parse json-pointer :name ("extensions" "KHR_animation_pointer" "pointer"))))

(define-element animation-sampler (gltf-element)
  ((input :ref accessors)
   (output :ref accessors)
   (interpolation :initform :linear :parse keyword)))

(define-element image (indexed-element uri-element named-element)
  (mime-type
   (buffer-view :ref buffer-views)))

(define-element sampler (named-element indexed-element)
  ((mag-filter :initform :linear :parse filter)
   (min-filter :initform :linear :parse filter)
   (wrap-s :initform :repeat :parse wrapping)
   (wrap-t :initform :repeat :parse wrapping)))

(define-element skin (named-element indexed-element)
  ((inverse-bind-matrices :ref accessors)
   (skeleton :ref nodes)
   (joints :initform #() :ref nodes)))

(define-element texture (named-element indexed-element)
  ((sampler :ref samplers)
   (source :ref images)))

(define-element texture-info (gltf-element)
  ((texture :name "index" :ref textures)
   (tex-coord :initform 0)
   (strength :initform 1.0)
   (scale :initform 1.0)
   (scale2 :initform #(0.0 0.0) :name ("extensions" "KHR_texture_transform" "scale"))
   (offset :initform #(0.0 0.0) :name ("extensions" "KHR_texture_transform" "offset"))
   (rotation :initform 0.0 :name ("extensions" "KHR_texture_transform" "rotation"))
   (tex-coord2 :initform NIL :name ("extensions" "KHR_texture_transform" "texCoord"))))

(define-element pbr (gltf-element)
  ((albedo :parse texture-info :name "baseColorTexture")
   (albedo-factor :initform #(1.0 1.0 1.0 1.0) :name "baseColorFactor")
   (metallic-factor :initform 1.0)
   (roughness-factor :initform 1.0)
   (metallic-roughness :parse texture-info :name "metallicRoughnessTexture")))

(define-element light (indexed-element named-element gltf-element)
  ((kind :parse keyword :name "type")
   (color :initform #(1.0 1.0 1.0))
   (intensity :initform 1.0)))

(define-element directional-light (light)
  ())

(define-element point-light (light)
  ((range :initform NIL)))

(define-element spot-light (light)
  ((range :initform NIL)
   (inner-angle :initform 0.0 :name ("spot" "innerConeAngle"))
   (outer-angle :initform (/ PI 4.0) :name ("spot" "outerConeAngle"))))

(define-element image-light (named-element indexed-element)
  ((rotation :initform #(0.0 0.0 0.0 1.0))
   (intensity :initform 1.0)
   irradiance-coefficients
   specular-image-size
   (specular-images :ref images)))

(define-element articulation (named-element gltf-element)
  ((stages :initform #() :parse articulation-stage)
   pointing-vector))

(define-element articulation-stage (gltf-element)
  (name
   (kind :parse keyword :name "type")
   minimum-value
   maximum-value
   initial-value))

(define-element shape (indexed-element)
  ((kind :parse keyword :name "type")))

(define-element box-shape (shape)
  ((size :initform #(1.0 1.0 1.0) :name ("box" "size"))))

(define-element capsule-shape (shape)
  ((height :initform 0.5 :name ("capsule" "height"))
   (radius-top :initform 0.25 :name ("capsule" "radiusTop"))
   (radius-bottom :initform 0.25 :name ("capsule" "radiusBottom"))))

(define-element cylinder-shape (shape)
  ((height :initform 0.5 :name ("cylinder" "height"))
   (radius-top :initform 0.25 :name ("cylinder" "radiusTop"))
   (radius-bottom :initform 0.25 :name ("cylinder" "radiusBottom"))))

(define-element sphere-shape (shape)
  ((radius :initform 0.5 :name ("sphere" "radius"))))

(define-element collider (gltf-element)
  ((geometry :parse collider-geometry)
   (physics-material :ref physics-materials)
   (collision-filter :ref collision-filters)))

(define-element collider-geometry (gltf-element)
  ((shape :ref shapes)
   (node :ref* nodes)
   (convex-p :initform NIL :name "convexHull")))

(define-element rigidbody (gltf-element)
  ((kinematic-p :name "isKinematic")
   (mass :initform 1.0)
   (angular-damping :initform 0.1)
   (linear-damping :initform 0.04)
   (deactivated-p :initform NIL :name "startDeactivated")
   (center-of-mass :initform #(0.0 0.0 0.0))
   (inertia-orientation :initform #(0.0 0.0 0.0 0.0))
   (inertia-diagonal :initform #(1.0 1.0 1.0))
   (linear-velocity :initform #(0.0 0.0 0.0))
   (angular-velocity :initform #(0.0 0.0 0.0))
   (gravity-factor :initform 1.0)))

(define-element trigger (gltf-element)
  ((geometry :parse collider-geometry)
   (collision-filter :ref collision-filters)))

(define-element shirakumo-envmap (gltf-element)
  (file
   (orientation :initform #(0.0 0.0 0.0))
   (color :initform #(1.0 1.0 1.0))))

(define-element shirakumo-effect (gltf-element)
  ((name :name "effect")
   start
   (end :initform NIL)))

(define-element shirakumo-trigger-data (gltf-element)
  ((filter :initform "T")))

(define-element shirakumo-trigger (shirakumo-trigger-data)
  (form))

(define-element shirakumo-spawner (shirakumo-trigger-data)
  ((spawn)
   (spawn-count :initform 1)
   (auto-deactivate-p :initform T :name "autoDeactivate")
   (snap-to-surface-p :initform T :name "snapToSurface")
   (respawn-cooldown :initform 0.0)))

(define-element shirakumo-killvolume (shirakumo-trigger-data)
  (kill))

(define-element shirakumo-checkpoint (shirakumo-trigger-data)
  ((spawn-point :initform #(0.0 0.0 0.0))))

(define-element shirakumo-progression (shirakumo-trigger-data)
  ((state :initform :progression :parse keyword)
   (value :initform 1.0)
   (mode :initform :inc :parse keyword)
   (condition :initform "T")))

(define-element shirakumo-camera (shirakumo-trigger-data)
  ((state :initform :free :parse keyword)
   target
   offset))

(define-element shirakumo-interactable (gltf-element)
  (form
   interaction
   (kind :initform :inspectable :parse keyword)))

(define-element physics-material (indexed-element)
  ((static-friction :initform 0.6)
   (dynamic-friction :initform 0.6)
   (restitution :initform 0.0)
   (friction-combine :parse keyword)
   (restitution-combine :parse keyword)))

(define-element physics-joint (gltf-element)
  ((connected-node :ref nodes)
   (joint-limits :ref physics-joint-limits)
   (collision-enabled-p :name "enableCollision")))

(define-element physics-joint-limit (indexed-element)
  ((minimum-value :name "min")
   (maximum-value :name "max")
   spring-constant
   spring-damping
   linear-axes
   angular-axes))

(define-element collision-filter (indexed-element)
  (collision-systems
   not-collide-with-systems
   collide-with-systems))

(define-element material-variant (named-element indexed-element)
  ())

(define-element material-variant-mapping (named-element)
  ((material :ref materials)
   (variants :ref material-variants)))
