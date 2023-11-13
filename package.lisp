(defpackage #:org.shirakumo.fraf.gltf
  (:use #:cl)
  (:local-nicknames
   (#:mmap #:org.shirakumo.fraf.trial.mmap)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  ;; accessor.lisp
  (:export
   #:construct-element-reader
   #:construct-element-writer
   #:buffer
   #:start
   #:byte-length
   #:buffer-view
   #:buffer
   #:byte-offset
   #:byte-length
   #:byte-stride
   #:target
   #:accessor
   #:buffer-view
   #:byte-offset
   #:component-type
   #:element-type
   #:size
   #:normalized
   #:maximum
   #:minimum
   #:byte-stride
   #:element-reader
   #:element-writer)
  ;; format.lisp
  (:export
   #:gltf-element
   #:extensions
   #:extras
   #:gltf
   #:named-element
   #:name
   #:uri-element
   #:uri
   #:path
   #:indexed-element
   #:idx
   #:gltf
   #:buffers
   #:buffer-views
   #:accessors
   #:asset
   #:cameras
   #:meshes
   #:images
   #:samplers
   #:textures
   #:materials
   #:skins
   #:nodes
   #:animations
   #:scenes
   #:lights
   #:image-lights
   #:articulations
   #:asset
   #:copyright
   #:generator
   #:version
   #:min-version
   #:scene
   #:nodes
   #:light
   #:node
   #:lods
   #:lod-screen-coverage
   #:shape
   #:rigidbody
   #:trigger
   #:physics-joint
   #:physics-material
   #:camera
   #:parent
   #:children
   #:skin
   #:mesh
   #:lights
   #:articulations
   #:shapes
   #:physics-materials
   #:physics-joint-limits
   #:collision-filters
   #:matrix
   #:rotation
   #:scale
   #:translation
   #:weights
   #:camera
   #:orthographic-camera
   #:xmag
   #:ymag
   #:zfar
   #:znear
   #:perspective-camera
   #:aspect-ratio
   #:fov
   #:zfar
   #:znear
   #:mesh
   #:primitives
   #:weights
   #:mesh-primitive
   #:attributes
   #:indices
   #:material
   #:mode
   #:targets
   #:material
   #:pbr
   #:normal-texture
   #:occlusion-texture
   #:emissive-texture
   #:emissive-factor
   #:alpha-mode
   #:alpha-cutoff
   #:double-sided-p
   #:occlusion-metalness-roughness-texture
   #:roughness-metallic-occlusion-texture
   #:2d-normal-texture
   #:animation
   #:channels
   #:samplers
   #:animation-channel
   #:sampler
   #:target
   #:animation-channel-target
   #:node
   #:path
   #:animation-sampler
   #:input
   #:output
   #:interpolation
   #:image
   #:mime-type
   #:buffer-view
   #:sampler
   #:mag-filter
   #:min-filter
   #:wrap-s
   #:wrap-t
   #:skin
   #:inverse-bind-matrices
   #:skeleton
   #:joints
   #:texture
   #:sampler
   #:source
   #:texture-info
   #:texture
   #:tex-coord
   #:scale
   #:strength
   #:pbr
   #:albedo
   #:albedo-factor
   #:metallic-factor
   #:roughness-factor
   #:metallic-roughness
   #:light
   #:kind
   #:color
   #:intensity
   #:range
   #:inner-angle
   #:outer-angle
   #:image-light
   #:rotation
   #:intensity
   #:irradiance-coefficients
   #:specular-image-size
   #:specular-images
   #:articulation
   #:stages
   #:pointing-vector
   #:articulation-stage
   #:name
   #:kind
   #:minimum-value
   #:maximum-value
   #:initial-value
   #:shape
   #:box-shape
   #:size
   #:capsule-shape
   #:height
   #:radius
   #:convex-shape
   #:mesh
   #:cylinder-shape
   #:height
   #:radius
   #:sphere-shape
   #:radius
   #:trimesh-shape
   #:mesh
   #:collider
   #:shape
   #:physics-material
   #:collision-filter
   #:rigidbody
   #:kinematic-p
   #:mass
   #:center-of-mass
   #:inertia-orientation
   #:inertia-diagonal
   #:linear-velocity
   #:angular-velocity
   #:gravity-factor
   #:trigger
   #:shape
   #:collision-filter
   #:physics-material
   #:static-friction
   #:dynamic-friction
   #:restitution
   #:friction-combine
   #:restitution-combine
   #:physics-joint
   #:connected-node
   #:joint-limits
   #:collision-enabled-p
   #:physics-joint-limit
   #:minimum-value
   #:maximum-value
   #:spring-constant
   #:spring-damping
   #:linear-axes
   #:angular-axes
   #:collision-filter
   #:collision-systems
   #:collide-with-systems
   #:not-collide-with-systems)
  ;; parser.lisp
  (:export
   #:parse
   #:with-gltf))
