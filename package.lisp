(defpackage #:org.shirakumo.fraf.gltf
  (:use #:cl)
  (:shadow #:condition)
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
  ;; construction.lisp
  (:export
   #:push-child
   #:make-indexed
   #:make-simple-view
   #:make-mesh-primitive
   #:extra)
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
   #:envmap
   #:node
   #:lods
   #:lod-screen-coverage
   #:shape
   #:rigidbody
   #:trigger
   #:shirakumo-trigger-data
   #:physics-joint
   #:virtual-p
   #:physics-material
   #:camera
   #:parent
   #:children
   #:skin
   #:mesh
   #:light
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
   #:kind
   #:velocity-scale
   #:loop-p
   #:next
   #:blend-duration
   #:effects
   #:animation-channel
   #:sampler
   #:target
   #:animation-channel-target
   #:node
   #:path
   #:pointer
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
   #:offset
   #:rotation
   #:scale2
   #:tex-coord2
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
   #:directional-light
   #:point-light
   #:range
   #:spot-light
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
   #:radius-top
   #:radius-bottom
   #:mesh
   #:cylinder-shape
   #:height
   #:radius-top
   #:radius-bottom
   #:sphere-shape
   #:radius
   #:mesh
   #:collider
   #:geometry
   #:shape
   #:node
   #:convex-p
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
   #:shirakumo-envmap
   #:file
   #:orientation
   #:color
   #:shirakumo-effect
   #:start
   #:end
   #:shirakumo-trigger-data
   #:filter
   #:shirakumo-trigger
   #:form
   #:shirakumo-spawner
   #:spawn
   #:spawn-count
   #:auto-deactivate-p
   #:respawn-cooldown
   #:shirakumo-killvolume
   #:kill
   #:shirakumo-checkpoint
   #:spawn-point
   #:shirakumo-progression
   #:state
   #:value
   #:mode
   #:condition
   #:shirakumo-camera
   #:state
   #:target
   #:offset
   #:shirakumo-interactable
   #:form
   #:interaction
   #:kind
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
   #:with-gltf)
  ;; printer.lisp
  (:export
   #:merge-buffers
   #:normalize-buffers
   #:urlify-buffers
   #:serialize))
