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
   #:asset
   #:copyright
   #:generator
   #:version
   #:min-version
   #:scene
   #:nodes
   #:node
   #:camera
   #:parent
   #:children
   #:skin
   #:mesh
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
   #:metallic-roughness)
  ;; parser.lisp
  (:export
   #:parse
   #:with-gltf))
