(in-package #:org.shirakumo.fraf.gltf)

;; accessor.lisp
(docs:define-docs
  (function construct-element-reader
    "Construct a reader function for the given element.

ELEMENT-TYPE can be (barring extensions) one of the following:
  :SCALAR
  :VEC2
  :VEC3
  :VEC4
  :MAT2
  :MAT3
  :MAT4

COMPONENT-TYPE can be (barring extensions) one of the following:
  :INT8
  :UINT8
  :INT16
  :UINT16
  :INT32
  :UINT32
  :INT64
  :UINT64
  :FLOAT
  :DOUBLE

This function should return a function of one argument, a
CFFI:FOREIGN-POINTER, and two return values: the read value at the
pointer and a pointer to the memory region immediately following the
read element.

See ACCESSOR (type)")

  (function construct-element-writer
    "Construct a reader function for the given element.

ELEMENT-TYPE can be (barring extensions) one of the following:
  :SCALAR
  :VEC2
  :VEC3
  :VEC4
  :MAT2
  :MAT3
  :MAT4

COMPONENT-TYPE can be (barring extensions) one of the following:
  :INT8
  :UINT8
  :INT16
  :UINT16
  :INT32
  :UINT32
  :INT64
  :UINT64
  :FLOAT
  :DOUBLE

This function should return a function of two arguments: the value to
write and a CFFI:FOREIGN-POINTER, and one return value: a pointer to
the memory region immediately following the read element.

See ACCESSOR (type)")

  (type buffer
    "Representation of an octet buffer.

This is a SEQUENCE and can be used as such.

Once the buffer is no longer required, it must be CLOSEd to free up
potential resources kept by the buffer in the back.

See START
See BYTE-LENGTH
See INDEXED-ELEMENT
See URI-ELEMENT
See NAMED-ELEMENT")

  (function start
    "Returns a pointer to the start of the memory region represented by the buffer, view, or accessor.

See BUFFER
See BUFFER-VIEW
See ACCESSOR")

  (function byte-length
    "Returns the length of the buffer in octets.

See BUFFER
See BUFFER-VIEW")

  (type buffer-view
    "Representation of a view into a buffer.

This is a SEQUENCE and can be used as such.

See BUFFER
See START
See BYTE-OFFSET
See BYTE-LENGTH
See BYTE-STRIDE
See TARGET
See INDEXED-ELEMENT
See NAMED-ELEMENT")

  (function buffer
    "Returns the buffer indexed into by the buffer-view.

See BUFFER
See BUFFER-VIEW")

  (function byte-offset
    "Returns the offset in octets from the start of the buffer.

See ACCESSOR
See BUFFER-VIEW")

  (function byte-stride
    "Returns the number of bytes between valid elements in the buffer.

Note that this may be NIL.

See ACCESSOR
See BUFFER-VIEW")

  (function target
    "Returns an OpenGL integer representing the target of the buffer data.

See BUFFER-VIEW")

  (type accessor
    "Representation of an attribute accessor into a buffer.

This is a SEQUENCE and can be used as such.

See BUFFER-VIEW
See BYTE-OFFSET
See COMPONENT-TYPE
See ELEMENT-TYPE
See SIZE
See NORMALIZED
See MAXIMUM
See MINIMUM
See START
See BYTE-STRIDE
See ELEMENT-READER
See ELEMENT-WRITER
See INDEXED-ELEMENT
See NAMED-ELEMENT")

  (function buffer-view
    "Returns the buffer-view this accessor indexes into.

See ACCESSOR")

  (function component-type
    "Returns the compound component type this accessor reads.

Barring extensions, this can be one of:
  :SCALAR
  :VEC2
  :VEC3
  :VEC4
  :MAT2
  :MAT3
  :MAT4

See ACCESSOR")

  (function element-type
    "Returns the underlying type of values read by the accessor for each compound value.

Barring extensions, this can be one of:
  :INT8
  :UINT8
  :INT16
  :UINT16
  :INT32
  :UINT32
  :INT64
  :UINT64
  :FLOAT
  :DOUBLE

See ACCESSOR")

  (function size
    "Returns the number of elements this accessor may read.

See ACCESSOR")

  (function normalized
    "Returns whether the values in the buffer are normalized or not.

See ACCESSOR")

  (function maximum
    "Returns a representation of the maximal value of any values this accessor may read.

This is typically either a single scalar value or an array of values.

See ACCESSOR")

  (function minimum
    "Returns a representation of the minimal value of any values this accessor may read.

This is typically either a single scalar value or an array of values.

See ACCESSOR")

  (function element-reader
    "Accesses the function used to read out values from the underlying buffer memory.

Unless manually set, this function is computed by calling
CONSTRUCT-ELEMENT-READER using the accessor's component-type and
element-type.

See CONSTRUCT-ELEMENT-READER
See ACCESSOR")

  (function element-writer
    "Accesses the function used to write values to the underlying buffer memory.

Unless manually set, this function is computed by calling
CONSTRUCT-ELEMENT-WRITER using the accessor's component-type and
element-type.

See CONSTRUCT-ELEMENT-WRITER
See ACCESSOR"))

;; format.lisp
(docs:define-docs
  (type gltf-element
    "Base class for any part of a glTF representation

See EXTENSIONS
See EXTRAS
See GLTF")

  (function extensions
    "Accessor to opaque extension blobs.

See GLTF-ELEMENT")

  (function extras
    "Accessor to opaque extras blobs.

See GLTF-ELEMENT")

  (function gltf
    "Returns the base GLTF instance this element is associated with.

See GLTF-ELEMENT")

  (type named-element
    "An element with a potential name attached.

See GLTF-ELEMENT
See NAME")

  (function name
    "Accesses the name of the element. May be NIL or a STRING.

See NAMED-ELEMENT")

  (type uri-element
    "An element with a URI attached.

See GLTF-ELEMENT
See URI
See PATH")

  (function uri
    "Accesses the URI. May be NIL or a STRING.

See URI-ELEMENT")

  (function path
    "Returns the pathname of the URI-ELEMENT, if available.

See URI-ELEMENT")

  (type indexed-element
    "An element that has a base index within the gltf file's collection.

See GLTF-ELEMENT
See IDX")

  (function idx
    "Returns the index of the element within its collection.

See INDEXED-ELEMENT")

  (type gltf
    "Base of a GLTF file's representation.

A GLTF instance is a collection of subobjects, which may nest and
refer to each other.

Once you are done, you **must** call CLOSE on the instance to ensure
backing buffers and other associated resources are properly freed.

See PARSE
See WITH-GLTF
See GLTF-ELEMENT
See URI
See BUFFERS
See BUFFER-VIEWS
See ACCESSORS
See ASSET
See CAMERAS
See MESHES
See IMAGES
See SAMPLERS
See TEXTURES
See MATERIALS
See SKINS
See NODES
See ANIMATIONS
See SCENES")

  (function buffers
    "Accesses the array of buffer objects.

See GLTF
See BUFFER")

  (function buffer-views
    "Accesses the array of buffer-view objects.

See BUFFER-VIEW
See GLTF")

  (function accessors
    "Accesses the array of accessor objects.

See ACCESSOR
See GLTF")

  (function asset
    "Accesses the asset object.

See ASSET
See GLTF")

  (function cameras
    "Accesses the array of camera objects.

See CAMERA
See GLTF")

  (function meshes
    "Accesses the array of mesh objects.

See MESH
See GLTF")

  (function images
    "Accesses the array of image objects.

See IMAGE
See GLTF")

  (function samplers
    "Accesses the array of sampler objects.

See SAMPLER
See GLTF")

  (function textures
    "Accesses the array of texture objects.

See TEXTURE
See GLTF")

  (function materials
    "Accesses the array of material objects.

See MATERIAL
See GLTF")

  (function skins
    "Accesses the array of skin objects.

See SKIN
See GLTF")

  (function nodes
    "Accesses the array of node objects.

See NODE
See SCENE
See GLTF")

  (function animations
    "Accesses the array of animation objects.

See ANIMATION
See GLTF")

  (function scenes
    "Accesses the array of scene objects.

See SCENE
See GLTF")

  (type asset
    "Represents basic metadata about the GLTF file.

See GLTF-ELEMENT
See COPYRIGHT
See GENERATOR
See VERSION
See MIN-VERSION")

  (function copyright
    "Accesses a string noting the copyright information.

See ASSET")

  (function generator
    "Accesses a string noting the software used to generate the file.

See ASSET")

  (function version
    "Accesses a string noting the version of the glTF format.

See ASSET")

  (function min-version
    "Accesses a string specifying the minimum version required to parse the file successfully.

See ASSET")

  (type scene
    "Represents a the root node of a scene.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See NODES")

  (type node
    "Represents a node in the scene graph.

A node may have transformation information associated with it, as well
as several child nodes or a mesh. A node may also not have any
children or meshes, such as in the case of bones or cameras.

See CAMERA
See PARENT
See CHILDREN
See SKIN
See MESH
See MATRIX
See ROTATION
See SCALE
See TRANSLATION
See WEIGHTS")

  (function camera
    "Accesses the camera attached to the node.

See CAMERA
See NODE")

  (function parent
    "Accesses the parent of the node.

See NODE")

  (function children
    "Accesses the array of child nodes.

See NODE")

  (function skin
    "Accesses the skin attached to the node.

See SKIN
See NODE")

  (function mesh
    "Accesses the mesh attached to the node.

See MESH
See NODE")

  (function instancing
    "Accesses the GPU instancing accessors for the node.

Each entry in the table is an identifier of the attribute to instance,
and the associated accessor to pull the data for attribute from. Every
accessor in the table must have the same count. Only the keys
:TRANSLATION, :ROTATION, :SCALE are standardised, all other attributes
are provided as strings.

See NODE")

  (function matrix
    "Accesses the transformation matrix attached to the node.

The matrix should be a 16-element float array representing the 4x4
matrix.

See NODE")

  (function rotation
    "Accesses the rotation quaternion attached to the node.

Expressed as a 4-element float array.

See NODE")

  (function scale
    "Accesses the scaling of the node.

Expressed as a 3-element float array.

See NODE")

  (function translation
    "Accesses the translation of the node.

Expressed as a 3-element float array.

See NODE")

  (function weights
    "Accesses the weights of the morph target.

Expressed as a float array.

See MESH
See NODE")

  (type camera
    "Base type for cameras.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See ORTHOGRAPHIC-CAMERA
See PERSPECTIVE-CAMERA")

  (type orthographic-camera
    "Representation of an orthographic projection camera.

See CAMERA
See XMAG
See YMAG
See ZFAR
See ZNEAR")

  (function xmag
    "Accesses the horizontal magnification of the view.

See ORTHOGRAPHIC-CAMERA")

  (function ymag
    "Accesses the vertical magnification of the view.

See ORTHOGRAPHIC-CAMERA")

  (function zfar
    "Accesses the distance to the far clipping plane.

See ORTHOGRAPHIC-CAMERA
See PERSPECTIVE-CAMERA")

  (function znear
    "Accesses the distance to the near clipping plane.

See ORTHOGRAPHIC-CAMERA
See PERSPECTIVE-CAMERA")

  (type perspective-camera
    "Representation of a perspective projection camera.

See CAMERA
See ASPECT-RATIO
See FOV
See ZFAR
See ZNEAR")

  (function aspect-ratio
    "Accesses the view aspect ratio of the camera.

See PERSPECTIVE-CAMERA")

  (function fov
    "Accesses the field of view value of the camera.

See PERSPECTIVE-CAMERA")

  (type mesh
    "Representation of a mesh.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See PRIMITIVES
See WEIGHTS")

  (function primitives
    "Accesses the array of mesh primitives that make up the mesh.

See MESH-PRIMITIVE
See MESH")

  (type mesh-primitive
    "Representation of a mesh primitive -- a singular geometric object of a consistent material.

See GLTF-ELEMENT
See ATTRIBUTES
See INDICES
See MATERIAL
See MODE
See TARGETS")

  (function attributes
    "Accesses the hash table of mesh attributes.

The values are ACCESSORs
The keys are keywords, out of:
  :POSITION
  :NORMAL
  :TANGENT
  :TEXCOORD_0 ...
  :COLOR_0 ...
  :JOINTS_0 ...
  :WEIGHTS_0 ...

See ACCESSOR
See MESH-PRIMITIVE")

  (function indices
    "Accesses the accessor of vertex indices.

See ACCESSOR
See MESH-PRIMITIVE")

  (function material
    "Accesses the material used for the mesh primitive.

See MATERIAL
See MESH-PRIMITIVE")

  (function mode
    "Accesses the \"mode\" of the vertices in the mesh.

Can be one of the following:
  :POINTS
  :LINES
  :LINE-LOOP
  :LINE-STRIP
  :TRIANGLES
  :TRIANGLE-STRIP
  :TRIANGLE-FAN

See MESH-PRIMITIVE")

  (function targets
    "Accesses the array of morph targets.

See MESH-PRIMITIVE")

  (type material
    "Representation of a mesh material description.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See PBR
See NORMAL-TEXTURE
See OCCLUSION-TEXTURE
See EMISSIVE-TEXTURE
See EMISSIVE-FACTOR
See ALPHA-MODE
See ALPHA-CUTOFF
See DOUBLE-SIDED-P")

  (function pbr
    "Accesses the PBR material descriptor.

See PBR
See MATERIAL")

  (function normal-texture
    "Accesses the normal-map texture.

See TEXTURE-INFO
See MATERIAL")

  (function occlusion-texture
    "Accesses the occlusion-map texture.

See TEXTURE-INFO
See MATERIAL")

  (function emissive-texture
    "Accesses the emission-map texture.

See TEXTURE-INFO
See MATERIAL")

  (function emissive-factor
    "Accesses the emission factor of the emission-map.

Should be an array of three floats.

See MATERIAL")

  (function alpha-mode
    "Accesses the alpha-blending mode of the material.

Can be one of the following:
  :OPAQUE
  :MASK
  :BLEND

See MATERIAL")

  (function alpha-cutoff
    "Accesses the alpha cutoff value when the material is in mask mode.

See MATERIAL")

  (function double-sided-p
    "Accesses whether the material should be double-sided or not.

See MATERIAL")

  (type animation
    "Representation of an animation.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See CHANNELS
See SAMPLERS")

  (function channels
    "Accesses the array of animation-channels.

See ANIMATION-CHANNEL
See ANIMATION")

  (function samplers
    "Accesses the array of animation-samplers

See ANIMATION-SAMPLER
See ANIMATION")

  (type animation-channel
    "Representation of an animation channel.

See GLTF-ELEMENT
See SAMPLER
See TARGET")

  (function sampler
    "Accesses for the index of the animation-sampler used for the animation-channel.

You should reference this into the SAMPLERS array of the ANIMATION.

See ANIMATION-CHANNEL")

  (function target
    "Accesses for the animation-channel-target.

See ANIMATION-CHANNEL-TARGET
See ANIMATION-CHANNEL")

  (type animation-channel-target
    "Representation of an animation channel target.

See GLTF-ELEMENT
See NODE
See PATH")

  (function node
    "Accesses for the node that the channel animates.

See NODE
See ANIMATION-CHANNEL-TARGET")

  (function path
    "Accesses for the property that the channel animates.

Can be one of the following:
  :TRANSLATION
  :ROTATION
  :SCALE
  :WEIGHTS

See ANIMATION-CHANNEL")

  (type animation-sampler
    "Representation of a sampler for an animation-channel.

See GLTF-ELEMENT
See INPUT
See OUTPUT
See INTERPOLATION")

  (function input
    "Accesses to the accessor that provides the times of the keyframes.

See ACCESSOR
See ANIMATION-SAMPLER")

  (function output
    "Accesses to the accessor that provides the values of the keyframes.

See ACCESSOR
See ANIMATION-SAMPLER")

  (function interpolation
    "Accesses to the interpolation type of the animation sampler.

Can be one of the following:
  :STEP
  :LINEAR
  :CUBICSPLINE

See ANIMATION-SAMPLER")

  (type image
    "Representation of an image.

An image may either be represented by an URI to another file, an
inline data-uri, or a buffer-view that provides the image data.

See INDEXED-ELEMENT
See URI-ELEMENT
See NAMED-ELEMENT
See MIME-TYPE
See BUFFER-VIEW")

  (function mime-type
    "Accesses to the string mime-type of the image.

Should be either:
  \"image/jpeg\"
  \"image/png\"

See IMAGE")

  (type sampler
    "Representation of a texture sampler.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See MAG-FILTER
See MIN-FILTER
See WRAP-S
See WRAP-T")

  (function mag-filter
    "Accesses to the magnification filter of the sampler.

Can be one of the following:
  :NEAREST
  :LINEAR

See SAMPLER")

  (function min-filter
    "Accesses to the minification filter of the sampler.

Can be one of the following:
  :NEAREST
  :LINEAR
  :NEAREST-MIPMAP-NEAREST
  :LINEAR-MIPMAP-NEAREST
  :NEAREST-MIPMAP-LINEAR
  :LINEAR-MIPMAP-LINEAR

See SAMPLER")

  (function wrap-s
    "Accesses to the U-wrapping mode of the sampler.

Can be one of the following:
  :CLAMP-TO-EDGE
  :MIRRORED-REPEAT
  :REPEAT

See SAMPLER")

  (function wrap-t
    "Accesses to the V-wrapping mode of the sampler.

Can be one of the following:
  :CLAMP-TO-EDGE
  :MIRRORED-REPEAT
  :REPEAT

See SAMPLER")

  (type skin
    "Representation of a mesh's skinning information.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See INVERSE-BIND-MATRICES
See SKELETON
See JOINTS")

  (function inverse-bind-matrices
    "Accesses to the accessor that contains the inverse bind matrices of the skin.

See ACCESSOR
See SKIN")

  (function skeleton
    "Accesses the node that represents the root of the skeleton for the skin.

See NODE
See SKIN")

  (function joints
    "Accesses the array of nodes used as joints for the skin.

See NODE
See SKIN")

  (type texture
    "Representation of a texture.

See INDEXED-ELEMENT
See NAMED-ELEMENT
See SAMPLER
See SOURCE")

  (function sampler
    "Accesses the sampler used for the texture.

See SAMPLER
See TEXTURE")

  (function source
    "Accesses the source image of the texture.

See IMAGE
See TEXTURE")

  (type texture-info
    "Representation of additional texture information.

See GLTF-ELEMENT
See TEXTURE
See TEX-COORD
See SCALE
See STRENGTH")

  (function texture
    "Accesses the texture the texture-info supplements.

See TEXTURE
See TEXTURE-INFO")

  (function tex-coord
    "Accesses the index of the texture coordinates used for the texture indexing.

See TEXTURE-INFO")

  (function scale
    "Accesses the scalar applied to each normal of the normal-map texture.

See TEXTURE-INFO")

  (function strength
    "Accesses the scalar for the strength of the occulsion-map texture.

See TEXTURE-INFO")

  (type pbr
    "Representation of the PBR attributes used for a material.

See GLTF-ELEMENT
See ALBEDO
See ALBEDO-FACTOR
See METALLIC-FACTOR
See ROUGHNESS-FACTOR
See METALLIC-ROUGHNESS")

  (function albedo
    "Accesses the texture-info for the albedo channel of the material.

See TEXTURE-INFO
See PBR")

  (function albedo-factor
    "Accesses the albedo-factor for the material.

This should be an array of four float values representing the
per-channel strength factors.

See PBR")

  (function metallic-factor
    "Accesses the metalness-factor for the material.

See PBR")

  (function roughness-factor
    "Accesses the roughness-factor for the material.

See PBR")

  (function metallic-roughness
    "Accesses the metal-roughness-map for the material.

See TEXTURE-INFO
See PBR"))

;; parser.lisp
(docs:define-docs
  (function parse
    "Parses a glTF file and returns the constructed GLTF instance.

This function can parse glTF files in both textual glTF format with
external buffers, glTF format with embedded buffers, and GLB binary
format from files, strings, and streams.

Note: you **must** call CLOSE on the glTF file or its individual
buffers when you are done with the file. Failing to do so will cause
file descriptors or other resources to stay allocated indefinitely. It
is therefore strongly recommended that you use WITH-GLTF to handle the
resource cleanup safely.

Consequently, any access to BUFFERs, BUFFER-VIEWs, or ACCESSORs after
CLOSE has been called on the GLTF instance or the respective
underlying buffer leads to undefined behaviour. DON'T DO IT.

See WITH-GLTF
See GLTF (type)")

  (function with-gltf
    "Parses a glTF file and handles cleanup safely.

GLTF must be a variable to which the GLTF instance is bound within the
execution of BODY. It is **not** safe to let the GLTF instance escape
from the body.

See PARSE
See GLTF (type)"))

;; printer.lisp
(docs:define-docs
  (function merge-buffers
    "Reduces buffers to a single STATIC-BUFFERS.

If the glTF already has no or a single buffer, no changes are made.
Otherwise buffers are merged to one and all buffer views are adjusted
to point to the new buffer.

Returns the modified glTF.

See SERIALIZE")

  (function normalize-buffers
    "Turns every buffer into an in-memory STATIC-BUFFER.

Returns the modified glTF.

See STATIC-BUFFER
See SERIALIZE")

  (function urlify-buffers
    "Turns every buffer into a URI-BUFFER with Base64 encoding.

Returns the modified glTF.

See URI-BUFFER
See SERIALIZE")
  
  (function serialize
    "Serialize a glTF object to a file.

This function can serialize glTF files both in textual glTF format
with external buffers, glTF format with embedded buffers, and GLB
binary format to files and file streams.

NOTE: when serialising a file with multiple buffers to a GLB file,
this function will call MERGE-BUFFERS for you, which modifies the glTF
structure.

You may want to call one of NORMALIZE-BUFFERS, MERGE-BUFFERS,
URLIFY-BUFFERS prior to calling SERIALIZE to consolidate the buffers
to suit the format you want. For instance, if you want to create a
single text JSON file, you should use URLIFY-BUFFERS. If you want to
create a single JSON+DATA file, you should call MERGE-BUFFERS. If you
want to avoid having any data stored in the JSON, you should call
NORMALIZE-BUFFERS.

If UPDATE-ASSET-GENERATOR is true, the GENERATOR field in the ASSET of
the glTF is updated to reflect this library as the generator.

See MERGE-BUFFERS
See NORMALIZE-BUFFERS
See URLIFY-BUFFERS"))
