#|
 This file is a part of cl-gltf
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-gltf
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to parse the glTF file format."
  :homepage "https://shirakumo.github.io/cl-gltf/"
  :bug-tracker "https://github.com/shirakumo/cl-gltf/issues"
  :source-control (:git "https://github.com/shirakumo/cl-gltf.git")
  :serial T
  :components ((:file "package")
               (:file "element")
               (:file "format")
               (:file "accessor")
               (:file "parser")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-extensible-sequences
               :mmap
               :shasht
               :cffi
               :qbase64)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-gltf-test))))
