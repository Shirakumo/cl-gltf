(asdf:defsystem cl-gltf
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to parse and serialize the glTF file format."
  :homepage "https://shirakumo.github.io/cl-gltf/"
  :bug-tracker "https://github.com/shirakumo/cl-gltf/issues"
  :source-control (:git "https://github.com/shirakumo/cl-gltf.git")
  :serial T
  :components ((:file "package")
               (:file "element")
               (:file "format")
               (:file "accessor")
               (:file "translations")
               (:file "construction")
               (:file "parser")
               (:file "printer")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :pathname-utils
               :trivial-extensible-sequences
               :mmap
               :com.inuoe.jzon
               :cffi
               :qbase64
               :nibbles
               :static-vectors
               (:feature :sbcl (:require :sb-posix)))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-gltf-test))))
