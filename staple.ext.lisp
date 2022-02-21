(asdf:load-system :staple-markless)

(defpackage "gltf-docs"
  (:use #:cl)
  (:local-nicknames
   (#:gltf #:org.shirakumo.fraf.gltf)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "gltf-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-gltf))))
  'page*)
