#|
 This file is a part of cl-gltf
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.gltf)

(defgeneric initargs (type json gltf)
  (:method-combination append))

(defun removef (plist &rest keys)
  (loop for (key val) on plist by #'cddr
        for found = (find key keys)
        unless found collect key
        unless found collect val))

(defun to-json-name (name)
  (with-output-to-string (out)
    (loop with upcase-next = NIL
          for char across (string name)
          do (cond ((char= #\- char)
                    (setf upcase-next T))
                   (upcase-next
                    (setf upcase-next NIL)
                    (write-char (char-upcase char) out))
                   (T
                    (write-char (char-downcase char) out))))))

(defun normalize-slotdef (slot &rest args &key name ref parse initarg initform accessor reader writer)
  (if (eql name 'null)
      (list* slot (removef args :name))
      (list* slot
             :name (or name (to-json-name slot))
             :ref ref
             :parse parse
             :initarg (or initarg (intern (string slot) "KEYWORD"))
             :initform initform
             (if (or reader writer accessor)
                 (append (when reader `(:reader ,reader))
                         (when writer `(:writer ,writer))
                         (when accessor `(:accessor ,accessor)))
                 `(:accessor ,slot)))))

(defmacro define-element (name superclasses slots &rest options)
  (let ((slots (loop for slot in slots collect (apply #'normalize-slotdef (if (listp slot) slot (list slot))))))
    `(progn
       (defclass ,name ,superclasses
         ,(loop for (slot . args) in slots
                collect (list* slot (removef args :name :ref :parse)))
         ,@options)

       (defmethod initargs append ((type ,name) json gltf)
         (list ,@(loop for (slot . args) in slots
                       when (getf args :name)
                       collect (getf args :initarg)
                       when (getf args :name)
                       collect (destructuring-bind (&key ref parse name &allow-other-keys) args
                                 (cond (ref
                                        `(resolve (gethash ,name json) ',ref gltf))
                                       (parse
                                        `(parse-from (gethash ,name json) ',parse gltf))
                                       (T
                                        `(gethash ,name json))))))))))
