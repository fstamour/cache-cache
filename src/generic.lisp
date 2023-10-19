(defpackage #:cache-cache.generic
  (:documentation "Generic interfaces.")
  (:use #:cl)
  (:export
   #:initialize
   #:id
   #:name
   #:source
   #:source-id))

(in-package #:cache-cache.generic)

(defgeneric id (object)
  (:documentation "Get the id of the OBJECT."))

(defgeneric name (object)
  (:documentation "Get the name of the OBJECT."))

(defgeneric source (object)
  (:documentation "Get the source of the OBJECT."))

(defgeneric source-id (object)
  (:documentation "Get the ID of the source."))

(defgeneric initialize (object &key &allow-other-keys)
  (:documentation "Initialize OBJECT."))
