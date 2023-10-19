(defpackage #:cache-cache.generic
  (:documentation "Generic interfaces.")
  (:use #:cl)
  (:export
   #:initialize
   #:id
   #:name
   #:source
   #:source-id
   #:resources
   #:item
   #:clear-cache
   #:read-cache
   #:write-cache
   #:search-source))

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


;;; Caching

(defgeneric resources (source type)
  (:documentation "Get the SOURCE's in-memory cache for the resouces of type TYPE"))

(defgeneric (setf resources) (new-resources source type)
  (:documentation "Set the SOURCE's in-memory cache for the resouces of type TYPE"))

(defgeneric item (source id)
  (:documentation "Get the item ID from SOURCE."))

(defgeneric clear-cache (source)
  (:documentation "Clear the SOURCE's persistent cache."))

(defgeneric read-cache (source)
  (:documentation "Read a SOURCE's persistent cache."))

(defgeneric write-cache (source)
  (:documentation "Write a SOURCE's in-memory cache to persistent storage."))


;;;  Others/WIP

;; TODO register-job: a wrapper on cl-cron:make-cron-job
;; - add error handling
;; - add logging
;; - perhaps more? breakers?
;; (defgeneric register-job (source function) ...)

;; TODO (defgeneric statistics (source))

(defgeneric search-source (source query &key &allow-other-keys)
  (:documentation "Search through SOURCE for QUERY."))
