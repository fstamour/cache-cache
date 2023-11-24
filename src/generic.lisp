
(defpackage #:cache-cache.generic
  (:documentation "Generic interfaces.")
  (:use #:cl)
  (:export
   #:id
   #:name
   #:source
   #:source-id
   #:initialize
   #:initialize-topic)
  (:export
   #:items
   #:supported-topics
   #:topics
   #:item)
  (:export
   #:clear-cache
   #:read-cache
   #:write-cache)
  (:export
   #:statistics
   #:search-source))

(in-package #:cache-cache.generic)


;;;

(defgeneric id (object)
  (:documentation "Get the id of the OBJECT."))

(defgeneric name (object)
  (:documentation "Get the name of the OBJECT."))

(defgeneric source (object)
  (:documentation "Get the source of the OBJECT."))

(defgeneric source-id (object)
  (:documentation "Get the ID of the source."))

(defgeneric initialize (object)
  (:documentation "Initialize OBJECT."))

(defgeneric initialize-topic (source topic)
  (:documentation "Initialize TOPIC in SOURCE."))

#++ ;; TODO
(defgeneric refresh (object)
  (:documentation "Refresh (e.g. re-sync) OBJECT."))


;;; Items and topics

(defgeneric items (source topic)
  (:documentation "Get the SOURCE's in-memory cache for the resouces of type TYPE"))

(defgeneric (setf items) (new-items source topic)
  (:documentation "Set the SOURCE's in-memory cache for the resouces of type TYPE"))

(defgeneric supported-topics (source)
  (:method-combination append)
  (:documentation "List of topics supported by this source."))

(defgeneric topics (source)
  (:documentation "List of topics currently availables from this source."))

(defgeneric item (source topic id)
  (:documentation "Get the item ID from SOURCE."))


;;; Caching

;; TODO What about in-memory cache (e.g. topics)?
(defgeneric clear-cache (source topic)
  (:documentation "Clear the SOURCE's persistent cache."))

(defgeneric read-cache (source topic)
  (:documentation "Read a SOURCE's persistent cache."))

(defgeneric write-cache (source topic)
  (:documentation "Write a SOURCE's in-memory cache to persistent storage."))


;;;  Others/WIP

(defgeneric statistics (source)
  (:documentation "Gather up some statistics about the source."))

(defgeneric search-source (source query &key &allow-other-keys)
  (:documentation "Search through SOURCE for QUERY."))
