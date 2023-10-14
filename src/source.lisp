(defpackage #:cache-cache.source
  (:documentation "Generic sources of information")
  (:use #:cl
        #:cache-cache.generic)
  (:export
   ;; #:source
   ;; #:source-id
   ;; #:name
   #:search-source))

(in-package #:cache-cache.source)

(defclass source ()
  ((source-id
    :initform (error ":source-id must be specified")
    :initarg :source-id
    :reader source-id
    :documentation "The identifier of the source, must be an integer.")
   (name
    :initform (error ":name must be specified")
    :initarg :name
    :reader name
    :documentation "The name of the source")
   ;; TODO slot: description
   )
  (:documentation ""))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "sid: ~s name: ~s"
            (source-id source)
            (name source))))

#++ ;; Use cache-cache.generic:initialize instead
(defgeneric initialize-source (source)
  (:documentation "Initialize a SOURCE. Use this to register jobs."))

;; TODO clear-cache (in-memory v.s. persistent)
#++
(defgeneric clear-cache (source)
  (:documentation ""))

(defgeneric read-cache (source)
  (:documentation "Read a SOURCE's persistent cache in-memory."))

(defgeneric write-cache (source)
  (:documentation "Write a SOURCE's in-memory cache to persistent storage."))

;; TODO register-job: a wrapper on cl-cron:make-cron-job
;; - add error handling
;; - add logging
;; - perhaps more? breakers?
;; (defgeneric register-job (source function) ...)

;; TODO (defgeneric statistics (source))

(defgeneric search-source (source query &key &allow-other-keys)
  (:documentation "Search through SOURCE for QUERY."))

(defgeneric item (source id)
  (:documentation "Get the item ID from SOURCE."))
