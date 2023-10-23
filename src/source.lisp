(uiop:define-package #:cache-cache.source
    (:documentation "Generic sources of information")
  (:use #:cl)
  (:use-reexport #:cache-cache.generic))

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
   (topics
    :initform (make-hash-table)
    :initarg :topics
    :reader topics
    :documentation "The topics provided by this source"))
  (:documentation ""))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "sid: ~s topics: ~s"
            (source-id source)
            (topics source))))



;; TODO retopics "resource" to "topic"
(defclass topic ()
  ((source
    :initform (error ":source must be specified")
    :initarg :source
    :reader source
    :documentation "The source of the topic")
   (topics
    :initform (error ":topics must be specified")
    :initarg :topics
    :reader topics
    :documentation "The topics of the topic")))
