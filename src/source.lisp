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
   )
  (:documentation ""))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "sid: ~s name: ~s"
            (source-id source)
            (name source))))
