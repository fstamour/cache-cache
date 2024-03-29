(uiop:define-package #:cache-cache.search
    (:documentation "Interface and utilities for searching.")
  (:use #:cl)
  (:use-reexport #:cache-cache.generic)
  (:export
   #:search-result
   #:find-by))

(in-package #:cache-cache.search)

(defun search-in-list (needle list
                       &key
                         (key #'identity)
                         (test #'string-equal))
  "Return the items that has NEEDLE in KEY."
  (remove-if-not
   #'(lambda (item)
       (search needle (funcall key item) :test test))
   list))

(defun search-in-list/and (needle-list list
                           &key
                             (key #'identity)
                             (test #'string-equal))
  "Return the items that has all needles from NEEDLE-LIST in KEY."
  (loop
    :for needle :in needle-list
    :for candidates = (search-in-list needle list :key key :test test)
      :then (search-in-list needle candidates :key key :test test)
    :finally (return candidates)))

(defclass search-result ()
  ((source
    :initform (error ":source must be specified")
    :initarg :source
    :reader source
    :documentation "The source in which the result can be found.")
   (id
    :initform (error ":id must be specified")
    :initarg :id
    :reader id
    :documentation "The id of the result, this id is source-specific."))
  (:documentation "Represents one item in a search-result"))

(defun find-by (query source topic key)
  "Return the TOPIC in SOURCE that contains all the parts of QUERY in their KEY."
  (when (items source topic)
    (search-in-list/and
     (split-sequence:split-sequence #\Space query :remove-empty-subseqs t)
     (alexandria:hash-table-values (items source topic))
     :key key)))
