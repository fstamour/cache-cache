(defpackage #:cache-cache.config
  (:documentation "")
  (:use #:cl #:cache-cache.generic)
  (:export
   #:*sources*
   #:source-by-id))

(in-package #:cache-cache.config)

(defvar *sources* '())

;; TODO port
;; TODO interface

(defun source-by-id (source-id)
  (find source-id *sources* :key #'source-id))

#++(source-by-id 1)

;; TODO a method on "resources" (to be renamed "topic") that accepts
;; number (source-id) as the source.
