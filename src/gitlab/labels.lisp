(defpackage #:cache-cache.gitlab.labels
  (:documentation "")
  (:use
   #:cl
   #:cache-cache.gitlab.source
   #:cache-cache.gitlab.client))

(in-package #:cache-cache.gitlab.labels)

;; Get all the root groups' labels
#++
(defvar *group-labels*
  (http-request-get-all
   (format nil
           "~a/groups/~a/labels?per_page=1000"
           *base-uri*
           *root-group-id*)))

(defvar *source*
  (make-instance
   'gitlab-group-source
   :id cache-cache::*root-group-id*)
  "This is temporary :tm:")

(defun get-labels (group)
  (http-request-get-all
   (format nil
           "~a/groups/~a/labels?per_page=1000"
           (api-url group)
           (id group))))

(defvar *group-labels* (get-labels *source*))

https://github.com/archimag/cl-closure-template
https://github.com/google/closure-templates

compile-template
:common-lisp-backend
:javascript-backend

(flute:h
  (html))

also; (flute:define-element)
