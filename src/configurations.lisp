(defpackage #:local-gitlab.config
  (:documentation "")
  (:use #:cl)
  (:export #:*base-uri*
           #:*root-group-id*))

(in-package #:local-gitlab.config)

(defvar *base-uri* "https://gitlab.com/api/v4"
  "The base uri to GitLab's API.")

(defvar *root-group-id* nil
  "The ID of the root GitLab group.")


;; TODO port
;; TODO interface
