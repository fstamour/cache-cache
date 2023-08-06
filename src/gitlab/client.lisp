(defpackage #:cache-cache.gitlab.client
  (:documentation "Utilities to make requests to GitLab")
  (:use #:cl))

(in-package #:cache-cache.gitlab.client)

(import 'cache-cache::http-request-get-all)
(export 'http-request-get-all)
