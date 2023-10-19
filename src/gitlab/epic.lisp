(in-package #:cache-cache.gitlab.client)


;;; Epics

(defun get-all-epics ()
  (http-request-get-all
   (format nil
           "~a/groups/~a/epics?per_page=1000"
           (api-url source)
           (group-id source))
   (token source)))
