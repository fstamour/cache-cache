(uiop:define-package #:cache-cache.gitlab.source
    (:documentation "")
  (:use #:cl)
  (:use-reexport
   #:cache-cache
   #:cache-cache.generic
   #:cache-cache.source
   #:cache-cache.cache)
  (:local-nicknames
   (#:a #:alexandria)
   (#:jzon #:com.inuoe.jzon))
  ;; classes
  (:export
   #:gitlab-instance
   #:gitlab-source
   #:gitlab-group-source)
  ;; functions
  (:export

   #:api-url
   #:graphql-url
   #:id
   #:domain
   #:token
   #:instance

   #:group-id))

(in-package #:cache-cache.gitlab.source)

(defclass gitlab-instance ()
  ((domain
    :initform "gitlab.com"
    :initarg :domain
    :accessor domain
    :documentation "The GitLab API v4 root URL.")
   (token
    :initform (error ":token must be specified")
    :initarg :token
    :accessor token
    :documentation "The token used to authenticate with GitLab.")))

(defclass gitlab-source (source)
  ((instance
    :initform (error ":instance must be specified")
    :initarg :instance
    :accessor instance
    :documentation "The GitLab instance's information, contains the credentials."))
  (:documentation "An abstract source from GitLab."))

(defmethod domain ((gitlab-source gitlab-source))
  (domain (instance gitlab-source)))

(defmethod token ((gitlab-source gitlab-source))
  (token (instance gitlab-source)))

(defun api-url (gitlab-source)
  "The GitLab API v4 root URL."
  (serapeum:fmt "https://~a/api/v4" (domain gitlab-source)))



(defparameter *test-instance* (make-instance 'gitlab-instance
                                             :token "1234"))

(defparameter *test-source* (make-instance 'gitlab-source
                                           :source-id -1
                                           :name "test"
                                           :instance *test-instance*))

#++
(equal (api-url *test-source*) "https://gitlab.com/api/v4")

(defun graphql-url (gitlab-source)
  "The GitLab API v4 root URL."
  (serapeum:fmt "https://~a/api/graphql" (domain gitlab-source)))

#++
(equal (graphql-url *test-source*) "https://gitlab.com/api/graphql")



#++
(defclass gitlab-personal-source (gitlab-source)
  (
   ;; No need for slots, the token should be enough...
   )
  (:documentation "Personal projects, groups, snippets, etc."))


;;; Topics

(defmethod supported-topics append ((source gitlab-source))
  (list))

#|
issue
project
epic
milestone
label
merge-request
|#
