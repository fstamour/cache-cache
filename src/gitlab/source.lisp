(defpackage #:cache-cache.gitlab.source
  (:documentation "")
  (:use #:cl
        #:cache-cache.generic
        #:cache-cache.source)
  ;; classes
  (:export
   #:gitlab-source
   #:gitlab-group-source)
  ;; functions
  (:export
   #:api-url
   #:graphql-url
   #:id
   #:domain
   #:token))

(in-package #:cache-cache.gitlab.source)

;; TODO serapeum struct for gitlab token

(defclass gitlab-source (source)
  ((domain
    :initform "gitlab.com"
    :initarg :domain
    :accessor domain
    :documentation "The GitLab API v4 root URL.")
   (token
    :initform (error ":token must be specified")
    :initarg :token
    :accessor token
    :documentation "The token used to authenticate with GitLab."))
  (:documentation "An abstract source with the common "))

(defun api-url (gitlab-source)
  "The GitLab API v4 root URL."
  (serapeum:fmt "https://~a/api/v4" (domain gitlab-source)))

#++
(equal (api-url (make-instance 'gitlab-source))
       "https://gitlab.com/api/v4")

(defun graphql-url (gitlab-source)
  "The GitLab API v4 root URL."
  (serapeum:fmt "https://~a/api/graphql" (domain gitlab-source)))

#++
(equal (graphql-url (make-instance 'gitlab-source :token nil))
       "https://gitlab.com/api/graphql")

(defclass gitlab-group-source (gitlab-source)
  ((id
    :initform (error "A group id must be specified.")
    :initarg :id
    :accessor id
    :documentation "The id of the GitLab group."))
  (:documentation "A specific GitLab group."))

#++
(make-instance 'gitlab-group-source)
#++
(make-instance 'gitlab-group-source :id cache-cache::*root-group-id*)

(defclass gitlab-personal-source (gitlab-source)
  (
   ;; No need for slots, the token should be enough...
   )
  (:documentation "Personal projects, groups, snippets, etc."))


#++ ;; TODO
(cl-cron:make-cron-job
 #'initialize-issues
 :hash-key 'update-issues)

;; (cl-cron:delete-cron-job 'update-issues)


;; TODO
(defun log-stats ()
  (log:info "There are currently ~D issues and ~D projects in memory."
            (hash-table-count *issues*)
            (hash-table-count *projects*)))

;; TODO this is wrong, it's just a placeholder
(defmethod initialize ((source gitlab-source))
  (read-cache)
  (initialize-issues)
  (initialize-projects)
  (log-stats)
  (write-cache))
