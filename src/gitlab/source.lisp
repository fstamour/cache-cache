(defpackage #:cache-cache.gitlab.source
  (:documentation "")
  (:use #:cl
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

   #:group-id

   #:initialize-epics
   #:initialize-issues
   #:initialize-labels
   #:initialize-projects))

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
    :documentation "The GitLab instance's information, contains the credentials.")
   (%projects
    :initform nil
    :accessor %projects
    :documentation "In-memory cache of projects.")
   (%issues
    :initform nil
    :accessor %issues
    :documentation "In-memory cache of issues."))
  (:documentation "An abstract source from GitLab."))

(defmethod domain ((gitlab-source gitlab-source))
  (domain (instance gitlab-source)))

(defmethod token ((gitlab-source gitlab-source))
  (token (instance gitlab-source)))

(defun api-url (gitlab-source)
  "The GitLab API v4 root URL."
  (serapeum:fmt "https://~a/api/v4" (domain gitlab-source)))



(defmethod resources ((source gitlab-source) (resouce (eql :project)))
  (%projects source))

(defmethod (setf resources) (new-resources (source gitlab-source) (resouce (eql :project)))
  (setf (%projects source) new-resources))

(defmethod resources ((source gitlab-source) (resouce (eql :issue)))
  (%issues source))

(defmethod (setf resources) (new-resources (source gitlab-source) (resouce (eql :issue)))
  (setf (%issues source) new-resources))



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




(defmethod initialize-epics (source))
(defmethod initialize-issues (source))
(defmethod initialize-labels (source))
(defmethod initialize-projects (source))


(defclass gitlab-personal-source (gitlab-source)
  (
   ;; No need for slots, the token should be enough...
   )
  (:documentation "Personal projects, groups, snippets, etc."))



;; TODO
(defun log-stats (source)
  (log:info "There are currently ~D issues and ~D projects in memory."
            ;; TODO use method "resources" instead
            (a:if-let ((issues (%issues source))) (hash-table-count issues) 0)
            (a:if-let ((projects (%projects source))) (hash-table-count projects) 0)))
