(in-package #:cache-cache.gitlab.source)

(defclass gitlab-group-source (gitlab-source)
  ((id
    :initform (error "A group id must be specified.")
    :initarg :id
    :accessor id
    :documentation "The id of the GitLab group."))
  (:documentation "A specific GitLab group."))

(defmethod supported-topics append ((source gitlab-group-source))
  (list :project :issue))

#++
(make-instance 'gitlab-group-source
               :id 42
               :source-id -1
               :name "test"
               :instance *test-instance*)


(defmethod group-id ((source gitlab-group-source))
  (id source))

;; (cl-cron:delete-cron-job 'update-issues)
