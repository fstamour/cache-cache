(in-package #:cache-cache.gitlab.source)

(defclass gitlab-group-source (gitlab-source)
  ((id
    :initform (error "A group id must be specified.")
    :initarg :id
    :accessor id
    :documentation "The id of the GitLab group."))
  (:documentation "A specific GitLab group."))

#++
(make-instance 'gitlab-group-source
               :id 42
               :source-id -1
               :name "test"
               :instance *test-instance*)


(defmethod group-id ((source gitlab-group-source))
  (id source))


#++ ;; TODO
(cl-cron:make-cron-job
 #'initialize-issues
 :hash-key 'update-issues)

;; (cl-cron:delete-cron-job 'update-issues)

(defmethod read-cache ((source gitlab-group-source))
  ;; TODO this code is horrible...
  (log:info "Reading all the projects from the cache...")
  (setf (resources source :project) (read-cache-file source :project))
  (log:info "Reading all the issues from the cache...")
  (setf (resources source :issue) (read-cache-file source :issue)))

(defmethod write-cache ((source gitlab-group-source))
  ;; TODO this code is horrible...
  (write-cache-file source :project)
  (write-cache-file source :issue))

(defmethod initialize ((source gitlab-group-source) &key &allow-other-keys)
  (log:info "initializing ~a..." source)
  (read-cache source)
  ;; TODO initialize issues and projects in parallel?
  ;; TODO don't fail if initialize-issues or initialize-projects fails
  ;; TODO MAKE-CRON-JOB
  (initialize-issues source)
  (initialize-projects source)
  (log-stats source)
  (write-cache source)
  (log:info "Done initializing ~a." source))

(defmethod item ((source gitlab-group-source) id)
  "Get the item ID from SOURCE."
  "TODO item ((source gitlab-group-source) id)")
