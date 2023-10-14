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
  (log:info "TODO read-cache ((source gitlab-group-source))"))

(defmethod write-cache ((source gitlab-group-source))
  (log:info "TODO write-cache ((source gitlab-group-source))")  )

(defmethod initialize ((source gitlab-group-source))
  (log:info "TODO initializing ~a..." source)
  (read-cache source)
  ;; (initialize-issues)
  ;; (initialize-projects)
  ;; (log-stats)
  (write-cache source)
  (log:info "Done initializing ~a." source))

(defmethod search-source ((source gitlab-group-source) query &key &allow-other-keys)
  "Search through the GitLab group for QUERY."
  (list "TODO search-source ((source <mark>gitlab-group-source</mark>) query &key &allow-other-keys)"))

(defmethod item ((source gitlab-group-source) id)
  "Get the item ID from SOURCE."
  "TODO item ((source gitlab-group-source) id)")
