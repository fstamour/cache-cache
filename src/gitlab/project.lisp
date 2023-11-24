(in-package #:cache-cache.gitlab.client)


;;; Projects

(defmethod get-all-projects ((source gitlab-group-source))
  (http-request-get-all
   (format nil
           "~a/groups/~a/projects?per_page=100&simple=true&include_subgroups=true"
           (api-url source)
           (group-id source))
   (token source)))

;; I can't find a way to ask gitlab for "new or updated project" using
;; their rest api.
;; Perhaps I could order_by=created_at to find the new projects
;; and order_by=updated_at to find the updated_projects?
;; UPDATE: See updated_after in:
;; https://docs.gitlab.com/ee/api/projects.html#list-all-projects

#+ (or)
(http-request-gitlab
 (format nil
         "~a/groups/~a/projects?per_page=5&include_subgroups=true&order_by=updated_at"
         *base-uri*
         *root-group-id*
         "2022-11-01T00:00:00.000-05:00"))

(defmethod initialize-topic ((source gitlab-group-source)
                             (topic (eql :project)))
  "Get all projects from GitLab's *root-group-id* (recursively), store them by id."
  (log:info "Getting all the projects from GitLab...")
  (setf (items source topic)
        (cache-cache::by-id (get-all-projects source))))

(defun project-by-id (source id)
  "Get a project by Id (from the in-memory cache)."
  (gethash id (items source :project)))
