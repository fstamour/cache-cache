(in-package #:local-gitlab)


(defun initialize-gitlab-token ()
  "Initialize the *token* special variable."
  (log:info "Initializing GitLab token...")
  (let ((token
          (cons :private
                (or (uiop:getenv "GITLAB_PRIVATE_TOKEN")
                    (error "Environment variable GITLAB_PRIVATE_TOKEN is not set or empty.")))))
    ;; TODO validate token (at least check it's not nil)
    (setf *token* token))
  (if *token*
      (log:info "GitLab token initialized.")
      (log:error "Faild to initialize GitLab token.")))


;; TODO This should be a global variable (see serapeum), or perhaps a constant?
;; TODO Not used yet
;; see https://docs.gitlab.com/ee/user/markdown.html#gitlab-specific-references for more
(defvar *gitlab-sigil-alist* '(("issues" . "#")
                               ("merge_requests" . "!")
                               ("epics" . "&")
                               ("milestones" . "%")))



#|
;;;;;;;;;;;; TODO Support other kind of access tokens

JOB-TOKEN: $CI_JOB_TOKEN

PAT: "PRIVATE-TOKEN: <your_access_token>"

OAuth: "Authorization: Bearer <your_access_token>"

git clone "https://my-project:$PROJECT_TOKEN@my.gitlab.host/my-group/my-project.git"
^^^ this works with CI_JOB_TOKEN too, if the user running the job has access to that repo.

There are also:
- _project_ access tokens
- _group_ access tokens
- {group,project} _deploy_ token
|#

(defun token-header (&optional (token *token*))
  "Given a TOKEN, generate a cons representing the right token to
send to GitLab for authentication"
  (case (car token)
    (:private (cons "PRIVATE-TOKEN" (cdr token)))))

;; (token-header)

(defun extract-links-from-header (headers)
  (alexandria:if-let ((link-header (cdr (assoc :link headers))))
    (loop
      :for i :from 0
      :for link :in (split-sequence:split-sequence #\, link-header)
      :for (uri rel) = (split-sequence:split-sequence #\; link)
      :collect (cons
                ;; The rel have the format " rel=\"...\""
                (subseq rel 6 (1- (length rel)))
                ;; The urls are surrounded with <>, remove
                ;; them. Except for the first one, they are prefixed
                ;; with a space
                (subseq uri (if (zerop i) 1 2) (1- (length uri)))))))

#+ (or)
(extract-links-from-header
 '((:link . "<url1>; rel=\"next\", <url2>; rel=\"first\", <url3>; rel=\"last\"")))
;; => (("next" . "uri1") ("first" . "uri2") ("last" . "uri3"))

#+ (or)
(extract-links-from-header '())
;; => nil


(defun extract-next-uri (headers)
  (alexandria:if-let ((links (extract-links-from-header headers)))
    (alexandria:if-let ((next-link (find "next" links :key #'first :test #'string=)))
      (cdr next-link))))

#+ (or)
(extract-next-uri
 '((:link . "<uri1>; rel=\"next\", <uri2>; rel=\"first\", <uri3>; rel=\"last\"")))
;; => "url1"

(defparameter *last-headers* nil
  "just for debugging")

#+ (or)
(progn
  (extract-links-from-header *last-headers*)
  (extract-next-uri *last-headers*))


;;; TODO Handle rate-limiting gracefully https://docs.gitlab.com/ee/user/admin_area/settings/user_and_ip_rate_limits.html#response-headers
;;; e.g.
;;; - use some kind of queues (maybe chanl?)
;;; - look at headers Rate-Limit-Remaining and RetryAfter
(defun http-request-gitlab (uri &rest rest)
  (multiple-value-bind
        (body status-code headers uri-object stream must-close reason-phrase)
      ;; TODO use dexador instead
      (apply #'drakma:http-request
             uri
             ;; TODO Parse "rest" to extract ":additional-headers"
             ;; Send the auth
             :additional-headers (list (token-header))
             rest)
    (declare (ignore stream must-close uri-object))
    (declare (ignorable status-code reason-phrase headers))
    (log:debug "Making a request to GitLab: \"~a\"..." uri)
    (setf *last-headers* headers)
    (let ((response (jzon:parse body)))
      (when (and (hash-table-p response)
                 (gethash "message" response))
        ;; TODO better error message
        ;; TODO add a restart
        (error "http-request REST error: message = ~a" (gethash "message" response)))
      (list response headers))))

(defun http-request-get-all (uri)
  "Calls uri and all the \"next\" links, returns a vector of all the results concatenated."
  (apply #'concatenate 'vector
         (loop
           :for %uri = uri
             :then (extract-next-uri headers)
           :while %uri
           :for (body headers) = (http-request-gitlab %uri)
           :collect body)))

(defun by-id (sequence-of-hash-table &optional destination)
  "Convert a sequence of items to a map of id -> item."
  (let ((result (or destination (make-hash-table))))
    (map nil
         #'(lambda (item &aux (id (gethash "id" item)))
             (setf (gethash id result) item))
         sequence-of-hash-table)
    result))


;;; Projects

(defun get-all-projects ()
  (http-request-get-all
   (format nil
           "~a/groups/~a/projects?per_page=100&simple=true&include_subgroups=true"
           *base-uri*
           *root-group-id*)))


;; I can't find a way to ask gitlab for "new or updated project" using
;; their rest api.
;; Perhaps I could order_by=created_at to find the new projects
;; and order_by=updated_at to find the updated_projects?

#+ (or)
(http-request-gitlab
 (format nil
         "~a/groups/~a/projects?per_page=5&include_subgroups=true&order_by=updated_at"
         *base-uri*
         *root-group-id*
         "2022-11-01T00:00:00.000-05:00"))

(defun initialize-projects ()
  "Get all projects from GitLab's *root-group-id* (recursively), store them by id."
  (log:info "Getting all the projects from GitLab...")
  (setf *projects* (by-id (get-all-projects))))

(defun project-by-id (id)
  (gethash id *projects*))


;;; Issues

(defun remove-moved-issues (issues)
  "Remove issues that were moved"
  (remove-if-not #'(lambda (issue)
                     (eq 'null (gethash "moved_to_id" issue)))
                 issues))

;; Listing all the possible issue properties
#+ (or)
(loop
  :with keys = (make-hash-table :test 'equal)
  :for issue :in (a:hash-table-values *issues*)
  :do (loop :for key :in (a:hash-table-keys issue)
            :do (setf (gethash key keys) t))
  :finally (return (sort
                    (a:hash-table-keys keys)
                    #'string<)))
#.`(progn
     ,@(loop :for property-key :in
             '("_links" "assignee" "assignees" "author" "blocking_issues_count" "closed_at"
               "closed_by" "confidential" "created_at" "description" "discussion_locked"
               "downvotes" "due_date" "epic" "epic_iid" "has_tasks" "id" "iid" "issue_type"
               "labels" "merge_requests_count" "milestone" "moved_to_id" "project_id"
               "references" "service_desk_reply_to" "state" "task_completion_status"
               "task_status" "time_stats" "title" "type" "updated_at" "upvotes"
               "user_notes_count" "web_url" "weight")
             :for property-name = (a:symbolicate (string-upcase (kebab:to-kebab-case property-key)))
             :for getter-name = (a:symbolicate '#:issue- property-name)
             :for predicate-name = (a:symbolicate '#:issue- property-name '#:-p)
             :append `((defun ,getter-name (issue)
                         ,(format nil "Return the ISSUE's \"~a\" property."
                                  property-key)
                         (gethash ,property-key issue))
                       (defun ,predicate-name (issue)
                         ,(format nil "Return true if the ISSUE has a \"~a\" and is not 'null."
                                  property-key)
                         (multiple-value-bind (,property-name present-p)
                             (gethash ,property-key issue)
                           (and present-p (not (eq 'null ,property-name))))))))


;; TODO Maybe make a modifier too (see serapeum)
(defun issue-by-id (id)
  (gethash id *issues*))

(defun issue-project (issue-id)
  (project-by-id
   (issue-project-id
    (issue-by-id issue-id))))

(defun find-latest (objects field)
  "Given a hash-table of OBJECTS, find the FIELD with the latest time."
  (loop
    :with latest = nil
    :for id :being :the :hash-key :of objects :using (hash-value object)
    :for current = (lt:parse-timestring (gethash field object))
    :if (null latest)
      :do (setf latest current)
    :else
      :do (when (lt:timestamp< latest current)
            (setf latest current))
    :finally (return latest)))

(defun find-last-update-time (issues)
  "Given a hash-table of ISSUES, find the lastest update-time."
  (find-latest issues "updated_at"))

#+ (or)
(time
 (find-last-update-time *issues*))

(defun get-all-issues ()
  (remove-moved-issues
   (http-request-get-all
    (format nil
            "~a/groups/~a/issues?per_page=100"
            *base-uri*
            *root-group-id*))))

#+ (or)
(by-id
 (car (http-request-gitlab
       (format nil
               "~a/groups/~a/issues?per_page=10"
               *base-uri*
               *root-group-id*))))

(defun get-new-and-updated-issues ()
  (let* ((latest-time (find-last-update-time *issues*))
         (new-and-updated-issues
           (if latest-time
               (http-request-get-all
                (format
                 nil
                 "~a/groups/~a/issues?per_page=100&updated_after=~a"
                 *base-uri*
                 *root-group-id*
                 (lt:format-rfc3339-timestring
                  nil
                  (lt:adjust-timestamp latest-time (offset :sec 1)))))
               (get-all-issues))))
    (log4cl:log-info (length new-and-updated-issues))
    new-and-updated-issues))

(defun initialize-issues ()
  (if *issues*
      (progn
        (log:info "Updating the list of issues from GitLab...")
        (by-id (get-new-and-updated-issues) *issues*))
      (progn
        (log:info "Getting all the issues from GitLab...")
        (setf *issues* (by-id (get-all-issues)))
        (log:info "Got all the issues."))))


;;; Common getters

#.`(progn
     ,@(loop :for property-key :in
             '("closed_at" "created_at" "description"
               "due_date" "id" "iid"
               "labels" "project_id"
               "state"
               "title" "name" "updated_at" "web_url"
               "path" "path_with_namespace"
               "name_with_namespace")
             :for property-name = (a:symbolicate (string-upcase (kebab:to-kebab-case property-key)))
             :for getter-name = (a:symbolicate '#:item- property-name)
             :for predicate-name = (a:symbolicate '#:item- property-name '#:-p)
             :append `((defun ,getter-name (item)
                         ,(format nil "Return the ITEM's \"~a\" property."
                                  property-key)
                         (gethash ,property-key item))
                       (defun ,predicate-name (item)
                         ,(format nil "Return true if the ITEM has a \"~a\" and is not 'null."
                                  property-key)
                         (multiple-value-bind (,property-name present-p)
                             (gethash ,property-key item)
                           (and present-p (not (eq 'null ,property-name))))))))


;;; Persistent cache

(defun cache-pathname (name)
  (merge-pathnames (format nil "~(~a~)-cache.sbin" name)
                   (ensure-directories-exist
                    (uiop/configuration:xdg-cache-home "local-gitlab/"))))

(defun issue-cache-pathname ()
  (cache-pathname :issue))

(defun write-cache-file (name hash-table)
  (simpbin:with-output-to-binary-file (output (cache-pathname name)
                                              :if-exists :supersede)
    (simpbin:write-header output)
    (loop :for id
            :being :the :hash-key :of hash-table
              :using (hash-value object)
          :do (simpbin:write-binary-string
               (jzon:stringify object :stream nil)
               output))))

(defun read-cache-file (name)
  (alexandria:if-let ((cache-pathname
                       (probe-file (cache-pathname name))))
    (simpbin:with-input-from-binary-file (input cache-pathname)
      (simpbin:read-header input)
      (by-id
       (loop
         :for json-string = (handler-case (simpbin:read-binary-string input)
                              (end-of-file (condition)
                                (declare (ignore condition))))
         :while json-string
         :for object = (jzon:parse json-string)
         :collect object)))))

(defun write-cache ()
  (write-cache-file :issue *issues*)
  (write-cache-file :project *projects*))

(defun read-cache ()
  (log:info "Reading all the issues from the cache...")
  (setf *issues* (or (read-cache-file :issue)
                     (by-id '())))
  (log:info "Reading all the projects from the cache...")
  (setf *projects* (or (read-cache-file :project)
                       (by-id '()))))



;; (time (read-cache-file :issue))
#++
(time (prog1 nil
        (jzon:parse
         (length (jzon:stringify *issues*))
         14 462 450
         :key-fn #'alexandria:make-keyword)))
;; 14 secs before updating jzon
;; 0.5 secs after
