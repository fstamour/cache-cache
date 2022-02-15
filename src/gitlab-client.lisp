(in-package #:local-gitlab)

(defvar *token* nil
  "The token used to authenticate with GitLab.")

(defun initialize-gitlab-token ()
  "Initialize the *token* special variable."
  (log:info "Initializing GitLab token...")
  (let ((token
          (cons :private
                (uiop:getenv "GITLAB_PRIVATE_TOKEN"))))
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
    (list
     (shasht:read-json (flexi-streams:octets-to-string body))
     headers)))

(defun http-request-get-all (uri)
  "Calls uri and all the \"next\" links, returns a vector of all the results concatenated."
  (apply #'concatenate 'vector
         (loop
           :for %uri = uri
             :then (extract-next-uri headers)
           :while %uri
           :for (body headers) = (http-request-gitlab %uri)
           :collect body)))



(defun get-all-projects ()
  (http-request-get-all
   (format nil
           "~a/groups/~a/projects?per_page=100&simple=true&include_subgroups=true"
           *base-uri*
           *root-group-id*)))


(defun by-id (vector-of-hash-table &optional destination)
  "Convert a sequence of items to a map of id -> item."
  (loop
    :with result = (or destination (make-hash-table :test #'equal))
    :for item :across vector-of-hash-table
    ;; Converting to string because montezuma only stores strings
    :for id = (princ-to-string (gethash "id" item))
    :do (setf (gethash id result) item)
    :finally (return result)))

(defvar *projects* nil)

(defun initialize-projects ()
  "Get all projects from GitLab's *root-group-id* (recursively), store them by id."
  (log:info "Getting all the projects from GitLab...")
  (setf *projects* (by-id (get-all-projects))))

(defun remove-moved-issues (issues)
  "Remove issues that were moved"
  (remove-if-not #'(lambda (issue)
                     (eq :null (gethash "moved_to_id" issue)))
                 issues))

(defvar *issues* nil)

;; (when *issues* (hash-table-count *issues*))


;; Listing all the possible issue properties (used to generate the
;; list of property-key in the next form
#+nil
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
                         ,(format nil "Return true if the ISSUE has a \"~a\" and is not :null."
                                  property-key)
                         (multiple-value-bind (,property-name present-p)
                             (gethash ,property-key issue)
                           (and present-p (not (eq :null ,property-name))))))))

(defun find-last-update-time (issues)
  "Given a hash-table of ISSUES, find the lastest update-time."
  (loop
    :with last-updated-at = nil
    :for id :being :the :hash-key :of issues :using (hash-value issue)
    :for updated-at = (lt:parse-timestring
                       (gethash "updated_at" issue))
    :if (null last-updated-at)
      :do (setf last-updated-at updated-at)
    :else
      :do (when (lt:timestamp< last-updated-at updated-at)
            (setf last-updated-at updated-at))
    :finally (return last-updated-at)))

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

(defun get-new-and-updated-issues ()
  (let ((new-and-updated-issues
          (http-request-get-all
           (format nil
                   "~a/groups/~a/issues?per_page=100&updated_after=~a"
                   *base-uri*
                   *root-group-id*
                   (lt:format-rfc3339-timestring
                    nil
                    (lt:adjust-timestamp
                     (find-last-update-time *issues*)
                     (offset :sec 1)))))))
    (log4cl:log-info (length new-and-updated-issues))
    new-and-updated-issues))

(defun initialize-issues ()
  (if *issues*
      (progn
        (log:info "Updating the list of issues from GitLab...")
        (by-id (get-new-and-updated-issues) *issues*))
      (progn
        (log:info "Getting all the issues from GitLab...")
        (setf *issues* (by-id (get-all-issues))))))


;;; Common getter

#.`(progn
     ,@(loop :for property-key :in
             '("closed_at" "created_at" "description"
               "due_date" "id" "iid"
               "labels" "project_id"
               "state"
               "title" "name" "updated_at" "web_url")
             :for property-name = (a:symbolicate (string-upcase (kebab:to-kebab-case property-key)))
             :for getter-name = (a:symbolicate '#:item- property-name)
             :for predicate-name = (a:symbolicate '#:item- property-name '#:-p)
             :append `((defun ,getter-name (item)
                         ,(format nil "Return the ITEM's \"~a\" property."
                                  property-key)
                         (gethash ,property-key item))
                       (defun ,predicate-name (item)
                         ,(format nil "Return true if the ITEM has a \"~a\" and is not :null."
                                  property-key)
                         (multiple-value-bind (,property-name present-p)
                             (gethash ,property-key item)
                           (and present-p (not (eq :null ,property-name))))))))
