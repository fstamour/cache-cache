(in-package #:cache-cache.gitlab.client)


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
