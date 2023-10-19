(defpackage #:cache-cache.gitlab.search
  (:documentation "Interface to search the local cache for GitLab sources")
  (:use #:cl
        #:cache-cache.gitlab.source)
  (:local-nicknames (#:a #:alexandria)
                    (#:lt #:local-time))
  (:import-from #:com.inuoe.jzon
                #:write-value*
                #:write-object*)
  (:import-from #:cache-cache.gitlab.client
                #:issue-title
                #:item-name-with-namespace
                #:item-web-url
                #:issue-updated-at
                #:issue-closed-at-p)
  (:import-from #:cache-cache.search
                #:find-by)
  (:export
   #:find-issues
   #:find-projects))

(in-package #:cache-cache.gitlab.search)

(defun find-issues (query source)
  "Return the issues that contains all the parts of QUERY in their title."
  (find-by query source :issue #'issue-title))

(defun find-projects (query source)
  "Return the projects that contains all the parts of QUERY in their \"name with namespace\"."
  (find-by query source :project #'item-name-with-namespace))


;; TODO utils
(defun timestamp-string< (a b)
  (lt:timestamp<
   (lt:parse-rfc3339-timestring a)
   (lt:parse-rfc3339-timestring b)))

;; TODO utils
(defun timestamp-string> (a b)
  (lt:timestamp>
   (lt:parse-rfc3339-timestring a)
   (lt:parse-rfc3339-timestring b)))

(defun compare-issues (issue1 issue2)
  "Should ISSUE1 be shown before ISSUE2?"
  (let ((closed1 (issue-closed-at-p issue1))
        (closed2 (issue-closed-at-p issue2)))
    (if (eq closed1 closed2)
        (timestamp-string>
         (issue-updated-at issue1)
         (issue-updated-at issue2))
        closed2)))

(defun issues-created-in-the-last-7-days ()
  (let ((last-week (lt:adjust-timestamp
                       (lt:today)
                     (offset :day -7))))
    (remove-if #'(lambda (issue)
                   (lt:timestamp<
                    (lt:parse-rfc3339-timestring (issue-created-at issue))
                    last-week))
               (a:hash-table-values *issues*))))



;; Testing find-issues
#+ (or)
(time
 (let ((query "auto update"))
   (format t "~&=================================")
   (mapcar #'issue-title
           (find-issues query
                        (source-by-id 1)))))

(defun handler/search (query source &optional type)
  ;; Add projects
  (when (or (null type)
            (eq type :project))
    (loop :for project :in (find-projects query source)
          :do (write-object*
               "type" "project"
               "id"  (id project)
               "text" (item-name-with-namespace project)
               "url" (format nil "~a/issues" (item-web-url project)))))
  ;; Add issues
  (when (or (null type)
            (eq type :issue))
    (loop
      :for issue :in
                 (sort
                  (if (str:non-empty-string-p query)
                      (find-issues query source)
                      (issues-created-in-the-last-7-days))
                  #'compare-issues)
      :do (write-object*
           "type" "issue"
           "id" (id issue)
           "text" (issue-title issue)
           "url" (item-web-url issue)
           "closed" (issue-closed-at-p issue)))))

(defmethod search-source ((source gitlab-group-source) query &key type &allow-other-keys)
  "Search through the GitLab group for QUERY."
  (handler/search query source type))
