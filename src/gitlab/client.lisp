(uiop:define-package #:cache-cache.gitlab.client
    (:documentation "Utilities to make requests to GitLab")
  (:use #:cl)
  (:use-reexport
   #:cache-cache.generic
   #:cache-cache.source
   #:cache-cache.gitlab.source)
  (:local-nicknames (#:a #:alexandria)
                    (#:lt #:local-time)
                    (#:jzon #:com.inuoe.jzon)))

(in-package #:cache-cache.gitlab.client)


;;; Find latest object in hash-table

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
 (find-last-update-time (items (source-by-id 1) :issue)))



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




;;; TODO Handle rate-limiting gracefully https://docs.gitlab.com/ee/user/admin_area/settings/user_and_ip_rate_limits.html#response-headers
;;; e.g.
;;; - use some kind of queues (maybe chanl?)
;;; - look at headers Rate-Limit-Remaining and RetryAfter
(defun http-request-gitlab (uri token &rest rest)
  (log:debug "Making a request to GitLab: \"~a\"..." uri)
  ;; TODO Move that handler-case somewhere else, e.g. in the labmda
  ;; ran by cl-cron.
  (handler-case
      (multiple-value-bind
            (body status-code headers uri-object stream must-close reason-phrase)
          ;; TODO use dexador instead
          (apply #'drakma:http-request
                 (ensure-uri-string uri)
                 ;; TODO Parse "rest" to extract ":additional-headers"
                 ;; Send the auth
                 :additional-headers (list (token-header token))
                 (alexandria:remove-from-plist rest
                                               :additional-headers))
        (declare (ignore stream must-close uri-object))
        (declare (ignorable headers))
        ;; status-code reason-phrase
        (log:debug "~A ~A" status-code reason-phrase)
        (setf *last-headers* headers
              *last-body* body)
        (let ((response (jzon:parse body)))
          (a:when-let ((message (errorp response)))
            ;; TODO better error message
            ;; TODO specific condition...
            ;; TODO add a restart
            (error "http-request REST error: message = ~a (~a)" message
                   (jzon:stringify response)))
          (values response headers)))
    #++
    (error (condition)
      (break)
      (log:error "~a" condition)
      nil)))
