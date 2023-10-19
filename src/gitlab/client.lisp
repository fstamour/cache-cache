(defpackage #:cache-cache.gitlab.client
  (:documentation "Utilities to make requests to GitLab")
  (:use #:cl
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
 (find-last-update-time *issues*))



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
