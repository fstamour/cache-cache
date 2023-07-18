(in-package #:local-gitlab)

(initialize-projects)
(initialize-issues)


;;; Group labels to terraform...

;; Get all the root groups' labels
(defvar *group-labels*
  (http-request-get-all
   (format nil
           "~a/groups/~a/labels?per_page=1000"
           *base-uri*
           *root-group-id*)))

;; Deduplicate the colors
(defvar *color* (make-hash-table :test 'equal))

;; Create a terraform file with one "local" per color
(alexandria:with-output-to-file (stream "./label-colors.tf"
                                        :if-exists :supersede)
  (format stream "locals {~%")
  (loop :for color :in
                   (sort (copy-seq
                          (a:hash-table-keys
                           (by *group-labels* :key (lambda (label)
                                                     (gethash "color" label)))))
                         #'string<)
        :for i :from 0
        :do
           (setf (gethash color *color*) i)
           (format stream "  color~d = ~s~%" i color))
  (format stream "}"))



(defun sanitize-string (string)
  (str:replace-using
   `("\\" "\\\\"
          ,(string #\newline) "\\n"
          "\"" "\\\"")
   (str:trim string)))

;; Generate a terraform file that contains all the group labels
(alexandria:with-output-to-file (stream "./labels.tf"
                                        :if-exists :supersede)
  (loop
    ;; set importp to t to also generate the import blocks (useful the
    ;; first time)
    :with importp = nil
    :for labels-by-name = (by *group-labels* :key 'item-name)
    :for name :across (sort
                       (map 'vector (lambda (label) (gethash "name" label))
                            *group-labels*)
                       #'string<)
    :for label = (gethash name labels-by-name)
    :for id = (item-id label)
    :for resource-name = (format nil "label~a" id)
    :do
       (format stream "~{~?~}"
               `(,@ (when importp "
import {
  id = \"~a:~a\"
  to = gitlab_group_label.~a
}
"
                          ,(list *root-group-id* id resource-name))
                    "
resource \"gitlab_group_label\" ~s {
   group       = ~s
   name        = ~s
   description = \"~a\"
   color       = local.color~d
}
"
                    ,(list
                      resource-name *root-group-id*
                      (gethash "name" label)
                      ;; TODO use sanitize-string
                      (str:replace-using
                       `("\\" "\\\\"
                              ,(string #\newline) "\\n"
                              "\"" "\\\"")
                       (gethash "description" label))
                      ;; (gethash "color" label)
                      (gethash (gethash "color" label) *color*)
                      )))))

;; IMO, it took too much time to plan with so many labels, so I move
;; these resources out into another (new) terraform state).
;;
;; Use this to remove them from the state (this takes forever, it might
;; have been faster (but less safe) to pull the state, edit and push).
;; 1. terraform state list | grep gitlab_group_label | xargs -n 1 echo tf state rm > remove-labels-from-state.sh
;; 2. < review remove-labels-from-state.sh >
;; 3. sh remove-labels-from-state.sh


;;; Getting a project's environements' ids (also to import into terraform)


(let ((project-id *the-project-id*))
  (defparameter *environments*
    (http-request-get-all
     (format nil
             "~a/projects/~a/environments"
             *base-uri*
             project-id))
    ))

;; Show the environments as json, but keeping only the "id" and "name"
;; attributes.
#++
(jzon:stringify
 *environments*
 :pretty t
 :replacer (lambda (k v)
             (if (and k (stringp k))
                 (and (member k '("id" "name" "external_url") :test #'string=) t)
                 t)))

;; Make an import terraform block for the environments
(loop :for env :across *environments*
      :do
         (format t "~%import {
  id = ~s
  to = gitlab_project_environment.apigateway[~s]
}"
                 (item-id env)
                 (item-name env)))


;;; Getting all epics

;; TODO updated_after
(defparameter *epics*
  (http-request-get-all
   (format nil
           "~a/groups/~a/epics?per_page=1000"
           *base-uri*
           *root-group-id*)))

(length *epics*)
(hash-table-count *epics*)
(aref *epics* 0)


;;; Trying to fetch my activities...

;; It seems to always return the last 7 days instead of respecting the
;; :after and :before...
(progn
  (format t "~%~%==================~%~%")
  (loop :for event :across (http-request-get-all
                            (puri:render-uri
                             (make-uri "/api/v4/events"
                                       (format-query
                                        :after "2023-07-04"
                                        :before "2023-07-07"
                                        :scope :all))
                             nil))
        :do (format t "~%~%~a~%"
                    (alexandria:hash-table-alist event))))


;;; Trying to use serapeum to generate the documentation

;; (asdf:load-system 'serapeum/docs)
;; I had some issues with guix... lol
(load
 (merge-pathnames
  "docs.lisp"
  (asdf:system-source-directory 'serapeum)))

(let ((root (asdf:system-source-directory 'local-gitlab))
      (home (user-homedir-pathname)))
  (if (uiop:absolute-pathname-p (enough-namestring root home))
      (error "The system's location ~s is not in the user's home directory (~s), did you load the system from guix or nix?"
             root home)

      (serapeum.docs:update-function-reference
       ;; (namestring (uiop:merge-pathnames* root "REFERENCE.md"))
       "REFERENCE.md"
       :local-gitlab)))

;; It failed miserably because swank wasn't able to locate the source
;; of a few symbols. I tried to modify serapeum/docs' code, but too
;; many things assume that source information is available.


