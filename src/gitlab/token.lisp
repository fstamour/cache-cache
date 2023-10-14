(in-package #:cache-cache.gitlab.client)


;;; GitLab Token


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


;; TODO get token from "source"
(defun token-header (token)
  "Given a TOKEN, generate a cons representing the right token to
send to GitLab for authentication"
  (case (car token)
    (:private (cons "PRIVATE-TOKEN" (cdr token)))))

;; (token-header)
