
(defpackage #:local-gitlab
  (:documentation "Package to interact with GitLab")
  (:use #:cl #:local-gitlab.config)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria)
                    (#:lt #:local-time))
  (:export #:main))
