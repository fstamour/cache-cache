
(defpackage #:local-gitlab
  (:documentation "Package to interact with GitLab")
  (:use #:cl #:local-gitlab.config)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria)
                    (#:lt #:local-time)
                    (#:jzon #:com.inuoe.jzon))
  (:import-from #:com.inuoe.jzon
                #:write-value*
                #:write-object*)
  (:export #:serve #:main-cli #:quit))
