
#+sbcl
(progn
  (sb-ext:restrict-compiler-policy 'debug 3 3)
  (sb-ext:restrict-compiler-policy 'safety 3 3))

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
