(cl:in-package #:cl)

(defpackage #:local-gitlab.asd
  (:use :cl :asdf))

(in-package #:local-gitlab.asd)

(asdf:defsystem #:local-gitlab
  :description "A tool to work with a local cache of part of GitLab."
  ;; TODO Long description: because it's faster
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :depends-on
  (#:local-gitlab/config
   ;; For HTTP requests
   ;; TODO replace drakma by #:dexador
   #:drakma
   ;; For JSON handling
   #:com.inuoe.jzon
   ;; logging
   #:log4cl
   ;; web server
   #:hunchentoot
   #:find-port
   ;; string manipulation
   #:kebab
   #:str
   ;; parsing time
   #:local-time
   ;; For persistent caching
   #:simpbin
   ;; Others
   #:cl-cron)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "variables")
   (:file "gitlab-client")
   (:file "local-gitlab")))

(asdf:defsystem #:local-gitlab/config
  :description "System to configure the \"local-gitlab\" system."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :pathname "src"
  :serial t
  :components
  ((:file "configurations")))
