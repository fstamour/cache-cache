(cl:in-package #:cl)

(defpackage #:local-gitlab.asd
  (:use :cl :asdf))

(in-package #:local-gitlab.asd)

(asdf:defsystem #:local-gitlab
  :description "A tool to work with a local cache of part of GitLab."
  ;; TODO Long description: because it's faster (actually #.(read-whole-file "readme.org")
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :depends-on
  (#:local-gitlab/config
   ;; For making HTTP requests
   #:drakma
   ;; For JSON
   #:com.inuoe.jzon
   ;; Logging
   #:log4cl
   ;; Web server
   #:hunchentoot
   #:find-port
   ;; String manipulation
   #:kebab
   #:str
   ;; Parsing time
   #:local-time
   ;; For persistent caching
   #:simpbin
   ;; To execute function at regular intervals
   #:cl-cron
   ;; For command line argument parsing
   #:adopt
   ;; To handle C-c
   #:with-user-abort
   ;; Way too many utilities
   #:serapeum)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "variables")
   (:file "utilities")
   (:file "gitlab-client")
   (:file "local-gitlab")
   (:file "cli")))

(asdf:defsystem #:local-gitlab/config
  :description "System to configure the \"local-gitlab\" system."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :pathname "src"
  :serial t
  :components
  ((:file "configurations")))
