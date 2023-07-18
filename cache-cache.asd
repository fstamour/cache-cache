(cl:in-package #:cl)

(defpackage #:cache-cache.asd
  (:use :cl :asdf))

(in-package #:cache-cache.asd)

(asdf:defsystem #:cache-cache
  :description "A tool to work with a local cache of part of GitLab."
  ;; TODO Long description: because it's faster (actually #.(read-whole-file "readme.org")
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :depends-on
  (#:cache-cache/config
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
   (:file "cache-cache")
   (:file "cli")))

(asdf:defsystem #:cache-cache/config
  :description "System to configure the \"cache-cache\" system."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :pathname "src"
  :serial t
  :components
  ((:file "configurations")))
