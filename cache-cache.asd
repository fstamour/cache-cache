(cl:in-package #:cl)

(defpackage #:cache-cache.asd
  (:use :cl :asdf))

(in-package #:cache-cache.asd)

(asdf:defsystem #:cache-cache
  :description "A tool to help cache information locally; and do stuff with it."
  ;; TODO Long description: because it's faster (actually #.(read-whole-file "readme.org")
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "Public domain"
  :depends-on
  (;; For making HTTP requests
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
  ((:file "generic")
   (:file "configuration")
   (:file "source")
   (:file "cache")
   (:file "search")
   (:file "package")
   (:file "variables")
   (:file "utilities")
   (:file "json")
   (:file "cache-cache")
   (:file "cli")
   (:module "gitlab"
    :components
    ((:file "source")                   ; source
     (:file "group")                    ; source
     (:file "client")                   ; client
     (:file "token")                    ; client
     (:file "pagination")               ; client
     (:file "project")                  ; client
     (:file "issue")                    ; client
     (:file "epic")                     ; client
     (:file "search")
     ;; TODO (:file "label")
     ;; TODO (:file "reference")
     ))))
