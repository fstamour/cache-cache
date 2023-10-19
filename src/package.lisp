
#+sbcl
(progn
  (sb-ext:restrict-compiler-policy 'debug 3 3)
  (sb-ext:restrict-compiler-policy 'safety 3 3))

(uiop:define-package #:cache-cache
    (:documentation "TODO")
  (:use #:cl)
  (:use-reexport
   #:cache-cache.config
   #:cache-cache.generic
   #:cache-cache.source
   #:cache-cache.cache
   #:cache-cache.search)
  (:local-nicknames (#:h #:hunchentoot)
                    (#:a #:alexandria)
                    (#:lt #:local-time)
                    (#:jzon #:com.inuoe.jzon))
  (:import-from #:com.inuoe.jzon
                #:write-value*
                #:write-object*)
  (:export #:serve #:main-cli #:quit))
