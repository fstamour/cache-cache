(in-package #:cache-cache)


;;; Web server

(defun start-server (&key
                       interface
                       port
                     &aux
                       (interface (or interface "127.0.0.1"))
                       (port (or port (find-port:find-port :interface interface))))
  "Start the web server."
  (log:info "Starting web server on ~a:~a..." interface port)
  (unless *server*
    (setf *server*
          (make-instance
           'h:easy-acceptor
           :address interface
           :port port)))
  (h:start *server*)
  (log:info "Web server started."))

(defun server-port ()
  "Get the port the server is listening to."
  (h:acceptor-port *server*))

(defun server-thread ()
  "Get the server's main thread."
  (when *server*
    (slot-value
     (slot-value *server* 'h::taskmaster)
     'h::acceptor-process)))



;;; Static files

(defvar *assets-dispatcher-and-handler*
  (h:create-folder-dispatcher-and-handler "/assets/" *root*))

(pushnew *assets-dispatcher-and-handler* h:*dispatch-table*)

(defvar *index-dispatcher-and-handler*
  (h:create-static-file-dispatcher-and-handler
   "/"
   (merge-pathnames "index.html" *root*)))

(pushnew *index-dispatcher-and-handler* h:*dispatch-table*)


;;;

(defun source-by-id (source-id)
  (find source-id *sources* :key #'source-id))

#++ (source-by-id 1)

(defun map-sources (function
                    &key (sources *sources*) result-type threadp catch-errors-p)
  (when (and threadp result-type)
    (error "Invalid combination of parameters: THREADP and RESULT-TYPE cannot be specified at the same time."))
  (if threadp
      (loop :for source :in sources
            :do (bt:make-thread
                 (lambda ()
                   ;; TODO error-handling
                   (funcall function source))
                 :name (format nil "map-sources: ~a ~a" function source)))
      (map result-type function sources)))


;;; Handlers

;; TODO be able to limit by source (e.g. add a "list of source-ids")
;; criterion)
(defun handler/search (query &optional type)
  (with-streaming-json-array ()
    (loop :for source :in *sources*
          ;; TODO perhaps make some kind of "streaming interface" for search-source?
          :for search-results = (search-source source query :limit 50 :type type)
          :do (loop :for search-result :in search-results
                    :do  (write-search-result source search-result)))))

(h:define-easy-handler (search-sources :uri "/search")
    ((query :parameter-type 'string :request-type :get :real-name "q")
     (type :parameter-type 'string :request-type :get :real-name "type"))
  (setf (hunchentoot:content-type*) "text/javascript")
  (if (str:non-blank-string-p query)
      (handler/search query type)
      "[]"))


(h:define-easy-handler (get-item :uri "/item")
    ((source-id-string :parameter-type 'string :request-type :get :real-name "source-id")
     (id-string :parameter-type 'string :request-type :get :real-name "id"))
  "Describe 1 item"
  (let* ((source-id (parse-integer source-id-string))
         (source (source-by-id source-id))
         (id (parse-integer id-string)))
    (jzon:stringify
     (item source id))))


;; TODO this has some GitLab-specific logic
;; See log-stats
#++
(h:define-easy-handler (handle-statistics :uri "/stats")
    ()
  "Get statistics"
  (setf (hunchentoot:content-type*) "text/javascript")
  (jzon:stringify
   (serapeum:dict
    :projects (hash-table-count *projects*)
    :issues (serapeum:dict*
             (serapeum:frequencies (a:hash-table-values *issues*)
                                   :key (lambda (item)
                                          (if (item-closed-at-p item)
                                              "closed"
                                              "open")))
             :total (hash-table-count *issues*)
             :last-updated
             (local-time:format-rfc1123-timestring
              nil
              (find-last-update-time *issues*))
             ))
   :pretty t))

(h:define-easy-handler (config-page :uri "/config")
    ()
  "Show the current configuration."
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A" *sources*))


;;; Initialization

(let ((cron-started nil))
  (defun start-cron ()
    "Function to start cron."
    (if cron-started
        (log:info "Cron already started.")
        (progn
          (log:info "Starting cron...")
          (cl-cron:start-cron)))))

(defun read-config ()
  (let ((config-file (uiop/configuration:xdg-config-home "cache-cache" "config.lisp")))
    (when (probe-file config-file))
    (let ((*package* (find-package 'cache-cache.config)))
      (load config-file))))

(defun serve (&key join-thread-p port interface)
  (handler-case
      (with-user-abort:with-user-abort
        (read-config)
        (map-sources #'initialize :threadp t)
        (start-cron)
        (start-server :port port :interface interface)
        ;; TODO don't hard-code the base url
        (log:info "Server started on \"http://localhost:~a\"." (server-port))
        (when join-thread-p
          ;; TODO with-user-abort
          (bt:join-thread (server-thread))))
    (with-user-abort:user-abort ()
      (quit 130))))

(defun quit (&optional (code 0))
  (log:info "Stopping cron...")
  (cl-cron:stop-cron)
  (log:info "Cron stopped.")
  (log:info "Stopping huchentoot...")
  (h:stop *server* :soft t)
  (log:info "Huchentoot stopped.")
  (log:info "Writing the persistent cache...")
  (map-sources #'write-cache)
  (log:info "Persistent cache written.")
  (log:info "Quitting...")
  (uiop:quit code)
  (log:info "Bye!"))
