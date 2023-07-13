(in-package #:local-gitlab)

;; For interactive debugging
#++
(progn
  (setf h:*catch-errors-p* nil)
  (setf h:*catch-errors-p* t))



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


;;; Handlers

(defun search-in-list (needle list
                       &key
                         (key #'identity)
                         (test #'string-equal))
  "Return the items that has NEEDLE in KEY."
  (remove-if-not
   #'(lambda (item)
       (search needle (funcall key item) :test test))
   list))

(defun search-in-list/and (needle-list list
                           &key
                             (key #'identity)
                             (test #'string-equal))
  "Return the items that has all needles from NEEDLE-LIST in KEY."
  (loop
    :for needle :in needle-list
    :for candidates = (search-in-list needle list :key key :test test)
      :then (search-in-list needle candidates :key key :test test)
    :finally (return candidates)))

(defun find-issues (query)
  "Return the issues that contains all the parts of QUERY in their KEY."
  (search-in-list/and
   (split-sequence:split-sequence #\Space query :remove-empty-subseqs t)
   (a:hash-table-values *issues*)
   :key #'issue-title))

(defun find-projects (query)
  "Return the issues that contains all the parts of QUERY in their KEY."
  (search-in-list/and
   (split-sequence:split-sequence #\Space query :remove-empty-subseqs t)
   (a:hash-table-values *projects*)
   :key #'item-name-with-namespace))

(defmacro with-json-array ((stream) &body body)
  "Macro to help print a big array as json into a stream."
  `(let ((jzon:*writer* (jzon:make-writer :stream ,stream)))
     (jzon:with-array* ,@body))
  ;; we don't use jzon:with-writer* because it closes the stream
  #++
  `(jzon:with-writer* (:stream ,stream)
     (jzon:with-array* ,@body)))

#++
(with-output-to-string (output)
  (with-json-array (output)
    (write-value* "some text")
    (write-object*
     "id" 42
     "text" "name of this"
     "url" "some url")))

(defun timestamp-string< (a b)
  (lt:timestamp<
   (lt:parse-rfc3339-timestring a)
   (lt:parse-rfc3339-timestring b)))

(defun timestamp-string> (a b)
  (lt:timestamp>
   (lt:parse-rfc3339-timestring a)
   (lt:parse-rfc3339-timestring b)))

(defun compare-issues (issue1 issue2)
  "Should ISSUE1 be shown before ISSUE2?"
  (let ((closed1 (issue-closed-at-p issue1))
        (closed2 (issue-closed-at-p issue2)))
    (if (eq closed1 closed2)
        (timestamp-string>
         (issue-updated-at issue1)
         (issue-updated-at issue2))
        closed2)))

(defun issues-created-in-the-last-7-days ()
  (let ((last-week (lt:adjust-timestamp
                       (lt:today)
                     (offset :day -7))))
    (remove-if #'(lambda (issue)
                   (lt:timestamp<
                    (lt:parse-rfc3339-timestring (issue-created-at issue))
                    last-week))
               (a:hash-table-values *issues*))))


(defmacro with-streaming-json-array (() &body body)
  (a:with-gensyms (stream-var)
    `(let ((,stream-var (or (when (boundp 'h:*reply*)
                              (setf (hunchentoot:content-type*) "text/javascript")
                              (h:send-headers))
                            *standard-output*)))
       (with-json-array (,stream-var)
         ,@body))))

(defun handler/search (query &optional type)
  (with-streaming-json-array ()
    ;; Add projects
    (when (and (str:non-empty-string-p query)
               (or (null type)
                   (eq type :project)))
      (loop :for project :in (find-projects query)
            :do (write-object*
                 "type" "project"
                 "id"  (item-id project)
                 "text" (item-name-with-namespace project)
                 "url" (format nil "~a/issues" (item-web-url project)))))
    ;; Add issues
    (when (or (null type)
              (eq type :issue))
      (loop
        :for issue :in
                   (sort
                    (if (str:non-empty-string-p query)
                        (find-issues query)
                        (issues-created-in-the-last-7-days))
                    #'compare-issues)
        :do (write-object*
             "type" "issue"
             "id" (issue-id issue)
             "text" (issue-title issue)
             "url" (issue-web-url issue)
             "closed" (issue-closed-at-p issue))))))

(h:define-easy-handler (search-gitlab :uri "/search")
    ((query :parameter-type 'string :request-type :get :real-name "q")
     (type :parameter-type 'string :request-type :get :real-name "type"))
  (setf (hunchentoot:content-type*) "text/javascript")
  (handler/search
   query
   (when (and type
              (member type '("project" "issue") :test #'string-equal))
     (intern (string-upcase type) #.(find-package :keyword)))))

;; Testing find-issues
#+ (or)
(time
 (let ((query "auto update"))
   (format t "~&=================================")
   (mapcar #'issue-title
           (find-issues query
                        (a:hash-table-values *issues*)
                        #'issue-title))))


(h:define-easy-handler (issues :uri "/issues") ()
  "Return _ALL_ issues."
  (with-streaming-json-array ()
    (maphash
     #'(lambda (id issue)
         (write-object*
          "id" id
          "text" (issue-title issue)
          "project" (item-name-with-namespace
                     (issue-project id))))
     *issues*)))

(h:define-easy-handler (projects :uri "/projects") ()
  "Return _ALL_ projects."
  (with-streaming-json-array ()
    (maphash
     #'(lambda (id project)
         (declare (ignore id))
         (write-value* project))
     *projects*)))

(h:define-easy-handler (get-item :uri "/item")
    ((id-string :parameter-type 'string :request-type :get :real-name "id"))
  "Describe 1 item"
  (let ((id (parse-integer id-string)))
    (jzon:stringify
     (or
      (issue-by-id id)
      (project-by-id id)))))


;;; Initialization

(let ((cron-started nil))
  (defun start-cron ()
    "Function to start cron."
    (if cron-started
        (log:info "Cron already started.")
        (progn
          (log:info "Starting cron...")
          (cl-cron:start-cron)))))


(cl-cron:make-cron-job
 #'initialize-issues
 :hash-key 'update-issues)

;; (cl-cron:delete-cron-job 'update-issues)

(defun log-stats ()
  (log:info "There are currently ~D issues and ~D projects in memory."
            (hash-table-count *issues*)
            (hash-table-count *projects*)))

(defun read-config ()
  (let ((config-file (uiop/configuration:xdg-config-home "local-gitlab" "config.lisp")))
    (when (probe-file config-file))
    (let ((*package* (find-package 'local-gitlab.config)))
      (load config-file))))

(defun serve (&key join-thread-p port interface)
  (read-config)
  (unless *root-group-id*
    (format t "~&Please enter the root-group-id: ")
    (finish-output)
    (setf *root-group-id* (read-line)))
  (initialize-gitlab-token)
  (read-cache)
  (initialize-issues)
  (initialize-projects)
  (log-stats)
  (write-cache)
  (start-server :port port :interface interface)
  (start-cron)
  (log:info "Server started on \"http://localhost:~a\"." (server-port))
  (when join-thread-p
    ;; TODO with-user-abort
    (bt:join-thread (server-thread))))

(defun quit ()
  (log:info "Stopping cron...")
  (cl-cron:stop-cron)
  (log:info "Cron stopped.")
  (log:info "Stopping huchentoot...")
  (h:stop *server* :soft t)
  (log:info "Huchentoot stopped.")
  (log:info "Writing the persistent cache...")
  (write-cache)
  (log:info "Persistent cache written.")
  (log:info "Quitting...")
  (uiop:quit)
  (log:info "Bye!"))
