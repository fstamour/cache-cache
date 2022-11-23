(in-package #:local-gitlab)


;;; Web server

(defun start-server ()
  "Start the web server"
  ;; TODO we should find-port here...
  (log:info "Starting web server...")
  (h:start *server*)
  (log:info "Web server started."))

(defun server-port ()
  ;; Get the port
  (h:acceptor-port *server*))


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
  `(jzon:with-writer* (:stream ,stream)
     (jzon:with-array* ,@body)))

#+ (or)
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
    `(progn
       (setf (hunchentoot:content-type*) "text/javascript")
       (let ((,stream-var (h:send-headers)))
         (with-json-array (,stream-var)
           ,@body)
         (finish-output ,stream-var)))))

(defun handler/search (query)
  (with-streaming-json-array ()
    ;; Add projects
    (when (str:non-empty-string-p query)
      (loop :for project :in (find-projects query)
            :do (write-object*
                 "type" "project"
                 "id"  (item-id project)
                 "text" (item-name-with-namespace project)
                 "url" (format nil "~a/issues" (item-web-url project)))))
    ;; Add issues
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
           "closed" (issue-closed-at-p issue)))))

(h:define-easy-handler (search-gitlab :uri "/search")
    ((query :parameter-type 'string :request-type :get :real-name "q"))
  (setf (hunchentoot:content-type*) "text/javascript")
  (handler/search query))

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
          "text" (issue-title issue)))
     *issues*)))

(h:define-easy-handler (projects :uri "/projects") ()
  "Return _ALL_ projects."
  (with-streaming-json-array ()
    (maphash
     #'(lambda (id project)
         (declare (ignore id))
         (write-value* project))
     *projects*)))


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

;; TODO Add command line interface
;; https://docs.stevelosh.com/adopt/usage/

(defun main ()
  ;; TODO (uiop/image:raw-command-line-arguments)
  (unless *root-group-id*
    (format t "~&Please enter the root-group-id: ")
    (finish-output)
    (setf *root-group-id* (read-line)))
  (initialize-gitlab-token)
  (initialize-projects)
  (alexandria:if-let ((issues (read-cache)))
      (setf *issues* issues))
  (initialize-issues)
  (log-stats)
  (start-server)
  (start-cron)
  (log:info "Server started on \"http://localhost:~a\"." (server-port))
  ;; TODO Keep the thread alive (if running as an executable)
  )

;; (main)

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
