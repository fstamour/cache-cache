(in-package #:local-gitlab)


;;; Web server

(defvar *root*
  (merge-pathnames "assets/"
                   (asdf:system-source-directory :local-gitlab)))

(defvar *server* (make-instance
                  'h:easy-acceptor
                  :port (find-port:find-port :interface "127.0.0.1"))
  "The server instance")

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

(defun find-issues (query issue-list key)
  "Return the issues that contains all the parts of QUERY in their KEY."
  (flet ((search-issues (needle issue-list key)
           "Return the issues that has NEEDLE in KEY."
           (loop
             :for issue :in issue-list
             :when (search needle (funcall key issue)
                           :test #'string-equal)
               :collect issue)))
    (loop
      :for needle :in (split-sequence:split-sequence
                       #\Space query
                       :remove-empty-subseqs t)
      :for candidates = (search-issues needle issue-list key)
        :then (search-issues needle candidates key)
      :finally (return candidates))))

(defmacro with-json-array-to-string ((var) &body body)
  "Macro to help print a big array as json into a string."
  `(let ((shasht:*write-alist-as-object* t)
         (*print-pretty* nil))
     (with-output-to-string (,var)
       (shasht:with-json-array ,var
         ,@body))))

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


(defun handler/search (query)
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-json-array-to-string (json)
    (loop
      :for issue :in
                 (sort
                  (if (str:non-empty-string-p query)
                      (find-issues query
                                   (a:hash-table-values *issues*)
                                   #'issue-title)
                      (issues-created-in-the-last-7-days))
                  #'compare-issues)
      :do (shasht:write-json
           (a:alist-hash-table
            `(("id" . ,(issue-id issue))
              ("text" . ,(issue-title issue))
              ("url" . ,(issue-web-url issue))
              ("closed" . ,(issue-closed-at-p issue))))
           json))))

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
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-json-array-to-string (json)
    (loop
      :for issue :in (a:hash-table-values *issues*)
      :do (shasht:write-json
           (a:alist-hash-table
            `(("id" . ,(issue-id issue))
              ("text" . ,(issue-title issue))))
           json))))

(h:define-easy-handler (projects :uri "/projects") ()
  "Return _ALL_ projects."
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-json-array-to-string (json)
    (loop
      :for project :in (a:hash-table-values *projects*)
      :do (shasht:write-json project json))))


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

(defun main ()
  (unless *root-group-id*
    (format t "~&Please enter the root-group-id: ")
    (setf *root-group-id* (read-line)))
  (initialize-gitlab-token)
  (initialize-projects)
  (alexandria:if-let ((issues (read-cache)))
    (setf *issues* issues))
  (initialize-issues)
  (log-stats)
  (start-server)
  (start-cron)
  (log:info "Server started on \"http://localhost:~a\"." (server-port)))

;; (main)

(defun quit ()
  (cl-cron:stop-cron)
  (h:stop *server*)
  (write-cache)
  (uiop:quit))
