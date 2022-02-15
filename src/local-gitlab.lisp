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

(defun froute-dispatcher (request)
  (let ((result
   (froute:invoke-route
    (h:request-uri request)
    :method (h:request-method request))))
    (when result #'(lambda () result))))

(pushnew 'froute-dispatcher h:*dispatch-table*)

(defclass route/vuejs ()
  ()
  (:metaclass froute:froute-class)
  (:route "/vue.js"))

(defclass route/mainjs ()
  ()
  (:metaclass froute:froute-class)
  (:route "/main.js"))

(defclass route/index ()
  ()
  (:metaclass froute:froute-class)
  (:route "/"))



;; Static file dispatcher for vuejs
#+ (or)
(let ((dispatcher
        (h:create-static-file-dispatcher-and-handler
         "/vue.js"
         (merge-pathnames "vue-2.6.14-dev.js" *root*))))
  (defun vuejs-dispatcher (&rest args)
    (apply dispatcher args))
  (pushnew 'vuejs-dispatcher h:*dispatch-table*))

(defmethod froute:run ((r route/vuejs) (method (eql :get)))
  (h:handle-static-file (merge-pathnames "vue-2.6.14-dev.js" *root*)))


(defmethod froute:run ((r route/mainjs) (method (eql :get)))
  ;; h:define-easy-handler (main-js :uri "/main.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps:ps
    ;; "fixing" parenscript internals...
    (defvar *__PS_MV_REG*)

    (defun debounce (delay fn)
      (let ((timer))
 (lambda ()
   (let ((self this)
  (args arguments))
     (clear-timeout timer)
     (setf timer (set-timeout (lambda ()
           ((chain fn apply) self args)
           (values))
         delay)))
   (values))))


    ((chain -Vue component)
     "search-result-item"
     (create
      :props (list "result")
      :template (lisp (spinneret:with-html-string
   (:li (:a :v-bind--href "result.url"
     :target "_blank"
     :rel "noopener noreferrer"
     "{{ result.closed ? \"(closed) \" : \"\" }}{{result.text}}"))))))

    ;; The state of the application
    (var db (create
      "query" ""
      "searchResultList"  (list)))

    (defun search (query)
      ((chain console log) query)
      (chain (fetch (+ "/search?q=" query))
      (then (lambda (response) ((chain response json))))
      (then (lambda (result)
       ((chain console log) result)
       (setf (chain db "searchResultList") result)))))

    ;; Create the app
    (var app
  (new (-vue
        (create
  :el "#content"
  :data db
  :watch (create
   "query" (debounce 250 search))))))))

#+ (or)
(ps
  (defun debounce (delay fn)
    (let ((timer))
      (lambda ()
 (let ((self this)
       (args arguments))
   (clearTimeout timer)
   (setf timer (set-timeout (lambda ()
         ((chain fn apply) self args))
       delay)))))))



(defmethod froute:run ((r route/index) (method (eql :get)))
  ;; h:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "GitLab quicksearch")
      (:script :src "vue.js"))
     (:body
      (:div :id "content"
     (:input :type "text" :v-model--value "query")
     (:ol
      (:search-result-item
       :v-for "result in searchResultList"
       :v-bind--result "result")))
      (:script :src "main.js")))))

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

(defun find-issues-fuzzy-match (query issue-list)
  (fuzzy-match:fuzzy-match
   query
   issue-list
   :suggestions-display (mapcar #'issue-title issue-list)))

#+ (or)
(defun find-issues-montezuma (query)
  (loop
    :for (doc-id . score) :in (search-issue (format nil "title:\"~a\"" query))
    :for issue-id = (montezuma:document-value (montezuma:get-document *index* doc-id) "id")
    :for issue = (gethash issue-id *issues*)
    :collect issue))

(defmacro with-json-array-to-string ((var) &body body)
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

(defun main ()
  (unless *root-group-id*
    (format t "~&Please enter the root-group-id: ")
    (setf *root-group-id* (read-line)))
  (initialize-gitlab-token)
  (initialize-projects)
  (initialize-issues)
  (start-server)
  (start-cron)
  (log:info "Server started on \"http://localhost:~a\"." (server-port)))

;; (main)
