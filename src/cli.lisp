(in-package #:local-gitlab)

(defparameter *program-name* "local-gitlab")

(defun ensure-unique (name &optional parameterp)
  (if parameterp
      #'(lambda (old-value &optional new-value)
          (format *standard-output* "~&old: ~A new: ~A" old-value new-value)
          (if old-value (error "--~A can be specified only once." name) new-value))
      #'(lambda (old-value)
          (if old-value (error "--~A can be specified only once." name) t))))


(defparameter *ui*
  (let* ((help (adopt:make-option
                'help
                :long "help"
                :short #\h
                :help "Display help and exit."
                :reduce (constantly t)))
         (serve (adopt:make-option
                 'serve
                 :long "serve"
                 :help "Start the web server."
                 :reduce (ensure-unique "serve")))
         (port (adopt:make-option
                'port
                :long "port"
                :short #\p
                :help "Port to listen to. Will try to find on if not provided."
                :parameter "PORT"
                :reduce (ensure-unique "port" t)
                :key #'parse-integer))
         (interface (adopt:make-option
                     'interface
                     :long "interface"
                     :short #\i
                     :help "Interface to listen to. Defaults to 127.0.0.1"
                     :parameter "INTERFACE"
                     :reduce (ensure-unique "interface" t)))
         (serve-group (adopt:make-group
                       'server-options
                       :title "Server options"
                       :options (list
                                 port
                                 interface)))
         (list-issues (adopt:make-option
                       'list-issues
                       :long "list-issues"
                       :help "List issues from cache."
                       :reduce (ensure-unique "list-issues"))))
    (adopt:make-interface
     :name *program-name*
     :summary #1="A tool to work with a local cache of part of GitLab."
     :usage "[-h|--serve]"
     :help #1#
     :contents (list
                help
                serve
                serve-group
                list-issues)
     :examples `(("Start a web server" . "local-gitlab --serve")))))



;; To see the generated help
#++
(with-output-to-string (*standard-output*)
  (adopt:print-help *ui*))

(defun print-help (&optional failurep)
  (if failurep
      (adopt:print-help
       *ui*
       :program-name *program-name*
       :stream *error-output*)
      (adopt:print-help
       *ui*
       :program-name *program-name*)))

;; TODO generate tests
#++
(let ((valid-examples '("-h" "--help"
                        ("--serve"
                         (&optional (or "--port" "-p"))
                         (&optional (or "--interface" "-i")))))))

(defun check-exclusive-options (keys options)
  (let ((provided-exclusive-options (loop :for key :in keys
                                          :when (gethash key options)
                                            :collect key)))
    (case (length provided-exclusive-options)
      (0 (format *error-output* "At least of of these options must be provided: ~(~{--~A~^, ~}~)~%" keys)
       nil)
      (1 (first provided-exclusive-options))
      (t
       (format *error-output* "The options ~{--~A~#[~; and ~:;, ~]~} cannot be specifed at the same time.~%" provided-exclusive-options)
       nil))))

#++
(progn
  (check-exclusive-options '(:a) (a:plist-hash-table '(:a t :b t :c)))
  (check-exclusive-options '(:a) (a:plist-hash-table '(:a nil :b t :c)))
  (check-exclusive-options '(:a) (a:plist-hash-table '(:b t :c)))
  (check-exclusive-options '(:a :b) (a:plist-hash-table '(:a t :b t :c)))
  (check-exclusive-options '(:a :b) (a:plist-hash-table '(:a t :c nil))))

#++
(progn
  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '())))

  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '("--serve" "--port" "8080"))))

  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '("--serve"))))

  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '("--serve"
                                            "--port" "8080"
                                            "--port" "8080"))))

  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '("--serve" "--port" "8080"
                                            "--port"))))
  (a:hash-table-plist
   (nth-value 1 (adopt:parse-options *ui* '("--serve"
                                            "--port" "8080"
                                            "--port" "asdf")))))

;; (trace check-exclusive-options)

(defun parse-options (args)
  (if args
      (multiple-value-bind (leftovers options)
          (adopt:parse-options *ui* args)
        (declare (ignorable leftovers))
        ;; (format *debug-io* "~&Leftover arguments: ~A~%" leftovers)
        (flet ((option (name &optional default)
                 (gethash name options default)))
          (if (option 'help) `(progn (print-help t) 0)
              (case (check-exclusive-options '(serve list-issues) options)
                (serve
                 `(progn (serve
                          :join-thread-p t
                          :port ,(option 'port)
                          :interface ,(option 'interface "localhost"))
                         0))
                (list-issues
                 `(progn
                    (read-cache)
                    (handler/search ,(format nil "~{~a~^ ~}" leftovers) :issue)
                    0))
                (t `(progn (print-help t) 1))))))
      `(progn (print-help) 1)))

;; (handler/search "api")
#++
(with-output-to-string (*standard-output*)
  (let* ((leftovers '("api"))
         (query (format nil "~{~a~^ ~}" leftovers)))
    (handler/search query :issue)))

#++
(progn
  (parse-options '())
  (parse-options '("--serve" "--port" "8080"))
  (parse-options '("--list-issues" "needle")))

;; TODO A macro to capture multiple streams could be nice
(defun test-parse-options (args)
  (let (out err code)
    (setf err
          (with-output-to-string (*error-output*)
            (setf out
                  (with-output-to-string (*standard-output*)
                    (setf code
                          (eval
                           (parse-options args)))))))
    (list :output out :error err :code code)))

#++
(test-parse-options '("-h"))

(defun main-cli (args)
  ;; (format *debug-io* "~&Args: ~S~%" args)
  (log:config :error)
  (handler-case
      (uiop:quit (eval (parse-options (rest args))))
    (error (condition)
      (adopt:print-error-and-exit condition))))

;; (main-cli '("--list-issues"))


(defparameter uiop/image:*image-entry-point*
  #'(lambda ()
      (main-cli (uiop/image:raw-command-line-arguments))))
