(in-package #:cache-cache)

;; http://localhost:40000/test/sse.html



;; Reminder: I have my own fork of flute, to fix the self-closing tags
;; TODO commit...
(progn
  (defvar *xdg-config-dirs*
    (uiop:getenv "XDG_CONFIG_DIRS")
    "The original value of the environment variable XDG_CONFIG_DIRS")
  ;; Update the environment variable
  (setf (uiop:getenv
         "XDG_CONFIG_DIRS")
        (format nil "~{~a~^:~}"
                (list
                 *xdg-config-dirs*
                 (merge-pathnames
                  "dev/flute"
                  (user-homedir-pathname))
                 (merge-pathnames
                  "dev/guix-configurations/lisp-profile/etc"
                  (user-homedir-pathname)))))
  (asdf:clear-source-registry)
  (asdf:load-system :flute))




(h:define-easy-handler (try-sse :uri "/test/sse.html")
    ()
  "Trying out SSE"
  (setf (hunchentoot:content-type*) "text/html")
  (flute:element-string
   (flute:h
     (html
      (head
       (meta :name "viewport"
             :content "width=device-width, initial-scale=1")
       (link :rel "icon" :href "data:,")
       (script '(:src "/assets/htmx-1.9.4.js"))
       (script :src "/assets/sse.js")
       (title "Cache-cache"))
      (body
       (div :hx-ext "sse"
            :sse-connect "/test/sse-stream"
            :sse-swap "message"))))))



(defun format-sse (data)
  ;; TODO support 'event 'id and 'retry
  ;; See https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
  (format nil
          "~{data: ~a~%~}~%"
          ;; TODO check that data is actually a sequence...
          (split-sequence:split-sequence #\newline data)))

;; (format-sse "hi")


(defun call-with-sse-stream (fn)
  ;; Make sure the method is GET
  (unless (eq :get (h:request-method*))
    (setf (h:return-code*) h:+http-method-not-allowed+)
    (h:abort-request-handler))
  ;; set the content type
  (setf (hunchentoot:content-type*) "text/event-stream")
  ;; start streaming
  (let ((stream (h:send-headers)))
    (funcall fn (lambda (&rest args)
                  (write-sequence (flexi-streams:string-to-octets
                                   (apply #'format-sse args)
                                   :external-format :utf8)
                                  stream)
                  (force-output stream)))))



;; TODO see/use myelin's clock.lisp
(defun get-time ()
  (/ (get-internal-real-time) 1.0
     internal-time-units-per-second))

(defun throttle (fn threshold)
  (let ((then 0))
    (lambda (&rest args
             &aux
               (now (get-time))
               (dt (- now then)))
      (when (< dt threshold)
        (sleep (- threshold dt)))
      (setf then (get-time))
      (apply fn args))))

#++
(let* ((i 0)
       (fn (throttle (lambda ()
                       (format t "~&~d time: ~a " (incf i) (get-time))
                       (force-output))
                     1)))
  (loop repeat 5 do (funcall fn)))


;; Basic sse

(defun generate-data (i)
  (format nil "Sup ~d" i))

(trace generate-data format-sse)

(h:define-easy-handler (sse-stream :uri "/test/sse-stream")
    ()
  "Trying out SSE"
  (call-with-sse-stream
   (lambda (send-message)
     (loop
       :with i = 10
       :with fn = (throttle
                   (lambda ()
                     (funcall send-message
                              (generate-data (incf i))))
                   1.0)
       :do (funcall fn)))))



(serapeum:defunit undefined)

(defparameter *message*
  (jzon:stringify
   (serapeum:dict
    :a 24)))

(defun on-change (get-value
                  on-new-value
                  &key (changedp optional)
                  &aux (changedp (or changedp
                                     (lambda (old new)
                                       (not (eq old new))))))
  (let ((last-value undefined))
    (lambda (&aux (new-value (funcall get-value)))
      (when (or
             (typep last-value 'undefined)
             (funcall changedp last-value new-value))
        (setf last-value new-value)
        (funcall on-new-value new-value)))))

(defun hey ()
  "43")

(h:define-easy-handler (sse-stream :uri "/test/sse-stream")
    ()
  "Trying out SSE"
  (call-with-sse-stream
   (lambda (send-message)
     (loop
       :with fn = (throttle
                   (on-change
                    ;; (lambda () *message*)
                    'hey
                    (lambda (new-value)
                      (funcall send-message new-value)))
                   0.1)
       :do (funcall fn)))))
