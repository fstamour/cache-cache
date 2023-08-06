(in-package #:cache-cache)

;; http://localhost:40000/index.html

(defun body ()
  (flute:element-string
   (flute:h
     (div :class "center"
          (input
           :id "query"
           :type "search"
           :name "q"
           :hx-get "/search.html"
           :hx-trigger "keyup changed delay:250ms, search"
           :hx-target "#results")
          (div#results )))))

(h:define-easy-handler (sse-stream :uri "/index-stream")
    ()
  ""
  (call-with-sse-stream
   (lambda (send-message)
     (loop
       :with fn = (throttle
                   (on-change
                    'body
                    (lambda (new-value)
                      (funcall send-message new-value))
                    :changedp (lambda (old new)
                                (string/= old new)))
                   0.1)
       :do (funcall fn)))))

(h:define-easy-handler (try-sse :uri "/index.html")
    ()
  "TODO DOCSTRING"
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
       (link :rel "stylesheet" :href "/assets/main.css")
       (title "Cache-cache"))
      (body
       (div :hx-ext "sse"
            :sse-connect "/index-stream"
            :sse-swap "message"))))))

#++
(progn (flute:define-element issue (issue)
         (flute:a :href (issue-web-url issue)
                  (issue-title issue)))

       (let ((issue (first (issues-created-in-the-last-7-days))))
         (flute:h (issue :issue issue))))

(defun issue-html (issue)
  (flute:a :href
           #++ (write-object*
                "type" "issue"
                "id" (issue-id issue)

                "closed" (issue-closed-at-p issue))
           (issue-web-url issue)
           (issue-title issue)))

(h:define-easy-handler (search-fragment :uri "/search.html")
    ((query :parameter-type 'string :request-type :get :real-name "q"))
  "TODO DOCSTRING"
  (setf (hunchentoot:content-type*) "text/html")
  (format *debug-io* "~&query: ~s~%" query)
  (force-output *debug-io*)
  (flute:element-string
   (flute:h
     (ol*
      nil
      (loop
        :for issue :in
                   (sort
                    (if (str:non-empty-string-p query)
                        (find-issues query)
                        (issues-created-in-the-last-7-days))
                    #'compare-issues)
        :collect (li (issue-html issue)))))))
