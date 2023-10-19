(in-package #:cache-cache.gitlab.client)


;;; Pagination

(defun extract-links-from-header (headers)
  "Given an alist of HEADERS, extract the URL from the \"link\" HTTP header."
  (alexandria:if-let ((link-header (cdr (assoc :link headers))))
    (loop
      :for i :from 0
      :for link :in (split-sequence:split-sequence #\, link-header)
      :for (uri rel) = (split-sequence:split-sequence #\; link)
      :collect (cons
                ;; The rel have the format " rel=\"...\""
                (subseq rel 6 (1- (length rel)))
                ;; The urls are surrounded with <>, remove
                ;; them. Except for the first one, they are prefixed
                ;; with a space
                (subseq uri (if (zerop i) 1 2) (1- (length uri)))))))

#+ (or)
(extract-links-from-header
 '((:link . "<url1>; rel=\"next\", <url2>; rel=\"first\", <url3>; rel=\"last\"")))
;; => (("next" . "uri1") ("first" . "uri2") ("last" . "uri3"))

#+ (or)
(extract-links-from-header '())
;; => nil


(defun extract-next-uri (headers)
  (alexandria:if-let ((links (extract-links-from-header headers)))
    (alexandria:if-let ((next-link (find "next" links :key #'first :test #'string=)))
      (cdr next-link))))

#+ (or)
(extract-next-uri
 '((:link . "<uri1>; rel=\"next\", <uri2>; rel=\"first\", <uri3>; rel=\"last\"")))
;; => "url1"


(defparameter *last-headers* nil
  "just for debugging")

;; TODO move to "workbench"
#+ (or)
(progn
  (extract-links-from-header *last-headers*)
  (extract-next-uri *last-headers*))


(defun errorp (response)
  (and (hash-table-p response)
       (or
        (gethash "message" response)
        (gethash "error" response)
        (gethash "error_description" response))))


;;; TODO Handle rate-limiting gracefully https://docs.gitlab.com/ee/user/admin_area/settings/user_and_ip_rate_limits.html#response-headers
;;; e.g.
;;; - use some kind of queues (maybe chanl?)
;;; - look at headers Rate-Limit-Remaining and RetryAfter
(defun http-request-gitlab (uri token &rest rest)
  (log:debug "Making a request to GitLab: \"~a\"..." uri)
  ;; TODO Move that handler-case somewhere else, e.g. in the labmda
  ;; ran by cl-cron.
  (handler-case
      (multiple-value-bind
            (body status-code headers uri-object stream must-close reason-phrase)
          ;; TODO use dexador instead
          (apply #'drakma:http-request
                 uri
                 ;; TODO Parse "rest" to extract ":additional-headers"
                 ;; Send the auth
                 :additional-headers (list (token-header token))
                 (alexandria:remove-from-plist rest :additional-headers))
        (declare (ignore stream must-close uri-object))
        (declare (ignorable headers))
        ;; status-code reason-phrase
        (log:debug "~A ~A" status-code reason-phrase)
        ;; (if )
        (setf *last-headers* headers)
        (let ((response (jzon:parse body)))
          (a:when-let ((message (errorp response)))
            ;; TODO better error message
            ;; TODO add a restart
            (error "http-request REST error: message = ~a (~a)" message
                   (jzon:stringify response)))
          (list response headers)))
    #++
    (error (condition)
      (break)
      (log:error "~a" condition)
      nil)))

(defun http-request-get-all (uri token)
  "Calls uri and all the \"next\" links, returns a vector of all the results concatenated."
  (apply #'concatenate 'vector
         (loop
           :for %uri = uri
             :then (extract-next-uri headers)
           :while %uri
           :for response = (http-request-gitlab %uri token)
           :while response
           :for (body headers) = response
           ;; :do (break "body: ~s" body)
           :collect body)))
