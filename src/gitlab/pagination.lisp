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

(defun ensure-uri-string (uri)
  (etypecase uri
    (string uri)
    (puri:uri (puri:render-uri uri nil))))

(defun http-request-get-all (uri token &optional callback)
  "Calls uri and all the \"next\" links, returns a vector of all the results concatenated."
  (apply #'concatenate 'vector
         (loop
           :for %uri = uri
             :then (extract-next-uri headers)
           :while %uri
           :for response = (multiple-value-list (http-request-gitlab %uri token))
           :while
           :for (body headers) = response
           ;; :do (break "body: ~s" body)
           :do (when callback (funcall callback body))
               ;; TODO remove :collect, make CALLBACK mandatory
           :collect body)))
