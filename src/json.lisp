;;;; JSON utilities

(in-package #:cache-cache)

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

(defmacro with-streaming-json-array (() &body body)
  (a:with-gensyms (stream-var)
    `(let ((,stream-var (or (when (boundp 'h:*reply*)
                              (setf (hunchentoot:content-type*) "text/javascript")
                              (h:send-headers))
                            *standard-output*)))
       (with-json-array (,stream-var)
         ,@body))))

(defgeneric write-search-result (source search-result)
  (:documentation "Serialize one SEARCH RESULT from SOURCE as a json object, using jzon:*writer*."))

(defmethod write-search-result (source (search-result string))
  "Serialize one string from SOURCE as a json object, using jzon:*writer*."
  (write-object*
   "type" "string"
   "id" 'null
   "text" search-result
   "url" 'null))
