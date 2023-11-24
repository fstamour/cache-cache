(uiop:define-package #:cache-cache.source
    (:documentation "Generic sources of information")
  (:use #:cl)
  (:use-reexport #:cache-cache.generic)
  (:export
   #:map-supported-topics))

(in-package #:cache-cache.source)

(defclass source ()
  ((source-id
    :initform (error ":source-id must be specified")
    :initarg :source-id
    :reader source-id
    :documentation "The identifier of the source, must be an integer.")
   (name
    :initform (error ":name must be specified")
    :initarg :name
    :reader name
    :documentation "The name of the source")
   ;; TODO add slot "description"
   ;; TODO we could make a more abstract "source" by removing this
   ;; slot:
   (items-by-topic
    :initform (make-hash-table)
    :initarg :items-by-topic
    :reader items-by-topic
    :documentation "The topics provided by this source"))
  (:documentation "A source of data."))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type t :identity t)
    (format stream "sid: ~s topics: ~s"
            (source-id source)
            (topics source))))

(defmethod topics (source)
  "List all the topics actually stored in the source's in-memory cache."
  (alexandria:hash-table-keys (items-by-topic source)))

(defmethod supported-topics append (source)
  "Fallback method: a source that doesn't specialize this method will
return an empty list."
  nil)

(defmethod items ((source source) (topic symbol))
  "Get all the items in SOURCE of \"type\" TOPIC."
  (gethash topic (items-by-topic source)))

(defmethod (setf items) (new-items (source source) (topic symbol))
  (setf (gethash topic (items-by-topic source)) new-items))

(defmethod item ((source source) topic id)
  "Get the item ID from SOURCE."
  (gethash id (items source topic)))
;; TODO (setf item) ?

(defmethod statistics (source)
  (loop :for topic :in (supported-topics source)
        :for items = (items source topic)
        :collect (list topic (or (and items
                                      (hash-table-count items))
                                 0))))



(defmethod map-supported-topics (fn (source source))
  ;; TODO add the option to do this in parallel (e.g. to initialize
  ;; the topics in parallel).
  (loop :for topic :in (supported-topics source)
        ;; TODO error handling?
        :do (funcall fn source topic)))

;; TODO initialize topics in parallel?
;; TODO don't fail if some topics fails to initialize
;; TODO MAKE-CRON-JOB (refresh topic)
(defmethod initialize ((source source))
  (log:info "Initializing ~a..." source)
  ;; Initialize each topic
  (map-supported-topics #'(lambda (source topic)
                            (setf (items source topic) (make-hash-table :test 'equal))
                            (read-cache source topic)
                            (initialize-topic source topic)
                            (write-cache source topic))
                        source)
  (log:info "Done initializing ~a." source))


;; TODO add :around method on initialize-topic, so that only 1 runs at the same time
;; (imaging a cron-job that is scheduled every minutes, but it takes multiple mitutes to complete...
