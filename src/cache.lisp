(defpackage #:cache-cache.cache ; did I stutter?
  (:documentation "Utilities for in-memory and on-disk caches")
  (:use #:cl
        #:cache-cache.generic
        #:cache-cache.source)
  (:export
   #:cache-filename
   #:cache-pathname
   #:write-cache-file
   #:read-cache-file))

(in-package #:cache-cache.cache)



;;; Filename

;; TODO use the source's name to generate the name of the cache file

(defun %cache-pathname (name)
  (merge-pathnames
   name
   (ensure-directories-exist
    (uiop/configuration:xdg-cache-home "cache-cache/"))))

(defmethod cache-filename (source type)
  (format nil "~(~a-~a~)" (source-id source) type))

(defun cache-pathname (source type &optional (suffix ".sbin"))
  (%cache-pathname
   (concatenate 'string
                (cache-filename source type)
                suffix)))


;;; Serialization and deserialization

(defun write-hash-table (pathname hash-table)
  (unless (or (null hash-table)
              (zerop (hash-table-count hash-table)))
    (simpbin:with-output-to-binary-file (output pathname
                                                :if-exists :supersede)
      (simpbin:write-header output)
      (loop :for id
              :being :the :hash-key :of hash-table
                :using (hash-value object)
            :do (simpbin:write-binary-string
                 (com.inuoe.jzon:stringify object :stream nil)
                 output)))))

(defun read-hash-table* (pathname callback)
  (simpbin:with-input-from-binary-file (input pathname)
    (simpbin:read-header input)
    (loop
      :for json-string = (handler-case (simpbin:read-binary-string input)
                           (end-of-file (condition)
                             (declare (ignore condition))))
      :while json-string
      :for object = (com.inuoe.jzon:parse json-string)
      :do (funcall callback object))))

;; TODO maybe add "destination" as an optional parameter
(defun read-hash-table (pathname &key (key #'id) (test 'equal))
  (let ((destination (make-hash-table :test test)))
    (read-hash-table*
     pathname
     (lambda (object &aux (k (funcall key object)))
       (setf (gethash k destination) object)))
    destination))

;; TODO add an argument "writer"
;; TODO write 2 files (.sbin and .offsets)
(defun write-cache-file (source type &aux (hash-table
                                           (items source type)))
  (unless (or (null hash-table)
              (zerop (hash-table-count hash-table)))
    (write-hash-table (cache-pathname source type)
                      hash-table)))

(defun read-cache-file (source type &key (key #'id) (test 'equal))
  ;; TODO maybe validate KEY and TEST
  (alexandria:if-let ((cache-pathname
                       (probe-file (cache-pathname source type))))
    (read-hash-table cache-pathname
                     :key key
                     :test test)))




;; TODO (defmethod clear-cache ((source source) (topic symbol)))

(defmethod read-cache ((source source) (topic symbol))
  (log:info "Reading all the ~a from the cache..." topic)
  (setf (items source topic)
        (read-cache-file source topic)))

(defmethod write-cache ((source source) (topic symbol))
  (log:info "Writing all the ~a to the cache..." topic)
  (write-cache-file source topic))
