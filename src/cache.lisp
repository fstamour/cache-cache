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

;; TODO add an argument "writer"
;; TODO write 2 files (.sbin and .offsets)
(defun write-cache-file (source type &aux (hash-table
                                           (resources source type)))
  (unless (or (null hash-table)
              (zerop (hash-table-count hash-table)))
    (simpbin:with-output-to-binary-file (output (cache-pathname source type)
                                                :if-exists :supersede)
      (simpbin:write-header output)
      (loop :for id
              :being :the :hash-key :of hash-table
                :using (hash-value object)
            :do (simpbin:write-binary-string
                 (com.inuoe.jzon:stringify object :stream nil)
                 output)))))

(defun read-cache-file (source type &key (key #'id) (test 'equal))
  ;; TODO maybe validate KEY and TEST
  (alexandria:if-let ((cache-pathname
                       (probe-file (cache-pathname source type))))
    (simpbin:with-input-from-binary-file (input cache-pathname)
      (simpbin:read-header input)
      (loop
        :with result = (make-hash-table :test test)
        :for json-string = (handler-case (simpbin:read-binary-string input)
                             (end-of-file (condition)
                               (declare (ignore condition))))
        :while json-string
        :for object = (com.inuoe.jzon:parse json-string)
        :for k = (funcall key object)
        :do (setf (gethash k result) object)
        :finally (return result)))))
