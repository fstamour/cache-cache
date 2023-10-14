;; TODO Create a package for this

(in-package #:cache-cache.gitlab.source)

;; TODO in-memory cache (e.g. replace *issues* and *project*)


;;; Persistent cache

;; TODO use the source's name to generate the name of the cache file

(defun cache-pathname (name)
  (merge-pathnames (format nil "~(~a~)-cache.sbin" name)
                   (ensure-directories-exist
                    (uiop/configuration:xdg-cache-home "cache-cache/"))))

(defun issue-cache-pathname ()
  (cache-pathname :issue))

(defun write-cache-file (name hash-table)
  (unless (or (null hash-table)
              (zerop (hash-table-count hash-table)))
    (simpbin:with-output-to-binary-file (output (cache-pathname name)
                                                :if-exists :supersede)
      (simpbin:write-header output)
      (loop :for id
              :being :the :hash-key :of hash-table
                :using (hash-value object)
            :do (simpbin:write-binary-string
                 (jzon:stringify object :stream nil)
                 output)))))

(defun read-cache-file (name)
  (alexandria:if-let ((cache-pathname
                       (probe-file (cache-pathname name))))
    (simpbin:with-input-from-binary-file (input cache-pathname)
      (simpbin:read-header input)
      (by-id
       (loop
         :for json-string = (handler-case (simpbin:read-binary-string input)
                              (end-of-file (condition)
                                (declare (ignore condition))))
         :while json-string
         :for object = (jzon:parse json-string)
         :collect object)))))

(defun write-cache ()
  (write-cache-file :issue *issues*)
  (write-cache-file :project *projects*))

(defun read-cache ()
  (log:info "Reading all the issues from the cache...")
  (setf *issues* (or (read-cache-file :issue)
                     (by-id '())))
  (log:info "Reading all the projects from the cache...")
  (setf *projects* (or (read-cache-file :project)
                       (by-id '()))))



;; (time (read-cache-file :issue))
#++
(time (prog1 nil
        (jzon:parse
         (length (jzon:stringify *issues*))
         14 462 450
         :key-fn #'alexandria:make-keyword)))
;; 14 secs before updating jzon
;; 0.5 secs after
