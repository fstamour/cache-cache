(in-package #:cache-cache)

(defun ensure/ (x)
  (if (and (stringp x)
           (char= (char x (1- (length x))) #\/))
      x
      (format nil "~a/" x)))

;; TODO don't hardcode the *base-uri*
(defun make-uri (&rest part-list)
  (loop :for uri = (puri:uri *base-uri*) :then (puri:merge-uris part uri)
        :for part :in (remove-if #'null part-list)
        :finally (return uri)))

(defun format-query (&rest plist)
  (when plist
    (format nil
            "?~{~a=~a~^&~}"
            (mapcar
             (lambda (x)
               (cond
                 ((eq t x) "true")
                 ((eq t x) "false")
                 ((keywordp x)
                  (kebab:to-snake-case (symbol-name x)))
                 (t x)))
             plist))))

(defun format-query* (plist)
  (apply #'format-query plist))

#++
(make-uri
 "groups/"
 (ensure/ *root-group-id*)
 "projects"
 (format-query
  :per-page 5
  :include-subgroups t
  :order-by :updated-at))
