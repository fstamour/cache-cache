(in-package #:local-gitlab)

(defun ensure/ (x)
  (if (and (stringp x)
           (char= (char x (1- (length x))) #\/))
      x
      (format nil "~a/" x)))

(defun make-uri (&rest part-list)
  (loop :for uri = (puri:uri *base-uri*) :then (puri:merge-uris part uri)
        :for part :in part-list
        :finally (return uri)))

(defun format-query (plist)
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
           plist)))

#++
(make-uri
 "groups/"
 (ensure/ *root-group-id*)
 "projects"
 (format-query
  '(:per-page 5
    :include-subgroups t
    :order-by :updated-at)))
