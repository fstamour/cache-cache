(in-package #:cache-cache)


;;; URI manipulations

(defun ensure/ (x)
  (if (and (stringp x)
           (char= (char x (1- (length x))) #\/))
      x
      (format nil "~a/" x)))

(defun make-uri (base &rest part-list)
  (loop :for uri = (puri:uri (ensure/ base))
          :then (puri:merge-uris (etypecase part
                                   (string part)
                                   (number (prin1-to-string part)))
                                 uri)
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


;;; Environment variables

(defun try-get-env (names &key (validatep t))
  "Try to get an environment variable's value."
  (or
   (loop :for name :in names
         :for value = (uiop:getenv name)
         :when (and value
                    (or (not validatep)
                        (str:non-blank-string-p value)))
           :do
              (return value))
   (when (eq :error validatep)
     (error "None of these environment variables are set and not empty:~&~{  - ~s~&~}"
            names))))

#++ (try-get-env '("GITLAB_TOKEN"))
#++ (try-get-env '("GITLAB_TOKEN" "GITLAB_PRIVATE_TOKEN" "PATH"))



;;; sequence to hash-table

(defmethod id ((object hash-table))
  "Return the OBJECT's \"id\" property."
  (gethash "id" object))

(defun by (sequence-of-hash-table &key (key #'id) destination)
  "Convert a sequence of items to a map of key -> item."
  (let ((result (or destination (make-hash-table :test 'equal))))
    (map nil
         #'(lambda (item &aux (key (funcall key item)))
             (setf (gethash key result) item))
         sequence-of-hash-table)
    result))

(defun by-id (sequence-of-hash-table &optional destination)
  "Convert a sequence of items to a map of id -> item."
  (by sequence-of-hash-table :destination destination))
