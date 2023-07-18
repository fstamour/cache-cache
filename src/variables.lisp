(in-package #:cache-cache)

(defvar *root*
  (or
   ;; For development
   (probe-file (merge-pathnames "assets/"))
   ;; Pretty sure this doesn't works well once compiled...
   (merge-pathnames "assets/"
                    (asdf:system-source-directory :cache-cache))))

(defvar *token* nil
  "The token used to authenticate with GitLab.")

(defvar *projects* nil)

(defvar *issues* nil)

;; (when *issues* (hash-table-count *issues*))

(defvar *server* nil "The server instance")
