(in-package #:local-gitlab)

(defvar *root*
  (merge-pathnames "assets/"
                   (asdf:system-source-directory :local-gitlab)))

(defvar *token* nil
  "The token used to authenticate with GitLab.")

(defvar *projects* nil)

(defvar *issues* nil)

;; (when *issues* (hash-table-count *issues*))

;; TODO This should be run much later, especially the find-port
(defvar *server*
  (let ((address "127.0.0.1"))
    (make-instance
     'h:easy-acceptor
     :address address
     :port (find-port:find-port :interface address)))
  "The server instance")
