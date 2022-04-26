(in-package #:local-gitlab)

;; TODO This should be in appdata or xdg
(defvar *root*
  (merge-pathnames "assets/"
                   (asdf:system-source-directory :local-gitlab)))

(defvar *token* nil
  "The token used to authenticate with GitLab.")

(defvar *projects* nil)

(defvar *issues* nil)

;; (when *issues* (hash-table-count *issues*))

(defvar *server*
  (let ((address "127.0.0.1"))
    (make-instance
     'h:easy-acceptor
     :address address
     :port (find-port:find-port :interface address)))
  "The server instance")
