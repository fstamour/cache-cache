#|
I have to clone a bunch of projects, going through GitLab's UI to get
each git repo's URL is annoying.
|#

(in-package #:cache-cache)

;; How do I want to name the directory for each repo?
(defun directory-names-from-git-repo-url (url)
  (string-downcase
   (subseq url
           (1+ (position #\/ url :from-end t))
           (position #\. url :from-end t))))

;; Find all the projects
(defparameter *p*
  (loop :for project-name :in '(<REDACTED LIST OF PROJECT NAMES>)
        :collect
        (append (list :needle project-name)
                (loop :for project :in (find-projects project-name)
                      ;; Search only in this group
                      :when (search "<REDACTED GROUP NAME>" (item-name-with-namespace project))
                        :append (list :id (item-id project)
                                      :path (item-name-with-namespace project)
                                      :name (item-name project)
                                      :git (gethash "ssh_url_to_repo" project))))))

;; Build the list of "git clone" commands to run
(with-output-to-string (*standard-output*)
  (loop for project in *p*
        do (format t "~&git clone --recursive ~a ~a"
                   (getf project :git)
                   (directory-names-from-git-repo-url (getf project :git)))))
