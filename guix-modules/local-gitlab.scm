(define-module (local-gitlab))

(use-modules ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages lisp-check)
             (guix packages)
             (guix gexp)
             (guix git)
             (guix git-download)
             (guix build-system asdf)
             (guix build-system trivial))

(define-public sbcl-jzon-newer
  (let ((commit "ba43faa1f2a07e83226d0e52b29cf6a816e3596d"))
    (package
     (inherit sbcl-jzon)
     (version (git-version "1.0.0" "0" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Zulu-Inuoe/jzon")
                    (commit commit)))
              (file-name (git-file-name (package-name sbcl-jzon) version))
              (sha256 (base32 "0msqjjbaxah5hwm8crap2wgmkfgzszrq213djphwr0kggm4ass1i")))))))

(define-public sbcl-simpbin
  (let ((commit "6f9f1c196ca8f363b478bab0a8623f53b89e5586")
        (repo-url "https://github.com/fstamour/simpbin"))
    (package
     (name "sbcl-simpbin")
     (version (git-version "0.0.1" "0" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url repo-url)
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 "1q768amvmjnmasy9kz3qik2p4inxc68a6ih3m51cm85zn1hr9c0l"))))
     (build-system asdf-build-system/sbcl)
     (inputs (list sbcl-alexandria sbcl-flexi-streams sbcl-fast-io sbcl-nibbles))
     (native-inputs (list sbcl-parachute))
     (home-page repo-url)
     (synopsis "A common lisp library to store data in a simple binary format")
     (description "")
     (license license:gpl3))))

(define-public sbcl-adopt
  (let ((commit "8c7d6ea1e74ae8ea0938945cc3b5b8bfe413ace1"))
    (package
     (name "sbcl-adopt")
     (version (git-version "1.2.0" "0" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sjl/adopt")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 "1s629i2ybqkd4grl8myzkmx88z0jaryqylq8z02j7gnys36sm5vf"))))
     (build-system asdf-build-system/sbcl)
     (inputs (list sbcl-split-sequence sbcl-bobbin))
     (native-inputs (list sbcl-1am))
     (home-page "https://docs.stevelosh.com/adopt/")
     (synopsis "A Damn OPTion parsing library.")
     (description "Adopt is a simple UNIX-style option parser in Common Lisp, heavily influenced by Python's optparse and argparse.")
     (license license:expat)))) ; MIT license

(define-public sbcl-local-gitlab
  (package
   (name "sbcl-local-gitlab")
   (version "0.0.1")
   (source (local-file "../" "local-gitlab" #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs (list sbcl-adopt
                 sbcl-drakma
                 sbcl-log4cl
                 sbcl-hunchentoot
                 sbcl-find-port
                 sbcl-kebab
                 sbcl-cl-str
                 sbcl-local-time
                 sbcl-cl-cron
                 sbcl-jzon-newer
                 sbcl-simpbin))
   (home-page "https://github.com/fstamour/local-gitlab")
   (synopsis "")
   (description "Caching gitlab issues and more locally, for bazingly fast search")
   (license license:expat)))

(define-public local-gitlab
  (package
   (inherit sbcl-local-gitlab)
   (name "local-gitlab")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    (list #:builder
          (let ((local-gitlab-build-script
                 (scheme-file "build-local-gitlab.lisp"
                              #~((require :asdf)
                                 (asdf:load-system '#:local-gitlab)
                                 (uiop/image:dump-image "local-gitlab" :executable t))
                              #:splice? #t)))
            (with-imported-modules '((guix build utils))
                                   #~(let ((bin (string-append #$output "/bin")))
                                       (use-modules (guix build utils))
                                       (setenv "XDG_CONFIG_DIRS" #$(file-append (this-package-input "sbcl-local-gitlab") "/etc"))
                                       (mkdir-p bin)
                                       (chdir bin)
                                       (system* #$(file-append sbcl "/bin/sbcl")
                                                "--load"
                                                #$local-gitlab-build-script))))))
   (inputs (list sbcl-local-gitlab))))
