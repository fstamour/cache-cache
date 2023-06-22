
guix := guix environment -L guix-modules --ad-hoc local-gitlab
# guix-shell := guix shell -L guix-modules -f guix.scm -e 'local-gitlab' --rebuild-cache

.PHONY: run-dev
run-dev:
	# Assuming everything is loaded by direnv
	@# See .envrc and guix.scm
	sbcl \
		--no-userinit \
		--eval "(require 'asdf)" \
		--eval '(asdf:load-asd (truename "local-gitlab.asd"))' \
		--eval "(mapc #'asdf:load-system '(#:local-gitlab #:swank))" \
		--eval "(swank:create-server :port (find-port:find-port :min 4005) :dont-close t)" \
		--eval "(local-gitlab:serve)"


.PHONY: help
help:
	@echo 'Default target is   run-dev'
	@echo ''
	@echo 'Available targets:'
	@echo '  build 	Build using guix'
	@echo '  help  	Show this help.'
	@echo '  install	Build and install using guix'
	@echo '  run   	Build using guix and run'
	@echo '  run-dev	Load the system in sbcl, start a swank server and the web server.'
	@echo '  shell 	Build using guix and open a shell'


.PHONY: run
run:
	${guix} -- local-gitlab

.PHONY: shell
# Start a new shell with the binary "local-gitlab" available
shell:
	${guix}

.PHONY: build
build:
	rm -f ./guix-results
	guix build -L guix-modules --root=./guix-results.tmp local-gitlab
	mv ./guix-results.tmp ./guix-results

.PHONY: install
install:
	guix install -L guix-modules local-gitlab
