
guix := guix environment -L guix-modules --ad-hoc local-gitlab
# guix-shell := guix shell -L guix-modules -f guix.scm -e 'local-gitlab' --rebuild-cache

.PHONY: run-dev
run-dev:
	# Assuming everything is loaded by direnv
	@# See .envrc and guix.scm
	sbcl \
		--eval '(asdf:load-asd (truename "local-gitlab.asd"))' \
		--eval "(mapc #'asdf:load-system '(#:local-gitlab #:swank))" \
		--eval "(swank:create-server :port (find-port:find-port :min 4005) :dont-close t)" \
		--eval "(local-gitlab:serve)"

.PHONY: run
run:
	${guix} -- local-gitlab

.PHONY: shell
# Start a new shell with the binary "local-gitlab" available
shell:
	${guix}

.PHONY: build
build:
	guix build -L guix-modules local-gitlab

.PHONY: install
install:
	guix install -L guix-modules local-gitlab
