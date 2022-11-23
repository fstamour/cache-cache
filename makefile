
guix-shell := guix shell -f guix.scm --rebuild-cache

run:
	${guix-shell} -- local-gitlab

shell:
	${guix-shell}
