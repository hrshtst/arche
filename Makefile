.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: link
link: ## Symlink dotfiles into home directory
	@scripts/symlink-dotfiles.bash

.PHONY: compile
compile: ## Byte-compile arche.el
	@scripts/byte-compile.bash

.PHONY: clean
clean: ## Remove build artifacts
	@rm -f emacs/arche.elc
