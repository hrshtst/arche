ROOTDIR    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*)
EXCLUSIONS := .git .gitmodules
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.DEFAULT_GOAL := help

all:

list: ## Show dot files to be deployed
	@$(foreach file, $(DOTFILES), /bin/ls -dF $(file);)

deploy: ## Create symbolic links to home directory
	@echo "Creating symbolic links..."
	@echo
	@$(foreach file, $(DOTFILES), echo ln -sfnv $(abspath $(file)) $(HOME)/$(file);)

init: ## Setup environment settings
#	@ROOTDIR=$(ROOTDIR) echo "ROOTDIR: "$(ROOTDIR)
	@echo "ROOTDIR: "$(ROOTDIR)
	@#echo "ROOTDIR: "$(ROOTDIR)
#	@bash $(ROOTDIR)/etc/init/init.sh

update: ## Update
	@echo "Update"
#	git pull origin master
#	git submodule init
#	git submodule update
#	git submodule foreach git pull origin master

install: update deploy init ## Run make update, deploy init
	@exec $$SHELL

clean: ## Remove dot files
	@echo "Remove dot files in home directory..."
#	@-$(foreach file, $(DOTFILES), rm -vrf $(HOME)/$(file);)


help: ## Show this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

