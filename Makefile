.PHONY: init run lint
.DEFAULT_GOAL := help

NAMESPACE := jads-master-team-1
NAME := sbm

help: ## Show this help
	@echo "${NAMESPACE}/${NAME}"
	@echo
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

##

init: ## Initialize the environment
	Rscript -e "renv::restore()"

##

run: ## Run the script
	@echo "Error: Not Implemented"

##

lint: ## Run lint
	Rscript -e "lintr::lint_dir('src')"
