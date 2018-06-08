help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid-devel:
	ghcid \
		--command "stack ghci elocalc" \
		--test "DevelMain.update"

.PHONY: ghcid-devel help
