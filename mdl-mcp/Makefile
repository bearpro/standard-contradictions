.PHONY: mdl-mcp-docker

GIT_SHA := $(shell git rev-parse --short HEAD)
GIT_DIRTY := $(shell test -n "$$(git status --porcelain)" && echo "_dirty")
MDL_MCP_TAG := $(GIT_SHA)$(GIT_DIRTY)

build-mdl-mcp-docker:
	rm -f ./out/mdl-mcp_$(MDL_MCP_TAG).tar.gz
	mkdir -p out
	docker build -f ./Dockerfile -t mdl-mcp:$(MDL_MCP_TAG) ..
	docker save mdl-mcp:$(MDL_MCP_TAG) | gzip > ./out/mdl-mcp_$(MDL_MCP_TAG).tar.gz

push-mdl-mcp-docker:
	docker tag mdl-mcp:$(MDL_MCP_TAG) docker-registry.mprokazin.tech/mdl-mcp:$(MDL_MCP_TAG)
	docker push docker-registry.mprokazin.tech/mdl-mcp:$(MDL_MCP_TAG)
