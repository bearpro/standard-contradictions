NPM ?= npm
TREE_SITTER ?= npx tree-sitter

.PHONY: install build generate clean

install:
	$(NPM) install

build: generate

generate:
	$(TREE_SITTER) generate

clean:
	rm -rf node_modules
