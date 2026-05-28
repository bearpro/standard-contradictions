VSCODE_EXTENSION_DIR := grammars/vscode
VSCODE_EXTENSION_OUT := dist
VSCODE_EXTENSION_VSIX := $(VSCODE_EXTENSION_OUT)/mdl-language-support-0.1.0.vsix
PACKAGE_NAME := mdl-ddl-ltlf
UV ?= uv
UV_PYTHON ?= 3.12

.PHONY: install uninstall build-vscode-extension clean-vscode-extension

install:
	UV_PYTHON=$(UV_PYTHON) $(UV) tool install --editable .

uninstall:
	$(UV) tool uninstall $(PACKAGE_NAME)

build-vscode-extension:
	mkdir -p $(VSCODE_EXTENSION_OUT)
	npm --prefix $(VSCODE_EXTENSION_DIR) ci
	npm --prefix $(VSCODE_EXTENSION_DIR) run package -- --out ../../$(VSCODE_EXTENSION_VSIX)

install-vscode-extension: build-vscode-extension
	code  --install-extension ./dist/mdl-language-support-0.1.0.vsix

clean-vscode-extension:
	rm -rf $(VSCODE_EXTENSION_OUT) $(VSCODE_EXTENSION_DIR)/node_modules
