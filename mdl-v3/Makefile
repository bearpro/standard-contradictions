VSCODE_EXTENSION_DIR := grammars/vscode
VSCODE_EXTENSION_OUT := dist
VSCODE_EXTENSION_VSIX := $(VSCODE_EXTENSION_OUT)/mdl-language-support-0.1.0.vsix

.PHONY: build-vscode-extension clean-vscode-extension

build-vscode-extension:
	mkdir -p $(VSCODE_EXTENSION_OUT)
	npm --prefix $(VSCODE_EXTENSION_DIR) ci
	npm --prefix $(VSCODE_EXTENSION_DIR) run package -- --out ../../$(VSCODE_EXTENSION_VSIX)

clean-vscode-extension:
	rm -rf $(VSCODE_EXTENSION_OUT) $(VSCODE_EXTENSION_DIR)/node_modules
