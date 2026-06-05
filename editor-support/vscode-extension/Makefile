VSCODE_EXTENSION_OUT := dist
VSCODE_EXTENSION_VSIX := $(VSCODE_EXTENSION_OUT)/mdl-language-support-0.1.1.vsix

.PHONY: build install clean

build:
	mkdir -p $(VSCODE_EXTENSION_OUT)
	npm ci
	npm run package -- --out $(VSCODE_EXTENSION_VSIX)

install: build
	code --install-extension ./dist/mdl-language-support-0.1.1.vsix

clean:
	rm -rf $(VSCODE_EXTENSION_OUT) node_modules
