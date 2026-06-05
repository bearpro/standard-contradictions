PACKAGE_NAME := mprokazin-mdl
UV ?= uv
UV_PYTHON ?= 3.12

.PHONY: generate-antlr install uninstall

generate-antlr:
	mkdir -p src/mdl/_antlr
	antlr4 -Dlanguage=Python3 -visitor -no-listener -o src/mdl/_antlr -Xexact-output-dir src/mdl/grammar/MDL.g4

install:
	UV_PYTHON=$(UV_PYTHON) $(UV) tool install --editable .

uninstall:
	$(UV) tool uninstall $(PACKAGE_NAME)
