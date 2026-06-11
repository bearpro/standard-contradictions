.PHONY: generate-antlr install uninstall

generate-antlr:
	mkdir -p src/mdl/_antlr
	antlr4 -Dlanguage=Python3 -visitor -no-listener -o src/mdl/_antlr -Xexact-output-dir src/mdl/grammar/MDL.g4

install:
	uv tool install --editable '.[full]'

uninstall:
	uv tool uninstall mprokazin-mdl
