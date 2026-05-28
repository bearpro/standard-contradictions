PACKAGE_NAME := mprokazin-mdl
UV ?= uv
UV_PYTHON ?= 3.12

.PHONY: install uninstall

install:
	UV_PYTHON=$(UV_PYTHON) $(UV) tool install --editable .

uninstall:
	$(UV) tool uninstall $(PACKAGE_NAME)
