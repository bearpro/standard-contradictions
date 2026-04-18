from __future__ import annotations

import mdl


def test_module_cannot_be_created_directly() -> None:
    try:
        mdl.Module({})
    except TypeError as error:
        assert "ModuleBuilder" in str(error)
    else:
        raise AssertionError("Module direct construction should fail.")


def test_module_build_validates_variable_references() -> None:
    value = mdl.Var("value", mdl.INT)

    with mdl.ModuleBuilder() as doc:
        doc.rule = mdl.Rule("uses-undeclared-variable", "O", None, value >= 1)

    try:
        doc.build()
    except NameError as error:
        assert "value" in str(error)
    else:
        raise AssertionError("Build should reject undeclared variables.")


def test_module_build_accepts_declared_variables() -> None:
    value = mdl.Var("value", mdl.INT)

    with mdl.ModuleBuilder() as doc:
        doc.value = value
        doc.rule = mdl.Rule("uses-declared-variable", "O", None, value >= 1)

    assert isinstance(doc.build(), mdl.Module)
