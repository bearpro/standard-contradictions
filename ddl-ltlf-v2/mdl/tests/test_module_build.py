from __future__ import annotations

import mdl


def test_module_cannot_be_created_directly() -> None:
    try:
        _ = mdl.Module({})
    except TypeError as error:
        assert "ModuleBuilder" in str(error)
    else:
        raise AssertionError("Module direct construction should fail.")


def test_module_build_validates_variable_references() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.rule = mdl.Rule("O", None, mdl.Var(mdl.INT) >= 1)

    try:
        _ = doc.build()
    except NameError as error:
        assert "Variable" in str(error)
    else:
        raise AssertionError("Build should reject undeclared variables.")


def test_module_build_accepts_declared_variables() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.value = mdl.Var(mdl.INT)
        doc.rule = mdl.Rule("O", None, doc.value >= 1)

    assert isinstance(doc.build(), mdl.Module)


def test_module_build_binds_unnamed_objects_from_builder_attributes() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.value = mdl.Var(mdl.INT)
        doc.ready = mdl.Proposition()
        doc.rule = mdl.Rule("O", None, doc.ready & (doc.value >= 1))

    module = doc.build()

    assert isinstance(module.value, mdl.Variable)
    assert isinstance(module.ready, mdl.Proposition)
    assert isinstance(module.rule, mdl.Rule)
    assert module.value.name == "value"
    assert module.ready.name == "ready"
    assert module.rule.source == "rule"
    assert mdl.solve(module).is_consistent


def test_module_build_binds_unnamed_product_types_from_builder_attributes() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.Person = mdl.ProductType({"age": mdl.INT, "name": mdl.STRING})
        doc.person = mdl.Var(doc.Person)
        doc.rule = mdl.Rule("O", None, doc.person.field("age") >= 18)

    module = doc.build()

    assert isinstance(module.Person, mdl.ProductType)
    assert isinstance(module.person, mdl.Variable)
    assert module.Person.name == "Person"
    assert module.person.name == "person"
    assert module.person.type.name == "Person"


def test_module_build_binds_unnamed_functions_from_builder_attributes() -> None:
    def increment_body(value: mdl.Term) -> mdl.Term:
        return value + 1

    with mdl.ModuleBuilder() as doc:
        doc.increment = mdl.Function({"value": mdl.INT}, mdl.INT, increment_body)
        doc.value = mdl.Var(mdl.INT)
        doc.rule = mdl.Rule("O", None, doc.increment(doc.value) >= 1)

    module = doc.build()

    assert isinstance(module.increment, mdl.Function)
    assert module.increment.name == "increment"
    assert mdl.solve(module).is_consistent
