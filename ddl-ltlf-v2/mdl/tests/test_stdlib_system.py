from __future__ import annotations

import importlib.util
from pathlib import Path
from typing import Protocol, cast

import mdl


class SystemModule(Protocol):
    STRING_LIST: mdl.SumType

    def char_at(self, value: mdl.Term, index: mdl.Term | int) -> mdl.Term: ...

    def nil(self, list_type: mdl.SumType) -> mdl.Term: ...

    def cons(self, head: mdl.Term, tail: mdl.Term) -> mdl.Term: ...

    def string_to_list(self, value: mdl.Term) -> mdl.Term: ...

    def count_string(self, xs: mdl.Term, value: mdl.Term) -> mdl.Term: ...


def _load_system_module():
    path = Path(__file__).resolve().parents[2] / "stdlib" / "system.mdl.py"
    spec = importlib.util.spec_from_file_location("system_mdl", path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _system() -> SystemModule:
    system = _load_system_module()
    module = cast(object, system.MODULE)
    assert isinstance(module, mdl.Module)
    assert cast(object, system.SYSTEM) is module
    return cast(SystemModule, cast(object, module))


def test_char_at_is_native_string_indexing() -> None:
    system = _system()

    with mdl.ModuleBuilder() as doc:
        doc.rule = mdl.Rule(
            "O",
            None,
            mdl.Eq(system.char_at(mdl.String("ab"), 1), mdl.String("b")),
        )

    assert mdl.solve(doc.build()).is_consistent


def test_string_to_list_is_defined_in_stdlib() -> None:
    system = _system()

    with mdl.ModuleBuilder() as doc:
        doc.expected = system.cons(
            mdl.String("a"),
            system.cons(mdl.String("b"), system.nil(system.STRING_LIST)),
        )
        doc.rule = mdl.Rule(
            "O",
            None,
            mdl.Eq(system.string_to_list(mdl.String("ab")), doc.expected),
        )

    assert mdl.solve(doc.build()).is_consistent


def test_stdlib_can_count_string_list_items() -> None:
    system = _system()

    with mdl.ModuleBuilder() as doc:
        doc.chars = system.string_to_list(mdl.String("test@example.com"))
        doc.rule = mdl.Rule(
            "O",
            None,
            mdl.Eq(system.count_string(doc.chars, mdl.String("@")), mdl.Int(1)),
        )

    assert mdl.solve(doc.build()).is_consistent
