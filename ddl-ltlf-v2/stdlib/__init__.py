from __future__ import annotations

from typing import Protocol, cast

import mdl


class SystemModule(Protocol):
    STRING_LIST: mdl.SumType

    def char_at(self, value: mdl.Term, index: mdl.Term | int) -> mdl.Term: ...

    def nil(self, list_type: mdl.SumType) -> mdl.Term: ...

    def cons(self, head: mdl.Term, tail: mdl.Term) -> mdl.Term: ...

    def string_to_list(self, value: mdl.Term) -> mdl.Term: ...

    def count_string(self, xs: mdl.Term, value: mdl.Term) -> mdl.Term: ...


def _system() -> SystemModule:
    with mdl.ModuleBuilder() as doc:
        doc.std = doc.import_mdl("./system.mdl.py")

    module = doc.build()
    system_module = module.std
    assert isinstance(system_module, mdl.Module)
    return cast(SystemModule, cast(object, system_module))

system = _system()
