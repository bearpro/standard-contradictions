from types import TracebackType
from typing import Self, override

from .model import Module

class ModuleBuilder:
    _objects: dict[str, object] | None

    def __init__(self):
        self._objects = None

    @override
    def __setattr__(self, name: str, value: object) -> None:
        if name == "_objects":
            object.__setattr__(self, name, value)
            return
        objects = self._objects
        if objects is None or name.startswith("_"):
            object.__setattr__(self, name, value)
            return
        objects[name] = value

    def __getattr__(self, name: str) -> object:
        objects = self._objects
        if objects is None:
            raise AttributeError(name)
        try:
            return objects[name]
        except KeyError:
            raise AttributeError(name)

    def __enter__(self) -> Self:
        object.__setattr__(self, "_objects", {})
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        pass

    def build(self) -> Module:
        objects = self._objects
        return Module.from_builder(dict(objects or {}))
