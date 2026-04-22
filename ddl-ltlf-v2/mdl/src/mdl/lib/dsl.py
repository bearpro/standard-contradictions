import importlib.util
import inspect
from pathlib import Path
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

    def import_mdl(self, relative_path: str) -> Module:
        path = Path(relative_path)
        if path.is_absolute():
            raise ValueError("ModuleBuilder.import_mdl only accepts relative paths.")

        frame = inspect.currentframe()
        caller = None if frame is None else frame.f_back
        if caller is None:
            raise RuntimeError("Cannot resolve module import caller.")
        caller_file = Path(caller.f_code.co_filename).resolve()
        module_path = (caller_file.parent / path).resolve()

        spec = importlib.util.spec_from_file_location(
            f"mdl_import_{abs(hash(module_path))}",
            module_path,
        )
        if spec is None or spec.loader is None:
            raise ImportError(f"Cannot load MDL module from {module_path}.")

        py_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(py_module)

        module = getattr(py_module, "MODULE", None)
        if not isinstance(module, Module):
            raise TypeError(f"MDL module {module_path} must export MODULE.")
        return module

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
