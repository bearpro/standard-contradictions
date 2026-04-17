from typing import Any
from .model import Module

class ModuleBuilder:
    def __init__(self):
        self._objects: Any = None

    def __setattr__(self, name, value):
        self._objects[name] = value

    def __getattr__(self, name):
        try:
            return self._objects[name]
        except KeyError:
            raise AttributeError(name)
        
    def __enter__(self):
        self._objects = {}
        return self

    def __exit__(self, exc_type, exc, tb):
        pass

    def build(self):
        return Module(self._objects)