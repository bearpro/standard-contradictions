from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import Any

from . import ast as A
from .printer import PrettyPrinter, format_expr

TEMPORAL_OPS = {
    "always": "G",
    "eventually": "F",
    "next": "X",
    "weak_next": "WX",
    "until": "U",
    "release": "R",
    "weak_until": "W",
}

BOOL_OPS = {
    "and": "and",
    "or": "or",
}


@dataclass
class CoreTranslator:
    """Translate MDL AST into a compact JSON-like DDL-LTLf core.

    The core intentionally remains backend-agnostic. It preserves traceability and keeps lifted
    atoms as strings plus metadata so a later solver-specific backend can choose its own symbol table.
    """

    printer: PrettyPrinter = field(default_factory=PrettyPrinter)
    atoms: dict[str, dict[str, Any]] = field(default_factory=dict)

    def translate(self, module: A.Module) -> dict[str, Any]:
        self.atoms.clear()
        core: dict[str, Any] = {
            "language": "MDL-DDL-LTLf-Core",
            "version": "0.1",
            "module": module.name,
            "annotations": module.annotations,
            "imports": [self.import_decl(i) for i in module.imports],
            "opens": [self.open_decl(o) for o in module.opens],
            "types": [],
            "values": [],
            "entities": [],
            "events": [],
            "rules": [],
            "priorities": [],
            "facts": [],
            "asserts": [],
            "alignments": [],
            "atoms": {},
        }
        for decl in module.declarations:
            if isinstance(decl, A.TypeDecl):
                core["types"].append({
                    "name": decl.name,
                    "definition": self.type_definition(decl.definition),
                    "annotations": decl.annotations,
                })
            elif isinstance(decl, A.ValueDecl):
                core["values"].append({
                    "name": decl.name,
                    "type": self.type_expr(decl.type_annotation),
                    "value": self.term(decl.value),
                    "annotations": decl.annotations,
                })
            elif isinstance(decl, A.FuncDecl):
                # Function bodies are intentionally not compiled here. Calls returning bool are lifted as atoms.
                core["values"].append({
                    "kind": "function",
                    "name": decl.name,
                    "params": [self.param(p) for p in decl.params],
                    "return_type": self.type_expr(decl.return_type),
                    "annotations": decl.annotations,
                })
            elif isinstance(decl, A.EntityDecl):
                core["entities"].append({
                    "name": decl.name,
                    "type": self.type_expr(decl.type_annotation),
                    "annotations": decl.annotations,
                })
            elif isinstance(decl, A.EventDecl):
                core["events"].append({
                    "name": decl.name,
                    "fields": [(name, self.type_expr(typ)) for name, typ in decl.fields],
                    "annotations": decl.annotations,
                })
            elif isinstance(decl, A.RuleDecl):
                core["rules"].append(self.rule(decl))
            elif isinstance(decl, A.PriorityDecl):
                core["priorities"].append({"chain": decl.chain, "annotations": decl.annotations})
            elif isinstance(decl, A.FactDecl):
                core["facts"].append({"target": decl.target, "value": self.term(decl.value), "annotations": decl.annotations})
            elif isinstance(decl, A.AssertDecl):
                core["asserts"].append({"formula": self.temporal(decl.expr), "annotations": decl.annotations})
            elif isinstance(decl, A.AlignDecl):
                core["alignments"].append({
                    "subject": decl.subject,
                    "target": decl.target,
                    "kind": decl.kind,
                    "annotations": decl.annotations,
                })
        core["atoms"] = self.atoms
        return core

    def import_decl(self, imp: A.ImportDecl) -> dict[str, Any]:
        return {"path": imp.path, "annotations": imp.annotations}

    def open_decl(self, opened: A.OpenDecl) -> dict[str, Any]:
        return {"module": opened.module, "annotations": opened.annotations}

    def param(self, p: A.Param) -> dict[str, Any]:
        return {"pattern": self.printer.pattern(p.pattern), "type": self.type_expr(p.type_annotation)}

    def rule(self, rule: A.RuleDecl) -> dict[str, Any]:
        return {
            "name": rule.name,
            "strength": rule.strength,
            "modality": rule.modality,
            "antecedent": self.temporal(rule.antecedent) if rule.antecedent is not None else None,
            "body": self.temporal(rule.body),
            "otherwise": self.temporal(rule.otherwise) if rule.otherwise is not None else None,
            "annotations": rule.annotations,
            "anonymous": rule.anonymous,
            "source_span": {"line": rule.line, "column": rule.column},
        }

    def temporal(self, expr: A.Expr | None) -> dict[str, Any]:
        if expr is None:
            return {"op": "true"}
        if isinstance(expr, A.TemporalUnary):
            if expr.op == "never":
                return {"op": "not", "arg": {"op": "F", "arg": self.temporal(expr.operand)}}
            return {"op": TEMPORAL_OPS.get(expr.op, expr.op), "arg": self.temporal(expr.operand)}
        if isinstance(expr, A.TemporalBinary):
            return {"op": TEMPORAL_OPS.get(expr.op, expr.op), "left": self.temporal(expr.left), "right": self.temporal(expr.right)}
        if isinstance(expr, A.UnaryOp) and expr.op == "not":
            return {"op": "not", "arg": self.temporal(expr.operand)}
        if isinstance(expr, A.BinaryOp) and expr.op in BOOL_OPS:
            return {"op": BOOL_OPS[expr.op], "left": self.temporal(expr.left), "right": self.temporal(expr.right)}
        if isinstance(expr, A.Literal) and expr.kind == "bool":
            return {"op": "true" if expr.value else "false"}
        if isinstance(expr, A.Name) and expr.name == "last":
            return {"op": "last"}
        return self.atom(expr)

    def atom(self, expr: A.Expr) -> dict[str, Any]:
        text = format_expr(expr)
        symbol = self.symbol_for(text)
        self.atoms.setdefault(symbol, {
            "text": text,
            "source_span": {"line": expr.line, "column": expr.column},
        })
        return {"op": "atom", "symbol": symbol}

    def symbol_for(self, text: str) -> str:
        cleaned = []
        for ch in text:
            cleaned.append(ch if ch.isalnum() else "_")
        symbol = "p_" + "".join(cleaned).strip("_")
        while "__" in symbol:
            symbol = symbol.replace("__", "_")
        if symbol == "p_":
            symbol = "p_atom"
        base = symbol
        i = 2
        existing = {meta["text"]: sym for sym, meta in self.atoms.items()}
        if text in existing:
            return existing[text]
        while symbol in self.atoms:
            symbol = f"{base}_{i}"
            i += 1
        return symbol

    def term(self, expr: A.Expr | None) -> dict[str, Any] | None:
        if expr is None:
            return None
        if isinstance(expr, A.Literal):
            value = str(expr.value) if expr.kind == "rat" else expr.value
            return {"kind": "literal", "literal_type": expr.kind, "value": value}
        if isinstance(expr, A.Name):
            return {"kind": "name", "name": expr.name}
        if isinstance(expr, A.Call):
            return {"kind": "call", "func": self.term(expr.func), "args": [self.term(a) for a in expr.args]}
        if isinstance(expr, A.FieldAccess):
            return {"kind": "field", "target": self.term(expr.target), "field": expr.field}
        if isinstance(expr, A.BinaryOp):
            return {"kind": "binary", "op": expr.op, "left": self.term(expr.left), "right": self.term(expr.right)}
        if isinstance(expr, A.UnaryOp):
            return {"kind": "unary", "op": expr.op, "operand": self.term(expr.operand)}
        if isinstance(expr, A.IfExpr):
            return {"kind": "if", "condition": self.term(expr.condition), "then": self.term(expr.then_branch), "else": self.term(expr.else_branch)}
        if isinstance(expr, A.RecordConstructor):
            return {"kind": "record", "type": expr.type_name, "fields": {k: self.term(v) for k, v in expr.fields}}
        if isinstance(expr, A.TupleLiteral):
            return {"kind": "tuple", "items": [self.term(i) for i in expr.items]}
        return {"kind": "expr", "text": format_expr(expr)}

    def type_definition(self, typ: A.TypeExpr | A.SumType | None) -> Any:
        if isinstance(typ, A.SumType):
            return {"kind": "sum", "variants": [
                {"name": v.name, "fields": [(label, self.type_expr(t)) for label, t in v.fields]}
                for v in typ.variants
            ]}
        return self.type_expr(typ)

    def type_expr(self, typ: A.TypeExpr | None) -> Any:
        if typ is None:
            return None
        if isinstance(typ, A.TypeRef):
            return {"kind": "type_ref", "name": typ.name, "args": [self.type_expr(a) for a in typ.args]}
        if isinstance(typ, A.RecordType):
            return {"kind": "record_type", "fields": [(name, self.type_expr(t)) for name, t in typ.fields]}
        if isinstance(typ, A.TupleType):
            return {"kind": "tuple_type", "items": [self.type_expr(i) for i in typ.items]}
        return {"kind": "type", "text": str(typ)}


def translate(module: A.Module) -> dict[str, Any]:
    return CoreTranslator().translate(module)


def to_json(module: A.Module, *, indent: int = 2) -> str:
    return json.dumps(translate(module), ensure_ascii=False, indent=indent, default=str)
