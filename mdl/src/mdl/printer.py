from __future__ import annotations

from fractions import Fraction

from . import ast as A


class PrettyPrinter:
    def __init__(self, indent: str = "    "):
        self.indent = indent

    PREC_LOWEST = 0
    BOOL_CHAIN_OPS = {"and", "or", "implies"}
    PREC_BINARY = {
        "implies": (2, "right"),
        "or": (3, "left"),
        "and": (4, "left"),
        "until": (5, "left"),
        "=": (6, "left"), "!=": (6, "left"),
        "<": (6, "left"), "<=": (6, "left"), ">": (6, "left"), ">=": (6, "left"),
        "+": (7, "left"), "-": (7, "left"),
        "*": (8, "left"), "/": (8, "left"), "%": (8, "left"),
    }
    PREC_PREFIX = 9
    PREC_POSTFIX_TEMPORAL = 10
    PREC_POSTFIX = 11
    PREC_RECORD_CONSTRUCTOR = 12
    PREC_ATOM = 13

    def module(self, module: A.Module) -> str:
        parts: list[str] = []
        parts.extend(self.annotations(module.annotations))
        parts.append(f"module {module.name}")
        if module.imports:
            parts.append("")
            for imp in module.imports:
                parts.append(self.import_decl(imp))
        if module.opens:
            if not module.imports:
                parts.append("")
            for opened in module.opens:
                parts.append(self.open_decl(opened))
        if module.declarations:
            parts.append("")
            for idx, decl in enumerate(module.declarations):
                if idx:
                    parts.append("")
                parts.append(self.declaration(decl))
        return "\n".join(parts).rstrip() + "\n"

    def annotations(self, annotations: list[str]) -> list[str]:
        lines: list[str] = []
        for ann in annotations:
            if ann.startswith("#"):
                lines.append(ann)
            else:
                lines.append(f"@ {ann}")
        return lines

    def import_decl(self, decl: A.ImportDecl) -> str:
        prefix = "\n".join(self.annotations(decl.annotations))
        path = self.import_path(decl.path)
        text = f"import {path}"
        return f"{prefix}\n{text}" if prefix else text

    def open_decl(self, decl: A.OpenDecl) -> str:
        prefix = "\n".join(self.annotations(decl.annotations))
        text = f"open {decl.module}"
        return f"{prefix}\n{text}" if prefix else text

    def import_path(self, path: str) -> str:
        if path.endswith(".mdl") or "/" in path or "\\" in path:
            return '"' + path.replace("\\", "\\\\").replace('"', '\\"') + '"'
        return path

    def declaration(self, decl: A.Declaration) -> str:
        prefix = "\n".join(self.annotations(decl.annotations))
        if isinstance(decl, A.TypeDecl):
            text = self.type_decl(decl)
        elif isinstance(decl, A.FuncDecl):
            text = self.func_decl(decl)
        elif isinstance(decl, A.EntityDecl):
            text = self.entity_decl(decl)
        elif isinstance(decl, A.RuleDecl):
            text = self.rule_decl(decl)
        elif isinstance(decl, A.PriorityDecl):
            text = self.priority_decl(decl)
        elif isinstance(decl, A.FactDecl):
            text = self.fact_decl(decl)
        else:  # pragma: no cover
            text = f"# unsupported declaration: {decl!r}"
        return f"{prefix}\n{text}" if prefix else text

    def type_decl(self, decl: A.TypeDecl) -> str:
        params = f"<{', '.join(decl.params)}>" if decl.params else ""
        return f"type {decl.name}{params} = {self.type_definition(decl.definition)}"

    def type_definition(self, typ: A.TypeExpr | A.SumType | None) -> str:
        if typ is None:
            return "unit"
        if isinstance(typ, A.SumType):
            return " | ".join(self.variant(v) for v in typ.variants)
        return self.type_expr(typ)

    def variant(self, variant: A.Variant) -> str:
        if not variant.fields:
            raise ValueError(f"sum type variant {variant.name!r} has no payload fields")
        fields = []
        for label, typ in variant.fields:
            fields.append(f"{label}: {self.type_expr(typ)}" if label else self.type_expr(typ))
        return f"{variant.name}({', '.join(fields)})"

    def type_expr(self, typ: A.TypeExpr | None) -> str:
        if typ is None:
            return "unit"
        if isinstance(typ, A.TypeRef):
            args = f"<{', '.join(self.type_expr(a) for a in typ.args)}>" if typ.args else ""
            return typ.name + args
        if isinstance(typ, A.RecordType):
            if not typ.fields:
                return "{}"
            return "{ " + ", ".join(f"{name}: {self.type_expr(t)}" for name, t in typ.fields) + " }"
        if isinstance(typ, A.TupleType):
            return "(" + ", ".join(self.type_expr(t) for t in typ.items) + ")"
        return "unit"

    def func_decl(self, decl: A.FuncDecl) -> str:
        params = ", ".join(self.param(p) for p in decl.params)
        tparams = f"<{', '.join(decl.type_params)}>" if decl.type_params else ""
        header = f"func {decl.name}{tparams}({params}) -> {self.type_expr(decl.return_type)}:"
        return header + "\n" + self.block(decl.body, level=1)

    def param(self, param: A.Param) -> str:
        return f"{self.pattern(param.pattern)}: {self.type_expr(param.type_annotation)}"

    def entity_decl(self, decl: A.EntityDecl) -> str:
        return f"entity {decl.name}: {self.type_expr(decl.type_annotation)}"

    def rule_decl(self, decl: A.RuleDecl) -> str:
        strength = "" if decl.strength == "defeasible" else decl.strength + " "
        modality = f"{decl.modality} " if decl.modality else ""
        if decl.anonymous:
            header = f"{strength}rule {modality}".rstrip()
        else:
            header = f"{strength}rule {modality}{decl.name}".rstrip()
        if decl.antecedent is not None:
            header += f" when {self.expr(decl.antecedent)}"
        body = self.expr(decl.body)
        if "\n" in body:
            text = f"{header}:\n{self.indent_text(body, 1)}"
        else:
            text = f"{header}: {body}"
        if decl.otherwise is not None:
            text += f" otherwise {self.expr(decl.otherwise)}"
        return text

    def priority_decl(self, decl: A.PriorityDecl) -> str:
        return f"override {' > '.join(decl.chain)}"

    def fact_decl(self, decl: A.FactDecl) -> str:
        if decl.target:
            return f"fact {decl.target} = {self.expr(decl.value)}"
        return f"fact {self.expr(decl.value)}"

    def block(self, block: A.Block | None, level: int = 1) -> str:
        if block is None:
            return self.indent * level + "()"
        lines: list[str] = []
        for stmt in block.statements:
            ann = f": {self.type_expr(stmt.type_annotation)}" if stmt.type_annotation else ""
            lines.append(self.indent * level + f"let {self.pattern(stmt.pattern)}{ann} = {self.expr(stmt.value)}")
        if block.result is not None:
            result = self.expr(block.result)
            if "\n" in result:
                lines.extend(self.indent * level + line if line else line for line in result.splitlines())
            else:
                lines.append(self.indent * level + result)
        return "\n".join(lines) if lines else self.indent * level + "()"

    def expr(self, expr: A.Expr | None, parent_prec: int = PREC_LOWEST, side: str = "") -> str:
        if expr is None:
            return "()"
        if isinstance(expr, A.Literal):
            text = self.literal(expr.value, expr.kind)
            prec = self.PREC_ATOM
        elif isinstance(expr, A.Name):
            text = expr.name
            prec = self.PREC_ATOM
        elif isinstance(expr, A.Call):
            text = f"{self.expr(expr.func, self.PREC_POSTFIX)}(" + ", ".join(self.expr(a) for a in expr.args) + ")"
            prec = self.PREC_POSTFIX
        elif isinstance(expr, A.FieldAccess):
            text = f"{self.expr(expr.target, self.PREC_POSTFIX)}.{expr.field}"
            prec = self.PREC_POSTFIX
        elif isinstance(expr, A.BinaryOp):
            prec, assoc = self.PREC_BINARY[expr.op]
            left_parent = prec + 1 if assoc == "right" else prec
            right_parent = prec if assoc == "right" else prec + 1
            left_parent = self.boolean_child_parent_prec(expr, expr.left, left_parent)
            right_parent = self.boolean_child_parent_prec(expr, expr.right, right_parent)
            text = f"{self.expr(expr.left, left_parent, 'left')} {expr.op} {self.expr(expr.right, right_parent, 'right')}"
        elif isinstance(expr, A.UnaryOp):
            text = f"{expr.op} {self.expr(expr.operand, self.PREC_PREFIX)}"
            prec = self.PREC_PREFIX
        elif isinstance(expr, A.IfExpr):
            text = f"if {self.expr(expr.condition)} then {self.expr(expr.then_branch)} else {self.expr(expr.else_branch)}"
            prec = self.PREC_LOWEST
        elif isinstance(expr, A.LetExpr):
            ann = f": {self.type_expr(expr.type_annotation)}" if expr.type_annotation else ""
            text = f"let {self.pattern(expr.pattern)}{ann} = {self.expr(expr.value)} in {self.expr(expr.body)}"
            prec = self.PREC_LOWEST
        elif isinstance(expr, A.MatchExpr):
            lines = [f"case {self.expr(expr.subject)}:"]
            for arm in expr.arms:
                guard = f" when {self.expr(arm.guard)}" if arm.guard is not None else ""
                if arm.body and not arm.body.statements and arm.body.result is not None:
                    result = self.expr(arm.body.result)
                    if "\n" in result:
                        lines.append(f"    | {self.pattern(arm.pattern)}{guard}:")
                        lines.append(self.indent_text(result, 2))
                    else:
                        lines.append(f"    | {self.pattern(arm.pattern)}{guard}: {result}")
                else:
                    lines.append(f"    | {self.pattern(arm.pattern)}{guard}:")
                    lines.append(self.block(arm.body, level=2))
            text = "\n".join(lines)
            prec = self.PREC_LOWEST
        elif isinstance(expr, A.RecordConstructor):
            if not expr.fields:
                text = f"{expr.type_name} {{}}"
            else:
                text = f"{expr.type_name} {{ " + ", ".join(f"{k} = {self.expr(v)}" for k, v in expr.fields) + " }"
            prec = self.PREC_RECORD_CONSTRUCTOR
        elif isinstance(expr, A.TupleLiteral):
            if len(expr.items) < 2:
                raise ValueError("tuple literals must contain at least two items; use () for unit")
            text = "(" + ", ".join(self.expr(i) for i in expr.items) + ")"
            prec = self.PREC_ATOM
        elif isinstance(expr, A.TemporalUnary):
            if isinstance(expr.operand, A.MatchExpr):
                operand_prec = self.PREC_LOWEST
            elif isinstance(expr.operand, (A.IfExpr, A.LetExpr)):
                operand_prec = self.PREC_POSTFIX_TEMPORAL
            else:
                operand_prec = self.PREC_LOWEST
            operand = self.expr(expr.operand, operand_prec)
            text = f"{operand}\n{expr.op}" if "\n" in operand else f"{operand} {expr.op}"
            prec = self.PREC_POSTFIX_TEMPORAL
        elif isinstance(expr, A.TemporalBinary):
            prec, assoc = self.PREC_BINARY[expr.op]
            left_parent = prec if assoc == "left" else prec + 1
            right_parent = prec + 1 if assoc == "left" else prec
            if isinstance(expr.left, A.TemporalUnary):
                left_parent = max(left_parent, self.PREC_POSTFIX_TEMPORAL + 1)
            if isinstance(expr.right, A.TemporalUnary):
                right_parent = max(right_parent, self.PREC_POSTFIX_TEMPORAL + 1)
            text = f"{self.expr(expr.left, left_parent, 'left')} {expr.op} {self.expr(expr.right, right_parent, 'right')}"
        else:
            text = repr(expr)
            prec = self.PREC_ATOM
        if prec < parent_prec:
            return f"({text})"
        return text

    def boolean_child_parent_prec(self, parent: A.BinaryOp, child: A.Expr | None, default: int) -> int:
        if not isinstance(child, A.BinaryOp):
            return default
        if parent.op not in self.BOOL_CHAIN_OPS or child.op not in self.BOOL_CHAIN_OPS:
            return default
        if parent.op == "implies" and child.op == "implies" or parent.op != child.op:
            child_prec, _ = self.PREC_BINARY[child.op]
            return max(default, child_prec + 1)
        return default

    def indent_text(self, text: str, level: int) -> str:
        prefix = self.indent * level
        return "\n".join(prefix + line if line else line for line in text.splitlines())

    def pattern(self, pattern: A.Pattern | None) -> str:
        if pattern is None:
            return "_"
        if isinstance(pattern, A.WildcardPattern):
            return "_"
        if isinstance(pattern, A.VarPattern):
            return pattern.name
        if isinstance(pattern, A.LiteralPattern):
            return self.literal(pattern.value, pattern.kind)
        if isinstance(pattern, A.ConstructorPattern):
            if not pattern.args:
                return pattern.name
            return pattern.name + "(" + ", ".join(self.pattern(a) for a in pattern.args) + ")"
        if isinstance(pattern, A.RecordPattern):
            parts = []
            for name, pat in pattern.fields:
                parts.append(name if pat is None else f"{name} = {self.pattern(pat)}")
            return "{ " + ", ".join(parts) + " }"
        if isinstance(pattern, A.TuplePattern):
            return "(" + ", ".join(self.pattern(i) for i in pattern.items) + ")"
        if isinstance(pattern, A.ListPattern):
            raise ValueError("list patterns are not canonical MDL syntax; use std.collections.list constructor patterns")
        return "_"

    def literal(self, value: object, kind: str = "unknown") -> str:
        if kind == "unit":
            return "()"
        if kind == "string":
            return '"' + str(value).replace('"', '\\"') + '"'
        if kind == "bool":
            return "true" if value else "false"
        if isinstance(value, Fraction):
            return f"{value.numerator}/{value.denominator}"
        return str(value)


def format_module(module: A.Module) -> str:
    return PrettyPrinter().module(module)


def format_expr(expr: A.Expr) -> str:
    return PrettyPrinter().expr(expr)
