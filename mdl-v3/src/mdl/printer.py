from __future__ import annotations

from fractions import Fraction

from . import ast as A


class PrettyPrinter:
    def __init__(self, indent: str = "    "):
        self.indent = indent

    def module(self, module: A.Module) -> str:
        parts: list[str] = []
        parts.extend(self.annotations(module.annotations))
        parts.append(f"module {module.name}")
        if module.imports:
            parts.append("")
            for imp in module.imports:
                parts.append(self.import_decl(imp))
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
        text = f"import {decl.path}"
        if decl.alias:
            text += f" as {decl.alias}"
        if decl.exposing:
            exposed = []
            for name, alias in decl.exposing:
                exposed.append(f"{name} as {alias}" if alias else name)
            text += " exposing (" + ", ".join(exposed) + ")"
        return f"{prefix}\n{text}" if prefix else text

    def declaration(self, decl: A.Declaration) -> str:
        prefix = "\n".join(self.annotations(decl.annotations))
        if isinstance(decl, A.TypeDecl):
            text = self.type_decl(decl)
        elif isinstance(decl, A.ValueDecl):
            text = self.value_decl(decl)
        elif isinstance(decl, A.FuncDecl):
            text = self.func_decl(decl)
        elif isinstance(decl, A.EntityDecl):
            text = self.entity_decl(decl)
        elif isinstance(decl, A.EventDecl):
            text = self.event_decl(decl)
        elif isinstance(decl, A.RuleDecl):
            text = self.rule_decl(decl)
        elif isinstance(decl, A.PriorityDecl):
            text = self.priority_decl(decl)
        elif isinstance(decl, A.FactDecl):
            text = self.fact_decl(decl)
        elif isinstance(decl, A.AssertDecl):
            text = self.assert_decl(decl)
        elif isinstance(decl, A.AlignDecl):
            text = self.align_decl(decl)
        else:  # pragma: no cover
            text = f"/* unsupported declaration: {decl!r} */"
        return f"{prefix}\n{text}" if prefix else text

    def visibility(self, decl: A.Declaration) -> str:
        return "" if decl.visibility == "public" else decl.visibility + " "

    def type_decl(self, decl: A.TypeDecl) -> str:
        params = f"<{', '.join(decl.params)}>" if decl.params else ""
        return f"{self.visibility(decl)}type {decl.name}{params} = {self.type_definition(decl.definition)}"

    def type_definition(self, typ: A.TypeExpr | A.SumType | None) -> str:
        if typ is None:
            return "unit"
        if isinstance(typ, A.SumType):
            return " | ".join(self.variant(v) for v in typ.variants)
        return self.type_expr(typ)

    def variant(self, variant: A.Variant) -> str:
        if not variant.fields:
            return variant.name
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

    def value_decl(self, decl: A.ValueDecl) -> str:
        ann = f": {self.type_expr(decl.type_annotation)}" if decl.type_annotation else ""
        return f"{self.visibility(decl)}val {decl.name}{ann} = {self.expr(decl.value)}"

    def func_decl(self, decl: A.FuncDecl) -> str:
        params = ", ".join(self.param(p) for p in decl.params)
        tparams = f"<{', '.join(decl.type_params)}>" if decl.type_params else ""
        header = f"{self.visibility(decl)}func {decl.name}{tparams}({params}) -> {self.type_expr(decl.return_type)}:"
        return header + "\n" + self.block(decl.body, level=1)

    def param(self, param: A.Param) -> str:
        return f"{self.pattern(param.pattern)}: {self.type_expr(param.type_annotation)}"

    def entity_decl(self, decl: A.EntityDecl) -> str:
        text = f"{self.visibility(decl)}entity {decl.name}: {self.type_expr(decl.type_annotation)}"
        for kind, expr in decl.clauses:
            if kind == "key":
                text += f" key ({self.expr(expr)})"
            else:
                text += f" where {self.expr(expr)}"
        return text

    def event_decl(self, decl: A.EventDecl) -> str:
        fields = ", ".join(f"{n}: {self.type_expr(t)}" for n, t in decl.fields)
        return f"{self.visibility(decl)}event {decl.name}({fields})"

    def rule_decl(self, decl: A.RuleDecl) -> str:
        strength = "" if decl.strength == "defeasible" else decl.strength + " "
        modality = f"{decl.modality} " if decl.modality else ""
        if decl.anonymous:
            header = f"{self.visibility(decl)}{strength}rule {modality}".rstrip()
        else:
            header = f"{self.visibility(decl)}{strength}rule {modality}{decl.name}".rstrip()
        if decl.antecedent is not None:
            header += f" when {self.expr(decl.antecedent)}"
        body = self.expr(decl.body)
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

    def assert_decl(self, decl: A.AssertDecl) -> str:
        return f"assert {self.expr(decl.expr)}"

    def align_decl(self, decl: A.AlignDecl) -> str:
        return f"align {decl.subject} to {decl.target} {decl.kind}"

    def block(self, block: A.Block | None, level: int = 1) -> str:
        if block is None:
            return self.indent * level + "unit"
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
        return "\n".join(lines) if lines else self.indent * level + "unit"

    def expr(self, expr: A.Expr | None) -> str:
        if expr is None:
            return "unit"
        if isinstance(expr, A.Literal):
            return self.literal(expr.value, expr.kind)
        if isinstance(expr, A.Name):
            return expr.name
        if isinstance(expr, A.Call):
            return f"{self.expr(expr.func)}(" + ", ".join(self.expr(a) for a in expr.args) + ")"
        if isinstance(expr, A.FieldAccess):
            return f"{self.expr(expr.target)}.{expr.field}"
        if isinstance(expr, A.IndexAccess):
            return f"{self.expr(expr.target)}[{self.expr(expr.index)}]"
        if isinstance(expr, A.BinaryOp):
            return f"{self.expr(expr.left)} {expr.op} {self.expr(expr.right)}"
        if isinstance(expr, A.UnaryOp):
            return f"{expr.op} {self.expr(expr.operand)}"
        if isinstance(expr, A.IfExpr):
            return f"if {self.expr(expr.condition)} then {self.expr(expr.then_branch)} else {self.expr(expr.else_branch)}"
        if isinstance(expr, A.LetExpr):
            return f"let {self.pattern(expr.pattern)} = {self.expr(expr.value)} in {self.expr(expr.body)}"
        if isinstance(expr, A.MatchExpr):
            lines = [f"case {self.expr(expr.subject)}:"]
            for arm in expr.arms:
                guard = f" when {self.expr(arm.guard)}" if arm.guard is not None else ""
                if arm.body and not arm.body.statements and arm.body.result is not None:
                    lines.append(f"| {self.pattern(arm.pattern)}{guard}: {self.expr(arm.body.result)}")
                else:
                    lines.append(f"| {self.pattern(arm.pattern)}{guard}:")
                    lines.append(self.block(arm.body, level=1))
            return "\n".join(lines)
        if isinstance(expr, A.RecordLiteral):
            if not expr.fields:
                return "{}"
            return "{ " + ", ".join(f"{k} = {self.expr(v)}" for k, v in expr.fields) + ", }"
        if isinstance(expr, A.ListLiteral):
            return "[" + ", ".join(self.expr(i) for i in expr.items) + "]"
        if isinstance(expr, A.SetLiteral):
            return "#{" + ", ".join(self.expr(i) for i in expr.items) + "}"
        if isinstance(expr, A.TupleLiteral):
            return "(" + ", ".join(self.expr(i) for i in expr.items) + ")"
        if isinstance(expr, A.BracedExpr):
            return "{ " + self.expr(expr.expr) + " }"
        if isinstance(expr, A.TemporalUnary):
            if expr.position == "postfix":
                return f"{self.expr(expr.operand)} {expr.op}"
            return f"{expr.op} {self.expr(expr.operand)}"
        if isinstance(expr, A.TemporalBinary):
            return f"{self.expr(expr.left)} {expr.op} {self.expr(expr.right)}"
        if isinstance(expr, A.QuantifierExpr):
            return f"{expr.quantifier} {self.pattern(expr.pattern)} in {self.expr(expr.domain)}: {self.expr(expr.body)}"
        return repr(expr)

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
            return "[" + ", ".join(self.pattern(i) for i in pattern.items) + "]"
        return "_"

    def literal(self, value: object, kind: str = "unknown") -> str:
        if kind == "string":
            return '"' + str(value).replace('"', '\\"') + '"'
        if kind == "char":
            return "'" + str(value).replace("'", "\\'") + "'"
        if kind == "bool":
            return "true" if value else "false"
        if isinstance(value, Fraction):
            return f"{value.numerator}/{value.denominator}"
        return str(value)


def format_module(module: A.Module) -> str:
    return PrettyPrinter().module(module)


def format_expr(expr: A.Expr) -> str:
    return PrettyPrinter().expr(expr)
