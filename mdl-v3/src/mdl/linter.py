from __future__ import annotations

from . import ast as A
from .diagnostics import Diagnostic, ParseError
from .parser import parse


class Linter:
    def lint_module(self, module: A.Module, path: str | None = None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        diagnostics.extend(self.check_duplicates(module, path))
        diagnostics.extend(self.check_rules(module, path))
        diagnostics.extend(self.check_alignments(module, path))
        diagnostics.extend(self.check_functions(module, path))
        return diagnostics

    def check_duplicates(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        seen: dict[tuple[str, str], A.Declaration] = {}
        diagnostics: list[Diagnostic] = []
        for decl in module.declarations:
            name = A.declaration_name(decl)
            if not name:
                continue
            kind = decl.__class__.__name__
            key = (kind, name)
            if key in seen:
                diagnostics.append(Diagnostic(
                    f"duplicate {kind} name {name!r}",
                    line=decl.line or 1,
                    column=decl.column or 1,
                    severity="error",
                    code="duplicate-name",
                    path=path,
                ))
            else:
                seen[key] = decl
        return diagnostics

    def check_rules(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        rule_names = set()
        for decl in module.declarations:
            if not isinstance(decl, A.RuleDecl):
                continue
            if decl.name in rule_names:
                diagnostics.append(Diagnostic(
                    f"duplicate rule name {decl.name!r}", decl.line or 1, decl.column or 1,
                    severity="error", code="duplicate-rule", path=path,
                ))
            rule_names.add(decl.name)
            if not decl.modality:
                diagnostics.append(Diagnostic(
                    f"rule {decl.name!r} has no deontic modality",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="missing-modality", path=path,
                ))
            if decl.anonymous:
                diagnostics.append(Diagnostic(
                    "anonymous rule is accepted, but named rules are better for traceability and priorities",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="anonymous-rule", path=path,
                ))
            if decl.body is not None and not self.has_temporal_operator(decl.body):
                diagnostics.append(Diagnostic(
                    f"rule {decl.name!r} has no explicit temporal operator; consider `always` or `eventually`",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="rule-without-temporal", path=path,
                ))
        for decl in module.declarations:
            if isinstance(decl, A.PriorityDecl):
                for name in decl.chain:
                    if name not in rule_names:
                        diagnostics.append(Diagnostic(
                            f"priority references unknown rule {name!r}",
                            decl.line or 1, decl.column or 1,
                            severity="warning", code="unknown-priority-rule", path=path,
                        ))
        return diagnostics

    def check_alignments(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        exported = {A.declaration_name(d) for d in module.declarations if A.declaration_name(d)}
        for decl in module.declarations:
            if isinstance(decl, A.AlignDecl):
                subject_root = decl.subject.split(".")[0]
                if subject_root not in exported and subject_root != module.name:
                    diagnostics.append(Diagnostic(
                        f"alignment subject {decl.subject!r} does not resolve to a declaration in this module",
                        decl.line or 1, decl.column or 1,
                        severity="warning", code="unresolved-alignment-subject", path=path,
                    ))
        return diagnostics

    def check_functions(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        for decl in module.declarations:
            if isinstance(decl, A.FuncDecl):
                if decl.body is None or decl.body.result is None:
                    diagnostics.append(Diagnostic(
                        f"function {decl.name!r} has no final expression",
                        decl.line or 1, decl.column or 1,
                        severity="error", code="function-without-result", path=path,
                    ))
                if self.has_temporal_operator_in_block(decl.body):
                    diagnostics.append(Diagnostic(
                        f"function {decl.name!r} contains temporal operators; temporal logic belongs in rules/asserts",
                        decl.line or 1, decl.column or 1,
                        severity="error", code="temporal-in-function", path=path,
                    ))
        return diagnostics

    def has_temporal_operator_in_block(self, block: A.Block | None) -> bool:
        if block is None:
            return False
        return any(self.has_temporal_operator(stmt.value) for stmt in block.statements) or self.has_temporal_operator(block.result)

    def has_temporal_operator(self, expr: A.Expr | None) -> bool:
        if expr is None:
            return False
        if isinstance(expr, (A.TemporalUnary, A.TemporalBinary)):
            return True
        if isinstance(expr, A.BracedExpr):
            return self.has_temporal_operator(expr.expr)
        if isinstance(expr, A.BinaryOp):
            return self.has_temporal_operator(expr.left) or self.has_temporal_operator(expr.right)
        if isinstance(expr, A.UnaryOp):
            return self.has_temporal_operator(expr.operand)
        if isinstance(expr, A.IfExpr):
            return any(self.has_temporal_operator(e) for e in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.Call):
            return self.has_temporal_operator(expr.func) or any(self.has_temporal_operator(a) for a in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.has_temporal_operator(expr.target)
        if isinstance(expr, A.IndexAccess):
            return self.has_temporal_operator(expr.target) or self.has_temporal_operator(expr.index)
        if isinstance(expr, A.MatchExpr):
            return self.has_temporal_operator(expr.subject) or any(
                self.has_temporal_operator(arm.guard) or self.has_temporal_operator_in_block(arm.body)
                for arm in expr.arms
            )
        if isinstance(expr, A.LetExpr):
            return self.has_temporal_operator(expr.value) or self.has_temporal_operator(expr.body)
        if isinstance(expr, A.QuantifierExpr):
            return self.has_temporal_operator(expr.domain) or self.has_temporal_operator(expr.body)
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            return any(self.has_temporal_operator(i) for i in expr.items)
        if isinstance(expr, A.RecordLiteral):
            return any(self.has_temporal_operator(v) for _, v in expr.fields)
        return False


def lint_source(source: str, path: str | None = None) -> list[Diagnostic]:
    try:
        module = parse(source)
    except ParseError as exc:
        return [exc.to_diagnostic(path)]
    return Linter().lint_module(module, path=path)
