#!/usr/bin/env python3
import os
import json
import argparse
import hashlib
from dataclasses import dataclass
from typing import List, Optional, Tuple, Dict, Any
from tenacity import retry, stop_after_attempt, wait_exponential, retry_if_exception_type

from dotenv import load_dotenv
load_dotenv()


# ---- OpenAI client (используем Chat Completions)
import openai

# =========================
# Схемы данных (минимальные)
# =========================

@dataclass
class AlgebraicExpr:
    Var: Optional[str] = None
    Const: Optional[int] = None
    Sum: Optional[Tuple['AlgebraicExpr','AlgebraicExpr']] = None
    Mod: Optional[Tuple['AlgebraicExpr','AlgebraicExpr']] = None

    @staticmethod
    def from_json(j: Dict[str, Any]) -> 'AlgebraicExpr':
        if "var" in j:   return AlgebraicExpr(Var=j["var"])
        if "const" in j: return AlgebraicExpr(Const=int(j["const"]))
        if "sum" in j:
            a = AlgebraicExpr.from_json(j["sum"]["a"])
            b = AlgebraicExpr.from_json(j["sum"]["b"])
            return AlgebraicExpr(Sum=(a,b))
        if "mod" in j:
            a = AlgebraicExpr.from_json(j["mod"]["a"])
            b = AlgebraicExpr.from_json(j["mod"]["b"])
            return AlgebraicExpr(Mod=(a,b))
        raise ValueError(f"Bad AlgebraicExpr JSON: {j}")

@dataclass
class PredNode:
    Type: str
    Lhs: Optional[AlgebraicExpr] = None
    Op: Optional[str] = None
    Rhs: Optional[AlgebraicExpr] = None
    A: Optional['PredNode'] = None
    B: Optional['PredNode'] = None
    Inner: Optional['PredNode'] = None
    Name: Optional[str] = None
    Args: Optional[List[str]] = None
    Def: Optional['PredicateDef'] = None
    In: Optional['PredNode'] = None

    @staticmethod
    def from_json(j: Dict[str, Any]) -> 'PredNode':
        t = j["type"]
        if t == "algebraic":
            return PredNode(Type=t,
                            Lhs=AlgebraicExpr.from_json(j["lhs"]),
                            Op=str(j["op"]),
                            Rhs=AlgebraicExpr.from_json(j["rhs"]))
        if t in ("and", "or"):
            return PredNode(Type=t, A=PredNode.from_json(j["a"]), B=PredNode.from_json(j["b"]))
        if t == "not":
            return PredNode(Type=t, Inner=PredNode.from_json(j["inner"]))
        if t == "call":
            return PredNode(Type=t, Name=str(j["name"]), Args=[str(x) for x in j.get("args", [])])
        if t == "nested":
            return PredNode(Type=t, Def=PredicateDef.from_json(j["def"]), In=PredNode.from_json(j["in"]))
        raise ValueError(f"Unsupported PredNode type: {t}")

@dataclass
class PredicateDef:
    Name: str
    Params: List[str]
    Body: PredNode

    @staticmethod
    def from_json(j: Dict[str, Any]) -> 'PredicateDef':
        return PredicateDef(Name=str(j["name"]),
                            Params=[str(x) for x in j.get("params", [])],
                            Body=PredNode.from_json(j["body"]))

@dataclass
class Rule:
    Modality: str
    Name: Optional[str]
    Head: PredNode
    When: Optional[PredNode]

    @staticmethod
    def from_json(j: Dict[str, Any]) -> 'Rule':
        return Rule(Modality=str(j["modality"]),
                    Name=(str(j["name"]) if j.get("name") else None),
                    Head=PredNode.from_json(j["head"]),
                    When=(PredNode.from_json(j["when"]) if j.get("when") else None))

@dataclass
class ExtractedSpec:
    Predicates: List[PredicateDef]
    Rules: List[Rule]

    @staticmethod
    def empty() -> 'ExtractedSpec':
        return ExtractedSpec(Predicates=[], Rules=[])

    @staticmethod
    def from_json(j: Dict[str, Any]) -> 'ExtractedSpec':
        preds = [PredicateDef.from_json(x) for x in j.get("predicates", [])]
        rules = [Rule.from_json(x) for x in j.get("rules", [])]
        return ExtractedSpec(preds, rules)

AlgebraicExpr.__annotations__['Sum'] = Optional[Tuple[AlgebraicExpr, AlgebraicExpr]]
AlgebraicExpr.__annotations__['Mod'] = Optional[Tuple[AlgebraicExpr, AlgebraicExpr]]
PredNode.__annotations__['A'] = Optional[PredNode]
PredNode.__annotations__['B'] = Optional[PredNode]
PredNode.__annotations__['Inner'] = Optional[PredNode]
PredNode.__annotations__['Def'] = Optional[PredicateDef]
PredNode.__annotations__['In'] = Optional[PredNode]

# =========================
# Эмиттер DSL
# =========================

def emit_expr(e: AlgebraicExpr) -> str:
    if e.Var is not None: return e.Var
    if e.Const is not None: return str(e.Const)
    if e.Sum is not None:
        a,b = e.Sum; return f"{emit_expr(a)} + {emit_expr(b)}"
    if e.Mod is not None:
        a,b = e.Mod; return f"{emit_expr(a)} % {emit_expr(b)}"
    raise ValueError("Invalid AlgebraicExpr")

def emit_pred(p: PredNode) -> str:
    t = p.Type
    if t == "algebraic":
        return f"{emit_expr(p.Lhs)} {p.Op} {emit_expr(p.Rhs)}"
    if t == "and": return f"{emit_pred(p.A)} and {emit_pred(p.B)}"
    if t == "or":  return f"{emit_pred(p.A)} or {emit_pred(p.B)}"
    if t == "not": return f"not {emit_pred(p.Inner)}"
    if t == "call":
        args = ", ".join(p.Args or []); return f"{p.Name}({args})"
    if t == "nested":
        d = p.Def; assert d and p.In
        return f"{emit_def(d)} in {emit_pred(p.In)}"
    raise ValueError(f"Unknown PredNode type: {t}")

def emit_def(d: PredicateDef) -> str:
    params = ", ".join(d.Params or [])
    return f"predicate {d.Name}({params}) = {emit_pred(d.Body)}"

def emit_rule(r: Rule) -> str:
    head = emit_pred(r.Head)
    name_part = (f" {r.Name} =" if r.Name else "")
    when_part = (f" when {emit_pred(r.When)}" if r.When else "")
    return f"{r.Modality}{name_part} {head}{when_part}"

# =========================
# Нормализация и дедупликация
# =========================

def stable_predicate_key(d: PredicateDef) -> str:
    def prednode_to_tuple(p: PredNode):
        if p.Type == "algebraic":
            return ("alg", expr_to_tuple(p.Lhs), p.Op, expr_to_tuple(p.Rhs))
        if p.Type in ("and", "or"):
            return (p.Type, prednode_to_tuple(p.A), prednode_to_tuple(p.B))
        if p.Type == "not":
            return ("not", prednode_to_tuple(p.Inner))
        if p.Type == "call":
            return ("call", p.Name, tuple(p.Args or []))
        if p.Type == "nested":
            d = p.Def
            return ("nested", (d.Name, tuple(d.Params), prednode_to_tuple(d.Body)), prednode_to_tuple(p.In))
        raise ValueError(f"Bad PredNode for key: {p.Type}")

    def expr_to_tuple(e: AlgebraicExpr):
        if e.Var is not None:  return ("var", e.Var)
        if e.Const is not None:return ("const", e.Const)
        if e.Sum is not None:  return ("sum", expr_to_tuple(e.Sum[0]), expr_to_tuple(e.Sum[1]))
        if e.Mod is not None:  return ("mod", expr_to_tuple(e.Mod[0]), expr_to_tuple(e.Mod[1]))
        raise ValueError("Bad AlgebraicExpr for key")

    tup = (d.Name, tuple(d.Params), prednode_to_tuple(d.Body))
    return hashlib.sha256(repr(tup).encode("utf-8")).hexdigest()

def dedupe_predicates(preds: List[PredicateDef]) -> List[PredicateDef]:
    seen: Dict[str, PredicateDef] = {}
    out = []
    for p in preds:
        k = stable_predicate_key(p)
        if k in seen: continue
        seen[k] = p; out.append(p)
    return out

# =========================
# Подсказки
# =========================

SYSTEM_PROMPT = """\
You convert Russian/English prose into STRICT JSON for a narrow deontic DSL.
Output MUST be valid JSON object only (no markdown).

Constraints:
- Extract ONLY deontic rules (obligated|permitted|forbidden|suggested) and predicate definitions.
- Allowed predicate bodies: algebraic comparisons (<, >, =), boolean ops (and, or, not),
  calls to named predicates, nested predicate definition ('predicate ... = ... in ...').
- DO NOT output facts, temporal operators, terms/records, or symbols like >=, <=, !=.
- Use ascii names for identifiers: [a-zA-Z_][a-zA-Z0-9_]* .

Exmaples:

Input paragraph:
> Диаметр трубы должен быть от 35 до 55 см.
Output DSL:
> predicate is_tube(x) = x.shape = Tube(_, _)
  obligated x.diameter >= 35 and x.diameter <= 55 when is_tube(x)

Input paragraph:
> Если давление превышает 10 бар, температура корпуса не должна быть выше 80 °C.
Output DSL:
> predicate high_pressure(x) = x.pressure > 10
  forbidden x.temperature > 80 when high_pressure(x)
  
Input paragraph:
> Использование пластиковых соединений запрещено, за исключением случаев, когда температура среды не превышает 30 °C.
Output DSL:
> predicate plastic_joint(x) = x.material = Plastic
  predicate low_temp(x) = x.temperature <= 30

  forbidden plastic_joint(x)
  permitted plastic_joint(x) when low_temp(x)
"""

USER_INSTRUCTION = """\
Extract JSON for the given segment. Follow the schema exactly.

Schema (informal):
{
  "predicates":[
    {"name":"string","params":["x","y"],"body":PredNode}
  ],
  "rules":[
    {"modality":"obligated|permitted|forbidden|suggested","name":null|string,"head":PredNode,"when":null|PredNode}
  ]
}

PredNode:
- {"type":"algebraic","lhs":Expr,"op":"<" | ">" | "=","rhs":Expr}
- {"type":"and","a":PredNode,"b":PredNode}
- {"type":"or","a":PredNode,"b":PredNode}
- {"type":"not","inner":PredNode}
- {"type":"call","name":"foo","args":["x","y"]}
- {"type":"nested","def":{...PredicateDef...},"in":PredNode}

Expr:
- {"var":"x"} | {"const":10} | {"sum":{"a":Expr,"b":Expr}} | {"mod":{"a":Expr,"b":Expr}}

Segment:
"""

# =========================
# Вызов OpenAI: structured outputs через Chat Completions
# =========================

@retry(reraise=True, stop=stop_after_attempt(4), wait=wait_exponential(multiplier=1, min=1, max=10),
       retry=retry_if_exception_type(Exception))
def call_openai_for_json(segment: str, model: str) -> Dict[str, Any]:
    """
    Используем chat.completions.create + response_format={"type":"json_object"}.
    Это даёт структурированный JSON без markdown. Если SDK древний и не знает
    response_format, пробуем без него и парсим content.
    """
    openai.api_key = os.getenv("OPENAI_API_KEY")

    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": USER_INSTRUCTION + segment},
    ]

    # Попытка с strict JSON object
    try:
        resp = openai.chat.completions.create(
            model=model,
            messages=messages,
            temperature=0.5,
            max_tokens=800,
            response_format={"type": "json_object"},
        )
        text = resp.choices[0].message.content
        return json.loads(text)
    except TypeError:
        # Старый SDK: параметра response_format нет — пробуем без него
        resp = openai.chat.completions.create(
            model=model,
            messages=messages,
            temperature=0.5,
            max_tokens=800,
        )
        text = resp.choices[0].message.content.strip()
        # Мягкая очистка случайных ```json
        if text.startswith("```"):
            text = text.strip("`")
            text = text[text.find("\n")+1:]
        return json.loads(text)

# =========================
# IO и пайплайн
# =========================

def load_segments(path: str) -> List[str]:
    with open(path, "r", encoding="utf-8") as f:
        raw = f.read().strip()
    try:
        arr = json.loads(raw)
        if isinstance(arr, list):
            return [str(x) for x in arr]
    except Exception:
        pass
    return [s.strip() for s in raw.split("\n\n") if s.strip()]

def normalize_name(name: str) -> str:
    safe = []
    for ch in name:
        if ch.isalnum() or ch == '_':
            safe.append(ch)
        elif ch in ['-', ' ']:
            safe.append('_')
    s = "".join(safe) or "p"
    if s[0].isdigit():
        s = "p_" + s
    return s[:64]

def postprocess_spec(spec: ExtractedSpec) -> ExtractedSpec:
    for p in spec.Predicates:
        p.Name = normalize_name(p.Name)
        p.Params = [normalize_name(x) for x in (p.Params or [])]
    preds = dedupe_predicates(spec.Predicates)
    return ExtractedSpec(preds, spec.Rules)

def aggregate_specs(specs: List[ExtractedSpec]) -> ExtractedSpec:
    all_preds, all_rules = [], []
    for s in specs:
        all_preds.extend(s.Predicates)
        all_rules.extend(s.Rules)
    all_preds = dedupe_predicates(all_preds)
    return ExtractedSpec(all_preds, all_rules)

def emit_dsl(spec: ExtractedSpec) -> str:
    parts = []
    for d in spec.Predicates:
        parts.append(emit_def(d))
    for r in spec.Rules:
        parts.append(emit_rule(r))
    return "\n".join(parts) + ("\n" if parts else "")

# =========================
# main
# =========================

def main():
    ap = argparse.ArgumentParser(description="Convert text segments to DDL-LTLf DSL via OpenAI structured outputs.")
    ap.add_argument("--in", dest="inp", required=True, help="Path to segments file (JSON array or raw text).")
    ap.add_argument("--out", dest="out", default="out.ddl", help="Output .ddl path (default: out.ddl)")
    ap.add_argument("--model", dest="model", default="gpt-4o-mini", help="OpenAI model (default: gpt-4o-mini)")
    ap.add_argument("--max-segments", type=int, default=None, help="Limit number of segments for quick runs")
    args = ap.parse_args()

    if not os.getenv("OPENAI_API_KEY"):
        raise SystemExit("OPENAI_API_KEY env var is not set.")

    segments = load_segments(args.inp)
    if args.max_segments:
        segments = segments[:args.max_segments]

    specs: List[ExtractedSpec] = []
    for idx, seg in enumerate(segments, 1):
        seg_short = (seg[:120] + "…") if len(seg) > 120 else seg
        print(f"[{idx}/{len(segments)}] extracting JSON for segment: {seg_short!r}")
        try:
            data = call_openai_for_json(seg, model=args.model)
            if "predicates" not in data or "rules" not in data:
                data = {"predicates": [], "rules": []}
            spec = ExtractedSpec.from_json(data)
            spec = postprocess_spec(spec)
            specs.append(spec)
        except Exception as e:
            print(f"  > skipped due to error: {e}")

    full = aggregate_specs(specs)
    dsl = emit_dsl(full)

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        f.write(dsl)

    print(f"\nDone. Wrote DSL to: {args.out}")
    if not dsl.strip():
        print("Note: output is empty — это нормально для ненормативных текстов (например, поэзия).")

if __name__ == "__main__":
    main()
