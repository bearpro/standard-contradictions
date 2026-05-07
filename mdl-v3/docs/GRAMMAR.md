# MDL grammar notes

The Python parser supports the main language constructs:

```text
module, import, annotation
private/public declarations
sum types and record types
val/let declarations
pure functions with block bodies
if / let-in / case expressions
records, lists, sets, tuples
entities, events, facts, asserts
rules with O/P/F, strict/defeasible/defeater strength
priority/override chains
align declarations
LTLf operators: always, eventually, next, weak_next, never, until, release, weak_until
bounded quantifiers: forall / exists
```

The grammar is ML-inspired but intentionally not a general-purpose programming language. Functions are expected to be pure, total and deterministic. Only boolean term expressions are lifted into temporal atoms.

For editor integration see:

- `grammars/vscode/` for TextMate syntax highlighting;
- `grammars/tree-sitter-mdl/` for a Tree-sitter scaffold.
