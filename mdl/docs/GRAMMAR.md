# MDL grammar notes

The Python parser supports the main language constructs:

```text
module, string-path import, annotation
private/public declarations
sum types and record types
val/let declarations
pure functions with block bodies
if / let-in / case expressions
record types, nominal record constructors, and tuples
entities, events, facts, asserts
rules with `rule O name: body`, optional `when` applicability conditions,
and strict/defeasible/defeater strength
priority/override chains
align declarations
LTLf operators: always, eventually, next, weak_next, never, until, release, weak_until
bounded quantifiers: forall / exists
```

The grammar is ML-inspired but intentionally not a general-purpose programming
language. Names must be used after they are defined; functions may refer to
themselves from their body for recursion. Functions are expected to be pure,
total and deterministic. Only boolean term expressions are lifted into temporal
atoms.

Record values are constructed with the record type name, for example
`Pipe { length = 10, radius = 2 }`. Bare `{ ... }` is not expression syntax.

Collection types such as `List`, `Set`, `Map`, and `Option` live in stdlib files
and must be imported explicitly, for example
`import "std/collections/list.mdl" as List exposing (List)`.
