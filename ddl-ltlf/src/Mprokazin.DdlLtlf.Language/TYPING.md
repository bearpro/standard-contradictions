# Type System Overview

This document explains the static type system of the language:

* what types exist,
* how expressions are typed,
* how predicates and deontic statements are typed,
* what it means for two types to be compatible.

The AST structure referenced here is the one in `Mprokazin.DdlLtlf.Language.Ast`.

The type checker/inferencer is responsible for:

1. Inferring missing types.
2. Ensuring all expressions are type-correct.
3. Attaching concrete function types to predicates and deontic statements.

---

## 1. Core language concepts

### 1.1 Primitive types

The language has three primitive types:

* `int`
* `rational`
* `bool`

These appear in the AST as:

```fsharp
type PrimitiveType = Int | Rational | Bool
```

### 1.2 Type descriptions (`TypeDescription`)

A type in the source language is represented in the AST as `TypeDescription`. It can be:

* **Reference**

  * `Primitive of PrimitiveType` (e.g. `int`)
  * `Named of string` (e.g. `Point`, where `type Point = ...`)

    * `Named` is a reference to a `type` alias defined somewhere in the program.

* **Product**

  * A product (record-like / tuple-like) type:

    ```ddl
    (f1: T1, f2: T2, ..., fk: Tk)
    ```
  * In the AST this is:

    ```fsharp
    Product of ProductTypeDescription
    ```

    where `ProductTypeDescription` contains a list of `ProductTypeField`.

* **Sum**

  * A tagged union / sum type:

    ```ddl
    (Ctor1 of T | Ctor2 | Ctor3 of U)
    ```
  * In the AST this is:

    ```fsharp
    Sum of SumTypeDescription
    ```

    where `SumTypeDescription` is a list of `SumTypeVariant`.
    Each variant has:

    * `Constructor : string`
    * `Payload    : TypeDescription option`
      (the optional payload type for that constructor)

* **Function**

  * A first-class function type:

    ```ddl
    (T1, T2, ..., Tn) -> R
    ```
  * In the AST this is:

    ```fsharp
    Function of FuncTypeDescription
    ```

### 1.3 Type definitions

A `type` definition in source code is a *type alias*:

```ddl
type X = <TypeDescription>
```

This does **not** introduce a new nominal type for product types or function types. It is just a name for that `TypeDescription`.

Examples:

```ddl
type Point = (x: rational, y: rational)
type Vec2  = (a: rational, b: rational)
```

`Point` and `Vec2` are considered structurally equivalent as product types, because they are both "two fields of type `rational` in that order", even though the field *names* differ.

However, sums behave differently:

```ddl
type Shape = (Circle of rational | Square of rational)
type Slot  = (Circle of rational | Square of rational)
```

Even though `Shape` and `Slot` look textually identical, they are distinct sum types. Constructors are scoped to / owned by their sum definition. A value constructed with `Circle(...)` from `Shape` is *not* automatically compatible with `Circle(...)` from `Slot`.

* **Product and function types are structurally typed.**
* **Sum types are nominal.**

This is crucial.

---

## 2. Values / expressions

### 2.1 `ExpressionKind`

Relevant AST excerpt:

```fsharp
type Expression =
    { Kind: ExpressionKind
      Annotation: TypeDescription option  // optional `expr: T` annotation
      Range: SourceRange }

and ExpressionKind =
    | Constant of ConstantValue           // 1, 2.0, true, ...
    | Name of string list                // x, or a.b.c
    | Tuple of Expression list           // (e1, e2, e3)
    | Constructor of ConstructorCall     // A(1), B(), ...
```

#### Constants

`ConstantValue`:

```fsharp
type ConstantValue =
    | IntConstant of int
    | RationalConstant of string
    | BooleanConstant of bool
```

Typing:

* `IntConstant`        → `int`
* `RationalConstant`   → `rational`
* `BooleanConstant`    → `bool`

#### Names and field access

`Name of string list` represents:

* a simple variable reference: `["x"]`  (i.e. `x`)
* chained field access: `["a"; "b"; "c"]` (i.e. `a.b.c`)

Typing rules:

1. The first identifier (e.g. `"a"`) must resolve in the current value environment (predicate parameters, local bindings, etc.).
2. If it's just one identifier (length = 1), the expression has that variable's type.
3. If it is a chain like `a.b` or `a.b.c`:

   * The type of `a` must be a product type (a tuple-like structure).
   * `a.b` means "select field `b` from `a`".

     * This is only valid if the product type for `a` has a field named `b`.
   * After selecting `b`, the resulting type becomes the type of that field.
   * Then `a.b.c` continues: the type we got for `a.b` must again be a product type with a field `c`, etc.
   * It is a type error if at any step we try to project a field from something that is not a product type or does not contain that field.

> Note: For structural compatibility we care about field positions, not field names.
> But for *field access syntax* (`a.b`) we require that the product type carried field names at definition time and that the requested name matches exactly.

So:

* Field names matter for `a.b` lookup.
* Field names do **not** affect whether two product types are considered the same type structurally.

#### Tuple literals `(e1, e2, ..., ek)`

`Tuple [e1; e2; ...; ek]` in the AST means a product value with `k` positional components.

Typing:

* `(1, 2)` has a product type like `( _0: int, _1: int )`
* `(1, true)` has `( _0: int, _1: bool )`

If the expression node has an explicit annotation `expr: T` in source, we must check that this tuple expression can be assigned to `T`. Concretely:

* `T` must be a product type of the same arity `k`.
* Each element type at position `i` must be compatible with the declared field type at position `i`.

Empty tuple `()` is the unique 0-arity product. It is compatible with any product type of zero fields.

Because product types are structurally typed:

```ddl
(1, 1): Point
(1, 1): Vec2
```

is allowed if both `Point` and `Vec2` are aliases for a 2-field product of `(rational, rational)` (or `(int, int)` depending on numeric rules). Field *names* don't have to match.

#### Constructor calls (sum values)

AST:

```fsharp
type ConstructorCall =
    { Constructor: string
      Arguments: Expression list
      Range: SourceRange }
```

Source examples:

```ddl
A(1)
B()
```

Typing a constructor call:

1. Look up the constructor name in a global constructor environment.

   * This environment maps a constructor (e.g. `"A"`) to:

     * the sum type it belongs to (e.g. `"OptionInt"`),
     * the payload type (if any).
2. Check argument count:

   * If variant was declared as `A of T`, then exactly one argument is expected, of type `T`.
   * If variant was declared as `B` (no payload), then zero arguments are expected.
   * (If we later allow multi-field payloads like `A of (x: T1, y: T2)`, then `A(e1, e2)` must match that product type structurally.)
3. Infer/check the types of the `Arguments`.
4. Ensure they match the expected payload type.
5. The resulting expression has the nominal sum type that owns this constructor.

Sum types are nominal: the constructor determines exactly which sum type you get.

---

## 3. Predicates and function types

### 3.1 Predicates

AST:

```fsharp
type PredicateDefinition =
    { Name: string
      Parameters: Parameter list
      Body: PredicateBody
      Range: SourceRange
      PredicateType: FuncTypeDescription option }
```

* Source form:

  ```ddl
  predicate name(p1: T1, p2, ...) = <body>
  ```

* Each parameter is in `Parameters`. A parameter may or may not have an explicit type:

  ```fsharp
  type Parameter = { Name: string; Type: TypeDescription option; Range: SourceRange }
  ```

  * If `Type = None`, that parameter's type is initially a fresh type variable and must be inferred from usage.

* The return type of a predicate is always `bool`.

The full predicate signature is therefore always:

```text
(p1Type, p2Type, ..., pnType) -> bool
```

After inference, we store this signature back in `PredicateDefinition.PredicateType` as a `FuncTypeDescription`.

### 3.2 Calling predicates

AST:

```fsharp
type PredicateCall =
    { PredicateName: string
      Arguments: Expression list
      Range: SourceRange }
```

Typing a `PredicateCall`:

1. Find the predicate definition and its (possibly partially inferred) parameter types.
2. Require that the call supplies exactly as many arguments as the predicate has parameters.
3. Infer each argument's type.
4. Check positional compatibility: argument `i` must be compatible with parameter `i`.

   * There is **no implicit "field spreading"** of product types. If the predicate expects one parameter of type `Point`, you must pass exactly one expression of type `Point`, e.g. `(1,2): Point`, not `1,2`.
5. The call expression itself always has type `bool`.

### 3.3 First-class function types

The AST form `Function of FuncTypeDescription` represents a function type:

```ddl
(T1, T2, ..., Tn) -> R
```

Semantics:

* Function types are first-class. They can appear:

  * as parameter types,
  * in product fields,
  * as sum payloads,
  * as named aliases, etc.
* Predicate signatures are just a special case where `R` is `bool`.
* Two function types are compatible structurally:

  * same arity,
  * each parameter type is pairwise compatible,
  * return types are compatible.

There is no restriction that functions can only appear in predicates. We allow storing them in types (even if we don't interpret/exe them yet).

---

## 4. Type compatibility (a.k.a. unification / assignability)

We need a notion of "is type A compatible with type B?" This is used when:

* checking a call argument against a parameter type,
* checking `expr: T` annotations,
* checking constructor payloads,
* checking tuple literals against expected product types.

### 4.1 References

For `Reference`:

* A primitive reference like `Primitive Int` must match exactly `int`.
* A named reference `Named "Point"` must resolve through the type environment:

  * If `"Point"` resolves to a product type, then we compare structurally as a product.
  * If it resolves to a sum type, we treat it as that sum's nominal identity.
  * If it resolves to a function type, we compare structurally as a function type.

So: `Named` behaves like an alias, not like a new distinct nominal product/function type. (Sum types are an exception; see below.)

### 4.2 Product types (tuples / records)

Two product types are compatible **structurally**:

* They must have the same arity (same number of fields).
* Each field type at position `i` must be compatible.

Field *names* do not affect structural compatibility. They exist for field access syntax (`a.b`) but do not make two otherwise identical `(rational, rational)` products "different types."

Special case: The empty product `()` is compatible with any zero-field product type.

### 4.3 Sum types (tagged unions)

Sum types are **nominal**.

When we declare:

```ddl
type V = (A of int | B)
```

we introduce a specific sum type `V` containing the constructors `A` and `B`.

* Calling `A(1)` constructs a value of *exactly* that sum type `V`.
* Calling `B()` also produces a value of the same `V`.

Two sum types are only compatible if they are literally the *same* declared sum type (same identity). We do **not** consider two sum types compatible just because they have constructors with the same names or shapes.

Example:

```ddl
type Shape = (Circle of int | Square of int)
type Slot  = (Circle of int | Square of int)
```

Values of `Shape` are not compatible with values of `Slot`, even though they look textually identical. `Shape.Circle` is not `Slot.Circle`.

This is different from products: products are structural; sums are nominal.

### 4.4 Function types

Two function types are compatible **structurally**:

* same number of parameters,
* each parameter type is pairwise compatible,
* return types are compatible.

Example:

* `(int) -> bool` is compatible with `(int) -> bool`.
* `(rational, bool) -> int` is only compatible with another `(rational, bool) -> int` (assuming `rational` is compatible with `rational` etc.).

Function types can appear anywhere types can appear (fields, payloads, aliases, etc.).

### 4.5 Primitives

`int`, `rational`, and `bool` are only compatible with themselves.
(If we later introduce `int -> rational` numeric promotion, that would become a special compatibility rule. For now, keep them strict.)

---

## 5. Pattern matching with `is`

In the AST, `expr is pattern` is represented as:

```fsharp
type PredicateBodyItem =
    | Call of PredicateCall
    | Expression of Expression
    | PatternMatch of Expression * Pattern   // <== expr is pattern
    | AlgebraicCondition of AlgebraicCondition
```

### Result type

`expr is pattern` always has type `bool`.

### Pattern typing

AST:

```fsharp
type Pattern =
    | Constant of ConstantValue * SourceRange
    | Tuple of Pattern list * SourceRange
    | Constructor of ConstructorPattern
    | Wildcard of SourceRange

and ConstructorPattern =
    { Constructor: string
      Arguments: Pattern list
      Range: SourceRange }
```

Typing rules:

1. `Constant(...)`

   * The pattern describes a specific primitive value.
   * The expression being matched must have a compatible primitive type (`int`, `rational`, `bool`).

2. `Tuple [p1; p2; ...; pk]`

   * The pattern describes a product type of arity `k`.
   * The expression being matched must be a product type with arity `k`.
   * Recursively check each sub-pattern against each field.

3. `Wildcard _`

   * Matches any type.
   * Places no constraints.

4. `Constructor { Constructor = "A"; Arguments = [p1; ...; pn] }`

   * Look up constructor `"A"` in the global constructor environment.

     * This uniquely identifies a particular sum type `S` (nominal).
   * The expression being matched must have type `S`.
   * If `"A"` has a payload type `T`:

     * The number/shape of `Arguments` must line up with that payload.
     * For example, if the payload is a product type `(f1:T1, f2:T2)`, then the pattern arguments must match arity 2 and recursively typecheck.
   * If `"A"` has no payload, then `Arguments` must be empty.
   * Any mismatch (wrong sum, wrong arity, etc.) is a type error.

Right now, patterns don't bind new names in the AST (e.g. `(x, y)` is not distinct from `(_, _)` yet). We may add that later by extending `Pattern`. For now, we treat patterns as purely structural tests that return `bool`.

---

## 6. `PredicateBody` (boolean logic layer)

The body of a predicate is modeled as:

```fsharp
type PredicateBodyItem =
    | Call of PredicateCall
    | Expression of Expression
    | PatternMatch of Expression * Pattern
    | AlgebraicCondition of AlgebraicCondition

and PredicateBody =
    | Item of PredicateBodyItem * SourceRange
    | Not of PredicateBody * SourceRange
    | And of PredicateBody * PredicateBody * SourceRange
    | Or  of PredicateBody * PredicateBody * SourceRange
    | Definition of PredicateDefinition * PredicateBody * SourceRange
```

**Rule:** Every `PredicateBody` must typecheck to `bool`.
That `bool` is the effective return type of the predicate.

Typing rules:

1. `Item item`:

   * `Call call`:

     * The call must typecheck, and it must have type `bool`.
   * `Expression e`:

     * `e` must have type `bool`.
   * `PatternMatch (expr, pat)`:

     * `expr is pat` is always `bool`.
   * `AlgebraicCondition cond`:

     * must typecheck (see below), and results in `bool`.

   If any of these produce a non-`bool` type, that's a type error.

2. `Not body`:

   * `body` must typecheck to `bool`.
   * The result is `bool`.

3. `And(left, right)` / `Or(left, right)`:

   * Both `left` and `right` must typecheck to `bool`.
   * The result is `bool`.

4. `Definition(def, rest)`:

   * This is like a local predicate definition (`let`-style).
   * Steps:

     * Add `def` to the local predicate environment (with a provisional signature).
     * Typecheck `def` itself (like a top-level predicate).
     * Then typecheck `rest` in that extended environment.
   * The result type of the `Definition` node is the type of `rest`, which must still be `bool`.

### Algebraic conditions

AST:

```fsharp
type AlgebraicCondition =
    { Condition: AlgebraicEqualityCondition   // e.g. Lt, Le, Eq, Ge, Gt, Ne
      LeftExpression: AlgebraicExpression
      RightExpression: AlgebraicExpression
      Range: SourceRange }

type AlgebraicExpression =
    | Expression of Expression
    | Operation of AlgebraicExpression * AlgebraicOperation * AlgebraicExpression * SourceRange
```

An `AlgebraicCondition` is something like `x = y`, `a < b`, etc.

Rules:

* The result type of an `AlgebraicCondition` is always `bool`.

Typechecking:

* Infer types of `LeftExpression` and `RightExpression`.
* Arithmetic operations `+ - * / %` require numeric types only (`int` or `rational`).

  * Both operands of an arithmetic operation must be compatible numeric types.
* For comparisons:

  * For `< <= > >=`, both sides must be numeric (same or compatible numeric type).
  * For `=` / `!=` (`Eq` / `Ne`), we can allow equality on non-numeric types in the future, but initially it's safe to restrict to numeric too.
* If the comparison doesn't make sense (e.g. `true < 5`), it's a type error.

---

## 7. Predicate type inference

### 7.1 Global environments

The type checker maintains:

* A **type environment**
  `Map<string, TypeDescription>`
  Maps `type Name = ...` aliases to their `TypeDescription`.
* A **constructor environment**
  `Map<string, (sumTypeName: string * payloadType: TypeDescription option)>`
  For each sum constructor name, tells us which sum type it belongs to and what payload (if any) it carries.
* A **predicate environment**
  `Map<string, FuncTypeDescription>` (or partially inferred equivalents).
  This is used both for external calls and recursion.

### 7.2 Inference for a `PredicateDefinition`

Steps:

1. For each parameter:

   * If it has an explicit type annotation (`Parameter.Type = Some TAnn`), resolve/normalize that.
   * If not, create a fresh type variable (`TVar`).
   * Add `(paramName -> that type)` to the local value environment.

2. Typecheck the `Body`:

   * It must ultimately evaluate to `bool`.
   * Recursive calls to the same predicate use the same parameter types (the same `TVar`s for unannotated params).

3. After typechecking `Body`, all parameter type variables should have been constrained enough to resolve concrete types. If any parameter's type is still completely unconstrained, that's an error ("unbound parameter type").

4. Build the function type `(p1Type, p2Type, ..., pnType) -> bool` as a `FuncTypeDescription`, and store it in `PredicateDefinition.PredicateType`.

---

## 8. Type annotations in expressions

Every `Expression` node has:

```fsharp
Annotation: TypeDescription option
```

corresponding to explicit syntax `expr: T`.

Rules:

* If `Annotation = Some TAnn`, then:

  * Infer the type of `expr`.
  * Check that it is compatible with `TAnn`.
  * If the inferred type was not fully known yet, `TAnn` can help fix it.

* After inference, we *may* choose to fill in missing annotations in the AST (e.g. rewrite `None` → `Some concreteType`) or maintain an external map from `(Range)` to inferred type.

The same applies to `Parameter.Type`: after inference we can backfill any `None` with the inferred type. This gives us a "fully typed" tree for later stages.

---

## 9. Type errors

The type checker should return something like:

```fsharp
Result<Program * TypedInfo, TypeError list>
```

`TypeError` should at minimum include variants like:

* `UnknownIdentifier(name, range)`
* `UnknownConstructor(name, range)`
* `FieldNotFound(fieldName, range)`
* `NotAProductType(range)`             // tried `a.b` on a non-product
* `ProductArityMismatch(expected, got, range)`
* `ProductFieldTypeMismatch(fieldIndex, expectedType, actualType, range)`
* `SumConstructorMismatch(constructor, expectedSum, actualType, range)`
* `WrongNumberOfConstructorArgs(constructor, expected, got, range)`
* `PredicateNotFound(name, range)`
* `PredicateArityMismatch(name, expected, got, range)`
* `ArgumentTypeMismatch(paramIndex, expectedType, actualType, range)`
* `PredicateBodyNotBool(range)`
* `LogicalOperandNotBool(range)`
* `ComparisonTypeMismatch(range)`
* `UnboundParameterType(paramName, range)` // could not infer param type
* `AnnotationMismatch(expected, actual, range)`

This list is not exhaustive but should cover most failure modes.

---

## 10. Internal type representation (`TypeExpr`)

For inference, we generally normalize `TypeDescription` into an internal representation like:

```fsharp
type TypeExpr =
    | TInt
    | TRational
    | TBool
    | TProduct of TypeExpr list                // product/tuple; order matters
    | TSum of SumId                            // nominal identity of a specific sum type
    | TFunction of TypeExpr list * TypeExpr    // (paramTypes..., returnType)
    | TVar of TyVarId                          // type variable for inference
```

Where:

* `SumId` uniquely identifies each declared sum type. (A simple approach is to use the sum's declared type name, assuming sum type names are unique.)
* `TProduct [...]` encodes product/tuple types structurally.
* `TFunction([...], ret)` encodes function signatures structurally.
* `TVar _` is used during inference/unification.

We also need helper functions:

* To resolve a `TypeDescription` (including `Named`) into a `TypeExpr`.
* To record sum constructors in `constructorEnv`, e.g.:

  * `Map["A"] = ("V", Some payloadType)`
  * `Map["B"] = ("V", None)`
    So we know `"A"` and `"B"` belong to sum `"V"`. That enforces nominal identity for sums.

We also need `compatible : TypeExpr -> TypeExpr -> bool`, roughly:

* Primitives must match exactly (`TInt` ↔ `TInt`, etc.).
* Products: same arity, pairwise compatible components.
* Functions: same arity, pairwise compatible params, compatible return type.
* Sums: same `SumId` only (nominal).
* `TVar` unifies with anything, recording that binding.
* Field names are *not* considered in structural compatibility checks; they’re only relevant for `a.b` lookup.

---

## 11. Summary of key properties

**Primitives**

* `int`, `rational`, `bool`

**Product types** (tuple/record-like)

* Structural typing.
* Two product types are compatible if:

  * they have the same number of fields,
  * each field type (by position) is compatible.
* Field names (`x`, `y`, etc.) are only for field access (`a.b`), not for deciding type equality.
* The empty tuple `()` is the unique 0-field product.

**Sum types** (tagged unions)

* Declared as named sets of constructors, e.g.:

  ```ddl
  type S = (A of int | B)
  ```
* Each constructor is owned by exactly one sum type.
* Sums are nominal:

  * Two sum types are not interchangeable even if they look textually identical.
  * A constructor always produces that specific sum type.
* Constructor calls act like value constructors: `A(1)`, `B()`.
* Patterns like `A(_)` or `B()` match specific variants.

**Function types**

* `(T1, ..., Tn) -> R` is a first-class type.
* Structural: same arity, pairwise compatible parameters, compatible return type.
* Predicate signatures are just a special case where `R = bool`.

**Predicates**

* Syntactically declared with named parameters.
* Body is a boolean formula (`PredicateBody`).
* No automatic tuple spreading: passing a product value still counts as a *single* argument.
* After inference, every predicate has a concrete function type `(paramTypes...) -> bool`.

**`expr is pattern`**

* Always returns `bool`.
* Typechecks by ensuring the value’s type can match the pattern’s shape:

  * constant literals,
  * tuples,
  * sum constructors (nominal),
  * wildcard `_`.

**Logical bodies**

* `PredicateBody` must evaluate to `bool`.
* `And`, `Or`, `Not` operate only on `bool`.
* Relational / arithmetic conditions must compare numeric expressions of compatible numeric type (currently `int` or `rational`).

**Structural vs nominal**

* Product types and function types are structurally typed.

  * `(x: rational, y: rational)` is compatible with `(a: rational, b: rational)`.
  * That means `(1: rational, 0: rational): Point` can be used where `Vec2` is expected, provided both aliases resolve to the same product shape.
* Sum types are nominal.

  * Constructors are not shared across sum aliases.
  * A value of `Shape` is not automatically a value of `Slot`, even if their variants happen to have the same names and payloads.

