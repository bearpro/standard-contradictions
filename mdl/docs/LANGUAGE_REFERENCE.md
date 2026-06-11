# MDL Language Reference

This document is the reference for the textual MDL language implemented by the
`mdl` package. It describes the concrete syntax accepted by the authoritative
ANTLR grammar, the static meaning checked by the linter/type inferencer, the
runtime behaviour of pure expressions, the temporal/deontic fragment translated
for solving, and the bundled standard library.

## 1. Overview

MDL is an indentation-sensitive, ML-inspired modelling language for normative
systems. A source file defines exactly one module and may contain:

- imports and opened modules;
- type declarations, including records and algebraic data types (ADTs);
- immutable values and pure functions;
- external entities, facts, temporal/deontic rules, and rule priorities;
- annotations attached to modules and top-level items.

The implementation deliberately separates three layers:

1. **Pure expression layer**: literals, arithmetic, booleans, records, tuples,
   functions, `let`, `if`, and `case`. This layer can be evaluated by `mdl run`.
2. **Temporal layer**: LTLf-style operators over boolean expressions and atoms:
   `always`, `eventually`, `next`, `initially`, `until`, and `last`.
3. **Deontic/rule layer**: defeasible DDL-LTLf rules with modalities `O`, `P`,
   and `F`, optional antecedents, priorities, strengths, and contrary-to-duty
   `otherwise` expressions.

## 2. Lexical structure

### 2.1 Encoding, whitespace, indentation

MDL source is plain text. Newlines are significant. Blocks following `func` and
some `case` arms may be written as a single expression or as an indented block.
The lexer emits `INDENT` and `DEDENT` tokens from leading whitespace, so nested
block contents must be indented deeper than the construct that introduces them.
Spaces and tabs inside a line are otherwise insignificant separators.

### 2.2 Comments

A `#` starts a line comment that continues to the end of the line:

```mdl
# ignored by the parser
let answer = 42  # also ignored
```

### 2.3 Identifiers and qualified names

An identifier starts with an ASCII letter or underscore and then contains ASCII
letters, digits, underscores, or apostrophes:

```text
[A-Za-z_] [A-Za-z0-9_']*
```

A qualified name is one or more name tokens separated by dots:

```mdl
std.collections.List.Cons
```

The grammar also permits the tokens `true`, `false`, `last`, `O`, `P`, and `F`
where a name token is expected. They keep their literal/modal meaning in
expression or rule positions, but can still appear inside qualified names where
the grammar needs a name component.

### 2.4 Keywords

The following words are reserved by the grammar:

```text
module import open type let func entity rule strict defeasible defeater
override fact if then else case when in true false last and or implies not
always eventually next initially until otherwise O P F
```

### 2.5 Operators and punctuation

```text
-> <= >= != ( ) { } , : . | + - * / % = < >
```

`=` is used both for equality expressions and for bindings/field assignments,
depending on syntactic context.

### 2.6 Literals

| Literal  | Syntax          | Static type | Runtime value                                       |
| -------- | --------------- | ----------- | --------------------------------------------------- |
| String   | `"..."`         | `string`    | Python `str`; escapes are interpreted by the parser |
| Integer  | `123`           | `int`       | Python `int`                                        |
| Decimal  | `12.34`         | `decimal`   | Python `float`                                      |
| Rational | `3/7`           | `rat`       | `fractions.Fraction`                                |
| Boolean  | `true`, `false` | `bool`      | Python `bool`                                       |
| Unit     | `()`            | `unit`      | `None` in the point-wise runtime                    |
| Last     | `last`          | `bool`      | temporal proposition for the final trace position   |

String escapes use the implementation's normal escape decoder. Common escapes
such as `\n`, `\t`, `\r`, `\"`, and `\\` are supported.

## 3. File and module structure

Every MDL file has this shape:

```mdl
@optional module annotation
module my.module.Name

@optional item annotation
import "std/collections.mdl"
open std.collections

# declarations follow
```

The first non-annotation construct must be `module <qualifiedName>`. Top-level
items may then appear in declaration order. Name checking is declaration-order
aware: most references must be visible at their use site, while self-recursive
function bodies are supported.

### 3.1 Annotations

Annotations begin with `@` and continue to the end of the line:

```mdl
@source article-7
@severity high
rule O retain_logs: always logs_retained
```

The parser stores the text after `@`, trimmed of surrounding whitespace.
Annotations are preserved on modules, imports, opens, declarations, rules,
priorities, and facts. MDL itself does not assign built-in semantics to
annotation payloads.

### 3.2 Imports

```mdl
import "relative/path.mdl"
```

An import path is a string literal. The resolver looks for imports relative to
the importing document, in the in-memory document set used by editor/LSP calls,
and in the configured standard-library root. Standard-library imports use paths
such as:

```mdl
import "std/collections.mdl"
import "std/system/strings.mdl"
```

### 3.3 Open declarations

```mdl
open std.collections
```

`open` makes names from a module available without writing their full module
prefix where the linter/resolver supports that visibility. Fully qualified names
remain valid even without `open`.

## 4. Types

### 4.1 Primitive types

The implementation recognises these primitive type names:

| Type      | Meaning                                               |
| --------- | ----------------------------------------------------- |
| `unit`    | Unit/no-information value, written `()`               |
| `bool`    | Boolean values and temporal propositions              |
| `int`     | Integer numbers                                       |
| `rat`     | Exact rational numbers                                |
| `decimal` | Decimal literals represented by the runtime as floats |
| `string`  | Text strings                                          |

### 4.2 Type references and type arguments

A type reference is a qualified name with optional type arguments:

```mdl
int
std.collections.List<string>
Map<string, int>
```

Generic type arguments are written in angle brackets and are separated by
commas.

### 4.3 Tuple types

```mdl
(int, string)
(int, string, bool)
```

Parentheses around a single type are grouping only:

```mdl
(int)  # same as int
```

### 4.4 Record types

Record types are structural field sets:

```mdl
type Address = { city: string, zip: int }
type Person = { name: string, address: Address }
```

Fields are named, separated by commas, and each field has a type expression.
Record field names must be unique within the record.

### 4.5 Algebraic data types

ADTs are declared with one or more variants separated by `|`:

```mdl
type Color = Red(unit) | Green(unit) | Blue(unit)
type Result<T> = Ok(value: T) | Error(message: string)
type Pair<T, U> = Pair(left: T, right: U)
```

Each variant has a constructor name and a parenthesised list of fields. Fields
may be named or positional:

```mdl
type Shape = Circle(radius: int) | Segment(int, int)
```

Constructors are called as functions. A constructor whose only payload is `unit`
can be called and pattern-matched with empty parentheses:

```mdl
Color.Red()
```

### 4.6 Type parameters

Types and functions can declare type parameters:

```mdl
type Box<T> = { value: T }
func identity<T>(x: T) -> T: x
```

Type parameters are scoped to the declaration. Generic functions are
instantiated at call sites by the type inferencer.

## 5. Declarations

### 5.1 Value declarations

```mdl
let name = expr
let name: Type = expr
```

A top-level value is immutable. In the runtime, top-level values are evaluated
before facts are applied. Type annotations are optional; when present, the value
must conform to the annotation.

### 5.2 Function declarations

```mdl
func name<T>(param: Type, {field}: Rec) -> ReturnType:
    let local = expr
    result
```

Functions are pure and have:

- a name;
- optional type parameters;
- zero or more typed parameters;
- an explicit return type;
- either a single-expression body or an indented block.

Parameters are patterns, not only variable names:

```mdl
func fst<T, U>((x, _): (T, U)) -> T: x
```

The runtime checks arity, binds parameter patterns, evaluates local `let`
statements in order, and returns the final expression. Function bodies may call
the same function recursively.

### 5.3 Entity declarations

```mdl
entity email: string
entity account: { id: string, active: bool }
```

An entity declares an externally supplied symbol used by facts, rules, solving,
and runtime evaluation. The point-wise runtime initialises entity values to
`None` until a fact assigns a value or the caller supplies a value.

### 5.4 Fact declarations

```mdl
fact condition
fact entity_name = value
```

A fact with a target assigns the evaluated value to that target in the runtime.
A fact without a target is evaluated and stored in the runtime's facts list. In
translation/solving, facts are emitted as model facts.

### 5.5 Rule declarations

Rules describe temporal/deontic obligations, permissions, prohibitions, and
supporting defeasible reasoning.

#### Named rule form

```mdl
rule O retain_logs when account.active: always logs_retained
strict rule P access_allowed: user_authenticated
```

Grammar:

```text
[strength] rule [modality] rule_name [when antecedent] : body [otherwise expr]
```

- `rule_name` is a qualified name.
- `modality` is optional in the named form and is one of `O`, `P`, `F`.
- `when` introduces an antecedent. If omitted, the rule is unconditional.
- `body` is usually a boolean/temporal expression.
- `otherwise` encodes a contrary-to-duty or fallback expression.

#### Anonymous modality form

```mdl
rule O: always valid_email
```

Grammar:

```text
[strength] rule modality : body [otherwise expr]
```

This form has no explicit name. The parser assigns an internal generated name
based on the source line and marks the rule as anonymous.

#### Rule strengths

| Strength   | Syntax                | Default? | Meaning in defeasible reasoning                                        |
| ---------- | --------------------- | -------- | ---------------------------------------------------------------------- |
| Strict     | `strict rule ...`     | no       | Non-defeasible rule                                                    |
| Defeasible | `defeasible rule ...` | yes      | Normal overridable rule                                                |
| Defeater   | `defeater rule ...`   | no       | Rule that can defeat another rule without deriving the same conclusion |

The concrete conflict-resolution semantics is implemented by the translator and
solver rather than by the point-wise runtime.

#### Deontic modalities

| Modality | Name        | Informal reading       |
| -------- | ----------- | ---------------------- |
| `O`      | Obligation  | the body ought to hold |
| `P`      | Permission  | the body is permitted  |
| `F`      | Prohibition | the body is forbidden  |

### 5.6 Priority declarations

```mdl
override specific_rule > general_rule
override highest > middle > lowest
```

A priority declaration is a chain of rule names. Earlier rules in the chain
override later rules. The declaration is used by consistency/solver tooling.

## 6. Expressions

### 6.1 Precedence and associativity

From tightest to loosest:

| Level | Operators/forms     | Associativity/notes                                                  |
| ----- | ------------------- | -------------------------------------------------------------------- |
| 1     | primary expressions | literals, names, tuples, grouping                                    |
| 2     | postfix             | function call, field access, record construction                     |
| 3     | unary               | `not`, unary `-`; special expressions `if`, `let`, `case` enter here |
| 4     | multiplicative      | `*`, `/`, `%` left-associative                                       |
| 5     | additive            | `+`, `-` left-associative                                            |
| 6     | comparison          | `=`, `!=`, `<`, `<=`, `>`, `>=`; chains parse left-to-right          |
| 7     | temporal binary     | `until` left-associative                                             |
| 8     | conjunction         | `and` left-associative                                               |
| 9     | disjunction         | `or` left-associative                                                |
| 10    | implication         | `implies` right-associative                                          |
| 11    | temporal postfix    | repeated `always`, `eventually`, `next`, `initially` suffixes        |

Temporal unary operators are postfix in the grammar:

```mdl
valid always
(valid implies accepted) eventually
```

The formatter/printer may render temporal unary nodes in its canonical form, but
accepted concrete syntax is postfix.

### 6.2 Primary expressions

```mdl
"text"
123
12.5
1/3
true
false
last
name
qualified.name
()
(expr)
(a, b, c)
```

A parenthesised single expression is grouping. Two or more comma-separated
expressions form a tuple. `()` is unit.

### 6.3 Function calls

```mdl
f()
f(x, y)
std.system.strings.to_list("abc")
```

Calls use postfix parentheses. Arity is checked for known functions and
constructors.

### 6.4 Field access

```mdl
person.name
person.address.city
```

Field access projects a field from a record value. The type checker validates
known record fields. At runtime records are represented as dictionaries.

### 6.5 Record construction

```mdl
Person { name = "Ada", age = 36 }
```

The expression before `{...}` must be a name. For declared record types, the
linter checks duplicate fields, unknown fields, and missing fields.

### 6.6 Arithmetic and numeric coercion

```mdl
1 + 2 * 3
x / y
n % 2
```

`+`, `-`, `*`, `/`, and `%` require numeric operands (`int`, `rat`, or
`decimal`). Comparisons `<`, `<=`, `>`, and `>=` also require numeric operands.
Mixing numeric kinds is accepted with a `numeric-coercion` warning. The inferred
result type for `/` is `rat`; other arithmetic returns `rat` if either operand
is `rat` or `decimal`, otherwise `int`.

Runtime arithmetic delegates to Python operators. Decimal literals are Python
floats and rational literals are exact `Fraction` values.

### 6.7 Equality and comparison

```mdl
x = y
x != y
x < y
```

`=` and `!=` return `bool`. The type checker unifies comparable values or emits
a type mismatch. Numeric ordering operators return `bool`.

### 6.8 Boolean operators

```mdl
not done
ready and enabled
failed or timeout
request implies response
```

`not`, `and`, `or`, and `implies` require boolean operands and return `bool`.
Runtime implication is material implication: `a implies b` is equivalent to
`(not a) or b`.

### 6.9 Conditional expressions

```mdl
if condition then expr1 else expr2
```

The condition must be `bool`. Both branches must have a common type. Numeric
branches of different numeric types may coerce with a warning.

### 6.10 Let expressions

```mdl
let pattern: Type = value in body
let pattern = value
in body
```

A `let` expression evaluates `value`, binds it to `pattern`, and evaluates
`body` in the extended environment. Type annotations are optional. Pattern
bindings in `let` expressions are polymorphically generalised by the type
inferencer.

### 6.11 Blocks

A block can be a single expression:

```mdl
func inc(x: int) -> int: x + 1
```

or an indented sequence of local `let` statements followed by an optional result
expression:

```mdl
func area(w: int, h: int) -> int:
    let product = w * h
    product
```

Local `let` statements use declaration syntax and must end before the final
result expression. If a block has no result expression, it evaluates as unit in
the type system and as `None` in the runtime.

### 6.12 Case expressions

```mdl
case value:
    | Pattern: result
    | Pattern when guard: result
```

A `case` expression evaluates the subject, tries arms in order, and returns the
first arm whose pattern matches and whose optional guard evaluates to `true`. If
no arm matches at runtime, evaluation fails with a non-exhaustive-match runtime
error. The linter checks guard expressions as booleans and computes a common
result type for all arm bodies.

Case arms may be followed by a single-expression body or an indented block:

```mdl
case xs:
    | List.Empty(): 0
    | List.Cons(head, tail):
        let rest = len(tail)
        1 + rest
```

## 7. Patterns

Patterns are used in function parameters, local `let` bindings, and `case` arms.

### 7.1 Wildcard

```mdl
_
```

Matches anything and binds nothing.

### 7.2 Variable pattern

```mdl
name
```

Binds the matched value to `name`.

### 7.3 Literal patterns

```mdl
"ok"
0
3.14
1/2
true
false
()
```

Match exactly equal literal values.

### 7.4 Constructor patterns

```mdl
Result.Ok(value)
List.Cons(head, tail)
List.Empty()
```

Constructor patterns match ADT values. Unit constructors may be matched with no
arguments. Known constructors are type-checked for arity and payload type.

### 7.5 Tuple patterns

```mdl
(a, b)
(_, second, third)
```

Match tuples of the same length.

### 7.6 Record patterns

```mdl
{ name, age = 18 }
{ left = x, right = _ }
```

A record pattern matches dictionaries/records containing the listed fields. A
field without `= pattern` binds a variable with the same name as the field. A
field with `= pattern` matches that nested pattern.

## 8. Temporal expressions

Temporal expressions are boolean expressions interpreted over finite traces by
the translator and solver.

| Syntax         | Informal LTLf meaning                        |
| -------------- | -------------------------------------------- |
| `p always`     | `p` holds at all future positions            |
| `p eventually` | `p` holds at some current-or-future position |
| `p next`       | `p` holds at the next position               |
| `p initially`  | `p` is evaluated at the initial position     |
| `p until q`    | `p` holds until `q` holds                    |
| `last`         | current position is the final trace position |

The point-wise runtime does not model traces. It evaluates temporal unary nodes
by evaluating their operand at the current point and rejects temporal binary
operators such as `until`.

## 9. Solving model

`mdl solve` collects imported modules and bundled standard-library modules, then
builds a bounded Z3 problem over a finite horizon. The solver supports the
temporal/deontic fragment used by the test suite and examples.

## 10. Runtime model

`mdl run <file> --expr '<expr>'` evaluates a pure expression in a deterministic,
point-wise runtime:

- top-level functions are callable by simple or qualified local names;
- top-level `let` values are evaluated before facts;
- entity declarations create mutable runtime slots initially set to `None`;
- targeted facts assign runtime values to entities or other names;
- untargeted facts are evaluated and stored;
- records are dictionaries;
- tuples are Python tuples;
- standard collection constructors have native Python representations:
  - `List.Empty()` -> `[]`, `List.Cons(x, xs)` -> `[x, *xs]`;
  - `Set.Empty()` -> `set()`, `Set.Insert(x, xs)` -> set insertion;
  - `Map.Empty()` -> `{}`, `Map.Put(k, v, m)` -> updated dictionary;
  - `Option.None()` -> `"None"`, `Option.Some(x)` -> `("Some", (x,))`.

The runtime is intentionally not a full temporal model checker. Use `mdl solve`
for temporal/deontic consistency checks.

## 11. Standard library

The bundled standard library is stored under `mdl/src/mdl/stdlib` and is
available to import with `std/...` paths.

### 11.1 `std.collections`

Import path:

```mdl
import "std/collections.mdl"
open std.collections
```

Module declaration:

```mdl
module std.collections
```

Definitions:

```mdl
type List<T> = Empty(unit) | Cons(T, List<T>)
type Set<T> = Empty(unit) | Insert(T, Set<T>)
type Map<K, V> = Empty(unit) | Put(K, V, Map<K, V>)
type Option<T> = None(unit) | Some(T)

func len<T>(l: List<T>) -> int:
    case l:
        | List.Empty(): 0
        | List.Cons(head, tail): 1 + len(tail)
```

Notes:

- `List<T>` is the canonical recursive list ADT.
- `Set<T>` and `Map<K, V>` are ADTs in the language; the runtime gives their
  constructors native Python `set`/`dict` behaviour.
- `Option<T>` is either `None(unit)` or `Some(T)`.
- `len` computes the length of a standard `List<T>` recursively.

### 11.2 `std.system.strings`

Import path:

```mdl
import "std/system/strings.mdl"
open std.system.strings
```

Module declaration:

```mdl
module std.system.strings
```

Definitions:

```mdl
func to_list(value: string) -> std.collections.List<string>:
    std.collections.List.Empty()
```

The declared MDL body is a portable placeholder. The runtime and solver
recognise `to_list`, `strings.to_list`, and `std.system.strings.to_list` as
built-ins. At runtime, `to_list("abc")` returns a list of one-character strings.
In the typed language, the function returns `std.collections.List<string>`.

## 12. Complete grammar summary

This section mirrors the accepted grammar in compact EBNF-like notation. Token
names are shown as lowercase keywords or symbolic punctuation where possible.

```text
program        ::= newlines? annotations moduleDecl newlines? topItem* EOF
topItem        ::= annotations (importDecl | openDecl | declaration) newlines?
annotations    ::= annotation*
moduleDecl     ::= "module" qualifiedName
importDecl     ::= "import" STRING
openDecl       ::= "open" qualifiedName

declaration    ::= typeDecl | valueDecl | funcDecl | entityDecl
                 | ruleDecl | priorityDecl | factDecl

typeDecl       ::= "type" name typeParams? "=" typeDefinition
typeDefinition ::= recordType | variant ("|" variant)*
typeParams     ::= "<" name ("," name)* ">"
variant        ::= name "(" variantField ("," variantField)* ")"
variantField   ::= name ":" typeExpr | typeExpr

typeExpr       ::= recordType | tupleOrParenType | typeRef
recordType     ::= "{" (name ":" typeExpr ("," name ":" typeExpr)*)? "}"
tupleType      ::= "(" typeExpr ")" | "(" typeExpr "," typeExpr ("," typeExpr)* ")"
typeRef        ::= qualifiedName ("<" typeExpr ("," typeExpr)* ">")?

valueDecl      ::= "let" name typeAnnotation? "=" expr
funcDecl       ::= "func" name typeParams? "(" paramList? ")" "->" typeExpr ":" block
param          ::= pattern ":" typeExpr
entityDecl     ::= "entity" name ":" typeExpr
factDecl       ::= "fact" (name "=")? expr
priorityDecl   ::= "override" qualifiedName (">" qualifiedName)*

ruleDecl       ::= ("strict" | "defeasible" | "defeater")? "rule" ruleBody
                   ("otherwise" expr)?
ruleBody       ::= modality ":" expr
                 | modality? qualifiedName ("when" expr)? ":" expr
modality       ::= "O" | "P" | "F"

block          ::= NEWLINE INDENT blockLetStmt* expr? newlines? DEDENT | expr
blockLetStmt   ::= "let" pattern typeAnnotation? "=" expr newlines
typeAnnotation ::= ":" typeExpr

expr           ::= implication temporalUnaryOp*
implication    ::= orExpr ("implies" implication)?
orExpr         ::= andExpr ("or" andExpr)*
andExpr        ::= temporalBinary ("and" temporalBinary)*
temporalBinary ::= comparison ("until" comparison)*
comparison     ::= additive (("=" | "!=" | "<" | "<=" | ">" | ">=") additive)*
additive       ::= multiplicative (("+" | "-") multiplicative)*
multiplicative ::= unary (("*" | "/" | "%") unary)*
unary          ::= ifExpr | letExpr | matchExpr | ("not" | "-") unary | postfix
ifExpr         ::= "if" expr newlines? "then" expr newlines? "else" expr
letExpr        ::= "let" pattern typeAnnotation? "=" expr newlines? "in" expr
matchExpr      ::= "case" expr ":" caseBody
caseArm        ::= "|" pattern ("when" expr)? ":" block
postfix        ::= primary postfixSuffix*
postfixSuffix  ::= "{" recordFields? "}" | "(" exprList? ")" | "." name
recordField    ::= name "=" expr
primary        ::= STRING | INT | DECIMAL | RAT | "true" | "false" | "last"
                 | qualifiedName | "(" ")" | "(" expr ")"
                 | "(" expr "," expr ("," expr)* ")"

pattern        ::= "_" | STRING | INT | DECIMAL | RAT | "(" ")"
                 | "(" pattern ")" | "(" pattern "," pattern ("," pattern)* ")"
                 | "{" recordPatternFields? "}"
                 | qualifiedName ("(" patternList? ")")?
recordPattern  ::= name ("=" pattern)?
qualifiedName  ::= name ("." name)*
```

## 13. Examples

### 13.1 Pure function with ADT matching

```mdl
module example.lists

import "std/collections.mdl"
open std.collections

func sum(xs: List<int>) -> int:
    case xs:
        | List.Empty(): 0
        | List.Cons(head, tail): head + sum(tail)
```

### 13.2 Record model and rule

```mdl
module example.policy

type Account = { id: string, active: bool }
entity account: Account
entity logs_retained: bool

rule O retain_logs when account.active: logs_retained always
```

### 13.3 Contrary-to-duty rule and priority

```mdl
module example.ctd

entity request_valid: bool
entity request_logged: bool
entity incident_reported: bool

rule O log_request when request_valid: request_logged always otherwise incident_reported eventually
strict rule F skip_logging: not request_logged
override log_request > skip_logging
```
