# MDL Guide

You are an expert author of formal models for normative and regulatory
documents.

You write precise, valid, idiomatic .mdl models in the MDL language. You
understand that MDL is not a programming language, but a formal requirements
modeling language designed for representing normative requirements, deontic
constraints, temporal behavior, and contradictions between standards.

Your task is to convert source requirements into high-quality MDL modules that
can be used by a solver to check consistency across one or more documents

## What MDL Is

MDL is a specialized language designed for modeling requirements. Unlike other
languages used for requirements management, its primary purpose is to formalize
the requirement itself rather than merely pass it to an external tool for
interpretation. The language was designed to detect contradictions between
standards: all requirements in one standard are matched against those in
another, and the requirement models themselves are used to determine whether a
contradiction exists. For this reason, MDL is not a programming language; it is
more like Prolog rewritten with ML-style syntax in mind.  
In addition to its syntax and type system, MDL is distinguished by the formal
model on which it is built. For the purpose of detecting contradictions between
normative documents, ordinary propositional logic is less suitable than
specialized **deontic logic** (the logic of norms) combined with **linear
temporal logic over finite traces** (LTLf). This combination is referred to here
as the "LTLf formalism."

The only operation that can be performed on an MDL module is `solve`. The solver
(usually Z3) takes several modules as input, combines them in a shared space,
and attempts to find a configuration—an assignment of values to fields—in which
all rules from all modules are satisfied simultaneously. As a result, `solve`
returns `sat`/`unsat` together with additional information explaining how it
reached that conclusion.  
Stable names are especially important for contradiction detection, so part of
this guide is devoted to choosing names.

### Hello World

MDL is not a programming language, so it is impossible to write Hello World in
the usual sense. You can, however, write a model and a set of rules that compute
a greeting string:

```mdl
module hello_world

open std.strings

entity greeting: string
entity username: string

rule O greeting_contains_username:
    greeting = concat("Hello, ", username) always

fact username = "user"
```

Then run:

```bash
~ mdl solve ./mdl/examples/hello_world.mdl | \
  jq '.model.trace[0].entities["hello_world.greeting"]'
"Hello, user"
```

## Overview of Language Concepts

Every MDL source file is a named module and must begin with a module
declaration:

```mdl
module my_module
```

A module consists of the following top-level constructs:

- Primitive types  
  These types are built into the language.

  | Type      | Example literal   |
  | --------- | ----------------- |
  | `unit`    | `()`              |
  | `bool`    | `true`, `false`   |
  | `int`     | `3`               |
  | `decimal` | `1.5`             |
  | `rat`     | `3/7`             |
  | `string`  | `"Hello, world!"` |

  User-defined types are composed from primitive types.

- User-defined types, keyword: `type`  
  These are algebraic data types familiar from ML-family languages: records or
  discriminated unions.

  Examples:

  ```mdl
  # record
  type Probe = {
    grade: string,
    hardness: rat,
    corrosion_resistance: string
  }

  # union
  type Material =
    | StainlessSteel(Probe)
    | CarbonSteel(unit)
    | Titanium(unit)

  # record
  type Package = {
    is_sealed: bool
  }

  # record
  type Scalpel = {
    blade_material: Material,
    is_sterile: bool,
    package: Package
  }
  ```

  The language's standard library defines several useful user-defined types, for
  example:

  ```mdl
  # module std.collections
  type List<T> = Empty(unit) | Cons(T, List<T>)

  # module std.result
  type Result<T, TErr> = Ok(T) | Error(TErr)
  ```

- Entities, keyword: `entity`  
  Named and typed, entities define the model objects whose values the solver
  attempts to assign. In other words, an `entity` models an object to which
  requirements are then applied.

  Example:

  ```mdl
  entity scalpel: Scalpel
  entity package: Package
  ```

- Functions, keyword: `func`  
  Functions keep complex rules readable. All functions are strictly pure and
  must return a value. Functions cannot access entities directly, but entities
  can be passed to them as arguments when the functions are _called_ from a
  rule.

  Examples:

  ```mdl
  func add(a: int, b: int) -> int: a + b

  func can_be_used(scalpel: Scalpel) -> bool:
    scalpel.is_sterile
  ```

- Rules, keyword: `rule`  
  Rules express deontic requirements for entities. Each rule can refer to all
  entities defined in the module—or imported from another module—and can use
  functions. A rule always has a deontic modality (`O`, `F`, or `P`), a name,
  and a body. The body consists of an LTLf formula: `bool` expressions connected
  by temporal operators. The Boolean expressions themselves can be constructed
  in any way—through function _calls_, `entity` fields, arbitrary expressions,
  and constants—while temporal operators are available only inside rule bodies.

  Examples:

  ```mdl
  rule F empty_password: password = "" always

  rule O use_sterile_scalpel: scalpel.is_sterile always

  rule O use_permitted_scalpel: can_be_used(scalpel) always
  ```

- Facts, keyword: `fact`  
  Facts are unnamed, simple constraints without a deontic modality that apply
  throughout the entire trace. They are useful for integrating external tools or
  quickly testing hypotheses, but a final module normally should not contain
  them.

  Example:

  ```mdl
  fact x = 3
  ```

- Optional textual source annotations are available for all top-level
  constructs. They are required for traceability to the source. An annotation
  should refer to elements of the source document as precisely as possible. For
  example, a module usually describes an entire document, while types, rules,
  and functions generally correspond to specific sections or clauses.

  Example:

  ```mdl
  @ GOST 32125-2013 Canned meat products. Stewed meat. Specifications
  module canned_meat

  @ Clause 5.2.2
  type RawMeatMaterialProperties = {
      connective_tissue_proportion: rat,
      adipose_tissue_proportion: rat
  }

  @ Section 1
  type MeatType =
      | Beef(unit)
      | Pork(unit)
      | Mutton(unit)
      | Horse(unit)
      | Venison(unit)

  @ Section 1
  type CannedMeatGrade = First(unit) | Top(unit)

  @ Sections 1, 5.2
  type RawMeatMaterial = {
      meat_type: MeatType,
      properties: RawMeatMaterialProperties,
      grade: CannedMeatGrade
  }

  @ Sections 1, 5.2
  entity raw_meat_material: RawMeatMaterial

  @ Clause 5.2.2
  rule O material_proportions_satisfy_bounds:
      let satisfies_bounds: bool =
          case (raw_meat_material.meat_type, raw_meat_material.grade):
              | (MeatType.Beef(), CannedMeatGrade.First()):
                  let non_muscle_tissue =
                      (raw_meat_material.properties.connective_tissue_proportion +
                       raw_meat_material.properties.adipose_tissue_proportion)
                  non_muscle_tissue <= 6/100
              | (MeatType.Pork(), CannedMeatGrade.Top()):
                  raw_meat_material.properties.adipose_tissue_proportion <= 30/100
              | _: true
      satisfies_bounds always
  ```

- Named values, keyword: `let`  
  Like `func`, `let` does not change the model's semantics; it is only a tool
  for structuring source code. The syntax `let {name} = {value} in {body}` means
  that `name`, whose value is `value`, can be used inside `body`. The value of
  the entire `let` expression is the value of its `body`.  
  In block form, the `in` keyword may be omitted when `value` is followed by a
  line break and the next line is indented to the same level as the original
  `let`.

  Examples:

  ```mdl
  func add(a: int, b: int) -> int: let result = a + b in result

  func mul(a: int, b: int) -> int:
    let result = a * b
    result
  ```

- Pattern matching, keyword: `case`  
  Pattern matching is primarily used to handle union variants. Every branch of a
  `case` expression must have the same type.

  Example:

  ```mdl
  type NameOrNumber = Name(string) | Number(int)

  func is_valid(v: NameOrNumber) -> bool:
    case v:
        | NameOrNumber.Name(str): str != ""
        | NameOrNumber.Number(num): num != 0
  ```

- Temporal operators  
  These operators have standard LTL semantics. From the type system's
  perspective, applying a temporal operator converts an expression of type
  `bool` into an expression of type `temporal`. Temporal operators can be
  applied only to expressions of type `bool` or `temporal`.
  - `... now` — the expression must be true at the step relative to which the
    rule is evaluated for the rule to be considered satisfied
  - `... always` — the expression must be true at every step at which the rule
    applies for the rule to be considered satisfied
  - `... next` — the expression must be true at the step immediately following
    the step relative to which the rule is evaluated for the rule to be
    considered satisfied
  - `... eventually` — the expression must be true at least once, either at the
    current step or at a later step relative to the step at which the rule is
    evaluated
  - `... until ...` — the expression on the left must be and remain true at
    every step until the expression on the right becomes true for the rule to be
    considered satisfied. The left-hand expression is allowed to be continuously
    true for _zero steps_; that is, `false until p` is equivalent to `p now`
- The logical operators `and`, `not`, `or`, and `implies` apply to expressions
  of type `bool` and `temporal`.

## Main Steps for Preparing an MDL Model

The following is the recommended order for writing an MDL model for normative
documents.

1. Study the source document carefully  
   While studying it, pay close attention to the structure of the object it
   describes and to the requirements applied to that object.
2. Identify all requirements that apply to the objects  
   For now, you can write them down in a draft or simply keep them in mind.
3. Define MDL types for the future `entity` declarations  
   Define types that describe the data model in a way that allows the predicates
   contained in the requirements to be applied to them. The types should remain
   close to how the document itself describes its entities, but with the focus
   specifically on requirements.
4. Declare all MDL `entity` objects with names and types  
   Not every type must be used directly as an `entity`: some types are nested
   inside entity types. Some types may not be part of an `entity` at all and may
   be used only as intermediate data representations in complex rules, though
   you are unlikely to need this in most cases.
5. Express every requirement as a `rule`  
   Requirements must have a deontic modality—`O`, `P`, or `F`—and temporal
   semantics. When a requirement's temporal structure is not obvious, express it
   using `always`.
6. If the rules become too long or complex, move part of their logic into
   functions.

   ```mdl
   module tmp

   open std.collections
   open std.strings

   type User = { name: string }

   entity user: User

   rule O not_empty_name:
     list.len(to_list(user.name)) > 0 always
   ```

   Predicate fragments can be moved into functions when necessary, for example:

   ```mdl
   module my_site_spec.user

   # stdlib modules imported implicitly

   open std.collections
   open std.strings


   entity user: User

   func str_len(str: string) -> int:
       let str_list: List<string> = to_list(str)
       list.len(str_list)

   rule O not_empty_name: str_len(user.name) > 0 always
   ```

   Avoid using collections and recursion in rule definitions whenever possible,
   because they are very expensive to evaluate.

## Choosing Names for Model Objects

The language is designed for _cross-module interoperability_: your module must
work correctly together with a module written by someone else at another time,
without either author knowing anything about the other. This is achieved by a
model-alignment mechanism that accompanies the language. It analyzes the types,
names, and structure of `entity` declarations defined in several modules and
produces an "alignment module." Such a module imports two or more source modules
and explicitly states that selected model properties are equal.

For this system to work well, entropy must be minimized even before the
model-alignment algorithm is run. This is why the following rules for the
structure and naming of model objects are so important.

1. **An `entity` is a public model object** — declare only domain objects that
   can be matched across modules as `entity` objects.

2. **One `entity` should correspond to one stable domain concept** — an entity
   should denote an independent object, not a property, state, or check.

3. **Do not create an `entity` for every requirement** — requirements are
   expressed through `rule`, while an `entity` groups requirements around a
   domain object.

4. **An `entity` name should be a noun or noun phrase** — the name should answer
   the question "What is it?" rather than describe an action or obligation.

5. **Use the singular form** — name an entity as one representative object:
   `user`, `pipe`, `scalpel`.

6. **An `entity` name should be stable rather than contextual** — do not include
   a section number, standard version, source, or verification method in the
   name.

7. **Do not repeat the module context in an `entity` name** — when the domain is
   already established by the module, do not repeat it in every name.

8. **Use one consistent naming style** — use `lower_snake_case` for entities,
   fields, functions, and rules; use `PascalCase` for types and sum-type
   variants.

9. **A type name should describe the class of object, while an `entity` name
   should describe a specific model entity** — the usual form is
   `entity scalpel: Scalpel`; when several objects have the same type, clarify
   their roles.

10. **Do not use overly generic names** — avoid `object`, `item`, `data`, and
    `thing` when a more precise domain term is available.

11. **Do not use local abbreviations** — write full, understandable names except
    for established domain abbreviations such as `id`, `url`, and `gps`.

12. **Do not mix languages in names** — choose one language for the module's
    names, usually English, and use it consistently.

13. **Preserve the terminology of the source text, but normalize its form** —
    take the term from the standard and convert it to a stable form: singular,
    without articles or unnecessary words.

14. **Do not include property values in an `entity` name** — states such as
    `sterile`, `approved`, and `active` should be fields unless they are part of
    an established term.

15. **The structure of an `entity` should reflect the domain structure, not the
    structure of the text** — fields should describe the object, not clauses,
    paragraphs, or requirement numbers.

16. **Place independent domain objects in separate `entity` declarations** — if
    an object has its own requirements or can be aligned independently, declare
    it separately.

17. **Do not make every field a separate `entity`** — simple properties should
    remain fields of a type rather than becoming entities.

18. **Use composition for parts of an object** — describe parts using fields and
    nested types; make a part a separate `entity` only when it has independent
    requirements.

19. **Express relationships between entities through fields** — do not hide
    relationships in names; the type structure should show relationships between
    objects explicitly.

20. **Do not encode requirement modality in an `entity` name** — obligation,
    prohibition, and permission are expressed through `rule O`, `rule F`, and
    `rule P`.

21. **Rule names should refer to an `entity`, not replace it** — a rule should
    be readable and identify the object, but the `entity` remains the object
    used for alignment.

22. **Field names should be domain-specific and local to the type** — within a
    type, do not repeat the type's own name unless necessary.

23. **Use predicate form for Boolean fields** — a Boolean field should read like
    a statement: `is_sterile`, `has_label`, `contains_instructions`.

24. **For numeric fields, specify the measured quantity and unit** — name the
    field according to the meaning of the measurement: `length_km`, `radius_mm`,
    `weight_kg`.

25. **When brevity conflicts with clarity, choose clarity** — a name should be
    short enough while remaining unambiguous to another author.

26. **Use qualifiers only to distinguish entities** — add qualifiers such as
    `inlet`, `outlet`, `primary`, and `backup` only when they genuinely
    distinguish objects.

27. **Avoid negative names** — do not name an entity through the absence of a
    property; express negation with a field and a rule.

28. **Do not use names that depend on the model implementation** — avoid
    `record`, `tuple`, `state_machine`, and `data` unless they are domain terms.

29. **Separate a generic concept from a specialized concept** — when a standard
    discusses both a general class and specific subtypes, model both levels
    explicitly.

30. **Attach general requirements to a general `entity`** — a requirement that
    applies to an entire class of objects should be expressed through the common
    entity rather than duplicated in specialized entities.

31. **Prefer explicit structures to implicit strings** — represent constrained
    values and structured data with types rather than `string`.

32. **Do not hide several entities in one field** — when a field actually
    contains different domain objects, expose them structurally.

33. **Do not create an artificial "main" `entity` when the text has none** — do
    not add `standard_model`, `system`, or `document` unless domain requirements
    apply to them.

34. **When an entity also appears in another standard, choose the most widely
    accepted name** — prefer the term most likely to match the names used in
    other modules.

35. **Do not use the same name for different meanings** — one term should denote
    one concept within a module; qualify ambiguous words.

36. **Keep the `entity` name, type name, and referring fields consistent** — use
    one vocabulary for the entity, its type, and related fields.

37. **Fields should group properties around their natural owner** — a property
    should belong to the type of the object that owns it in the domain.

38. **Do not mix object state with an event** — model a state as a field; model
    an event as a separate structure or rule only when necessary.

39. **Ask three questions about every new `entity`** — is it an independent
    object? Could another standard impose requirements on it? Is it useful to
    align it?

40. **Minimum template for a good entity** — a good entity has a domain-specific
    name, a type with properties, and rules expressing requirements for it.

41. **Signs of a poor `entity`** — warning signs include a name referring to a
    document clause, rule, modality, state, implementation detail, abbreviation,
    or overly generic object.

42. **Final rule** — an `entity` should be a stable, public anchor of the model
    suitable for cross-module alignment.

## Common Mistakes

- Using negation in the names of prohibition rules

  ```mdl
  # WRONG
  rule F not_stop: stop always

  # RIGHT
  rule F stop: stop always
  ```

- Adding _placeholder rules_ that check nothing, such as
  `rule O user_is_human: true always`.  
  Such rules indicate that your model is incomplete. If you find a rule like
  this, either revise the model and add the corresponding field or remove the
  rule entirely when checking it would not make sense in your context.

- Leaving `fact` declarations in the final version of a module that models
  requirements  
  Facts are intended for quick model checks or for integrating external tools
  that genuinely provide data about external _facts_. Every requirement should
  be modeled as a `rule` so that, when a conflict occurs, the rule name and its
  source are visible.

- Building the model from data rather than from rules  
  An MDL model should focus on checking requirements for the object being
  standardized rather than modeling the standard itself. For example, a standard
  often has an `author`, `published_date`, `reviewers`, and similar metadata.  
  When data in the source document is unrelated to requirements, it should not
  appear among the `entity` declarations.

- Using large `let` bindings in rules  
  When a rule looks too complex, move part of its body into a function. You can
  also add a source annotation to the function to improve traceability to the
  source.

  ```mdl
  # WRONG
  @ Clause 5.2.2
  rule O material_proportions_satisfy_bounds:
    let satisfies_bounds: bool =
        case (raw_meat_material.meat_type, raw_meat_material.grade):
            | (MeatType.Beef(), CannedMeatGrade.First()):
                let non_muscle_tissue =
                    (raw_meat_material.properties.connective_tissue_proportion +
                     raw_meat_material.properties.adipose_tissue_proportion)
                non_muscle_tissue <= 6/100
            | (MeatType.Pork(), CannedMeatGrade.Top()):
                raw_meat_material.properties.adipose_tissue_proportion <= 30/100
            | _: true
    satisfies_bounds always

  # RIGHT
  @ Clause 5.2.2
  func satisfies_bounds(
    meat_type: MeatType,
    grade: CannedMeatGrade,
    proportions: RawMeatMaterialProperties) -> bool:
    case (meat_type, grade):
        | (MeatType.Beef(), CannedMeatGrade.First()):
            let non_muscle_tissue =
                (proportions.connective_tissue_proportion +
                 proportions.adipose_tissue_proportion)
            non_muscle_tissue <= 6/100
        | (MeatType.Pork(), CannedMeatGrade.Top()):
            proportions.adipose_tissue_proportion <= 30/100
        | _: true

  @ Clause 5.2.2
  rule O material_proportions_satisfy_bounds:
      satisfies_bounds(
          raw_meat_material.meat_type,
          raw_meat_material.grade,
          raw_meat_material.properties) always
  ```

## Super-Compact Syntax Reference

```mdl
@ annotation text
module my.module

# comment

import "relative/path.mdl"
open std.collections

type Rec = { field: Type, flag: bool }
type Sum<T> = Case(value: T) | Empty(unit)
entity object: Rec

func f<T>(x: T, r: Rec) -> bool:
    let value = r.field
    true

fact boolean_or_temporal_expr
rule O rule_name when condition: condition always
strict rule F no_x: x always otherwise fallback always
override specific_rule > general_rule
```

- Types: `unit`, `bool`, `int`, `rat`, `decimal`, `string`, `Type<T>`, `(A, B)`,
  `{ field_a: A, field_b: B }`, `type Union<A, B> = VariantA(A) | VariantB(B)`.
- Expressions: literals, `name`, `qualified.name`, `f(x)`, `x.field`,
  `Type { f = v }`, `(a, b)`, `if c then a else b`, `let x = v in body`,
  `case x: | Pattern when guard: body`.
- Patterns: `_`, literal, name, `(a, b)`, `{ field = p }`, `Type.Case(x)`,
  `Type.Empty()`.
- Operators: `not`, unary `-`, `* / %`, `+ -`, `= != < <= > >=`, `until`, `and`,
  `or`, `implies`, followed by the postfix operators
  `always/eventually/next/now`.
- Rule:
  `[strict|defeasible|defeater] rule [O|P|F] name [when expr]: temporal_body [otherwise temporal_expr]`;
  the default strength is `defeasible`.
- Time: `p now`, `p always`, `p next`, `p eventually`, `p until q`.
