# DDL LTLf language

A domain-specific language (DSL) for representing **normative documents**  
(standards, laws, technical requirements) in a formal, machine-checkable way.

- **DDL**: Defeasible Deontic Logic (obligations, permissions, prohibitions with exceptions)
- **LTLf**: Linear Temporal Logic over finite traces (time-bounded reasoning)

Unlike general-purpose programming languages, programs in this DSL are not executed.
They are **solved** — translated into logical constraints and fed to the [Z3 SMT solver](https://github.com/Z3Prover/z3) to check for **consistency** and **conflicts**.

## Quick Example

```dsl
predicate big_enough(tube: Tube) = tube.length > 55
predicate is_tube(x) = x.shape = Tube(_, _)

obligated big_enough(x) when is_tube(x)

fact x.shape = Tube(length: 30, diameter: 5)
```

## Sources in this directory

- `Mprokazin.DdlLtlf.Language.Antlr` - ANTLR grammar of the language.
- `Mprokazin.DdlLtlf.Language` - parser and semantic analyzer, and probably other language (design time) features that if ever there are.
- `Mprokazin.DdlLtlf.Solver` - runtime of the language. Translate ddl-ltlf to Z3.
- `Mprokazin.DdlLtlf.Cli` - shell tool to work with language sources (`validate`, `solve`)
- `Mprokazin.DdlLtlf.Tests` - parser and semantics tests that do not require the native Z3 library.
- `Mprokazin.DdlLtlf.Tests.Solver` - solver tests that exercise Z3-backed conflict detection.
- `Mprokazin.DdlLtlf.Language.Ast.SchemaGen` - generator of json-schema matching lanugae syntax tree structure. May be useful for some software integrations later, but for now just collects dust there.

## Language Overview

- The _program_ in this language not meant to be executed as traditional code of
  general purpose programming language

- Main aim of _program_ is to be **_solved_**, by translating all statements,
  declarations and constaints (facts) to Z3 SMT solver.

- The _program_ consists of this main entitites:
  
  - **Deontic statements**  
    The single most important entity here. Represents the rule one must follow
    to satisfy requirements.

    Examples:  
    ```
    obligated x > 0
    forbidden x < 10 when y = 0
    ```

  - `TODO` **Temporal statements**  
    Temporal statements meant to represent temporal relationshisps between
    deontic statements.

    Example:
    ```
    obligated A = x > 3
    always A
    
    obligated B = x > 10
    obligated C = y > 0
    
    B until C
    ```

  - `TODO` **Defeasability statements**  
    Meant to represent complex relationships between deontic statements,
    such as _"have to A unless B"_.
  
  - **Named predicates**  
    Simple syntax sugar to make code more ergonomic

    Examples:
    ```
    predicate big_enough(x) = x > 55
    
    predicate is_tube(x) = 
        predicate is_round(x) = x.shape = Cylindric in
        is_round(x)
    
    obligated big_enough(x) when is_tube(x)
    ```
  
  - `TODO` **Type definition**  
    Optional definition of data structure that can be passed to predicateds and contrained by facts. Will be infered if not stated implicitly.  
    The type is discrimined union which fields can be `int`, `float`, `bool` or unions themselve.  
    Note that single programm uses just single type.

    Example:
    ```
    type Tube 
    | length of float * diameter of float

    root type 
    | x of (
        weight of int 
            * shape of
                | tube of Tube
                | cube of (x of float * y of float * z of float)
        )

    predicate is_tube (x) =
        match x.shape with 
        | Tube _ -> true
        | _ -> false
    
    obligated x.weight < 10 when is_tube(x)
    ```

  - `TODO` **Facts**  
    Known contraints for values of _type_. Meant to be exposed
    from CAD software integration to check stasifaction of particular product to deontic statements.

    Examples:
    ```
    fact x.shape = Tube(30, 5) # x is Tube with known dimensions
    # or
    fact x.shape = Tube(_, _)  # x is Tube with unknown dimensions
    # or
    fact x.shape =             
        | Tube(_, 10)          # if x is Tube, its diameter is 10 
        | _                    # but maybe x is not a Tube
    ```
  - `TODO` **Attributes**  
    Attributes do not have predefined language semantic, but they
    can be used for software integrations.  
    Attributes can be applied to any program entity.
    
    Exmaples:
    ```
    # Deontic statements extracted from IEEE 754 standard

    @origin('IEEE 754')
    obligated x.sign.size = 1 when x.size = 32
    
    @origin('IEEE 754')
    obligated x.exponent.size = 8 when x.size = 32

    @origin('IEEE 753')
    obligated x.mantissa.size = 23 when x.size = 32

    # This field value should provided as fact by CAD software
    root type 
        @expose('x')
        | x of int
    ```
  
- Code of this language not meant to be written by hand. Typically, you
  build a system that converts source normative documents to this language in some way (by using LLMs for natural text source, or some translator for formal normative documents).

## Typical use cases of this language
  
- Generate code from some source and check if it can be solved, so you check for conflicts in this source.

- Generate code from **two** or more sources and check if they can be solved **combined** (by concatinating all definitions), so you check for conflicts **between** your source.

- Generate code from source, feed it to some CAD software plugin, and check if code can be solved **with respect to CAD provided facts**, so you check for conflicts between source and CAD project.
