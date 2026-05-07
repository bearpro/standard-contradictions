(source_trace) @attribute

[
  "module"
  "import"
  "as"
  "private"
  "type"
  "func"
  "case"
  "if"
  "then"
  "else"
  "let"
  "entity"
  "rule"
  "fact"
  "always"
] @keyword

[
  "bool"
  "string"
  "int"
] @type.builtin

[
  "true"
  "false"
] @boolean

(string) @string

(module_declaration name: (identifier) @namespace)
(type_declaration name: (identifier) @type)
(type_declaration variants: (identifier) @constructor)
(function_declaration name: (identifier) @function)
(entity_declaration name: (identifier) @variable)
(rule_declaration name: (identifier) @label)

(parameter name: (identifier) @variable.parameter)
(let_statement name: (identifier) @variable)

(constructor_pattern constructor: (qualified_identifier) @constructor)
(match_branch pattern: (pattern (identifier) @constructor))

(call_expression function: (qualified_identifier) @function.call)
(qualified_identifier) @variable

((identifier) @keyword
  (#match? @keyword "^(if|then|else|case|let)$"))

[
  "="
  "and"
  "->"
  "|"
  ":"
] @operator
