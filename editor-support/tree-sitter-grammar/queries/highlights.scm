(annotation) @attribute
(line_comment) @comment
(string) @string
(integer) @number
(decimal) @number.float
(rational) @number
(boolean) @constant.builtin
(identifier) @variable
(type_decl name: (identifier) @type)
(func_decl name: (identifier) @function)
(entity_decl name: (identifier) @variable)
(event_decl name: (identifier) @function)
(rule_decl (deontic_mod) @keyword.operator)
[
  "module" "import" "type" "val" "let" "func" "entity" "event" "rule"
  "strict" "defeasible" "defeater" "priority" "override" "fact"
] @keyword
[
  "if" "then" "else" "case" "when" "in"
] @keyword.control
[
  "always" "eventually" "next" "weak_next" "never" "until" "release" "weak_until"
] @keyword.operator
[
  "and" "or" "not"
] @operator
