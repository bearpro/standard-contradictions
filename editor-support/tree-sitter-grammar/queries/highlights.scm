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
(rule_decl (deontic_mod) @keyword.operator)
[
  "module" "import" "type" "let" "func" "entity" "rule"
  "strict" "defeasible" "defeater" "override" "fact"
] @keyword
[
  "if" "then" "else" "case" "when" "in"
] @keyword.control
[
  "always" "eventually" "next" "now" "until"
] @keyword.operator
[
  "and" "or" "implies" "not"
] @operator
