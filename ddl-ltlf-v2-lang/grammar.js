function commaSep(rule) {
  return optional(seq(rule, repeat(seq(",", rule)), optional(",")));
}

function pipeSep1(rule) {
  return seq(rule, repeat(seq("|", rule)));
}

module.exports = grammar({
  name: "mdl",

  extras: $ => [/\s/],

  word: $ => $.identifier,

  rules: {
    source_file: $ => repeat(choice($._declaration, $.source_trace)),

    source_trace: $ => seq(field("marker", $.trace_marker), field("text", $.trace_text)),
    trace_marker: _ => "@",
    trace_text: _ => token(/[^\n]+/),

    _declaration: $ => choice(
      $.module_declaration,
      $.import_declaration,
      $.type_declaration,
      $.function_declaration,
      $.entity_declaration,
      $.rule_declaration,
      $.fact_declaration,
    ),

    module_declaration: $ => seq("module", field("name", $.identifier)),

    import_declaration: $ => seq(
      "import",
      field("module", $.qualified_identifier),
      optional(seq("as", field("alias", $.identifier))),
    ),

    type_declaration: $ => seq(
      optional("private"),
      "type",
      field("name", $.identifier),
      "=",
      field("variants", pipeSep1($.identifier)),
    ),

    function_declaration: $ => seq(
      optional("private"),
      "func",
      field("name", $.identifier),
      field("parameters", $.parameter_list),
      "->",
      field("return_type", $.type),
      ":",
      repeat($._function_statement),
    ),

    parameter_list: $ => seq(
      "(",
      commaSep($.parameter),
      ")",
    ),

    parameter: $ => seq(field("name", $.identifier), ":", field("type", $.type)),

    type: $ => choice(
      $.generic_type,
      $.qualified_identifier,
      "bool",
      "string",
      "int",
    ),

    generic_type: $ => seq($.identifier, "<", $.type, ">"),

    _function_statement: $ => choice(
      $.case_statement,
      $.match_branch,
      $.if_statement,
      $.then_statement,
      $.else_if_statement,
      $.else_statement,
      $.let_statement,
      $.return_expression,
    ),

    case_statement: $ => seq("case", field("value", $.identifier), ":"),

    match_branch: $ => seq("|", field("pattern", $.pattern), ":", field("body", $._expression)),

    pattern: $ => choice($.constructor_pattern, $.identifier),

    constructor_pattern: $ => seq(
      field("constructor", $.qualified_identifier),
      "(",
      commaSep($.identifier),
      ")",
    ),

    if_statement: $ => seq("if", field("condition", $._expression)),

    then_statement: $ => seq("then", field("body", $._expression)),

    else_if_statement: $ => seq(
      "else",
      "if",
      field("condition", $._expression),
      "then",
      field("body", $._expression),
    ),

    else_statement: $ => seq("else", field("body", $._expression)),

    let_statement: $ => seq(
      "let",
      field("name", $.identifier),
      "=",
      field("value", $._expression),
    ),

    return_expression: $ => $._expression,

    entity_declaration: $ => seq(
      "entity",
      field("name", $.identifier),
      ":",
      field("type", $.type),
    ),

    rule_declaration: $ => seq(
      "rule",
      field("kind", $.identifier),
      field("name", $.identifier),
      "=",
      field("body", $._expression),
      optional("always"),
    ),

    fact_declaration: $ => seq("fact", field("value", $.string)),

    _expression: $ => choice(
      $.binary_expression,
      $.call_expression,
      $.qualified_identifier,
      $.string,
      $.boolean,
    ),

    binary_expression: $ => prec.left(1, seq(
      $._expression,
      field("operator", choice("=", "and")),
      $._expression,
    )),

    call_expression: $ => seq(
      field("function", $.qualified_identifier),
      field("arguments", $.argument_list),
    ),

    argument_list: $ => seq("(", commaSep($._expression), ")"),

    qualified_identifier: $ => seq($.identifier, repeat(seq(".", $.identifier))),

    boolean: _ => choice("true", "false"),

    string: _ => token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),

    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,
  },
});
