// Tree-sitter grammar scaffold for MDL / DDL-LTLf.
// The authoritative parser for the toolkit lives in src/mdl/parser.py.

module.exports = grammar({
  name: 'mdl',

  extras: $ => [
    /[ \t\r\n]+/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.identifier,

  rules: {
    source_file: $ => seq(optional($.annotation_list), $.module_decl, repeat($._top_item)),

    _top_item: $ => choice($.import_decl, $.declaration),

    annotation_list: $ => repeat1($.annotation),
    annotation: $ => token(seq('@', /.*/)),

    module_decl: $ => seq('module', $.qualified_name),

    import_decl: $ => seq(
      'import', $.qualified_name,
      optional(seq('as', $.identifier)),
      optional(seq('exposing', '(', optional(commaSep($.import_name)), ')'))
    ),
    import_name: $ => seq($.identifier, optional(seq('as', $.identifier))),

    declaration: $ => seq(optional($.visibility), choice(
      $.type_decl,
      $.value_decl,
      $.func_decl,
      $.entity_decl,
      $.event_decl,
      $.rule_decl,
      $.priority_decl,
      $.fact_decl,
      $.assert_decl,
      $.align_decl
    )),

    visibility: $ => choice('public', 'private'),

    type_decl: $ => seq('type', $.identifier, optional($.type_params), '=', choice($.sum_type, $.type_expr)),
    type_params: $ => seq('<', commaSep1($.identifier), optional(','), '>'),
    sum_type: $ => seq($.variant, repeat1(seq('|', $.variant))),
    variant: $ => seq($.identifier, optional(seq('(', optional(commaSep($.variant_field)), ')'))),
    variant_field: $ => choice(seq($.identifier, ':', $.type_expr), $.type_expr),

    type_expr: $ => choice(
      $.record_type,
      $.tuple_type,
      seq($.qualified_name, optional(seq('<', optional(commaSep($.type_expr)), '>')))
    ),
    record_type: $ => seq('{', optional(commaSep($.field_decl)), optional(','), '}'),
    field_decl: $ => seq($.identifier, ':', $.type_expr),
    tuple_type: $ => seq('(', commaSep1($.type_expr), ')'),

    value_decl: $ => seq(choice('val', 'let'), $.identifier, optional($.type_annotation), '=', $.expr),
    type_annotation: $ => seq(':', $.type_expr),

    func_decl: $ => seq('func', $.identifier, optional($.type_params), '(', optional(commaSep($.param)), ')', '->', $.type_expr, ':', $.block),
    param: $ => seq($.pattern, ':', $.type_expr),

    entity_decl: $ => seq('entity', $.identifier, ':', $.type_expr, repeat($.entity_clause)),
    entity_clause: $ => choice(seq('key', '(', $.expr, ')'), seq('where', $.expr)),

    event_decl: $ => seq('event', $.identifier, optional(seq('(', optional(commaSep($.field_decl)), ')'))),

    rule_decl: $ => seq(optional($.rule_strength), 'rule', optional($.deontic_mod), optional($.qualified_name), optional(seq('when', $.expr)), ':', $.expr, optional(seq('otherwise', $.expr))),
    rule_strength: $ => choice('strict', 'defeasible', 'defeater'),
    deontic_mod: $ => choice('O', 'P', 'F'),

    priority_decl: $ => seq(choice('priority', 'override'), $.qualified_name, repeat(seq('>', $.qualified_name))),
    fact_decl: $ => seq('fact', optional(seq($.identifier, '=')), $.expr),
    assert_decl: $ => seq('assert', $.expr),
    align_decl: $ => seq('align', $.qualified_name, 'to', choice($.qualified_name, $.iri, $.string), optional(choice('equivalent', 'broader', 'narrower', 'related'))),

    block: $ => choice($.expr, repeat1(choice($.let_stmt, $.expr))),
    let_stmt: $ => seq('let', $.pattern, optional($.type_annotation), '=', $.expr),

    expr: $ => choice(
      $.if_expr,
      $.case_expr,
      $.let_expr,
      $.quantifier_expr,
      $.temporal_prefix_expr,
      $.unary_expr,
      $.binary_expr,
      $.call_expr,
      $.field_expr,
      $.index_expr,
      $.temporal_postfix_expr,
      $._primary_expr
    ),

    if_expr: $ => seq('if', $.expr, 'then', $.expr, 'else', $.expr),
    let_expr: $ => seq('let', $.pattern, optional($.type_annotation), '=', $.expr, 'in', $.expr),
    case_expr: $ => seq(choice('case', 'switch'), $.expr, ':', repeat1($.case_arm)),
    case_arm: $ => seq('|', $.pattern, optional(seq('when', $.expr)), ':', $.block),
    quantifier_expr: $ => seq(choice('forall', 'exists'), $.pattern, 'in', $.expr, ':', $.expr),

    temporal_prefix_expr: $ => prec(9, seq(choice('always', 'eventually', 'next', 'weak_next', 'never'), $.expr)),
    temporal_postfix_expr: $ => prec(10, seq($.expr, choice('always', 'eventually', 'next', 'weak_next', 'never'))),
    unary_expr: $ => prec(9, seq(choice('not', '-'), $.expr)),
    binary_expr: $ => choice(
      prec.left(1, seq($.expr, choice('implies', '->'), $.expr)),
      prec.left(2, seq($.expr, choice('iff', '<->'), $.expr)),
      prec.left(3, seq($.expr, 'or', $.expr)),
      prec.left(4, seq($.expr, 'and', $.expr)),
      prec.left(5, seq($.expr, choice('until', 'release', 'weak_until'), $.expr)),
      prec.left(6, seq($.expr, choice('=', '==', '!=', '<', '<=', '>', '>='), $.expr)),
      prec.left(7, seq($.expr, choice('+', '-'), $.expr)),
      prec.left(8, seq($.expr, choice('*', '/', '%'), $.expr))
    ),
    call_expr: $ => prec(11, seq($.expr, '(', optional(commaSep($.expr)), ')')),
    field_expr: $ => prec(11, seq($.expr, '.', $.identifier)),
    index_expr: $ => prec(11, seq($.expr, '[', $.expr, ']')),

    _primary_expr: $ => choice(
      $.qualified_name,
      $.literal,
      $.record_literal,
      $.list_literal,
      $.set_literal,
      $.tuple_literal,
      seq('{', $.expr, '}'),
      seq('(', $.expr, ')')
    ),

    record_literal: $ => seq('{', optional(commaSep(seq($.identifier, '=', $.expr))), optional(','), '}'),
    list_literal: $ => seq('[', optional(commaSep($.expr)), optional(','), ']'),
    set_literal: $ => seq('#{', optional(commaSep($.expr)), optional(','), '}'),
    tuple_literal: $ => seq('(', commaSep1($.expr), optional(','), ')'),

    pattern: $ => choice(
      '_',
      $.literal,
      $.qualified_name,
      seq($.qualified_name, '(', optional(commaSep($.pattern)), ')'),
      seq('[', optional(commaSep($.pattern)), ']'),
      seq('(', commaSep1($.pattern), ')'),
      seq('{', optional(commaSep(choice($.identifier, seq($.identifier, '=', $.pattern)))), '}')
    ),

    literal: $ => choice($.string, $.char, $.rational, $.decimal, $.integer, $.boolean),
    boolean: $ => choice('true', 'false'),
    string: $ => token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),
    char: $ => token(seq("'", repeat(choice(/[^'\\]/, /\\./)), "'")),
    rational: $ => /[0-9]+\/[0-9]+/,
    decimal: $ => /[0-9]+\.[0-9]+/,
    integer: $ => /[0-9]+/,
    iri: $ => token(seq('<', /[^>]+/, '>')),

    qualified_name: $ => seq($.identifier, repeat(seq('.', $.identifier))),
    identifier: $ => /[A-Za-z_][A-Za-z0-9_']*/,
    line_comment: $ => token(choice(seq('//', /.*/), seq('#', /.*/))),
    block_comment: $ => token(seq('/*', /[^]*?/, '*/')),
  }
});

function commaSep(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}
