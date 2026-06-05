// Tree-sitter scaffold for MDL / DDL-LTLf.
// The authoritative parser for the toolkit lives in mdl/src/mdl/parser.py.

const PREC = {
  implies: 1,
  iff: 2,
  or: 3,
  and: 4,
  temporal: 5,
  compare: 6,
  add: 7,
  multiply: 8,
  prefix: 9,
  postfix_temporal: 10,
  postfix: 11,
  record_constructor: 12,
};

module.exports = grammar({
  name: 'mdl',

  extras: $ => [
    /[ \t\r\n]+/,
    $.line_comment,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.block],
    [$.let_stmt, $.let_expr],
  ],

  rules: {
    source_file: $ => seq(repeat($.annotation), $.module_decl, repeat($._top_item)),

    _top_item: $ => seq(repeat($.annotation), choice($.import_decl, $.open_decl, $.declaration)),

    annotation: $ => token(seq('@', /.*/)),

    module_decl: $ => seq('module', $.qualified_name),

    import_decl: $ => seq('import', field('path', $.string)),
    open_decl: $ => seq('open', field('module', $.qualified_name)),

    declaration: $ => choice(
      $.type_decl,
      $.value_decl,
      $.func_decl,
      $.entity_decl,
      $.event_decl,
      $.rule_decl,
      $.priority_decl,
      $.fact_decl,
      $.assert_decl,
      $.align_decl,
    ),

    type_decl: $ => seq(
      'type',
      field('name', $.identifier),
      optional($.type_params),
      '=',
      field('definition', choice($.sum_type, $.type_expr)),
    ),
    type_params: $ => seq('<', optional(commaSep($.identifier)), '>'),
    sum_type: $ => choice(
      seq($.variant, repeat1(seq('|', $.variant))),
      $.variant_with_fields,
    ),
    variant: $ => seq($.identifier, optional(seq('(', optional(commaSep($.variant_field)), ')'))),
    variant_with_fields: $ => seq($.identifier, '(', optional(commaSep($.variant_field)), ')'),
    variant_field: $ => choice(seq($.identifier, ':', $.type_expr), $.type_expr),

    type_expr: $ => choice(
      $.record_type,
      $.parenthesized_type,
      $.tuple_type,
      $.type_ref,
    ),
    record_type: $ => seq('{', optional(commaSep($.field_decl)), '}'),
    field_decl: $ => seq($.identifier, ':', $.type_expr),
    parenthesized_type: $ => seq('(', $.type_expr, ')'),
    tuple_type: $ => seq('(', $.type_expr, ',', commaSep($.type_expr), ')'),
    type_ref: $ => seq($.qualified_name, optional(seq('<', optional(commaSep($.type_expr)), '>'))),

    value_decl: $ => seq(choice('val', 'let'), field('name', $.identifier), optional($.type_annotation), '=', $.expr),
    type_annotation: $ => seq(':', $.type_expr),

    func_decl: $ => seq(
      'func',
      field('name', $.identifier),
      optional($.type_params),
      '(',
      optional(commaSep($.param)),
      ')',
      '->',
      field('return_type', $.type_expr),
      ':',
      $.block,
    ),
    param: $ => seq($.pattern, ':', $.type_expr),

    entity_decl: $ => seq('entity', field('name', $.identifier), ':', $.type_expr, repeat($.entity_clause)),
    entity_clause: $ => choice(seq('key', '(', $.expr, ')'), seq('where', $.expr)),

    event_decl: $ => seq('event', field('name', $.identifier), optional(seq('(', optional(commaSep($.field_decl)), ')'))),

    rule_decl: $ => seq(
      optional($.rule_strength),
      'rule',
      choice(
        seq($.deontic_mod, ':', field('body', $.expr)),
        seq(optional($.deontic_mod), field('name', $.qualified_name), optional(seq('when', field('antecedent', $.expr))), ':', field('body', $.expr)),
      ),
      optional(seq('otherwise', field('otherwise', $.expr))),
    ),
    rule_strength: $ => choice('strict', 'defeasible', 'defeater'),
    deontic_mod: $ => choice('O', 'P', 'F'),

    priority_decl: $ => seq(choice('priority', 'override'), $.qualified_name, repeat(seq('>', $.qualified_name))),
    fact_decl: $ => seq('fact', choice(seq(field('target', $.identifier), '=', $.expr), $.expr)),
    assert_decl: $ => seq('assert', $.expr),
    align_decl: $ => seq(
      'align',
      field('subject', $.qualified_name),
      'to',
      field('target', choice($.qualified_name, $.iri, $.string)),
      optional(field('kind', choice('equivalent', 'broader', 'narrower', 'related'))),
    ),

    block: $ => repeat1(choice($.let_stmt, $.expr)),
    let_stmt: $ => seq('let', $.pattern, optional($.type_annotation), '=', $.expr),

    expr: $ => choice(
      $.if_expr,
      $.let_expr,
      $.case_expr,
      $.quantifier_expr,
      $.temporal_prefix_expr,
      $.temporal_postfix_expr,
      $.unary_expr,
      $.binary_expr,
      $.record_constructor,
      $.call_expr,
      $.field_expr,
      $.index_expr,
      $._primary_expr,
    ),

    if_expr: $ => seq('if', $.expr, 'then', $.expr, 'else', $.expr),
    let_expr: $ => prec.right(seq('let', $.pattern, optional($.type_annotation), '=', $.expr, 'in', $.expr)),
    case_expr: $ => prec.right(seq(choice('case', 'switch'), $.expr, ':', repeat1($.case_arm))),
    case_arm: $ => seq('|', $.pattern, optional(seq('when', $.expr)), ':', $.block),
    quantifier_expr: $ => seq(choice('forall', 'exists'), $.pattern, 'in', $.expr, ':', $.expr),

    temporal_prefix_expr: $ => prec(PREC.prefix, seq(choice('always', 'eventually', 'next', 'weak_next', 'never'), $.expr)),
    temporal_postfix_expr: $ => prec.left(PREC.postfix_temporal, seq($.expr, choice('always', 'eventually', 'next', 'weak_next', 'never'))),
    unary_expr: $ => prec(PREC.prefix, seq(choice('not', '-'), $.expr)),
    binary_expr: $ => choice(
      prec.right(PREC.implies, seq($.expr, choice('implies', '->'), $.expr)),
      prec.left(PREC.iff, seq($.expr, choice('iff', '<->'), $.expr)),
      prec.left(PREC.or, seq($.expr, 'or', $.expr)),
      prec.left(PREC.and, seq($.expr, 'and', $.expr)),
      prec.left(PREC.temporal, seq($.expr, choice('until', 'release', 'weak_until'), $.expr)),
      prec.left(PREC.compare, seq($.expr, choice('=', '==', '!=', '<', '<=', '>', '>='), $.expr)),
      prec.left(PREC.add, seq($.expr, choice('+', '-'), $.expr)),
      prec.left(PREC.multiply, seq($.expr, choice('*', '/', '%'), $.expr)),
    ),
    record_constructor: $ => prec(PREC.record_constructor, seq($.qualified_name, $.record_constructor_fields)),
    record_constructor_fields: $ => seq('{', optional(commaSep(seq($.identifier, '=', $.expr))), '}'),
    call_expr: $ => prec(PREC.postfix, seq($.expr, '(', optional(commaSep($.expr)), ')')),
    field_expr: $ => prec(PREC.postfix, seq($.expr, '.', $.identifier)),
    index_expr: $ => prec(PREC.postfix, seq($.expr, '[', $.expr, ']')),

    _primary_expr: $ => choice(
      $.qualified_name,
      $.literal,
      $.tuple_literal,
      $.parenthesized_expr,
    ),
    parenthesized_expr: $ => seq('(', $.expr, ')'),
    tuple_literal: $ => choice(
      seq('(', ')'),
      seq('(', $.expr, ',', optional(commaSep($.expr)), ')'),
    ),

    pattern: $ => choice(
      '_',
      $.literal,
      $.tuple_pattern,
      $.record_pattern,
      $.constructor_pattern,
      $.qualified_name,
    ),
    tuple_pattern: $ => choice(
      seq('(', $.pattern, ')'),
      seq('(', $.pattern, ',', commaSep($.pattern), ')'),
    ),
    record_pattern: $ => seq('{', optional(commaSep(choice($.identifier, seq($.identifier, '=', $.pattern)))), '}'),
    constructor_pattern: $ => seq($.qualified_name, '(', optional(commaSep($.pattern)), ')'),

    literal: $ => choice($.string, $.rational, $.decimal, $.integer, $.boolean, 'last'),
    boolean: $ => choice('true', 'false'),
    string: $ => token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),
    rational: $ => /[0-9]+\/[0-9]+/,
    decimal: $ => /[0-9]+\.[0-9]+/,
    integer: $ => /[0-9]+/,
    iri: $ => token(seq('<', /[^>]+/, '>')),

    qualified_name: $ => prec.left(seq($.identifier, repeat(seq('.', $.identifier)))),
    identifier: $ => /[A-Za-z_][A-Za-z0-9_']*/,
    line_comment: $ => token(seq('#', /.*/)),
  },
});

function commaSep(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}
