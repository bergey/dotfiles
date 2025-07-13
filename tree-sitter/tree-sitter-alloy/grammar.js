/**
 * @file Alloy Verifier - alloytools.org
 * @author Daniel Bergey <dmbergey@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

function comma_separated(expr) {
  return seq(expr, repeat(seq(",", expr)), optional(","));
  // where are trailing commas allowed?  useful to allow while typing regardless
}

module.exports = grammar({
  name: "alloy",

  extras: $ => [
    /\s/,
    $.comment,
  ],
  word: $ => $._identifier,
  conflicts: $ => [[$.implies_else]],

  // see https://alloytools.org/spec.html
  rules: {
    source_file: $ => repeat($._paragraph),
    _paragraph: $ => choice($.sig, $.fact), // or fact, pred, run, check

    sig: $ => seq(
      optional("var"), // make this show up in the syntax tree?
      optional("abstract"),
      field("mult", optional($.mult)),
      "sig",
      field("name", comma_separated($.identifier)),
      optional($.sig_extends),
      "{",
      optional(field("field", comma_separated($.field))),
      "}",
      // optional block
    ),
    sig_extends: $ => choice(
      seq("extends", $.identifier),
      seq("in", $.identifier) // multiple allowed here?
    ),
    field: $ => seq(
      optional("var"),
      // TODO split out decl
      field("name", $.identifier),  // TODO multiple
      ":",
      optional($.mult),
      $.identifier
    ),

    fact: $ => seq(
      "fact",
      field("name", optional($.identifier)),
      $.block
    ),

    block: $ => seq(
      "{",
      repeat($.expr),
      "}"
    ),

    // TODO make the syntax tree nicer – what nodes should be named?
    expr: $ => choice(
      $.const, $.qualName, "this", // @name ?
      // prec(21, seq(/~|^|\*/, $.expr)), // Grammar error: Unexpected rule ExpandRegex(Assertion)
      prec(20, seq($.expr, token.immediate("'"))),
      prec.left(19, seq($.expr, ".", $.expr)),
      prec(18, seq($.expr, "[", comma_separated($.expr), "]")), // is expr[] allowed?
      prec.left(17, seq($.expr, /<:|:>/, $.expr)),
      prec.left(16, seq($.expr, optional(choice($.mult, "set")), "->", $.expr)),
      prec.left(15, seq($.expr, "&", $.expr)),
      prec.left(14, seq($.expr, "++", $.expr)),
      prec(13, seq("#", $.expr)),
      prec.left(12, seq($.expr, choice("+", "-"), $.expr)),
      prec(11, seq($.mult, $.expr)),
      prec(11, seq(choice("no", "set"), $.expr)),
      prec(10, seq(choice("!", "not"), $.expr)),
      prec.left(9, seq($.expr, token(seq(
        optional(choice("!", "not")),
        choice("in", "=", "<", ">", "=<", ">="))),
        $.expr),
      ),
      // TODO let, quant, {}
      // TODO more logical operators
      prec.left(6, seq($.expr, choice("&&", "and"), $.expr)),
      $.implies_else,
      prec.left(4, seq($.expr, choice("<=>", "iff"), $.expr)),
      prec.left(3, seq($.expr, choice("||", "or"), $.expr)),
      // TODO let quant, ;

      seq("(", $.expr, ")"),
      $.block
    ),
    implies_else: $ => seq($.expr, choice("=>", "implies"), $.expr, "else", $.expr),


    const: _ => choice(/-?[0-9]+/, "none", "univ", "iden"),

    _identifier: _ => /[A-Za-z_]+/, // TODO full character class
    identifier: $ => $._identifier,
    // Maybe it's better to have named identifier nodes when there are several?
    qualName: $ => seq(optional("this/"), repeat(seq($._identifier, token.immediate("/"))), $._identifier),
    mult: _ => choice("lone", "some", "one"),
    comment: _ => token(choice(
      seq("//", /[^\\\n]*/), // line comment
      // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
      // this is used in the C & Java tree-sitter grammars
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      ),
    )),
  }
});
