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
  word: $ => $.identifier,

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

    expr: $ => $.const, // TODO FIXME

    const: _ => choice(/-?[0-9]+/, "none", "univ", "iden"),
    arrowOp: $ => seq(optional(choice($.mult, "set")), "->", optional(choice($.mult, "set"))),
    // _negate: _ => choice("!", "not"),

    identifier: _ => /[A-Za-z_]+/, // TODO full character class
    qualName: $ => seq(optional("this/"), repeat(seq($.identifier, token.immediate("/"))), $.identifier),
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
