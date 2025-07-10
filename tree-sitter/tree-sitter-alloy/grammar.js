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
    _paragraph: $ => $.sig, // or fact, pred, run, check
    sig: $ => seq( // TODO in / extends
      optional("var"), // make this show up in the syntax tree?
      optional("abstract"),
      field("mult", optional($.mult)),
      "sig",
      field("name", comma_separated($.identifier)),
      // TODO extends / in 
      "{",
      optional(field("field", comma_separated($.field))),
      // optional(field("field", $.field)),
      "}",
      // optional block
    ),
    field: $ => seq(
      // optional("var"),
      // TODO split out decl
      field("name", $.identifier),  // multiple
      ":",
      optional($.mult),
      $.identifier
    ),
    identifier: _ => /[A-Za-z_]+/, // TODO full character class
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
