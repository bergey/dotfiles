/**
 * @file Alloy Verifier - alloytools.org
 * @author Daniel Bergey <dmbergey@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "alloy",

  extras: $ => [
    /\s/,
    $.comment,
  ],
  word: $ => $.identifier,

  rules: {
    source_file: $ => repeat($._declaration),
    _declaration: $ => $.sig, // or fact, pred, run, check
    sig: $ => seq( // TODO in / extends
      "sig",
      field("name", $.identifier),
      "{",
      // TODO fields
      "}",
    ),
    identifier: _ => /[A-Za-z_]+/, // TODO full character class
    comment: _ => choice(
      seq(token.immediate("//"), /.*/), // line comment
      // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
      // this is used in the C & Java tree-sitter grammars
      token(seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      )),
    ),
  }
});
