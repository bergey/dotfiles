/**
 * @file Alloy Verifier - alloytools.org
 * @author Daniel Bergey <dmbergey@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "alloy",

  rules: {
    source_file: $ => $.declaration,
    declaration: $ => $.sig, // or fact, pred, run, check
    sig: $ => seq( // TODO in / extends
      "sig",
      field("name", $.identifier),
      "{",
      // TODO fields
      "}",
    ),
    identifier: _ => /[A-Za-z_]+/,
  }
});
