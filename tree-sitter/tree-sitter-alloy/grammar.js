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
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
