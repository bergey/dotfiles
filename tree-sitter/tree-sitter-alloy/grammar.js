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
  word: $ => $._name,
  conflicts: $ => [
    [$.implies_else],
    [$.name, $.qual_name] // TODO resolve this better
  ],

  // see https://alloytools.org/spec.html
  rules: {
    source_file: $ => repeat($._paragraph),
    _paragraph: $ => choice($.sig, $.fact, $.pred, $.fun), // or run, check

    sig: $ => seq(
      optional("var"), // make this show up in the syntax tree?
      optional("abstract"),
      field("mult", optional($.mult)),
      "sig",
      field("name", comma_separated($.name)),
      optional($.sig_extends),
      "{",
      optional(field("field", comma_separated($.field))),
      "}",
      // optional block
    ),
    sig_extends: $ => choice(
      seq("extends", $.name),
      seq("in", $.name) // multiple allowed here?
    ),
    field: $ => seq(
      optional("var"),
      // TODO split out decl
      field("name", $.name),  // TODO multiple
      ":",
      optional($.mult),
      $.name
    ),

    fact: $ => seq(
      "fact",
      field("name", optional($.name)),
      $.block
    ),

    pred: $ => seq(
      "pred",
      field("namespace", optional(seq($.qual_name, "."))),
      field("name", $.name),
      optional($.parameters),
      $.block
    ),

    fun: $ => seq(
      "fun",
      field("namespace", optional(seq($.qual_name, "."))),
      field("name", $.name),
      optional($.parameters),
      ":", field("type", $.expr),
      "{", field("definition", $.expr), "}"
    ),

    block: $ => seq(
      "{",
      repeat($.expr),
      "}"
    ),
    _block_or_bar: $ => prec.right(choice($.block, seq("|", $.expr))),
    parameters: $ => choice(
      seq("(", comma_separated($.decl), ")"),
      seq("[", comma_separated($.decl), "]"),
    ),

    // TODO make the syntax tree nicer – what nodes should be named?
    expr: $ => choice(
      $.const, $.qual_name, "this", // @name ?
      prec(21, seq(choice("~", "^", "*"), $.expr)),
      prec(20, seq($.expr, token.immediate("'"))),
      prec.left(19, seq($.expr, ".", $.expr)),
      prec(18, seq($.expr, "[", comma_separated($.expr), "]")), // is expr[] allowed?
      prec.left(17, seq($.expr, /<:|:>/, $.expr)),
      // prec.left(16, seq($.expr, optional(choice($.mult, "set")), "->", $.expr)), // parse errors, ambiguity?
      prec.left(16, seq($.expr, "->", $.expr)), // omit mult / set for now
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
      // TODO more logical operators, {}
      prec.left(6, seq($.expr, choice("&&", "and"), $.expr)),
      $.implies_else,
      prec.left(4, seq($.expr, choice("<=>", "iff"), $.expr)),
      prec.left(3, seq($.expr, choice("||", "or"), $.expr)),
      prec.left(2, seq(choice($.mult, $.quant), comma_separated($.decl), $._block_or_bar)),
      // TODO let ;
      // let let_decl
      // braces decl

      seq("(", $.expr, ")"),
      $.block
    ),
    implies_else: $ => seq($.expr, choice("=>", "implies"), $.expr, "else", $.expr),

    const: _ => choice(/-?[0-9]+/, "none", "univ", "iden"),

    _name: _ => /[A-Za-z_]+/, // TODO full character class
    name: $ => $._name,
    // Maybe it's better to have named name nodes when there are several?
    qual_name: $ => seq(optional("this/"), repeat(seq($._name, token.immediate("/"))), $._name),
    mult: _ => choice("lone", "some", "one"),

    quant: $ => choice("all", "no", "sum"), // TODO fix overlap
    decl: $ => seq(optional("disj"), comma_separated($.name), ":", optional("disj"), $.expr),

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
