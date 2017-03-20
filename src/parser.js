/*
A simple parser based on the grammer of r7rs small.
Transforms a stream of tokens to an abstract syntax tree.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/
'use strict';

var lexer = require('./lexer');
var langTable = require('./lang-table');
var ast = require('./ast');
var common = require('./common');
var basic = require('./forms/basic');
var defineSet = require('./forms/define-set');

require('./forms/simple');
require('./forms/lambda');
require('./forms/let');
require('./forms/cond');
require('./forms/do');

var TokenTypes = lexer.TokenTypes;
var raiseSyntaxError = common.raiseSyntaxError;

// A cache for syntaxes of all natural languages.
var syntaxCache = {};
// Extracts values from the language tables and stores them
// in the syntax caches.
function populateSyntax(lang) {
  var existingSyntax = syntaxCache[lang];
  if (existingSyntax) {
    return;
  }
  else {
    var get = function get(key) {
      return langTable.get(lang, 'syntax', key);
    };
    syntaxCache[lang] = {
      'quote': get('quote'),
      'lambda': get('lambda'),
      'if': get('if'),
      'set!': get('set!'),
      'begin': get('begin'),
      'cond': get('cond'),
      'and': get('and'),
      'or': get('or'),
      'case': get('case'),
      'let': get('let'),
      'let*': get('let*'),
      'letrec': get('letrec'),
      'do': get('do'),
      'delay': get('delay'),
      'quasiquote': get('quasiquote'),
      'else': get('else'),
      '=>': get('=>'),
      'define': get('define'),
      'unquote': get('unquote'),
      'unquote-splicing': get('unquote-splicing'),
    };
  }
}

function getSyntax(lang) {
  populateSyntax(lang);
  return syntaxCache[lang];
}
function getProcedures(lang) {
  return { // TODO cache
    'eqv?': langTable.get(lang, 'procedures', 'eqv?'),
  };
}

// Reads an expression or a definition from the given token stream.
function readCommandOrDefinition(parsingInfo) {
  // The defintions should be processed first
  // otherwise they will be treated as unknown expressions.
  return defineSet.readDefintion(parsingInfo) || basic.readExpression(parsingInfo);
}
// Reads the whole program from the given token stream.
// While iterating the token stream the parser tries
// to read whole forms and create AST nodes for each of them.
function readProgram(parsingInfo) {
  // TODO read the import declaration
  var forms = [];
  var form = readCommandOrDefinition(parsingInfo);
  while (form) {
    forms.push(form);
    form = readCommandOrDefinition(parsingInfo);
  }
  var token = parsingInfo.tokenStream.advance();
  if (token) {
    if (token.type === TokenTypes.rightParen) {
      raiseSyntaxError(parsingInfo, 'unnecessary_right_paren');
    }
    else {
      raiseSyntaxError(parsingInfo, 'bad_syntax');
    }
  }
  return ast.createProgram(forms);
}

// The main function in the module.
// Creats an AST from programming code.
function parse(text, lang) {
  lang = lang || 'en';
  var syntax = getSyntax(lang);
  var tokenStream = new lexer.TokenStream(text, lang);
  var parsingInfo = {
    lang: lang,
    syntax: syntax,
    procedures: getProcedures(lang),
    tokenStream: tokenStream,
  };
  return readProgram(parsingInfo);
}

module.exports = {
  parse: parse,
};