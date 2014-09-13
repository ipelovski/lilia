'use strict';

var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');
var readDefintion = require('./define-set').readDefintion;

var TokenTypes = lexer.TokenTypes;
var readExpression = basic.readExpression;
var raiseSyntaxError = common.raiseSyntaxError;

// Reads the alternate expression in an 'if' form
// from the given token stream.
function readAlternate(parsingInfo) {
  var token = parsingInfo.tokenStream.peek();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'if_expr_end_unexpected');
  }
  if (token.type === TokenTypes.rightParen) {
    return null;
  }
  return readExpression(parsingInfo);
}
// Reads an 'if' expression from the given token stream.
function readIf(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'if_expr_end_unexpected');
  }
  var test = readExpression(parsingInfo);
  var consequent = readExpression(parsingInfo);
  var alternate = readAlternate(parsingInfo);
  token = tokenStream.advance();
  if (! token || token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'if_expression_end_expected');
  }
  return ast.createIf(test, consequent, alternate);
}  

// Reads an 'and' or an 'or' expressions
// from the given token stream.
function readBoolOperations(parsingInfo, syntaxKey, constructor) {
  var tests = []
  var expression = readExpression(parsingInfo);
  while (expression) {
    tests.push(expression);
    expression = readExpression(parsingInfo);
  }
  var token = parsingInfo.tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'expr_end_unexpected', [syntax[syntaxKey]]);
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'right_paren_expected', [syntax[syntaxKey]]);
  }
  return constructor(tests);
}
// Reads an 'and' expressions from the given token stream.
function readConjunction(parsingInfo) {
  return readBoolOperations(parsingInfo, 'and', ast.createConjunction);
}
// Reads an 'or' expressions from the given token stream.
function readDisjunction(parsingInfo) {
  return readBoolOperations(parsingInfo, 'or', ast.createDisjunction);
}
// Reads a 'begin' expression
// from the given token stream.
function readBegin(parsingInfo) {
  var forms = [];
  var form = readDefintion(parsingInfo);
  var readFunc;
  if (form) {
    readFunc = readDefintion;
  }
  else {
    readFunc = readExpression;
    form = readFunc(parsingInfo);
  }
  while (form) {
    forms.push(form);
    form = readFunc(parsingInfo);
  }
  var token = parsingInfo.tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'expr_end_unexpected', [syntax['begin']]);
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'right_paren_expected', [syntax['begin']]);
  }
  return ast.createBegin(forms);
}

basic.registerFormReader('and', readConjunction);
basic.registerFormReader('or', readDisjunction);
basic.registerFormReader('begin', readBegin);
basic.registerFormReader('if', readIf);

module.exports = {
  readConjunction: readConjunction,
  readDisjunction: readDisjunction,
  readBegin: readBegin,
  readIf: readIf,
};