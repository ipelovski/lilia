'use strict';

var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');
var defineSet = require('./define-set');

var TokenTypes = lexer.TokenTypes;
var readExpression = basic.readExpression;
var isVariable = basic.isVariable;
var raiseSyntaxError = common.raiseSyntaxError;

// Reads the formal parameters of a lambda expression
// from the given token stream.
function readLambdaFormals(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var syntax = parsingInfo.syntax;
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'lambda_expr_end_unexpected');
  }
  if (isVariable(syntax, token)) {
    return token.value;
  }
  if (token.type === TokenTypes.leftParen) {
    var formals = [];
    token = tokenStream.advance();
    while(token && isVariable(syntax, token)) {
      formals.push(token.value);
      token = tokenStream.advance();
    }
    if (!token) {
      raiseSyntaxError(parsingInfo, 'lambda_formals_end_unexpected');
    }
    if (token.type === TokenTypes.dot) {
      formals.push('.');
      token = tokenStream.advance();
      if (!token) {
        raiseSyntaxError(parsingInfo, 'lambda_formals_end_unexpected');
      }
      if (!isVariable(syntax, token)) {
        raiseSyntaxError(parsingInfo, 'lambda_expr_variable_expected');
      }
      formals.push(token.value);
      token = tokenStream.advance();
      if (!token) {
        raiseSyntaxError(parsingInfo, 'lambda_formals_end_unexpected');
      }
    }
    if (token.type === TokenTypes.rightParen) {
      return formals;
    }
    else {
      raiseSyntaxError(parsingInfo, 'expected_lambda_formals_end');
    }
  }
  else {
    raiseSyntaxError(parsingInfo, 'invalid_lambda_formals');
  }
}
// Reads a sequence of forms that are a part of another form
// from the given token stream.
function readBody(parsingInfo, missingBodyExpressionsErrorKey) {
  var forms = [];
  var definition = defineSet.readDefintion(parsingInfo);
  while (definition) {
    forms.push(definition);
    definition = defineSet.readDefintion(parsingInfo);
  }
  var expression = readExpression(parsingInfo);
  if (!expression) {
    raiseSyntaxError(parsingInfo, missingBodyExpressionsErrorKey);
  }
  while (expression) {
    forms.push(expression);
    expression = readExpression(parsingInfo);
  }
  return forms;
}
// Reads the body forms of a lambda expression
// from the given token stream.
function readLambdaBody(parsingInfo) {
  return readBody(parsingInfo, 'no_lambda_body_exprs');
}
// Reads a lambda expression
// from the given token stream.
function readLambda(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var formals = readLambdaFormals(parsingInfo);
  var body = readLambdaBody(parsingInfo);
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'lambda_body_end_unexpected');
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'invalid_lambda_body_end');
  }
  return ast.createLambda(parsingInfo, formals, body);
}

basic.registerFormReader('lambda', readLambda);

exports.readBody = readBody;
exports.readLambda = readLambda;