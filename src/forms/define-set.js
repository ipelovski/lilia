'use strict';

var langTable = require('../lang-table');
var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');
var lambda = require('./lambda'); // TODO circular require

var TokenTypes = lexer.TokenTypes;
var readExpression = basic.readExpression;
var isIdentifier = basic.isIdentifier;
var isVariable = basic.isVariable;
var raiseSyntaxError = common.raiseSyntaxError;
var createDefinition = ast.createDefinition;
var createLambda = ast.createLambda;
var createAssignment = ast.createAssignment;

// Reads the storing of a value to a location
// from the given token stream.
function readValueAssignment(parsingInfo, constructor, formName) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.advance();
  if (!token || !isVariable(parsingInfo.syntax, token)) {
    raiseSyntaxError(parsingInfo, 'variable_expected', [formName]);
  }
  var variable = token.value;
  token = tokenStream.peek(); // TODO maybe move the reading in the readExpression
  if (!token) {
    raiseSyntaxError(parsingInfo, 'expr_expected', [formName]);
  }
  var expression = readExpression(parsingInfo);
  if (!expression) {
    raiseSyntaxError(parsingInfo, 'expr_expected', [formName]);
  }
  token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'expr_end_unexpected', [formName]);
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'right_paren_expected', [formName]);
  }
  return constructor(variable, expression);
}
// Reads the variable name and the formal parameters of a lambda definition
// from the given token stream.
function readDefintionHeader(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.advance();
  if (!token || !isVariable(parsingInfo.syntax, token)) {
    raiseSyntaxError(parsingInfo, 'variable_expected', [formName]);
  }
  var variable = token.value;
  var formals = [];
  token = tokenStream.advance();
  while(token && isVariable(parsingInfo.syntax, token)) {
    formals.push(token.value);
    token = tokenStream.advance();
  }
  if (!token) {
    raiseSyntaxError(parsingInfo, 'definition_formals_end_unexpected');
  }
  if (token.type === TokenTypes.dot) {
    formals.push('.');
    token = tokenStream.advance();
    if (!token) {
      raiseSyntaxError(parsingInfo, 'definition_formals_end_unexpected');
    }
    if (!isVariable(parsingInfo.syntax, token)) {
      raiseSyntaxError(parsingInfo, 'definition_expr_variable_expected');
    }
    formals.push(token.value);
    token = tokenStream.advance();
    if (!token) {
      raiseSyntaxError(parsingInfo, 'definition_formals_end_unexpected');
    }
  }
  if (token.type === TokenTypes.rightParen) {
    if (formals[0] === '.') {
      formals = formals[1];
    }
    return {
      identifier: variable,
      formals: formals
    };
  }
  else {
    raiseSyntaxError(parsingInfo, 'expected_lambda_formals_end');
  }
}
// Reads a defintion form from the given token stream.
function readDefintion(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  var nextToken;
  if (!token) {
    return null;
  }
  var formName = langTable.get(parsingInfo.lang, 'syntax-common', 'definition'); // TODO cache
  if (token.type === TokenTypes.leftParen) {
    nextToken = tokenStream.peek(1);
    if (!nextToken) {
      raiseSyntaxError(parsingInfo, 'list_end_unexpected');
    }
    if (isIdentifier(nextToken) && nextToken.value === parsingInfo.syntax['define']) {
      tokenStream.advance(2);
      token = tokenStream.peek();
      if (!token) {
        raiseSyntaxError(parsingInfo, 'definition_end_unexpected');
      }
      if (isVariable(parsingInfo.syntax, token)) {
        return readValueAssignment(parsingInfo, createDefinition, formName);
      }
      else if (token.type === TokenTypes.leftParen) {
        tokenStream.advance();
        var header = readDefintionHeader(parsingInfo);
        var body = lambda.readBody(parsingInfo, 'no_definition_exprs');
        token = tokenStream.advance();
        if (!token) {
          raiseSyntaxError(parsingInfo, 'definition_body_end_unexpected');
        }
        if (token.type !== TokenTypes.rightParen) {
          raiseSyntaxError(parsingInfo, 'invalid_definition_body_end');
        }
        // markTailContext(body[body.length - 1]);
        return createDefinition(header.identifier, createLambda(header.formals, body, parsingInfo));
      }
    }
  }
  return null;
}
// Reads a 'set!' expression
// from the given token stream.
function readAssignment(parsingInfo) {
  return readValueAssignment(parsingInfo, createAssignment, langTable.get(parsingInfo.lang, 'syntax-common', 'assignment')); // TODO cache
}

basic.registerFormReader('set!', readAssignment);

exports.readDefintion = readDefintion;