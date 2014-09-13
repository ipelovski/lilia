'use strict';

var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');

var TokenTypes = lexer.TokenTypes;
var FormTypes = ast.FormTypes;
var readExpression = basic.readExpression;
var isIdentifier = basic.isIdentifier;
var raiseSyntaxError = common.raiseSyntaxError;

// Reads a clause for a 'cond' expression from the given token stream.
function readCondClause(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var syntax = parsingInfo.syntax;
  var token = tokenStream.peek();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'cond_clause_end_unexpected');
  }
  if (token.type === TokenTypes.leftParen) {
    tokenStream.advance();
    token = tokenStream.peek();
    var test, expression;
    var elseFound = false;
    if (isIdentifier(token) && token.value === syntax['else']) {
      tokenStream.advance();
      elseFound = true;
    }
    else {
      test = readExpression(parsingInfo);
      if (!test) {
        raiseSyntaxError(parsingInfo, 'cond_clause_test_expected');
      }
    }
    token = tokenStream.peek();
    if (isIdentifier(token) && token.value === syntax['=>']) {
      if (elseFound) {
        raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
      }
      tokenStream.advance();
      expression = readExpression(parsingInfo);
      if (!expression) {
        raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
      }
      token = tokenStream.advance();
      if (token.type === TokenTypes.rightParen) {
        return ast.createCondArrow(test, expression);
      }
      else {
        raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
      }
    }
    else {
      var expressions = [];
      expression = readExpression(parsingInfo);
      while (expression) {
        expressions.push(expression);
        expression = readExpression(parsingInfo);
      }
      token = tokenStream.advance();
      if (!token) {
        raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
      }
      if (token.type === TokenTypes.rightParen) {
        if (elseFound) {
          return ast.createCondElse(expressions);
        }
        else {
          return ast.createCondClause(test, expressions);
        }
      }
      else {
        raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
      }
    }
  }
  else {
    return null;
  }
}
// Reads a 'cond' expression from the given token stream.
function readCond(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var clauses = [];
  var clause = readCondClause(parsingInfo);
  if (!clause) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  var elseFound = false;
  while (clause) {
    if (elseFound) {
      raiseSyntaxError(parsingInfo, 'cond_else_last');
    }
    if (clause.type === FormTypes.condelse) {
      elseFound = true;
    }
    clauses.push(clause);
    clause = readCondClause(parsingInfo);
  }
  var token = tokenStream.advance();
  if (!token || token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  return ast.createCond(clauses);
}

basic.registerFormReader('cond', readCond);

exports.readCond = readCond;