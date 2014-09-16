'use strict';

var common = require('../common');
var lexer = require('../lexer');
var quote = require('./quote');
var ast = require('../ast');

var TokenTypes = lexer.TokenTypes;
var raiseSyntaxError = common.raiseSyntaxError;

// Checks if a token object is tagged as an identifier.
function isIdentifier(token) {
  return token.type === TokenTypes.identifier;
}
function isSyntax(value, syntax) {
  for (var key in syntax) {
    if (syntax[key] === value) {
      return true;
    }
  }
  return false;
}
// Checks if a token object contains a syntax keyword.
function isSyntaticKeyword(syntax, token) {
  return isIdentifier(token) && isSyntax(token.value, syntax);
}
// Checks if a token object contains a variable.
function isVariable(syntax, token) {
  return isIdentifier(token) && !isSyntax(token.value, syntax);
}
// Checks if a token object represents a self-evaluating expression.
function isSelfEvaluating(token) {
  var tokenType = token.type;
  return tokenType === TokenTypes.boolean ||
     tokenType === TokenTypes.number ||
     tokenType === TokenTypes.character ||
     tokenType === TokenTypes.string;
}

var formReaderKeys = [];
var formReadersMap = {};
function getFormReader(syntax, tokenValue) {
  var formReaderKey;
  for (var i = 0; i < formReaderKeys.length; i++) {
    formReaderKey = formReaderKeys[i];
    if (syntax[formReaderKey] === tokenValue) {
      return formReadersMap[formReaderKey];
    }
  }
  return null;
}
function registerFormReader(formReaderKey, formReader) {
  formReaderKeys.push(formReaderKey);
  formReadersMap[formReaderKey] = formReader;
}

// Reads procedure call from the given token stream.
function readProcedureCall(parsingInfo) {
  var token;
  var procedure = readExpression(parsingInfo);
  var args = [];
  var expression = readExpression(parsingInfo);
  while(expression) {
    args.push(expression);
    expression = readExpression(parsingInfo);
  }
  token = parsingInfo.tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'proc_call_end_unexpected');
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'invalid_proc_call');
  }
  return ast.createProcedureCall(procedure, args);
}

// Reads any expression from the given token stream.
function readExpression(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var syntax = parsingInfo.syntax;
  var token = tokenStream.peek();
  var nextToken;
  if (!token) {
    return null;
  }
  // If it is a lone variable then it will be evaluated.
  if (isIdentifier(token)) {
    if (!isSyntaticKeyword(syntax, token)) {
      tokenStream.advance();
      return ast.createVariable(token.value);
    }
    else {
      raiseSyntaxError(parsingInfo, 'syntax_keywords_as_variables');
    }
  }
  // Checks if it is a self-evaluating expression, e.g. a string literal.
  if (isSelfEvaluating(token)) {
    tokenStream.advance();
    return ast.createLiteral(token.type, token.value);
  }
  // If it is an openning vector parenthesis then reads the whole vector.
  if (token.type === TokenTypes.vectorParen) {
    tokenStream.advance();
    return quote.readVectorDatum(parsingInfo);
  }
  // If it is a quotation then reads the quoted datum.
  if (token.type === TokenTypes.quote) {
    tokenStream.advance();
    return quote.readDatum(parsingInfo);
  }
  // If it is an openning parenthesis
  // then tries to read a specific expression.
  if (token.type === TokenTypes.leftParen) {
    tokenStream.advance();
    nextToken = tokenStream.peek();
    if (!nextToken) {
      raiseSyntaxError(parsingInfo, 'list_end_unexpected');
    }
    if (isIdentifier(nextToken)) {
      var formReader = getFormReader(syntax, nextToken.value);
      if (formReader) {
        tokenStream.advance();
        return formReader(parsingInfo);
      }
      if (nextToken.value === syntax['quote']) {
        tokenStream.advance();
        return quote.readQuote(parsingInfo);
      }
      if (nextToken.value === syntax['define']) {
        raiseSyntaxError(parsingInfo, 'expr_expected_definition_found');
      }
    }
    if (nextToken.type === TokenTypes.leftParen || isVariable(syntax, nextToken)) {        
      return readProcedureCall(parsingInfo);
    }
    if (nextToken.type === TokenTypes.boolean
      || nextToken.type === TokenTypes.number
      || nextToken.type === TokenTypes.character
      || nextToken.type === TokenTypes.string) {
      raiseSyntaxError(parsingInfo, 'data_not_procedure');
    }
    raiseSyntaxError(parsingInfo, 'unknown_expr');
  }
  return null;
}

module.exports = {
  registerFormReader: registerFormReader,
  readExpression: readExpression,
  isIdentifier: isIdentifier,
  isVariable: isVariable,
};