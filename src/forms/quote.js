'use strict';

var common = require('../common');
var lexer = require('../lexer');
var ast = require('../ast');

var TokenTypes = lexer.TokenTypes;
var raiseSyntaxError = common.raiseSyntaxError;
var createLiteral = ast.createLiteral;

// A constant containing the types of simple data
var simpleDatums = [
  TokenTypes.boolean,
  TokenTypes.number,
  TokenTypes.character,
  TokenTypes.string,
  TokenTypes.identifier
  // TODO byte vector
];
// Reads a simple datum from the given token stream.
function readSimpleDatum(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  if (simpleDatums.indexOf(token.type) !== -1) {
    tokenStream.advance();
    return createLiteral(token.type, token.value);
  }
  return null;
}
// Reads a list datum from the given token stream.
function readListDatum(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var listItems = [];
  var literal = readDatum(parsingInfo);
  while (literal) {
    listItems.push(literal);
    literal = readDatum(parsingInfo);
  }
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'list_end_unexpected');
  }
  if (token.type === TokenTypes.dot) {
    listItems.push('.');
    token = tokenStream.peek();
    if (!token) {
      raiseSyntaxError(parsingInfo, 'list_end_unexpected');
    }
    literal = readDatum(parsingInfo);
    if (!literal) {
      raiseSyntaxError(parsingInfo, 'datum_expected');
    }
    listItems.push(literal);
    token = tokenStream.peek();
    if (!token) {
      raiseSyntaxError(parsingInfo, 'list_end_unexpected');
    }
    tokenStream.advance();
  }
  if (token.type === TokenTypes.rightParen) {      
    return createLiteral('list', listItems);
  }
  else {
    raiseSyntaxError(parsingInfo, 'expected_list_end');
  }
}
// Reads a vector datum from the given token stream.
function readVectorDatum(parsingInfo) {
  var vectorItems = [];
  var literal = readDatum(parsingInfo);
  while (literal) {
    vectorItems.push(literal);
    literal = readDatum(parsingInfo);
  }
  var token = parsingInfo.tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'vector_end_unexpected');
  }
  if (token.type === TokenTypes.rightParen) {
    return createLiteral('vector', vectorItems);
  }
  else {
    raiseSyntaxError(parsingInfo, 'expected_vector_end');
  }
}
// Reads a list or a vector datum from the given token stream.
function readCompoundDatum(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  if (token.type === TokenTypes.leftParen) {
    tokenStream.advance();
    return readListDatum(parsingInfo);
  }
  if (token.type === TokenTypes.vectorParen) {
    tokenStream.advance();
    return readVectorDatum(parsingInfo);
  }
  if (token.type === TokenTypes.quote) {
    tokenStream.advance();
    var quoteLiteral = createLiteral(TokenTypes.identifier, parsingInfo.syntax['quote']);
    var datum = readDatum(parsingInfo);
    return createLiteral('list', [quoteLiteral, datum]);
  }
  return null;
  // TODO read abbreviations
}
// Reads a single datum from the given token stream.
function readDatum(parsingInfo, raise) {
  var datum = readSimpleDatum(parsingInfo) || readCompoundDatum(parsingInfo);
  if (!datum && raise) {
    raiseSyntaxError(parsingInfo, 'invalid_datum');
  }
  return datum;
}
// Reads a quote literal from the given token stream.
function readQuote(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var datum = readDatum(parsingInfo);
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'expr_end_unexpected', [parsingInfo.syntax['quote']]);
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'right_paren_expected', [parsingInfo.syntax['quote']]);
  }
  return datum;
}

module.exports = {
  readVectorDatum: readVectorDatum,
  readDatum: readDatum,
  readQuote: readQuote,
};