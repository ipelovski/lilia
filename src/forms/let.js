'use strict';

var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');
var readBody = require('./lambda').readBody;

var TokenTypes = lexer.TokenTypes;
var readExpression = basic.readExpression;
var isVariable = basic.isVariable;
var raiseSyntaxError = common.raiseSyntaxError;
var createLambda = ast.createLambda;
var createDefinition = ast.createDefinition;
var createVariable = ast.createVariable;
var createProcedureCall = ast.createProcedureCall;

function guardLetBinding(parsingInfo, token) {
  if (!token) {
    raiseSyntaxError(parsingInfo, 'let_binding_end_unexpected');
  }
}
// Reads a binding in a 'let' expression
// from the given token stream.
function readBinding(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.advance();
  guardLetBinding(parsingInfo, token);
  if (token.type === TokenTypes.rightParen) {
    return null;
  }
  if (token.type !== TokenTypes.leftParen) {
    raiseSyntaxError(parsingInfo, 'expected_let_binding');
  }
  token = tokenStream.advance();
  guardLetBinding(parsingInfo, token);
  if (!isVariable(parsingInfo.syntax, token)) {
    raiseSyntaxError(parsingInfo, 'expected_let_variable');
  }
  var variable = token.value;
  var init = readExpression(parsingInfo);
  if (!init) {
    raiseSyntaxError(parsingInfo, 'expected_let_init');
  }
  token = tokenStream.advance();
  guardLetBinding(parsingInfo, token);
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'expected_let_binding_end');
  }
  return {
    variable: variable,
    init: init
  };
}
// Reads the bindings in a 'let' expression
// from the given token stream.
function readBindings(parsingInfo) {
  var bindings = [];
  var binding = readBinding(parsingInfo);
  while (binding) {
    bindings.push(binding);
    binding = readBinding(parsingInfo);
  }
  return bindings;
}
// Reads the body forms of a 'let' expression
// from the given token stream.
function readLetBody(parsingInfo) {
  return readBody(parsingInfo, 'no_let_body_exprs');
}
function guardLet(parsingInfo, token) {
  if (!token) {
    raiseSyntaxError(parsingInfo, 'let_end_unexpected');
  }
}
// Reads the content of let forms
// from the given token stream
// since they have common syntax.
function readLetContent(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  // TODO parameterize the error messages for different let forms
  var bindings;
  var token = tokenStream.advance();
  guardLet(parsingInfo, token);
  if (token.type === TokenTypes.leftParen) {
    bindings = readBindings(parsingInfo);
  }
  else {
    raiseSyntaxError(parsingInfo, 'let_bindings_exptected');
  }

  var body = readLetBody(parsingInfo);
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'let_body_end_unexpected');
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'invalid_let_body_end');
  }
  return [bindings, body];
}
// Reads a 'let' expression
// from the given token stream.
function readLet(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  guardLet(parsingInfo, token);
  var name;    
  if (isVariable(parsingInfo.syntax, token)) {
    name = token.value;
    tokenStream.advance();
  }    
  var letContent = readLetContent(parsingInfo);
  var bindings = letContent[0];
  var body = letContent[1];
  var bindingsCount = bindings.length;
  var formals = new Array(bindingsCount);
  var inits = new Array(bindingsCount);
  for (var i = 0; i < bindingsCount; i++) {
    formals[i] = bindings[i].variable;
    inits[i] = bindings[i].init;
  }
  var procedure = createLambda(formals, body, parsingInfo);
  if (!name) {
    /*
    (let ((a 0)) (+ a 1)) =>
    ((lambda (a) (+ a 1)) 0)
    */
    return createProcedureCall(procedure, inits);
  }
  else {
    /*
    (let foo ((a 0))
      (if (< a 10)
          (foo (+ a 1))
        a))
    =>
    ((lambda ()
      (define foo
        (lambda (a)
          (if (< a 10)
              (foo (+ a 1))
            a)))
      (foo 0)))
    */
    var lambdaDefinition = createDefinition(name, procedure);
    var lambdaApplication = createProcedureCall(createVariable(name), inits);
    var enclosingProcedure = createLambda([], [lambdaDefinition, lambdaApplication], parsingInfo);
    return createProcedureCall(enclosingProcedure, []);
  }
}
// Reads a 'letrec' expression
// from the given token stream.
function readLetrec(parsingInfo) {
  var letContent = readLetContent(parsingInfo);
  var bindings = letContent[0];
  var body = letContent[1];
  /*
  (letrec ((a 0) (b 0)) (+ a b)) =>
  ((lambda () (define a 0) (define b 0) (+ a b)))
  */
  var bindingsCount = bindings.length;
  var definitions = new Array(bindingsCount);
  var binding;
  for (var i = 0; i < bindingsCount; i++) {
    binding = bindings[i];
    definitions[i] = createDefinition(binding.variable, binding.init);
  }
  body = definitions.concat(body);
  var procedure = createLambda([], body, parsingInfo);
  return createProcedureCall(procedure, []);
}
// Reads a 'let*' expression
// from the given token stream.
function readLetstar(parsingInfo) {
  var letContent = readLetContent(parsingInfo);
  var bindings = letContent[0];
  var body = letContent[1];
  /*
  (let* ((a 0) (b 0)) (+ a b)) =>
  ((lambda (a)
    ((lambda (b)
      (+a b)) 0)) 0)
  */
  var bindingsCount = bindings.length;
  var formals, inits, procedure;
  if (bindingsCount === 0) {
    procedure = createLambda([], body, parsingInfo);
    return createProcedureCall(procedure, []);
  }
  else {
    for (var i = bindingsCount - 1; i >= 0; i--) {
      formals = [bindings[i].variable];
      inits = [bindings[i].init];
      procedure = createLambda(formals, body, parsingInfo);
      body = [createProcedureCall(procedure, inits)];
    }
    return body[0];
  }
}

basic.registerFormReader('let', readLet);
basic.registerFormReader('letrec', readLetrec);
basic.registerFormReader('let*', readLetstar);

module.exports = {
  readLet: readLet,
  readLetrec: readLetrec,
  readLetstar: readLetstar,
};