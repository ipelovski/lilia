'use strict';

var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');

var TokenTypes = lexer.TokenTypes;
var FormTypes = ast.FormTypes;
var raiseSyntaxError = common.raiseSyntaxError;
var readExpression = basic.readExpression;
var createVariable = ast.createVariable;
var createProcedureCall = ast.createProcedureCall;
var createLambda = ast.createLambda;
var createBegin = ast.createBegin;
var createIf = ast.createIf;
var createDefinition = ast.createDefinition;

function readDoVariable(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  if (token.type === TokenTypes.leftParen) {
    var variable = readExpression(parsingInfo);
    if (!variable || variable.type !== FormTypes.variable) {
      raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
    }
    var init = readExpression(parsingInfo);
    if (!init) {
      raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
    }
    var step = readExpression(parsingInfo);
    token = tokenStream.advance();
    if (token.type === TokenTypes.rightParen) {
      return {
        variable: variable.value,
        init: init,
        step: step
      };
    }
    else {
      raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
    }
  }
  else {
    return null;
  }
}
function readDoVariables(parsingInfo) {
  var token = parsingInfo.tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  if (token.type === TokenTypes.leftParen) {
    var variables = [];
    var variable = readDoVariable(parsingInfo);
    while (variable) {
      variables.push(variable);
      variable = readDoVariable(parsingInfo);
    }
    return variables;
  }
  else {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
}
function readDoTest(parsingInfo) {
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  if (token.type === TokenTypes.leftParen) {
    var test = readExpression(parsingInfo);
    if (!test) {
      raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
    }
    var expressions = [];
    var expression = readExpression(parsingInfo);
    while (expression) {
      expressions.push(expression);
      expression = readExpression(parsingInfo);
    }
    token = tokenStream.advance();
    if (token.type === TokenTypes.rightParen) {
      return {
        test: test,
        expressions: expressions
      };
    }
    else {
      raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
    }
  }
  else {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
}
// Reads a 'do' expression from the given token stream.
function readDo(parsingInfo) {
  var variables = readDoVariables(parsingInfo);
  var test = readDoTest(parsingInfo);
  var commands = [];
  var command = readExpression(parsingInfo);
  while (command) {
    commands.push(command);
    command = readExpression(parsingInfo);
  }
  var token = parsingInfo.tokenStream.advance();
  if (!token || token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'bad_syntax'); // TODO make it specific
  }
  /*
  (do ((variable1 init1 step1) ...)
    (test expression ...)
    command ...)
  =>
  ((lambda ()
    (define @-60-@
      (lambda (variable1 ...)
        (if test
          (begin expression ...)
          (begin
            command ...
            (@-60-@ step1 ...)))))
    (@-60-@ init1 ...)))

  (do ((vec (vector 0 0 0 0 0))
       (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i i))
  =>
  ((lambda ()
    (define @-60-@
      (lambda (vec i)
        (if (= i 5)
          (begin vec)
          (begin
            (vector-set! vec i i)
            (@-60-@ vec (+ i 1))))))
    (@-60-@ (vector 0 0 0 0 0) 0)))
  */
  var innerLambdaName = '@-60-@';
  var innerLambdaVariable = createVariable(innerLambdaName);
  var tailCall = createProcedureCall(innerLambdaVariable,
    variables.map(function (variable) {
      return variable.step || createVariable(variable.variable);
    }));
  commands.push(tailCall);
  var ifExpression = createIf(test.test,
    createBegin(test.expressions),
    createBegin(commands));
  var innerLambda = createLambda(parsingInfo,
    variables.map(function (variable) {
      return variable.variable;
    }),
    [ifExpression]);
  var outerLambdaBody = [
    createDefinition(innerLambdaName, innerLambda),
    createProcedureCall(createVariable(innerLambdaName),
      variables.map(function (variable) {
        return variable.init;
      }))
  ];
  var outerLambda = createLambda(parsingInfo, [], outerLambdaBody);
  return createProcedureCall(outerLambda, []);
}

basic.registerFormReader('do', readDo);

exports.readDo = readDo;