(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
require('./src/common');
var langEn = require('./src/lang-en');
var langBg = require('./src/lang-bg');
var evaluator = require('./src/evaluator');

if (typeof window !== 'undefined'){
  window.lilia = {
    evaluate: evaluator.evaluate,
    session: evaluator.session,
    setOutputPortHandler: evaluator.setOutputPortHandler,
    string: evaluator.string,
    'lang-en': langEn,
    'lang-bg': langBg,
  }
}
},{"./src/common":3,"./src/evaluator":5,"./src/lang-bg":14,"./src/lang-en":15}],2:[function(require,module,exports){
'use strict';

var TokenTypes = require('./lexer').TokenTypes;
var raiseSyntaxError = require('./common').raiseSyntaxError;

// The values used to tag special forms.
var FormTypes = {
  program: 'program',
  ops: 'ops',
  variable: 'variable',
  literal: 'literal',
  procedureCall: 'procedureCall',
  arguments: 'arguments',
  tailCall: 'tailCall',
  lambda: 'lambda',
  lambdaFormals: 'lambdaFormals',
  lambdaBody: 'lambdaBody',    
  ifexpr: 'ifexpr',
  test: 'test',
  ifthen: 'ifthen',
  ifelse: 'ifelse',
  cond: 'cond',
  condclause: 'condclause',
  condelse: 'condelse',
  condarrow: 'condarrow',
  condarrowthen: 'condarrowthen',
  assignment: 'assignment',
  definition: 'definition',
  internalDefinition: 'internalDefinition',
  conjunction: 'conjunction',
  disjunction: 'disjunction',
  begin: 'begin',
  void: 'void',
};

function createValueNode(type, value) {
  return {
    type: type,
    value: value,
    nodes: null
  };
}

function createInnerNode(type, children) {
  return {
    type: type,
    value: null,
    nodes: children
  };
}

function cloneNode(node) {
  return {
    type: node.type,
    value: node.value,
    nodes: (node.nodes ? node.nodes.map(function(node) {
      return cloneNode(node);
    }) : null)
  };
}
// Traverses a subtree of an AST and marks the nodes
// in a tail context by adding a boolean property to them.
function markTailContext(expression) {
  if (expression.type === FormTypes.ifexpr) {
    markTailContext(expression.nodes[1]);
    if (expression.nodes[2]) {
      markTailContext(expression.nodes[2]);
    }
  }
  else if ((expression.type === FormTypes.ifthen
    || expression.type === FormTypes.ifelse
    || expression.type === FormTypes.test) // the 'test' expressions in conjunctions and disjunctions
    && expression.nodes) { // ifelse can have nodes === null
    markTailContext(expression.nodes[0]);
  }
  else if (expression.type === FormTypes.conjunction
    || expression.type === FormTypes.disjunction
    || expression.type === FormTypes.begin) {
    var tests = expression.nodes;
    if (tests.length > 0) {
      markTailContext(tests[tests.length - 1]);
    }
  }
  else if (expression.type === FormTypes.procedureCall) {
    var procedure = expression.nodes[1];
    // tail 'let' expression
    if (procedure.type === FormTypes.lambda) {
      var procedureBody = procedure.nodes[1];
      markTailContext(procedureBody.nodes[procedureBody.nodes.length - 1]);
    }
    else {
      expression.type = FormTypes.tailCall;
    }
  }
}
// A constructor for an AST node containing a definition.
function createDefinition(identifier, value) {
  return createInnerNode(FormTypes.definition,
    [createLiteral(TokenTypes.identifier, identifier), value]);
}
// A constructor for an AST node containing an internal definition.
function createInternalDefinition(identifiers) {
  return createValueNode(FormTypes.internalDefinition, identifiers);
}
// A constructor for an AST node containing an assignment.
function createAssignment(identifier, value) {
  return createInnerNode(FormTypes.assignment,
    [createLiteral(TokenTypes.identifier, identifier), value]);
}
// A constructor for an AST node containing an 'if' conditional.
function createIf(test, consequent, alternate) {
  return createInnerNode(FormTypes.ifexpr,
    [createInnerNode(FormTypes.test, [test]),
     createInnerNode(FormTypes.ifthen, [consequent]),
     createInnerNode(FormTypes.ifelse, alternate ? [alternate] : null)]);
}  
// A constructor for an AST node containing an 'cond' 'else' clause.
function createCondClause(test, expressions) {
  var nodes = [createInnerNode(FormTypes.test, [test])];
  if (expressions.length > 0) {
    nodes.push(createInnerNode(FormTypes.ifthen,
      [createBegin(expressions)]));
  }
  return createInnerNode(FormTypes.condclause, nodes);
}
// A constructor for an AST node containing an 'cond' 'else' clause.
function createCondElse(expressions) {
  return createInnerNode(FormTypes.condelse,
    [createInnerNode(FormTypes.test, [createLiteral(TokenTypes.boolean, true)]),
     createInnerNode(FormTypes.ifthen, [createBegin(expressions)])]);
}
// A constructor for an AST node containing an 'cond' '=>' clause.
function createCondArrow(test, expression) {
  return createInnerNode(FormTypes.condarrow,
    [createInnerNode(FormTypes.test, [test]),
     createInnerNode(FormTypes.condarrowthen,
       [createProcedureCall(expression, 1)])]);
}
// A constructor for an AST node containing an 'cond' conditional.
function createCond(clauses) {
  // TODO mark clauses for tail call if necessary
  return createInnerNode(FormTypes.cond, clauses);
}
// A constructor for an AST node containing a procedure call.
function createProcedureCall(procedure, args, position) {
  if (Array.isArray(args)) {
    return createInnerNode(FormTypes.procedureCall,
      [createInnerNode(FormTypes.arguments, args), procedure, createValueNode('position', position || null)]);
  }
  else {
    return createInnerNode(FormTypes.procedureCall,
      [createValueNode(FormTypes.arguments, args), procedure, createValueNode('position', position || null)]);
  }
}
// Converts definitions into internal definitions and assignments.
// Checks for duplicate definitions.
function transformAndCheckDefinitions(parsingInfo, forms) {
  var identifiers = [];
  var exprFound = false;
  function traverse(forms) {
    var form, identifier;
    for (var i = 0; i < forms.length; i++) {
      form = forms[i];
      if (form.type === FormTypes.begin) {
        traverse(form.nodes);
      }
      if (form.type === FormTypes.definition) {
        // checks for expressions mixed with definitions
        if (exprFound) {
          // TODO the position of the token stream will be incorrect
          raiseSyntaxError(parsingInfo, 'definition_expected_expr_found');
        }
        identifier = form.nodes[0].value.value;
        if (identifiers.indexOf(identifier) !== -1) {
          // TODO the position of the token stream will be incorrect
          raiseSyntaxError(parsingInfo, 'duplicate_definitions', [identifier]);
        }
        else {
          identifiers.push(identifier);
          form.type = FormTypes.assignment;
        }
      }
      else if (form.type !== FormTypes.begin) {
        exprFound = true;
      }
    }
  }
  traverse(forms);
  forms.unshift(createInternalDefinition(identifiers));
  return forms;
}
// A constructor for an AST node containing a lambda definition.
function createLambda(parsingInfo, formals, body, name) {
  body = transformAndCheckDefinitions(parsingInfo, body);
  if (body.length > 0) {
    markTailContext(body[body.length - 1]);
  }
  return createInnerNode(FormTypes.lambda,
    [createValueNode(FormTypes.lambdaFormals, formals),
     createInnerNode(FormTypes.lambdaBody, body),
     createValueNode(FormTypes.variable, name)]);
}
// A constructor for an AST node containing an 'and' expression.
function createConjunction(tests) {
  return createInnerNode(FormTypes.conjunction,
    tests.map(function(test) {
      return createInnerNode(FormTypes.test, [test]);
    }));
}
// A constructor for an AST node containing an 'or' expression.
function createDisjunction(tests) {
  return createInnerNode(FormTypes.disjunction,
    tests.map(function(test) {
      return createInnerNode(FormTypes.test, [test]);
    }));
}
// A constructor for an AST node containing a variable evaluation.
function createVariable(identifier) {
  return createValueNode(FormTypes.variable, identifier);
}
// A constructor for an AST node containing a self-evaluating literal.
function createLiteral(type, value) {
  return createValueNode(FormTypes.literal, {
    type: type,
    value: value
  });
}
// A constructor for an AST node containing a sequence of forms.
function createBegin(forms) {
  return createInnerNode(FormTypes.begin, forms);
}
// A constructor for an AST node containing a whole program.
function createProgram(forms) {
  if (forms.length === 0) {
    forms.push(createValueNode(FormTypes.void, null));
  }
  return createInnerNode(FormTypes.program, forms);  
}

module.exports = {
  FormTypes: FormTypes,
  createDefinition: createDefinition,
  createAssignment: createAssignment,
  createIf: createIf,
  createCond: createCond,
  createCondArrow: createCondArrow,
  createCondElse: createCondElse,
  createCondClause: createCondClause,
  createProcedureCall: createProcedureCall,
  createLambda: createLambda,
  createConjunction: createConjunction,
  createDisjunction: createDisjunction,
  createVariable: createVariable,
  createLiteral: createLiteral,
  createBegin: createBegin,
  createProgram: createProgram,
};
},{"./common":3,"./lexer":17}],3:[function(require,module,exports){
/*
A module containing common functions.
*/
// Formats a string similar to the .net String.Format() method.
var langTable = require('./lang-table');

var langName = '@-1Ang-@';

if (!String.format) {
  String.format = function(format, args) {
    if (!Array.isArray(args)) {
      args = Array.prototype.slice.call(arguments, 1);
    }
    return format.replace(/{(\d+)}/g, function(match, number) { 
      return typeof args[number] !== 'undefined'
        ? args[number] 
        : match;
    });
  };
}
function guardArgsCountExact(env, argsCount, expected) {
  if (argsCount !== expected) {
    raiseRuntimeError(env, 'exact_args_count_expected', [expected, argsCount]);
  }
}
function guardArgsCountMin(env, argsCount, expected) {
  if (argsCount < expected) {
    raiseRuntimeError(env, 'min_args_count_expected', [expected, argsCount]);
  }
}
function guardArgsCountMax(env, argsCount, expected) {
  if (argsCount > expected) {
    raiseRuntimeError(env, 'max_args_count_expected', [expected, argsCount]);
  }
}
function guardArgPredicate(env, arg, predicate, argPosition, predicateCat, predicateKey) {
  if (!predicate([arg], env)) {
    var lang = env.getVar(langName);
    var predicateName = langTable.get(lang, predicateCat, predicateKey);
    raiseRuntimeError(env, 'argument_predicate_false', [argPosition, predicateName]);
  }
}
function guardImmutable(env, arg) {
  if (arg.immutable) {
    raiseRuntimeError(env, 'mutating_immutable_object');
  }
}
function cloneEnvs(envs) {
  return envs.map(function (env) {
    return env.clone();
  });
}
// Raises an error by throwing it.
// This function is used only in the parser.
function raiseSyntaxError(parsingInfo, messageKey, messageParams) {
  var message = langTable.get(parsingInfo.lang, 'syntax-errors', messageKey);
  if (messageParams) {
    message = String.format(message, messageParams);
  }
  var err = new Error(message);
  var position = parsingInfo.tokenStream.getPosition();
  err.line = position.line;
  err.column = position.column;
  err.toString = function() {
    return Error.prototype.toString.call(this) +
      ' (line: ' + this.line + ', column: ' + this.column + ')';
  };
  throw err;
}
function raiseRuntimeError(env, messageKey, messageParams) {
  var lang = env.getVar(langName);
  var message = langTable.get(lang, 'runtime-errors', messageKey);
  if (messageParams) {
    message = String.format(message, messageParams);
  }
  var err = new Error(message);
  throw err;
}
module.exports = {
  langName: langName,
  guardArgsCountExact: guardArgsCountExact,
  guardArgsCountMin: guardArgsCountMin,
  guardArgsCountMax: guardArgsCountMax,
  guardArgPredicate: guardArgPredicate,
  guardImmutable: guardImmutable,
  cloneEnvs: cloneEnvs,
  raiseSyntaxError: raiseSyntaxError,
  raiseRuntimeError: raiseRuntimeError,
};
},{"./lang-table":16}],4:[function(require,module,exports){
'use strict';

var lexer = require('./lexer');
var parser = require('./parser');
var ast = require('./ast');
var types = require('./types');

var TokenTypes = lexer.TokenTypes;
var FormTypes = ast.FormTypes;
var Symbol = types.Symbol;
var Pair = types.Pair;
var Vector = types.Vector;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var EmptyList = types.EmptyList;

var OPTypes = {
  define: 'define',
  set: 'set',
  internaldefine: 'internaldefine',
  constant: 'constant',
  variable: 'variable',
  jump: 'jump',
  jumpiffalse: 'jumpiffalse',
  jumpiffalsekeep: 'jumpiffalsekeep',
  jumpifnotfalsekeep: 'jumpifnotfalsekeep',
  lambda: 'lambda',
  call: 'call',
  tailcall: 'tailcall',
  discard: 'discard',
  void: 'void',
};

function evalListLiteral(listItems) {
  if (listItems.length === 0) {
    return EmptyList;
  }
  var listLength = listItems.length;
  var improperList = listItems[listLength - 2] === '.';
  var count = improperList ? listLength - 4 : listLength - 1;
  var args = [];
  var pair;
  if (improperList) {
    pair = new Pair(evalLiteral(listItems[listLength - 3]),
      evalLiteral(listItems[listLength - 1]), true);
  }
  else {
    pair = EmptyList;
  }
  for (var i = count; i >= 0; i--) {
    pair = new Pair(evalLiteral(listItems[i]), pair, true);
  }
  return pair;
}
function evalVectorLiteral(vectorItems) {
  var count = vectorItems.length;
  var args = new Array(count);
  for (var i = 0; i < count; i++) {
    args[i] = evalLiteral(vectorItems[i]);
  }
  return Vector.create(args, null, true);
}
function evalLiteral(literal) {
  var value = literal.value.value;
  var type = literal.value.type;
  switch (type) {
    case TokenTypes.boolean:
      return value;
    case TokenTypes.number:
      if (value.indexOf('.') === -1) {
        return parseInt(value);
      }
      else {
        return parseFloat(value);
      }
    case TokenTypes.character:
      return new SchemeChar(value);
    case TokenTypes.string:
      return new SchemeString(value, true);
    case TokenTypes.identifier:
      return new Symbol(value);
    case 'list':
      return evalListLiteral(value);
    case 'vector':
      return evalVectorLiteral(value);
    default:
      throw new Error('Unkown type'); // TODO should raise a more user frienly error, is this code reachable?
  }
}
function createCallOP(opType, form) {
  // form.nodes[0] are the procedure arguments
  // the op argument is the number of procedure arguments
  var args = form.nodes[0];
  var argsCount;
  if (args.nodes) {
    argsCount = args.nodes.length;
  }
  else {
    argsCount = args.value;
  }
  // the position of the procedure call in the code
  var position = form.nodes[2].value;
  return [opType, argsCount, position];
}
function analyzeForm(form, idx, conds) {
  var op, argsCount, i, bodyCode;
  switch (form.type) {
    case FormTypes.definition:
      return [OPTypes.define];
    case FormTypes.internalDefinition:
      return [OPTypes.internaldefine, form.value];
    case FormTypes.assignment:
      return [OPTypes.set];
    case FormTypes.literal:
      return [OPTypes.constant, evalLiteral(form)];
    case FormTypes.variable:
      return [OPTypes.variable, form.value];
    case FormTypes.test:
      op = [OPTypes.jumpiffalse, -1];
      conds.push(op);
      return op;
    case FormTypes.ifthen:
      op = conds.pop(); // test op, i.e. jumpiffalse
      // jump to the next op
      // since there is an op for this form
      // the idx is incremented to point to the right op
      op[1] = idx + 1;
      op = [OPTypes.jump, -1];
      conds.push(op);
      return op;
    case FormTypes.ifelse:
      op = conds.pop(); // ifthen op, i.e. jump
      if (!form.nodes) { // the 'if' has no 'else' clause
        op[1] = idx + 1;
        return [OPTypes.void];
      }
      else {
        op[1] = idx; // jump to the next op
        return null;
      }
    case FormTypes.ifexpr:
      return null;
    case FormTypes.cond:
      argsCount = form.nodes.length;
      for (i = 0; i < argsCount; i++) {
        op = conds.pop(); // ifthen op, i.e. jump
        op[1] = idx + 1;
      }
      return [OPTypes.void];
    case FormTypes.condarrowthen:
      op = conds.pop(); // test op, i.e. jumpiffalse
      op[0] = OPTypes.jumpiffalsekeep;
      // jump to the next op
      // since there is an op for this form
      // the idx is incremented to point to the right op
      op[1] = idx + 1;
      op = [OPTypes.jump, -1];
      conds.push(op);
      return op;
    case FormTypes.condclause:
      if (form.nodes.length === 1) { // only a test clause, no then
        op = conds[conds.length - 1]; // test op, i.e. jumpiffalse
        op[0] = OPTypes.jumpifnotfalsekeep;
      }
      return null;
    case FormTypes.conjunction:
      argsCount = form.nodes.length;
      if (argsCount === 0) {
        return [OPTypes.constant, true];
      }
      else {
        while (argsCount > 0) {
          op = conds.pop();
          op[0] = OPTypes.jumpiffalsekeep;
          op[1] = idx;
          argsCount -= 1;
        }
        return null;
      }
    case FormTypes.disjunction:
      argsCount = form.nodes.length;
      if (argsCount === 0) {
        return [OPTypes.constant, false];
      }
      else {
        while (argsCount > 0) {
          op = conds.pop();
          op[0] = OPTypes.jumpifnotfalsekeep;
          op[1] = idx;
          argsCount -= 1;
        }
        return null;
      }
    case FormTypes.arguments:
      return null;
    case FormTypes.procedureCall:
      return createCallOP(OPTypes.call, form);
    case FormTypes.tailCall:
      return createCallOP(OPTypes.tailcall, form);
    case FormTypes.program:
    case FormTypes.lambdaBody:
      return [OPTypes.discard, form.nodes.length - 1];
    case FormTypes.lambda:
      bodyCode = analyze([form.nodes[1]]);
      return [OPTypes.lambda,
        form.nodes[0].value, // lambda formals
        bodyCode, // analyzed lambda body
        form.nodes[2].value // the name of the lambda
      ];
    case FormTypes.void: // TODO is it used?
      return [OPTypes.void];
    case FormTypes.begin:
      if (form.nodes.length > 0) {
        return [OPTypes.discard, form.nodes.length - 1];
      }
      else {
        return [OPTypes.void];
      }
    default:
      return null;
  }
}
function traverse(forms, ops, conds) {
  var form, op;
  for (var i = 0; i < forms.length; i++) {
    form = forms[i];
    if (form.nodes && form.type !== FormTypes.lambda) {
      traverse(form.nodes, ops, conds);
    }
    op = analyzeForm(form, ops.length, conds);
    if (op) {
      if (Array.isArray(op[0])) {
        ops.push.apply(ops, op);
      }
      else {
        ops.push(op);
      }
    }        
  }
}
function printOps(ops) {
  ops.forEach(function (op, idx) {
    var type = op[0];
    var value = '';
    for (var i = 1; i < op.length; i++) {
      value += op[i] + ' ';
    }
    console.log(idx + ' ' + type + ' ' + value);
  });
}
function analyze(forms) {
  var ops = [];
  var conds = [];
  traverse(forms, ops, conds);   
  // printOps(ops);
  return ops;
}

function compile(text, lang) {
  var program = parser.parse(text, lang);
  return analyze([program]);
}

module.exports = {
  OPTypes: OPTypes,
  compile: compile,
};
},{"./ast":2,"./lexer":17,"./parser":18,"./types":26}],5:[function(require,module,exports){
/*
A simple evaluator based on the grammer of r7rs small.
Evaluates programming code by transforming it
to an abstract syntax tree and then executs it.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/

'use strict';

var common = require('./common');
var compiler = require('./compiler');
var lexer = require('./lexer');
var langTable = require('./lang-table');
var types = require('./types');
var equivalenceProcedures = require('./procedures/equivalence');
var numberProcedures = require('./procedures/number');
var pairListProcedures = require('./procedures/pair-list');
var vectorProcedures = require('./procedures/vector');
var stringProcedures = require('./procedures/string');
var charProcedures = require('./procedures/char');
var ffi = require('./procedures/ffi');

var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgPredicate = common.guardArgPredicate;
var cloneEnvs = common.cloneEnvs;
var raiseRuntimeError = common.raiseRuntimeError;

var OPTypes = compiler.OPTypes;
var TokenTypes = lexer.TokenTypes;
var langName = common.langName;

var Environment = types.Environment;
var OutputPort = types.OutputPort;
var Procedure = types.Procedure;
var PrimitiveProcedure = types.PrimitiveProcedure;
var Application = types.Application;
var ContinuationProcedure = types.ContinuationProcedure;
var Continuation = types.Continuation;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Vector = types.Vector;
var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;
var SchemeError = types.SchemeError;

var ffiProcedures = ffi.procedures;
var convert = ffi.convert;

var outputPort;
function setOutputPortHandler(fn) {
  outputPort = new OutputPort(fn);
}

var procedureTypes = [Procedure, PrimitiveProcedure, Application, ContinuationProcedure, Continuation];
function isProcedure(arg) {
  return procedureTypes.some(function (type) {
    return arg instanceof type;
  });
}
function objectToString(obj, lang) {
  if (typeof obj === 'boolean') {
    return '#' + langTable.get(lang, 'tokens', obj.toString());
  }
  return obj.toString();
}
var primitiveFunctions = {  
  'boolean?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return typeof args[0] === 'boolean';
  },
  'not': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] === false;
  },
  'boolean=?': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    var i;
    for (i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], primitiveFunctions['boolean?'], i, 'procedures', 'boolean?');
    }
    for (i = 1; i < args.length; i++) {
      if (args[i - 1] !== args[i]) {
        return false;
      }
    }
    return true;
  },
  'procedure?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return isProcedure(args[0]);
  },
  'write-string': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    if (outputPort) {
      outputPort.emit(args[0].value);
    }
    return Unspecified;
  },
  'write-char': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], charProcedures['char?'], 0, 'procedures', 'char?');
    if (outputPort) {
      outputPort.emit(args[0].value);
    }
    return Unspecified;
  },
  'write': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var obj = args[0];    
    if (outputPort) {
      outputPort.emit(objectToString(obj, env.getVar(langName)));
    }
    return Unspecified;
  },
  'display': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var obj = args[0];
    if (obj instanceof SchemeChar) {
      return primitiveFunctions['write-char'](args, env);
    }
    if (obj instanceof SchemeString) {
      return primitiveFunctions['write-string'](args, env);
    }
    if (outputPort) {
      outputPort.emit(objectToString(obj, env.getVar(langName)));
    }
    return Unspecified;
  },
  'newline': function (args, env) {
    guardArgsCountExact(env, args.length, 0);
    if (outputPort) {
      outputPort.emit('\n');
    }
    return Unspecified;
  },
  'raise': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return new SchemeError(args[0].toString());
  },
};
function addProceduers(env, lang, procedures) {
  for (var name in procedures) {
    var translatedName = langTable.get(lang, 'procedures', name);
    env.addVar(translatedName,
      new PrimitiveProcedure(procedures[name], translatedName));
  }
}
function addPrimitivesAndLang(env, lang) {
  env.addVar(langName, lang);
  addProceduers(env, lang, primitiveFunctions);
  addProceduers(env, lang, equivalenceProcedures);
  addProceduers(env, lang, numberProcedures);
  addProceduers(env, lang, pairListProcedures);
  addProceduers(env, lang, vectorProcedures);
  addProceduers(env, lang, stringProcedures);
  addProceduers(env, lang, charProcedures);
  addProceduers(env, lang, ffiProcedures);
  addApplication(env, lang);
  addContinuationProcedures(env, lang);
}
function addApplication(env, lang) {
  var translatedName = langTable.get(lang, 'procedures', 'apply');
  env.addVar(translatedName, new Application(translatedName));
}
function callcc(args, env, envs) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], primitiveFunctions['procedure?'], 0, 'procedures', 'procedure?');
  // remove the env in which the callcc is called
  envs.pop();
  var clonedEnvs = cloneEnvs(envs);
  return new Continuation(clonedEnvs);
}
var continuationProcedures = {
  'call-with-current-continuation': callcc,
  'call/cc': callcc,
};
function addContinuationProcedures(env, lang) {
  for (var name in continuationProcedures) {
    var translatedName = langTable.get(lang, 'procedures', name);
    env.addVar(translatedName,
      new ContinuationProcedure(continuationProcedures[name], translatedName));
  }
}

function applyArguments(formals, actualArgs, procEnv) {
  var i;
  if (!Array.isArray(formals)) {
    procEnv.addVar(formals, Pair.createList(actualArgs));
  }
  else if (formals[formals.length - 2] === '.') {
    if (formals.length -1 > actualArgs.length) {
      raiseRuntimeError(procEnv, 'min_args_count_expected', [formals.length, actualArgs.length]);
    }
    for (i = 0; i < formals.length - 2; i++) {
      procEnv.addVar(formals[i], actualArgs[i]);
    }
    procEnv.addVar(formals[formals.length - 1], Pair.createList(actualArgs.slice(formals.length - 2)));
  }
  else {
    if (formals.length !== actualArgs.length) {
      raiseRuntimeError(procEnv, 'exact_args_count_expected', [formals.length, actualArgs.length]);
    }
    for (i = 0; i < formals.length; i++) {
      procEnv.addVar(formals[i], actualArgs[i]);
    }
  }
}
function applySchemeProcedure(procedure, actualArgs) {
  var formals = procedure.args;
  var env = new Environment(procedure.env);
  applyArguments(formals, actualArgs, env);
  return evalOPs(procedure.body, env);
}

function peek(arr) {
  return arr[arr.length - 1];
}
function setProcedureName(value, identifier) {
  if (value instanceof Procedure && !value.name) {
    value.name = identifier;
  }
}
function evalOPDefine(op, env) {
  var value = env.expressionStack.pop();
  var identifier = env.expressionStack.pop();
  env.addVar(identifier.value, value);
  setProcedureName(value, identifier);
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOPInternalDefine(op, env) {
  var identifiers = op[1];
  for (var i = 0; i < identifiers.length; i++) {
    env.addVar(identifiers[i], Unspecified);
  }
  return -1;
}
function evalOPSet(op, env) {
  var value = env.expressionStack.pop();
  var identifier = env.expressionStack.pop();
  env.setVar(identifier.value, value);
  setProcedureName(value, identifier);
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOPVariable(op, env) {
  var value = env.getVar(op[1]);
  env.expressionStack.push(value);
  return -1;
}
function evalOPConstant(op, env) {
  var value = op[1];
  env.expressionStack.push(value);
  return -1;
}
function evalOPJumpIfFalse(op, env) {
  var value = env.expressionStack.pop();
  if (value === false) {
    return op[1];
  }
  else {
    return -1;
  }
}
function evalOPJumpIfFalseKeep(op, env) {
  var value = peek(env.expressionStack);
  if (value === false) {
    return op[1];
  }
  else {
    return -1;
  }
}
function evalOPJumpIfNotFalseKeep(op, env) {
  var value = peek(env.expressionStack);
  if (value !== false) {
    return op[1];
  }
  else {
    return -1;
  }
}
function evalOPLambda(op, env) {
  var procedure = new Procedure(op[1], op[2], env, op[3]);
  env.expressionStack.push(procedure);
  return -1;
}
function evalOPDiscard(op, env) {
  var value = env.expressionStack.pop();
  var count = op[1];
  while (count > 0) {
    env.expressionStack.pop();
    count --;
  }
  env.expressionStack.push(value);
  return -1;
}
function evalOPVoid(op, env) {
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOP(op, env) {
  switch (op[0]) {
    case OPTypes.define:
      return evalOPDefine(op, env);
    case OPTypes.internaldefine:
      return evalOPInternalDefine(op, env);
    case OPTypes.set:
      return evalOPSet(op, env);
    case OPTypes.constant:
      return evalOPConstant(op, env);
    case OPTypes.variable:
      return evalOPVariable(op, env);
    case OPTypes.jump:
      return op[1];
    case OPTypes.jumpiffalse:
      return evalOPJumpIfFalse(op, env);
    case OPTypes.jumpiffalsekeep:
      return evalOPJumpIfFalseKeep(op, env);
    case OPTypes.jumpifnotfalsekeep:
      return evalOPJumpIfNotFalseKeep(op, env);
    case OPTypes.lambda:
      return evalOPLambda(op, env);
    case OPTypes.discard:
      return evalOPDiscard(op, env);
    case OPTypes.void:
      return evalOPVoid(op, env);
  }
}
function getCodeLocation(env) {
  if (env.ops) { // if there is compiled code
    var procedureCall = env.ops[env.opIndex];
    var position = procedureCall[2];
    if (position) {
      return ' (line: ' + position.line + ', column: ' + position.column + ')';
    }
    else {
      return '';
    }
  }
  else {
    return ' [native code]';
  }
}
function getStack(envs) {
  var env, procedure, procedureName, location;
  var stackInfo = [];
  for (var i = envs.length - 1; i >= 0; i--) {
    env = envs[i];
    procedure = env.procedure;
    location = getCodeLocation(env);
    // if (!env.ops) { // a primitive procedure
    //   env = envs[i - 1];
    //   if (env) {
    //     location = getCodeLocation(env);
    //   }
    // }
    if (i > 0) {
      if (procedure) {
        procedureName = procedure.name || 'anonymous';
      }
    }
    else { // global env
      procedureName = 'global';
    }
    stackInfo.push(procedureName + location);
  }
  return stackInfo.join('\n');
}
function evalOPs(ops, env) {
  function applyProcedure(procedure, actualArgs) {
    var formals = procedure.args;
    env = new Environment(procedure.env);
    if (op[0] === OPTypes.tailcall) {
      envs.pop();
    }
    applyArguments(formals, actualArgs, env);
    envs.push(env);
    env.procedure = procedure;
    ops = env.ops = procedure.body;
    idx = 0;
  }
  var idx, op, value;
  var i = 0;
  var envs = [env];
  var maxEnvCount = 1000;
  env.ops = ops; // TODO move it
  main:
  while (true) {
    op = ops[i];
    if (op[0] === OPTypes.call
      || op[0] === OPTypes.tailcall) {
      var application = false;
      while (true) {
        if (envs.length >= maxEnvCount) {
          raiseRuntimeError(env, 'maximum_stack_size_exceeded');
        }
        var procedure;
        var actualArgs;
        if (application) {
          actualArgs = applicationArgs;
          application = false;
        }
        else {
          procedure = env.expressionStack.pop();
          var argsCount = op[1];
          actualArgs = new Array(argsCount);
          for (var a = argsCount - 1; a >= 0; a--) {
            actualArgs[a] = env.expressionStack.pop();
          }
        }

        if (!(procedure instanceof Procedure)) {
          // if it is not Procedure it will not have an env, so it gets the global env
          env = new Environment(envs[0]);
          envs.push(env);
          env.procedure = procedure;
        }

        if (procedure instanceof PrimitiveProcedure) {
          try {
            value = procedure.execute(actualArgs, env);
          }
          catch (e) {
            var schemeError = new SchemeError(e.message);
            schemeError.stack = getStack(envs);
            return schemeError;
          }
          if (value instanceof SchemeError) {
            value.stack = getStack(envs);
            return value;
          }
          envs.pop();
          env = peek(envs);
          env.expressionStack.push(value);
          idx = -1;
        }
        else if (procedure instanceof Procedure) {
          try {
            applyProcedure(procedure, actualArgs);
          }
          catch (e) {
            var schemeError = new SchemeError(e.message);
            schemeError.stack = getStack(envs);
            return schemeError;
          }
        }
        else if (procedure instanceof Application) {
          procedure = actualArgs[0];
          try {
            guardArgPredicate(env, procedure, primitiveFunctions['procedure?'], 0, 'procedures', 'procedure?');
          }
          catch (e) {
            var schemeError = new SchemeError(e.message);
            schemeError.stack = getStack(envs);
            return schemeError;
          }
          var applicationArgs = [];
          for (var a = 1, l = actualArgs.length - 1; a < l; a++) {
            applicationArgs.push(actualArgs[a]);
          }
          var lastArg = peek(actualArgs);
          if (pairListProcedures['list?']([lastArg], env)) {
            var argsVector = vectorProcedures['list->vector']([lastArg], env);
            Array.prototype.push.apply(applicationArgs, argsVector.value);
          }
          else {
            applicationArgs.push(lastArg);
          }
          application = true;
          envs.pop();
          env = peek(envs);
          continue;
        }
        else if (procedure instanceof ContinuationProcedure) {
          try {
            value = procedure.execute(actualArgs, env, envs);
          }
          catch (e) {
            var schemeError = new SchemeError(e.message);
            schemeError.stack = getStack(envs);
            return schemeError;
          }
          env = peek(envs);
          // TODO the passed lambda should accept only one argument
          procedure = actualArgs[0];
          applyProcedure(procedure, [value]);
        }
        else if (procedure instanceof Continuation) {
          try {
            value = procedure.execute(actualArgs, env);
          }
          catch (e) {
            var schemeError = new SchemeError(e.message);
            schemeError.stack = getStack(envs);
            return schemeError;
          }
          // just clear the old env
          envs.pop();
          env = peek(envs);

          envs = value.envs;
          env = peek(envs);
          env.expressionStack.push(value.value);
          ops = env.ops;
          idx = env.opIndex + 1;
        }
        break;
      }
    }
    else {
      try {
        idx = evalOP(op, env);
      }
      catch (e) {
        var schemeError = new SchemeError(e.message);
        schemeError.stack = getStack(envs);
        return schemeError;
      }
    }
    if (idx !== -1) {
      i = idx;
    }
    else {
      while (true) {
        i += 1;
        if (i === ops.length) { // the procedure ends
          if (envs.length === 1) { // all code is evaluated, including the global - the first environment
            break main;
          }
          value = env.expressionStack.pop(); // return value of procedure
          envs.pop();
          env = peek(envs);
          ops = env.ops;
          i = env.opIndex;
          env.expressionStack.push(value);
        }
        else {
          break;
        }
      }
    }
    env.opIndex = i;
  }
  return env.expressionStack.pop();
}

function Result(object, env) {
  this.object = object;
  this.env = env;
}
Result.prototype.valueOf = function valueOf() {
  return this.object;
};
Result.prototype.toString = function toString() {
  return objectToString(this.object, this.env);
};
Result.prototype.toJS = function toJS() {
  return convert(this.object, this.env);
};
function evaluate(text, lang) {
  lang = lang || 'en';
  try {
    var program = compiler.compile(text, lang);
  }
  catch (e) {
    return new SchemeError(e.message);
  }
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  var result = evalOPs(program, env);
  return new Result(result, env);
}
function session(lang) {
  lang = lang || 'en';
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  return {
    environment: env,
    evaluate: function evalFragment(text) {
      var fragment = compiler.compile(text, lang);
      var result = evalOPs(fragment, env);
      return new Result(result, env);
    },
  };
}

exports.applySchemeProcedure = applySchemeProcedure;
exports.evaluate = evaluate;
exports.session = session;
exports.setOutputPortHandler = setOutputPortHandler;
exports.string = toString;
},{"./common":3,"./compiler":4,"./lang-table":16,"./lexer":17,"./procedures/char":19,"./procedures/equivalence":20,"./procedures/ffi":21,"./procedures/number":22,"./procedures/pair-list":23,"./procedures/string":24,"./procedures/vector":25,"./types":26}],6:[function(require,module,exports){
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
  var tokenStream = parsingInfo.tokenStream;
  var token = tokenStream.peek();
  var position = { line: token.line, column: token.column };
  var procedure = readExpression(parsingInfo);
  var args = [];
  var expression = readExpression(parsingInfo);
  while(expression) {
    args.push(expression);
    expression = readExpression(parsingInfo);
  }
  token = tokenStream.advance();
  if (!token) {
    raiseSyntaxError(parsingInfo, 'proc_call_end_unexpected');
  }
  if (token.type !== TokenTypes.rightParen) {
    raiseSyntaxError(parsingInfo, 'invalid_proc_call');
  }
  return ast.createProcedureCall(procedure, args, position);
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
},{"../ast":2,"../common":3,"../lexer":17,"./quote":12}],7:[function(require,module,exports){
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
},{"../ast":2,"../common":3,"../lexer":17,"./basic":6}],8:[function(require,module,exports){
'use strict';

var langTable = require('../lang-table');
var common = require('../common');
var lexer = require('../lexer');
var basic = require('./basic');
var ast = require('../ast');
var lambda = require('./lambda'); // TODO circular require

var TokenTypes = lexer.TokenTypes;
var FormTypes = ast.FormTypes;
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
function setLambdaName(node) {
  // if the value is lambda
  if (node.nodes[1].type === FormTypes.lambda) {
    // get the identifier
    var identifier = node.nodes[0].value.value;
    // set the name of the lambda
    node.nodes[1].nodes[2].value = identifier;
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
        var node = readValueAssignment(parsingInfo, createDefinition, formName);
        //setLambdaName(node);
        return node;
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
        return createDefinition(header.identifier, createLambda(parsingInfo, header.formals, body));
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
},{"../ast":2,"../common":3,"../lang-table":16,"../lexer":17,"./basic":6,"./lambda":10}],9:[function(require,module,exports){
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
},{"../ast":2,"../common":3,"../lexer":17,"./basic":6}],10:[function(require,module,exports){
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
},{"../ast":2,"../common":3,"../lexer":17,"./basic":6,"./define-set":8}],11:[function(require,module,exports){
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
  var procedure = createLambda(parsingInfo, formals, body);
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
    var enclosingProcedure = createLambda(parsingInfo, [], [lambdaDefinition, lambdaApplication]);
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
  var procedure = createLambda(parsingInfo, [], body);
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
    procedure = createLambda(parsingInfo, [], body);
    return createProcedureCall(procedure, []);
  }
  else {
    for (var i = bindingsCount - 1; i >= 0; i--) {
      formals = [bindings[i].variable];
      inits = [bindings[i].init];
      procedure = createLambda(parsingInfo, formals, body);
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
},{"../ast":2,"../common":3,"../lexer":17,"./basic":6,"./lambda":10}],12:[function(require,module,exports){
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
},{"../ast":2,"../common":3,"../lexer":17}],13:[function(require,module,exports){
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
},{"../ast":2,"../common":3,"../lexer":17,"./basic":6,"./define-set":8}],14:[function(require,module,exports){
'use strict';

var langTable = require('./lang-table');

var data = {
  'common': {
    'missing_entry': 'Липсва превод за език "{0}", кетегория "{1}", ключ "{2}".',
  },
  'syntax': {
    'define': 'определи',
    'set!': 'смени!',
    'lambda': 'функция',
    'if': 'ако',
    'quote': 'цитирай',
    'begin': 'почни',
    'cond': 'условие',
    'and': 'и',
    'or': 'или',
    'case': 'случай',
    'let': 'нека',
    'let*': 'нека*',
    'letrec': 'некарек',
    'letrec*': 'некарек*',
    'do': 'прави',
    'delay': 'отложи',
    'quasiquote': 'почтицитирай',
    'else': 'иначе',
    '=>': '=>',
    'unquote': 'нецитирай',
    'unquote-splicing': 'нецитирай-съединяване',
  },

  'syntax-common': {
    'definition': 'определение',
    'assignment': 'присвояване'
  },

  'syntax-errors': {
    'if_expression_end_expected': 'Очаква се края на израза "ако".',
    'if_expr_end_unexpected': 'Неочакван край на израза "ако".',
    'proc_call_end_unexpected': 'Неочакван край на извикване на функция. Очаква се затваряща скоба.',
    'invalid_proc_call': 'Невалидно извикване на функция. Очаква се затваряща скоба.',
    'lambda_expr_end_unexpected': 'Неочакван край на израза "функция"',
    'lambda_formals_end_unexpected': 'Неочакван край на формалните параметри на функцията.',
    'lambda_expr_variable_expected': 'Очаква се променлива след точката в списъка с формални параметри.',
    'expected_lambda_formals_end': 'Очаква се затваряща скоба след края на формалните параметри.',
    'invalid_lambda_formals': 'Очаква се списък с формални параметри или единствена променлива.',
    'no_lambda_body_exprs': 'Тялото на функцията трябва да има поне един израз.',
    'lambda_body_end_unexpected': 'Неочакван край на тялото на функцията.',
    'invalid_lambda_body_end': 'Тялото на функцията трябва да завършва със затваряща скоба.',
    'variable_expected': 'Невалидно "{0}". Очаква се променлива.',
    'expr_expected': 'Невалидно "{0}". Очаква се израз.',
    'expr_end_unexpected': 'Неочакван край на {0}. Очаква се затваряща скоба.',
    'right_paren_expected': 'Невалидно {0}. Очаква се затваряща скоба.',
    'list_end_unexpected': 'Неочакван край на списъка.',
    'unknown_expr': 'Непознат израз.',
    'expected_let_variable': 'Очаква се променлива за израза "нека".',
    'expected_let_init': 'Очаква се израз при инициализирането на променливата.',
    'expected_let_binding': 'Очаква се инициализация за израза "нека". Инициализацията трябва да е във вида (променлива израз).',
    'expected_let_binding_end': 'Очаква се края на инициализацията за израза "нека".',
    'let_binding_end_unexpected': 'Неочакван край на инициализацията за израза "нека".',
    'let_end_unexpected': 'Неочакван край на израза "нека".',
    'let_bindings_exptected': 'Очаква се списък от инициализации на израза "нека".',
    'let_body_end_unexpected': 'Неочакван край на тялото на израза "нека".',
    'no_let_body_exprs': 'Изразът "нека" трябва да има поне един израз в тялото си.',
    'invalid_let_body_end': 'Тялото на израза "нека" трябва да завършва със затваряща скоба.',
    'no_definition_exprs': 'Определението трябва да съдържа поне един подизраз.',
    'definition_formals_end_unexpected': 'Неочакван край на формалните параметри на определението.',
    'definition_expr_variable_expected': 'Очаква се променлива след точката в списъка с формални параметри.',
    'definition_body_end_unexpected': 'Неочакван край на тялото на определението.',
    'invalid_definition_body_end': 'Тялото на определението трябва да завършва със затваряща скоба.',
    'definition_end_unexpected': 'Неочакван край на определението.',
    'syntax_keywords_as_variables': 'Синтактичните ключови думи не могат да се използват като променливи..',
    'datum_end_unexpected': 'Неочакван край на данните.',
    'datum_expected': 'Очакват се данни.',
    'expected_list_end': 'Очаква се списъка да завършва със затваряща скоба.',
    'vector_end_unexpected': 'Неочакван край на вектор.',
    'expected_vector_end': 'Очаква се вектора да завършва със затваряща скоба.',
    'invalid_datum': 'Невалидно представяне на данните.',
    'expr_expected_definition_found': 'Очаква се израз но е открито определение. ' +
      'Опрделенията трябва да се поставят в началото на тялото на фукция.',
    'definition_expected_expr_found': 'Очаква се определение но е открит израз. ' +
      'Опрделенията трябва да се поставят в началото на тялото на фукция.',
    'duplicate_definitions': 'Открити са повече от едно определения за променлива "{0}".',
    'data_not_procedure': 'Прости данни не могат да се използват като фукция.',
    'cond_else_last': '"иначе" може да се изполва единствено в последната част на израза "условие".',
    'unnecessary_right_paren': 'Ненужно използване на затваряща скоба.',
    'bad_syntax': 'Грешен синтаксис.',
  },

  'tokens': {
    'true': 'в',
    'false': 'л',
    'newline': 'новред',
    'space': 'интервал',
    'tab': 'табулация',
    'valid_escaped_chars': 'нрт',
  },

  'token-errors': {
    'invalid_token': 'Невалидно съдържание.',
    'invalid_char': 'Невалиден знак.',
    'invalid_identifier': 'Невалиден символ.',
    'invalid_string': 'Невалиден низ от знаци.',
    'invalid_escaped_char': 'Невалиден знак.',
    'invalid_number': 'Невалидно число.',
  },

  'procedures': {
    '+': '+',
    '-': '-',
    '*': '*',
    '/': '/',
    '=': '=',
    '>': '>',
    '<': '<',
    'number?': 'число?',
    'nonnegative-integer?': 'неотрицателно-цяло-число?',
    'exp': 'експ',
    'log': 'лог',
    'sin': 'син',
    'cos': 'кос',
    'tan': 'тан',
    'asin': 'асин',
    'acos': 'акос',
    'atan': 'атан',
    'sqrt': 'квкорен',
    'expt': 'степен',

    'boolean?': 'булева?',
    'not': 'не',
    'boolean=?': 'булеви=?',
    'list': 'списък',
    'make-list': 'създай-списък',
    'cons': 'конс',
    'pair?': 'двойка?',
    'car': 'първо',
    'cdr': 'второ',
    'set-car!': 'смени-първо!',
    'set-cdr!': 'смени-второ!',
    'caar': 'пърпърво',
    'cadr': 'първторо',
    'cdar': 'втопърво',
    'cddr': 'втовторо',
    'null?': 'нищо?',
    'list?': 'списък?',
    'length': 'дължина',
    'list-ref': 'списък-вземи',
    'list-set!': 'списък-смени!',
    'append': 'добави',
    'reverse': 'обърни',
    'list-tail': 'списък-остатък',
    'memq': 'част-ра',
    'memv': 'част-рав',
    'member': 'част',
    'assq': 'елемент-ра',
    'assv': 'елемент-рав',
    'assoc': 'елемент',
    'list-copy': 'списък-повтори',

    'vector': 'вектор',
    'make-vector': 'създай-вектор',
    'vector?': 'вектор?',
    'vector-length': 'вектор-дължина',
    'vector-ref': 'вектор-вземи',
    'vector-set!': 'вектор-смени!',
    'vector->list': 'вектор->списък',
    'list->vector': 'списък->вектор',
    'vector->string': 'вектор->низ',
    'string->vector': 'низ->вектор',
    'vector-copy': 'вектор-повтори',
    'vector-copy!': 'вектор-повтори!',
    'vector-append': 'вектор-добави',
    'vector-fill!': 'вектор-попълни!',

    'procedure?': 'функция?',
    'call-with-current-continuation': 'извикай-със-сегашното-продължение',
    'call/cc': 'извикай/сп',
    'write': 'напиши',
    'write-char': 'напиши-знак',
    'write-string': 'напиши-низ',
    'display': 'покажи',
    'newline': 'новред',
    'eq?': 'ра?',
    'eqv?': 'рав?',
    'equal?': 'равно?',
    'string?': 'низ?',
    'char?': 'знак?',
    'make-string': 'създай-низ',
    'string': 'низ',
    'string-length': 'низ-дължина',
    'string-ref': 'низ-вземи',
    'string-set!': 'низ-смени!',
    'string=?': 'низ=?',
    'js-eval': 'джс-изчисли',
    'raise': 'предизвикай',
    'apply': 'приложи',
  },

  'runtime-errors': {
    'number_expected': 'Очаква се число но е подадено {0}.',
    'exact_args_count_expected': 'Очакват се {0} на брой аргументи, но са подадени {1}.',
    'min_args_count_expected': 'Очакват се поне {0} на брой аргументи, но са подадени {1}.',
    'max_args_count_expected': 'Очакват се не повече от {0} на брой аргументи, но са подадени {1}.',
    'invalid_proc': 'Невалидна функция.',
    'undefined_variable': 'Неопределена променлива с име "{0}".',
    'argument_predicate_false': 'Очаква се аргументът на позиция {0} да изпълнява условието {1}.',
    'mutating_immutable_object': 'Неизменими данни не могат да се променят..',
    'vector_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на вектора, която е {0}.',
    'list_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на списъка, която е {0}.',
    'list_index_reached_non_pair': 'Индексът е извън позволения обхват. На позиция {0} е намерена стойност, която не е двойка.',
    'string_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на низа, която е {0}.',
    'maximum_stack_size_exceeded': 'Твърде много рекурсивни извиквания без да се използват крайни извиквания.',
    'start_index_greater_end_index': 'Началният индекс е по-голям от крайния.',
    'vector_argument_predicate_false': 'Очаква се елементите на вектора от позиция {0} до позиция {1} да изпълняват условието {2}.',
    'vector_copy_space_needed': 'Няма достатъчно място, за да се запишат елементите от втория вектор в първия. ' +
      'Броят на елементите, които трябва да се запишат, е {0}, а свободното място е за {1} елемента.',
    'improper_list': 'Подаденият списък е неправилен, той не завършва с празен списък.',
    'improper_alist': 'Подаденият асоциативен списък е неправилен, той съдържа елементи, които не са двойки.',
  },
};

langTable.register('bg', function get(category, key) {
  var cat = data[category];
  if (cat) {
    var value = cat[key];
    if (value) {
      return value;
    }
  }
  return null;
});

module.exports = data;
},{"./lang-table":16}],15:[function(require,module,exports){
'use strict';

var langTable = require('./lang-table');

var data = {
  'common': {
    'missing_entry': 'Missing translation for language "{0}", category "{1}", key "{2}".',
  },
  'syntax': {
    'define': 'define',
    'set!': 'set!',
    'lambda': 'lambda',
    'if': 'if',
    'quote': 'quote',
    'begin': 'begin',
    'cond': 'cond',
    'and': 'and',
    'or': 'or',
    'case': 'case',
    'let': 'let',
    'let*': 'let*',
    'letrec': 'letrec',
    'letrec*': 'letrec*',
    'do': 'do',
    'delay': 'delay',
    'quasiquote': 'quasiquote',
    'else': 'else',
    '=>': '=>',
    'unquote': 'unquote',
    'unquote-splicing': 'unquote-splicing',
  },

  'syntax-common': {
    'definition': 'definition',
    'assignment': 'assignment'
  },

  'syntax-errors': {
    'if_expression_end_expected': 'Expected end of "if" expression.',
    'if_expr_end_unexpected': 'Unexpected end of "if" expression.',
    'proc_call_end_unexpected': 'Unexpected end of a procedure call. Expected a closing parentheses.',
    'invalid_proc_call': 'Invalid procedure call. Expected a right parentheses.',
    'lambda_expr_end_unexpected': 'Unexpected end of a lambda expression.',
    'lambda_formals_end_unexpected': 'Unexpected end of a lambda formals.',
    'lambda_expr_variable_expected': 'Expected a variable after a dot in lambda formals.',
    'expected_lambda_formals_end': 'Expected a right parentheses to end the lambda formals.',
    'invalid_lambda_formals': 'Expected a list of formals or a single variable.',
    'no_lambda_body_exprs': 'The lambda body needs at least one expression.',
    'lambda_body_end_unexpected': 'Unexpected end of a lambda body.',
    'invalid_lambda_body_end': 'The lambda body should be closed with a right parentheses.',
    'variable_expected': 'Invalid "{0}". Expected a variable.',
    'expr_expected': 'Invalid "{0}". Expected an expression.',
    'expr_end_unexpected': 'Unexpected end of a {0}. Expected a right parentheses.',
    'right_paren_expected': 'Invalid {0}. Expected a right parentheses.',
    'list_end_unexpected': 'Unexpected end of a list.',
    'unknown_expr': 'Unknown expression.',
    'expected_let_variable': 'Expected a variable for a let binding.',
    'expected_let_init': 'Expected an expression for a let binding initialization.',
    'expected_let_binding': 'Expected a let binding. A binding is (variable init-expression).',
    'expected_let_binding_end': 'Expected the end of a let binding.',
    'let_binding_end_unexpected': 'Unexpected end of a let binding.',
    'let_end_unexpected': 'Unexpected end of let expression.',
    'let_bindings_exptected': 'Expected a list of let bindings.',
    'let_body_end_unexpected': 'Unexpected end of a let body.',
    'no_let_body_exprs': 'The let body needs at least one expression.',
    'invalid_let_body_end': 'The let body should be closed with a right parentheses.',
    'no_definition_exprs': 'The definition needs at least one subexpression.',
    'definition_formals_end_unexpected': 'Unexpected end of a defintion formals.',
    'definition_expr_variable_expected': 'Expected a variable after a dot in definition formals.',
    'definition_body_end_unexpected': 'Unexpected end of a definition body.',
    'invalid_definition_body_end': 'The definition body should be closed with a right parentheses.',
    'definition_end_unexpected': 'Unexpected end of a definition.',
    'syntax_keywords_as_variables': 'Syntax keywords cannot be used as variables.',
    'datum_end_unexpected': 'Unexpected end of datum.',
    'datum_expected': 'Expected a datum.',
    'expected_list_end': 'Expected a right parentheses to end the list.',
    'vector_end_unexpected': 'Unexpected end of a vector.',
    'expected_vector_end': 'Expected a right parentheses to end the vector.',
    'invalid_datum': 'Invalid datum representation.',
    'expr_expected_definition_found': 'Expected an expression but found a definition. ' +
      'Definitions should be placed at the beginning of the lambda body.',
    'definition_expected_expr_found': 'Expected a definition but found an expression. ' +
      'Definitions should be placed at the beginning of the lambda body.',
    'duplicate_definitions': 'Found multiple definitions for variable "{0}".',
    'data_not_procedure': 'Simple data cannot be used as a procedure.',
    'cond_else_last': '"else" can be used only at the last part of a "cond" expression.',
    'cond_clause_end_unexpected': 'Unexpected end of a "cond" clause.',
    'cond_clause_test_expected': 'Expected a test expression for a "cond" clause.',
    'unnecessary_right_paren': 'Unnecessary usage of right parentheses.',
    'bad_syntax': 'Bad syntax.',
  },

  'tokens': {
    'true': 't',
    'false': 'f',
    'newline': 'newline',
    'space': 'space',
    'tab': 'tab',
    'valid_escaped_chars': 'nrt',
  },

  'token-errors': {
    'invalid_token': 'Invalid content.',
    'invalid_char': 'Invalid character.',
    'invalid_identifier': 'Invalid identifier.',
    'invalid_string': 'Invalid string.',
    'invalid_escaped_char': 'Invalid escaped character.',
    'invalid_number': 'Invalid number.',      
  },

  'procedures': {
    '+': '+',
    '-': '-',
    '*': '*',
    '/': '/',
    '=': '=',
    '>': '>',
    '<': '<',
    'number?': 'number?',
    'nonnegative-integer?': 'nonnegative-integer?',
    'exp': 'exp',
    'log': 'log',
    'sin': 'sin',
    'cos': 'cos',
    'tan': 'tan',
    'asin': 'asin',
    'acos': 'acos',
    'atan': 'atan',
    'sqrt': 'sqrt',
    'expt': 'expt',

    'boolean?': 'boolean?',
    'not': 'not',
    'boolean=?': 'boolean=?',
    'list': 'list',
    'make-list': 'make-list',
    'cons': 'cons',
    'pair?': 'pair?',
    'car': 'car',
    'cdr': 'cdr',
    'set-car!': 'set-car!',
    'set-cdr!': 'set-cdr!',
    'caar': 'caar',
    'cadr': 'cadr',
    'cdar': 'cdar',
    'cddr': 'cddr',
    'null?': 'null?',
    'list?': 'list?',
    'length': 'length',
    'list-ref': 'list-ref',
    'list-set!': 'list-set!',
    'append': 'append',
    'reverse': 'reverse',
    'list-tail': 'list-tail',
    'memq': 'memq',
    'memv': 'memv',
    'member': 'member',
    'assq': 'assq',
    'assv': 'assv',
    'assoc': 'assoc',
    'list-copy': 'list-copy',

    'vector': 'vector',
    'make-vector': 'make-vector',
    'vector?': 'vector?',
    'vector-length': 'vector-length',
    'vector-ref': 'vector-ref',
    'vector-set!': 'vector-set!',
    'vector->list': 'vector->list',
    'list->vector': 'list->vector',
    'vector->string': 'vector->string',
    'string->vector': 'string->vector',
    'vector-copy': 'vector-copy',
    'vector-copy!': 'vector-copy!',
    'vector-append': 'vector-append',
    'vector-fill!': 'vector-fill!',

    'procedure?': 'procedure?',
    'call-with-current-continuation': 'call-with-current-continuation',
    'call/cc': 'call/cc',
    'write': 'write',
    'write-char': 'write-char',
    'write-string': 'write-string',
    'display': 'display',
    'newline': 'newline',
    'eq?': 'eq?',
    'eqv?': 'eqv?',
    'equal?': 'equal?',
    'string?': 'string?',
    'char?': 'char?',
    'make-string': 'make-string',
    'string': 'string',
    'string-length': 'string-length',
    'string-ref': 'string-ref',
    'string-set!': 'string-set!',
    'string=?': 'string=?',
    'js-eval': 'js-eval',
    'raise': 'raise',
    'apply': 'apply',
  },

  'runtime-errors': {
    'number_expected': 'Expected a number but received {0}.',
    'exact_args_count_expected': 'Expected {0} arguments, but got {1}.',
    'min_args_count_expected': 'Expected at least {0} arguments, but got {1}.',
    'max_args_count_expected': 'Expected at most {0} arguments, but got {1}.',
    'invalid_proc': 'Invalid procedure.',
    'undefined_variable': 'Undefined variable with name "{0}".',
    'argument_predicate_false': 'Expected argument on position {0} to satisfy predicate {1}.',
    'mutating_immutable_object': 'Cannot mutate an immutable object.',
    'vector_index_out_range': 'Index is out of range. The length of the vector is {0}.',
    'list_index_out_range': 'Index is out of range. The length of the list is {0}.',
    'list_index_reached_non_pair': 'Index is out of range. A non-pair is reached at position {0}.',
    'string_index_out_range': 'Index is out of range. The length of the string is {0}.',
    'maximum_stack_size_exceeded': 'Too much recursive calls with non-tail calls.',
    'start_index_greater_end_index': 'The start index is greater than the end index.',
    'vector_argument_predicate_false': 'Expected the elements of the vector from index {0} to {1} to satisfy predicate {2}.',
    'vector_copy_space_needed': 'Not enough space to copy elements from the second vector to the first. ' +
      'The amount of elements to copy is {0} and the available space is for {1} elements.',
    'improper_list': 'The given list is improper, it does not end with the empty list.',
    'improper_alist': 'The given association list is improper, it contains non-pair elements.',
  },
};

langTable.register('en', function get(category, key) {
  var cat = data[category];
  if (cat) {
    var value = cat[key];
    if (value) {
      return value;
    }
  }
  return null;
});

module.exports = data;
},{"./lang-table":16}],16:[function(require,module,exports){
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define([], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory();
  } else {
    root.langTable = factory();
  }
}(this, function () {
  var data = {};
  function raiseError(lang, category, key, suppress) {
    var missingEntryMsg = get(lang, 'common', 'missing_entry', true) ||
      'Missing entry for language "{0}", category "{1}", key "{2}".';
    throw new Error(String.format(missingEntryMsg, lang, category, key));
  }
  function get(lang, category, key, suppress) {
    var getter = langList[lang];
    if (!getter && !suppress) {
      raiseError(lang, category, key, suppress);
    }
    var value;
    if (getter) {
      value = getter(category, key);
    }
    if (value) {
      return value;
    }
    else if(!suppress) {
      raiseError(lang, category, key, suppress);
    }
    else {
      return null;
    }

    var langSet = data[lang];
    if (langSet) {
      var cat = langSet[category];
      if (cat) {
        var val = cat[key];
        if (val) {
          return val;
        }
      }
    }
    if (!suppress) {
      var missingEntryMsg = get(lang, 'common', 'missing_entry', true) ||
        'Missing entry for language "{0}", category "{1}", key "{2}".';
      throw new Error(String.format(missingEntryMsg, lang, category, key));
    }
    return null;
  }
  function set(lang, category, key, value) {
    var langSet = data[lang] || (data[lang] = {});
    var cat = langSet[category] || (langSet[category] = {});
    cat[key] = value;
  }

  var langList = {};
  function register(lang, getter) {
    langList[lang] = getter;
  }
  return {
    get: get,
    set: set,
    register: register,
  };
}));
},{}],17:[function(require,module,exports){
/*
A simple lexer based on the grammer of r7rs small.
Transforms programming code to a stream of tokens.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lang-table'));
  } else {
    root.lexer = factory(root.langTable);
  }
}(this, function (langTable) {
  'use strict';
  
  // The values used to tag tokens.
  // It seems there is no panalty for comparing
  // strings compared to numbers.
  // String values are easier to debug.
  var TokenTypes = {
    identifier: 'identifier',
    boolean: 'boolean',
    number: 'number',
    character: 'character',
    string: 'string',
    leftParen: 'leftParen',
    rightParen: 'rightParen',
    vectorParen: 'vectorParen',
    quote: 'quote',
    backquote: 'backquote',
    unquote: 'unquote',
    unquoteSplicing: 'unquoteSplicing',
    dot: 'dot'
  };
  // A constructor for iterators of tokens.
  // It reads a programming code character by character
  // and produces a token when a valid sequence is
  // recognised. Otherwise raises an error.
  function tokenize(text, lang) {
    // The index of the current character in the text.
    var idx = 0;
    // Sometimes the iterator needs to look at the next
    // character in order to recognise a token.
    // The previously read character is kept here
    // by explicitly assinging a value to the variable.
    var oldchar = null;
    // The line of the current character.
    var charLine = 1;
    // The column of the current character.
    var charColumn = 0;
    // The line of the current token.
    var line = 0;
    // The column of the current token.
    var column = 0;

    // The natural language used in the programming code.
    // The default value is English.
    lang = lang || 'en';
    // A utility function for getting translations of
    // tokens in the current natural language.
    var get = function get(key) {
      return langTable.get(lang, 'tokens', key);
    }
    // Constants containing the translations of several tokens.
    var charTrue = get('true');
    var charFalse = get('false');
    var charSpace = get('space');
    var charNewLine = get('newline')
    var charTab = get('tab');
    var whitespaceChars  = [charSpace[0], charNewLine[0], charTab[0]];

    // A constructor for token objects.
    // When the token iterator reads a valid token
    // it packs the token information in a token object.
    function createToken(type, value) {
      return {
        // The line on which the token starts.
        line: line,
        // TODO
        // Well if the token is composed by several characters
        // then the column will not be correct because it is
        // incremented on reading every character.
        column: column,
        // A tag describing the token.
        // Must be one of the values in TokenTypes.
        type: type,
        // The value of the token object.
        // Optional. A string or a boolean.
        value: value
      };
    }
    // When the current character is of type undefined
    // then this counter is incremented.
    // It is used for capturing unknown errors.
    var undefchar = 0;
    // Retrieves the next character in the text.
    function getNextChar() {
      var char;
      // If there is a previously read character then it is returned.
      if (oldchar !== null) {
        char = oldchar;
        oldchar = null;
        return char;
      }
      char = text[idx];
      idx += 1;
      if (char) {
        if (char === '\n') {
          charLine += 1;
          charColumn = 0;
        }
        else {
          charColumn += 1;
        }
      }
      // Checking for unknown errors.
      if (!char) {
        undefchar += 1;
      }
      if (undefchar > 1) {
        throw new Error('Parse error');
      }
      return char;
    }
    // Raises an error by throwing it.
    // This function is used only in the lexer.
    function raiseError(messageKey) {
      var message = langTable.get(lang, 'token-errors', messageKey);
      var err = new Error(message);
      err.line = line;
      err.column = column;
      err.toString = function() {
        return Error.prototype.toString.call(this) +
          ' (line: ' + this.line + ', column: ' + this.column + ')';
      };
      throw err;
    }
    // Reads all characters until a delimiter character is read.
    // Returns a string.
    function getCharsTillDelimeter() {
      var char = getNextChar();
      var charBuffer = [];
      while (char && !isDelimeter(char)) {
        charBuffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return charBuffer.join('');
    }
    // Reads the text charcter by character until a valid token is read.
    // Constructs a token object and returns it.
    function getNextToken() {
      // Holds the current character.
      var char = getNextChar();
      // If the token is composed by several characters
      // then these characters are kept in this variable as a string.
      var chars,
      // When the next character is needed to be read it is 
      // sometimes kept in this variable.
        nextChar;
      // Clears the previously read undefined characters.
      undefchar = 0;

      // Skips characters while they are atmosphere (whitespace or comment).
      while (true) {
        if (isWhitespace(char)) {
          char = getNextChar();
          continue;
        }
        if (char === ';') {
          while (char && char !== '\n') {
            char = getNextChar();
          }
          continue;
        }
        break;
      }
      // If no character is read after the atmosphere then there is no token.
      if (!char) {
        return null;
      }
      // 'char' holds the first character of the token,
      // so the character line and column
      // and token line and column are the same.
      line = charLine;
      column = charColumn;
      // Reads single character tokens.
      var token = getTokenFromChar(char);
      // If such a single character token is read then it is returned.
      if (token) {
        return token;
      }
      // Tokens starting with a '#'' are vectors, booleans and characters.
      if (char === '#') {
        char = getNextChar();
        if (!char) {
          raiseError('invalid_token');
        }
        // '#(' is the beginning of a vector literal
        if (char === '(') {
          return createToken(TokenTypes.vectorParen);
        }
        // '#t' and '#f' are the boolean literals
        if (char === charTrue || char === charFalse) {
          nextChar = getNextChar();
          // TODO #true and #false are also boolean literals
          if (nextChar && !isDelimeter(nextChar)) {
            raiseError('invalid_token');
          }
          oldchar = nextChar;
          return createToken(TokenTypes.boolean, char === charTrue);
        }
        // '#\character' are character literals
        if (char === '\\') {
          char = getNextChar();
          if (!char) {
            raiseError('invalid_char');
          }
          // '#\character name', etc. are special character literals
          // TODO not all character names are implemented
          if (whitespaceChars.indexOf(char) !== -1) {
            chars = char + getCharsTillDelimeter();
            if (chars.length > 1) {
              if (chars === charSpace || chars === charNewLine || chars === charTab) {
                return createToken(TokenTypes.character, chars);
              }
              else {
                raiseError('invalid_char');
              }
            }
          }
          nextChar = getNextChar();
          if (nextChar && !isDelimeter(nextChar)) {
            raiseError('invalid_char');
          }
          // TODO '#\xhex' is not implemented
          oldchar = nextChar;
          return createToken(TokenTypes.character, char);
        }
        raiseError('invalid_token');
        // TODO get number
      }
      // Tokens starting with '"' are string literals.
      if (char === '"') {
        return readString();
      }
      // Tokens starting with a digit are number literals.
      // TODO not all number literal formats are implemented.
      if (isDigit(char)) {
        return readNumber(char);
      }
      // If the character is a valid first character for an identifier
      // then the token is treated as an identifier.
      if (isInitial(char)) {
        return readIdentifier(char);
      }
      // Tokens starting with a dot are '.' (used in definitions),
      // '...' (used in macros),
      // float numbers (TODO).
      if (char === '.') {
        chars = getCharsTillDelimeter();
        if (chars.length === 0) {
          return createToken(TokenTypes.dot);
        }
        if (chars === '..') {
          return createToken(TokenTypes.identifier, '...');
        }
        raiseError('invalid_identifier');
      }
      // Tokens starting with a sign are numbers or arithmetic operations.
      if (char === '+' || char === '-') {
        nextChar = getNextChar();
        oldchar = nextChar;
        if (isDigit(nextChar)) {
          return readNumber(char);
        }
        else {
          return readPeculiarIdentifier(char);
        }
      }
      raiseError('invalid_token');
    }
    // A constant holding valid escaped characters in strings like \n, \r, \t.
    var validEscapedChars = get('valid_escaped_chars');
    var validEscapedCharCodes = [10, 13, 9]
    function guardString(char) {
      if (!char) {
        raiseError('invalid_string');
      }
    }
    // Reads a token representing a string literal and returns its object.
    function readString() {
      var buffer = [];
      var char = getNextChar();
      guardString(char);
      while(char !== '"') {
        if (char === '\\') {
          // TODO add also escaped ", \, etc
          char = getNextChar();
          guardString(char);
          var idx = validEscapedChars.indexOf(char);
          if (idx === -1) {
            raiseError('invalid_escaped_char');
          }
          buffer.push(String.fromCharCode(validEscapedCharCodes[idx]));
        }
        else {
          buffer.push(char);
        }
        char = getNextChar();
        guardString(char);
      }
      return createToken(TokenTypes.string, buffer.join(''));
    }
    // Reads a token representing a number literal and returns its object.
    function readNumber(char) {
      var buffer = [];
      var dot = false;
      if (char === '-' || char === '+') {
        buffer.push(char);
        char = getNextChar();
      }
      while (char && !isDelimeter(char)) {
        if (char === '.') {
          // If there is more than one dot in a number then it is invalid.
          if (dot) {
            raiseError('invalid_number');
          }
          dot = true;
        }
        // If there are non digit characters in the number then it is invalid.
        // NOTE: actually there are such cases and they need to be implemented.
        else if (!isDigit(char)) {
          raiseError('invalid_number');
        }
        buffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return createToken(TokenTypes.number, buffer.join(''));
    }
    // Reads a token representing an identifier, i.e. symbol.
    // TODO implement identifiers enclosed in vertical lines.
    function readIdentifier(char) {
      var buffer = [];
      while (char && !isDelimeter(char)) {
        if (!isSubsequent(char)) {
          raiseError('invalid_identifier');
        }
        buffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return createToken(TokenTypes.identifier, buffer.join(''));
    }
    // Reads a token representing a peculiar identifier, i.e. '+' or '-'.
    function readPeculiarIdentifier(char) {
      var nextChar = getNextChar();
      if (!nextChar || isDelimeter(nextChar)) {
        oldchar = nextChar;
        return createToken(TokenTypes.identifier, char);
      }
      else {
        raiseError('invalid_identifier');
      }
    }
    // Checks if a character is an initial one.
    function isInitial(char) {
      return isLetter(char) || isSpecialInitial(char);
    }
    // A hack function checking if a character is a letter.
    function isLetter(char) {
      return char.toLowerCase() !== char.toUpperCase();
    }
    // A constant containing the list of special initial characters.
    var specialInitials = /[!$%&*\/:<=>?^_~]/;
    // Checks if a character is a special initial one.
    function isSpecialInitial(char) {
      return specialInitials.test(char);
    }
    // A constant containing the list of digits.
    var digits = /[0-9]/;
    // Checks if a character is a digit.
    function isDigit(char) {
      return digits.test(char);
    }
    // A constant containing the list of special subsequent characters.
    var specialSubsequent = /[+-.@]/;
    // Checks if a character is a special subsequent one.
    function isSpecialSubsequent(char) {
      return specialSubsequent.test(char);
    }
    // Checks if a character is a subsequent one.
    function isSubsequent(char) {
      return isInitial(char) || isDigit(char) || isSpecialSubsequent(char);
    }

    // Checks if the character is a token and returns the token object.
    function getTokenFromChar(char) {
      if (char === '(') {
        return createToken(TokenTypes.leftParen);
      }
      if (char === ')') {
        return createToken(TokenTypes.rightParen);
      }
      if (char === '\'') {
        return createToken(TokenTypes.quote);
      }
      if (char === '`') {
        return createToken(TokenTypes.backquote);
      }
      if (char === ',') {
        return createToken(TokenTypes.unquote);
      }
      return null;
    }
    // Checks if a character is a whitespace.
    function isWhitespace(char) {
      return char === ' ' ||
        char === '\n' ||
        char === '\t';
    }
    // Checks if a character is a delimeter.
    function isDelimeter(char) {
      return isWhitespace(char) ||
        char === '(' ||
        char === ')' ||
        char === '"' ||
        char === ';';
    }
    // Returns the position of the last read token.
    function getPosition() {
      return {
        line: line,
        column: column
      }
    }
    // Returns a token iterator.
    return {
      getNextToken: getNextToken,
      getPosition: getPosition
    };
  }
  // The construction of the TokenStream type.
  // It wraps a token iterator and provides
  // functionality for peeking next tokens without consuming them.
  function TokenStream(text, lang) {
    // A cahce holding the peeked tokens.
    // If the cache is not empty then tokens are
    // retrieved from it one by one and consumed.
    this.cache = [];
    // The token iterator that is wrapped.
    this.tokenIterator = tokenize(text, lang);
  }
  // Peeks a token without consuming it.
  // Peeked tokens are kept in a local cache.
  // 'idx' argument specifies how many tokens
  // to be skipped before returning a token.
  // Default is 0. An integer.
  TokenStream.prototype.peek = function peek(idx) {
    idx = idx || 0;
    if (this.cache && this.cache.length > idx) {
      return this.cache[idx];
    }
    var token;
    do {
      token = this.tokenIterator.getNextToken();
      this.cache.push(token);
      if (!token) {
        break;
      }      
    }
    while (this.cache.length < idx);
    return token;
  };
  // Consumes a token and returs it.
  // If the local cache of tokens is not empty
  // then it is traversed first.
  // 'howMany' argument specifies how many tokens
  // to be consumed. The last consumed token is returned.
  // Default is 1. An integer.
  TokenStream.prototype.advance = function advance(howMany) {
    howMany = howMany || 1;
    var token;
    while (howMany) {
      if (this.cache.length) {
        token = this.cache.shift();
      }
      else {
        token = this.tokenIterator.getNextToken();
      }
      howMany -= 1;
    }
    return token;
  };
  // Returns the position of the last consumed token.
  TokenStream.prototype.getPosition = function() {
    return this.tokenIterator.getPosition();
  };

  // Exports the TokenStream type and the TokenTypes tag values.
  return {
    TokenStream: TokenStream,
    TokenTypes: TokenTypes
  };
}));
},{"./lang-table":16}],18:[function(require,module,exports){
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
},{"./ast":2,"./common":3,"./forms/basic":6,"./forms/cond":7,"./forms/define-set":8,"./forms/do":9,"./forms/lambda":10,"./forms/let":11,"./forms/simple":13,"./lang-table":16,"./lexer":17}],19:[function(require,module,exports){
'use strict';

var common = require('../common');
var types = require('../types');

var guardArgsCountExact = common.guardArgsCountExact;
var SchemeChar = types.SchemeChar;

var charProcedures = {
  'char?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof SchemeChar;
  },
};

module.exports = charProcedures;
},{"../common":3,"../types":26}],20:[function(require,module,exports){
'use strict';

var common = require('../common');
var types = require('../types');

var guardArgsCountExact = common.guardArgsCountExact;
var Procedure = types.Procedure;
var PrimitiveProcedure = types.PrimitiveProcedure;
var Application = types.Application;
var ContinuationProcedure = types.ContinuationProcedure;
var Continuation = types.Continuation;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Vector = types.Vector;
var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

function haveConstructor(args, constructor) {
  return args[0] instanceof constructor && args[1] instanceof constructor;
}
function isEquivalent(args, env) {
  guardArgsCountExact(env, args.length, 2);
  var obj1 = args[0];
  var obj2 = args[1];
  if (typeof obj1 === 'boolean' && typeof obj2 === 'boolean') {
    return obj1 === obj2;
  }
  if (typeof obj1 === 'number' && typeof obj2 === 'number') {
    return obj1 === obj2;
  }
  if (haveConstructor(args, SchemeChar)) {
    return obj1.value === obj2.value;
  }
  if (haveConstructor(args, Symbol)) {
    return obj1.value === obj2.value;
  }
  if (obj1 === EmptyList && obj2 === EmptyList) {
    return true;
  }
  if (obj1 === Unspecified && obj2 === Unspecified) {
    return true;
  }
  // TODO bytevectors, records, ports, promises
  if (haveConstructor(args, Pair)
    || haveConstructor(args, Vector)
    || haveConstructor(args, SchemeString)
    // TODO not sure for the procedures
    || haveConstructor(args, PrimitiveProcedure)
    || haveConstructor(args, Procedure)
    || haveConstructor(args, ContinuationProcedure)
    || haveConstructor(args, Continuation)
    || haveConstructor(args, Application)) {
    return obj1 === obj2;
  }
  return false;
}
function isEqual(args, env) {
  guardArgsCountExact(env, args.length, 2);
  var eqv = isEquivalent(args, env);
  if (eqv) {
    return true;
  }
  // TODO bytevectors
  if (haveConstructor(args, Pair)) {
    var pair1 = args[0];
    var pair2 = args[0];
    while (pair1 instanceof Pair && pair2 instanceof Pair) {
      if (!isEqual([pair1.car, pair2.car], env)) {
        return false;
      }
      pair1 = pair1.cdr;
      pair2 = pair2.cdr;
    }
    return pair1 === EmptyList && pair2 === EmptyList;
  }
  if (haveConstructor(args, Vector)) {
    var array1 = args[0].value;
    var array2 = args[1].value;
    if (array1.length !== array2.length) {
      return false;
    }
    for (var i = 0; i < array1.length; i++) {
      if (!isEqual([array1[i], array2[i]], env)) {
        return false;
      }
    }
    return true;
  }
  if (haveConstructor(args, SchemeString)) {
    return args[0].value === args[1].value;
  }
  return false;
}
var equivalenceProcedures = {
  'eq?': isEquivalent,
  'eqv?': isEquivalent,
  'equal?': isEqual,
};

module.exports = equivalenceProcedures;
},{"../common":3,"../types":26}],21:[function(require,module,exports){
'use strict';

var types = require('../types');
var evaluator = require('../evaluator');
var common = require('../common');
var stringProcedures = require('./string');

var Environment = types.Environment;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Vector = types.Vector;
var Pair = types.Pair;
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgPredicate = common.guardArgPredicate;
var applySchemeProcedure = evaluator.applySchemeProcedure;

function convertSchemeObjectToJs(obj, env) {
  if (typeof obj === 'number'
    || typeof obj === 'boolean') {
    return obj;
  }
  if (obj instanceof SchemeString
    || obj instanceof SchemeChar
    || obj instanceof Symbol) {
    return obj.value;
  }
  if (obj instanceof Vector) {
    return obj.value.map(function (item) {
      return convertSchemeObjectToJs(item);
    });
  }
  if (obj instanceof Pair) {
    if (Pair.isProperList(obj)) {
      var isDict = listToVector(obj).value.every(function (item) {
        return item instanceof Pair;
      });
      if (isDict) {
        return convertSchemeDictToJsObject(obj);
      }
      else {
        return convertSchemeObjectToJs(listToVector(obj));
      }
    }
  }
  if (obj instanceof PrimitiveProcedure) {
    return function () {
      var args = Array.prototype.map.apply(arguments, function (arg) {
        return convertSchemeObjectToJs(arg, env);
      });
      obj.fn(args, env);
    }
  }
  if (obj instanceof Procedure) {
    return function () {
      var args = Array.prototype.map.apply(arguments, function (arg) {
        return convertSchemeObjectToJs(arg, env);
      });
      var value = applySchemeProcedure(obj, args);
      return convertSchemeObjectToJs(value);
    };
  }
}
function convertJsObjectToDcit(obj) {
  var pairs = Object.keys(obj).map(function (prop) {
    return new Pair(convertJsObjectToScheme(prop), convertJsObjectToScheme(obj[prop]));
  });
  return Pair.createList(pairs);
}
function convertJsObjectToScheme(obj) {
  if (typeof obj === 'number'
    || typeof obj === 'boolean') {
    return obj;
  }
  if (typeof obj === 'string') {
    return new SchemeString(obj);
  }
  if (typeof obj === 'function') {
    return new PrimitiveProcedure(function (args, env) {
      var value = obj.apply(null, args.map(function (arg) {
        return convertSchemeObjectToJs(arg);
      }))
      return convertJsObjectToScheme(value);
    })
  }
  if (typeof obj === 'object' && obj !== null) {
    if (Array.isArray(obj)) {
      return new Vector(obj, false);
    }
    if (obj instanceof Date) {
      return new SchemeString(obj.toString());
    }
    if (typeof obj.constructor === 'undefined'
      || obj.constructor.name === 'Object') {
      return convertJsObjectToDcit(obj);
    }
  }
  return Unspecified;
}
function convertSchemeDictToJsObject(dict) {
  var obj = {};
  var list = dict, pair;
  while (list instanceof Pair) {
    pair = list.car;
    obj[pair.car.toString()] = convertSchemeObjectToJs(pair.cdr);
    list = list.cdr;      
  }
  return obj;
}
function listToVector(list) {
  var arr = [];
  while (list instanceof Pair) {
    arr.push(list.car);
    list = list.cdr;
  }
  return new Vector(arr);
}
var ffiProcedures = {
  'js-eval': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');      
    var value = eval(args[0].value);
    return convertJsObjectToScheme(value);
  },
};

exports.procedures = ffiProcedures;
exports.convert = convertSchemeObjectToJs;
},{"../common":3,"../evaluator":5,"../types":26,"./string":24}],22:[function(require,module,exports){
var common = require('../common');
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgsCountMax = common.guardArgsCountMax;
var raiseRuntimeError = common.raiseRuntimeError;

function guardNumbers(env, args) {
  for (var i = 0; i < args.length; i++) {
    if (typeof args[i] !== 'number') {
      raiseRuntimeError(env, 'number_expected', [typeof args[i]]);
    }
  }
}
function mathFn(fn) {
  return function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardNumbers(env, args);
    return fn(args[0]);
  };
}
var numberProcedures = {
  'number?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return typeof args[0] === 'number';
  },
  // borrowed from Racket where it is "exact-nonnegative-integer?"
  'nonnegative-integer?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var x = args[0];
    return typeof x === 'number' && Math.round(x) === x && x >= 0;
  },
  '+': function (args, env){
    guardNumbers(env, args);
    var res = 0;
    for (var i = 0; i < args.length; i++) {
      res += args[i];
    }
    return res;
  },
  '-': function (args, env){
    guardArgsCountMin(env, args.length, 1);
    guardNumbers(env, args);
    if (args.length === 1) {
      return -args[0];
    }
    else {
      var res = args[0];
      for (var i = 1; i < args.length; i++) {
        res -= args[i];
      }
      return res;
    }
  },
  '*': function (args, env){
    guardNumbers(env, args);
    var res = 1;
    for (var i = 0; i < args.length; i++) {
      res *= args[i];
    }
    return res;
  },
  '/': function (args, env){
    guardArgsCountMin(env, args.length, 1);
    guardNumbers(env, args);
    if (args.length === 1) {
      return 1 / args[0];
    }
    else {
      var res = args[0];
      for (var i = 1; i < args.length; i++) {
        res /= args[i];
      }
      return res;
    }
  },
  '=': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    guardNumbers(env, args);
    for (var i = 0, l = args.length - 1; i < l; i++) {
      if (args[i] !== args[i + 1]) {
        return false;
      }
    }
    return true;
  },
  '<': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    guardNumbers(env, args);
    for (var i = 0, l = args.length - 1; i < l; i++) {
      if (args[i] >= args[i + 1]) {
        return false;
      }
    }
    return true;
  },
  '>': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    guardNumbers(env, args);
    for (var i = 0, l = args.length - 1; i < l; i++) {
      if (args[i] <= args[i + 1]) {
        return false;
      }
    }
    return true;
  },
  'exp': mathFn(Math.exp),
  'sin': mathFn(Math.sin),
  'cos': mathFn(Math.cos),
  'tan': mathFn(Math.tan),
  'asin': mathFn(Math.asin),
  'acos': mathFn(Math.acos),
  'log': function (args, env) {
    guardArgsCountMin(env, args.length, 1);
    guardArgsCountMax(env, args.length, 2);
    guardNumbers(env, args);
    if (args.length === 1) {
      return Math.log(args[0]);
    }
    else { // args.length === 2
      return Math.log(args[0]) / Math.log(args[1]);
    }
  },
  'atan': function (args, env) {
    guardArgsCountMin(env, args.length, 1);
    guardArgsCountMax(env, args.length, 2);
    guardNumbers(env, args);
    if (args.length === 1) {
      return Math.atan(args[0]);
    }
    else { // args.length === 2
      return Math.atan2(args[0], args[1]);
    }
  },
  'sqrt': mathFn(Math.sqrt),
  'expt': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardNumbers(env, args);
    return Math.pow(args[0], args[1]);
  },
};

module.exports = numberProcedures;
},{"../common":3}],23:[function(require,module,exports){
'use strict';

var common = require('../common');
var types = require('../types');
var numberProcedures = require('./number');
var equivalenceProcedures = require('./equivalence');

var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgsCountMax = common.guardArgsCountMax;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;
var raiseRuntimeError = common.raiseRuntimeError;

var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

var isEq = equivalenceProcedures['eq?'];
var isEqv = equivalenceProcedures['eqv?'];
var isEqual = equivalenceProcedures['equal?'];

function car(args, env) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
  return args[0].car;
}
function cdr(args, env) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
  return args[0].cdr;
}
function findMember(compare) {
  return function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[1], pairListProcedures['pair?'], 1, 'procedures', 'pair?');
    var obj = args[0];
    var pair = args[1];
    while (pair instanceof Pair) {
      if (compare([pair.car, obj], env)) {
        return pair;
      }
      pair = pair.cdr;
    }
    if (pair === EmptyList) {
      return false;
    }
    else {
      raiseRuntimeError(env, 'improper_list');
    }
  };
}
function findPair(compare) {
  return function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[1], pairListProcedures['pair?'], 1, 'procedures', 'pair?');
    var obj = args[0];
    var pair = args[1];
    var element;
    while (pair instanceof Pair) {
      element = pair.car;
      if (!pairListProcedures['pair?']([element], env)) {
        raiseRuntimeError(env, 'improper_alist');
      }
      if (compare([element.car, obj], env)) {
        return element;
      }
      pair = pair.cdr;
    }
    if (pair === EmptyList) {
      return false;
    }
    else {
      raiseRuntimeError(env, 'improper_list');
    }
  };
}
var pairListProcedures = {
  'cons': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    return new Pair(args[0], args[1]);
  },
  'pair?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof Pair;
  },
  'car': car,
  'cdr': cdr,
  'set-car!': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardImmutable(env, args[0]);
    args[0].car = args[1];
    return Unspecified;
  },
  'set-cdr!': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardImmutable(env, args[0]);
    args[0].cdr = args[1];
    return Unspecified;
  },
  'caar': function (args, env) {
    return car([car(args, env)], env);
  },
  'cadr': function (args, env) {
    return car([cdr(args, env)], env);
  },
  'cdar': function (args, env) {
    return cdr([car(args, env)], env);
  },
  'cddr': function (args, env) {
    return cdr([cdr(args, env)], env);
  },
  'null?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] === EmptyList;
  },
  'list?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return Pair.isProperList(args[0]);
  },
  'list': Pair.createList,
  'make-list': function (args, env) {
    guardArgsCountMin(env, args.length, 1);
    guardArgsCountMax(env, args.length, 2);
    guardArgPredicate(env, args[0], numberProcedures['nonnegative-integer?'], 0, 'procedures', 'nonnegative-integer?');
    var fill = 0;
    if (args.length === 2) {
      fill = args[1];
    }
    var arr = new Array(args[0]);
    for (var i = 0; i < arr.length; i++) {
      arr[i] = fill;
    }
    return Pair.createList(arr, env);
  },
  'length': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var list = args[0];
    if (list === EmptyList) {
      return 0;
    }
    var length = 0;
    while (list instanceof Pair) {
      length += 1;
      list = list.cdr;      
    }
    if (list === EmptyList) {
      return length;
    }
    else {
      raiseRuntimeError(env, 'argument_predicate_false', [0, 'list?']);
    }
  },
  'list-ref': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      return pair.car;
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
  },
  'list-set!': function (args, env) {
    guardArgsCountExact(env, args.length, 3);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];    
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      guardImmutable(env, pair);
      pair.car = args[2];
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
    return Unspecified;
  },
  'append': function (args, env) {
    var argsCount = args.length;
    if (argsCount === 0) {
      return EmptyList;
    }
    if (argsCount === 1) {
      return args[0];
    }
    var i, l;
    for (i = 0, l = argsCount - 1; i < l; i++) {
      guardArgPredicate(env, args[i], pairListProcedures['list?'], i, 'procedures', 'list?');
    }
    var last = args[argsCount - 1];
    var first, pair, nextPair, list;
    for (i = 0, l = argsCount - 1; i < l; i++) {
      list = args[i];
      if (list === EmptyList) {
        continue;
      }        
      nextPair = new Pair(list.car, EmptyList);
      if (pair) {
        pair.cdr = nextPair;
      }
      pair = nextPair;
      if (!first) {
        first = pair;
      }
      while (list.cdr !== EmptyList) {
        list = list.cdr;
        pair.cdr = new Pair(list.car, EmptyList);
        pair = pair.cdr;
      }
    }
    if (pair) {
      pair.cdr = last;
    }
    return first || last;
  },
  'reverse': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var list = args[0];
    if (list === EmptyList) {
      return EmptyList;
    }
    var newList = EmptyList;
    while (list instanceof Pair) {
      newList = new Pair(list.car, newList);
      list = list.cdr;      
    }
    if (list === EmptyList) {
      return newList;
    }
    else {
      raiseRuntimeError(env, 'argument_predicate_false', [0, 'list?']);
    }
  },
  'list-tail': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      return pair;
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
  },
  'memq': findMember(equivalenceProcedures['eq?']),
  'memv': findMember(equivalenceProcedures['eqv?']),
  'member': findMember(equivalenceProcedures['equal?']), // TODO implement (member obj list compare)
  'assq': findPair(equivalenceProcedures['eq?']),
  'assv': findPair(equivalenceProcedures['eqv?']),
  'assoc': findPair(equivalenceProcedures['equal?']), // TODO implement (assoc obj alist compare)
  'list-copy': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var obj = args[0];
    if (!(obj instanceof Pair)) {
      return obj;
    }
    var pair, nextPair, list;
    list = pair = nextPair = new Pair(obj.car, obj.cdr);
    obj = obj.cdr;
    while (obj instanceof Pair) {
      nextPair = new Pair(obj.car, obj.cdr);
      if (pair) {
        pair.cdr = nextPair;
      }
      pair = nextPair;
      obj = obj.cdr;
    }
    return list;
  },
};

module.exports = pairListProcedures;
},{"../common":3,"../types":26,"./equivalence":20,"./number":22}],24:[function(require,module,exports){
'use strict';

var common = require('../common');
var types = require('../types');
var numberProcedures = require('./number');
var charProcedures = require('./char');

var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Unspecified = types.Unspecified;
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgsCountMax = common.guardArgsCountMax;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;

var stringProcedures = {
  'string?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof SchemeString;
  },
  'make-string': function (args, env) {
    guardArgsCountMin(env, args.length, 1);
    guardArgsCountMax(env, args.length, 2);
    guardArgPredicate(env, args[0], numberProcedures['nonnegative-integer?'], 0, 'procedures', 'nonnegative-integer?');
    var fill = '\x00';
    if (args.length === 2) {
      guardArgPredicate(env, args[1], charProcedures['char?'], 1, 'procedures', 'char?');
      fill = args[1].value;
    }
    var arr = new Array(args[0]);
    for (var i = 0; i < arr.length; i++) {
      arr[i] = fill;
    }
    return new SchemeString(arr.join(''));
  },
  'string': function (args, env) {
    for (var i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], charProcedures['char?'], i, 'procedures', 'char?');
    }
    var arr = new Array(args.length);
    for (var i = 0; i < arr.length; i++) {
      arr[i] = args[i].value;
    }
    return new SchemeString(arr.join(''));
  },
  'string-length': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    return args[0].value.length;
  },
  'string-ref': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var str = args[0].value;
    if (k > str.length - 1) {
      raiseRuntimeError(env, 'string_index_out_range', [str.length]);
    }
    return new SchemeChar(str[k]);
  },
  'string-set!': function (args, env) {
    guardArgsCountExact(env, args.length, 3);
    var k = args[1];
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    guardArgPredicate(env, args[2], charProcedures['char?'], 2, 'procedures', 'char?');
    guardImmutable(env, args[0]);
    var str = args[0].value;
    if (k > str.length - 1) {
      raiseRuntimeError(env, 'string_index_out_range', [str.length]);
    }
    args[0].value = str.substring(0, k) + args[2].value + str.substring(k + 1);
    return Unspecified;
  },
  'string=?': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    for (var i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], stringProcedures['string?'], i, 'procedures', 'string?');
    }
    for (var i = 0, l = args.length - 1; i < l; i++) {
      if (args[i].value !== args[i + 1].value) {
        return false;
      }
    }
    return true;
  },
};

module.exports = stringProcedures;
},{"../common":3,"../types":26,"./char":19,"./number":22}],25:[function(require,module,exports){
var common = require('../common');
var types = require('../types');
var langTable = require('../lang-table');
var charProcedures = require('./char');
var stringProcedures = require('./string');
var numberProcedures = require('./number');

var Vector = types.Vector;
var Pair = types.Pair;
var EmptyList = types.EmptyList;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Unspecified = types.Unspecified;
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgsCountMax = common.guardArgsCountMax;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;
var raiseRuntimeError = common.raiseRuntimeError;
var langName = common.langName;

function getCopyVectorArgs(args, env, firstArgPredicate, firstArgPredicateName) {
  guardArgsCountMin(env, args.length, 1);
  guardArgsCountMax(env, args.length, 3);
  guardArgPredicate(env, args[0], firstArgPredicate, 0, 'procedures', firstArgPredicateName);
  var arr = args[0].value;
  var start = 0;
  var end = arr.length;
  if (args.length >= 2) {
    guardArgPredicate(env, args[1], numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    start = args[1];
  }
  if (args.length === 3) {
    guardArgPredicate(env, args[2], numberProcedures['nonnegative-integer?'], 2, 'procedures', 'nonnegative-integer?');
    end = args[2];
  }
  if (start > arr.length - 1) {
    raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
  }
  if (end > arr.length) {
    raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
  }
  if (start > end) {
    raiseRuntimeError(env, 'start_index_greater_end_index');
  }
  return {
    arr: arr,
    start: start,
    end: end,
  };
}
function guardVectorContentPredicate(env, arg, predicate, startIndex, endIndex, predicateCat, predicateKey) {
  if (!predicate([arg], env)) {
    var lang = env.getVar(langName);
    var predicateName = langTable.get(lang, predicateCat, predicateKey);
    raiseRuntimeError(env, 'vector_argument_predicate_false', [startIndex, endIndex, predicateName]);
  }
}
var vectorProcedures = {
  'vector': Vector.create,
  'make-vector': function (args, env) {
    guardArgsCountMin(env, args.length, 1);
    guardArgsCountMax(env, args.length, 2);
    guardArgPredicate(env, args[0], numberProcedures['nonnegative-integer?'], 0, 'procedures', 'nonnegative-integer?');
    var fill = 0;
    if (args.length === 2) {
      fill = args[1];
    }
    var arr = new Array(args[0]);
    for (var i = 0; i < arr.length; i++) {
      arr[i] = fill;
    }
    return new Vector(arr);
  },
  'vector?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof Vector;
  },
  'vector-length': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], vectorProcedures['vector?'], 0, 'procedures', 'vector?');
    return args[0].value.length;
  },
  'vector-ref': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], vectorProcedures['vector?'], 0, 'procedures', 'vector?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var arr = args[0].value;
    if (k > arr.length - 1) {
      raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
    }
    return arr[k];
  },
  'vector-set!': function (args, env) {
    guardArgsCountExact(env, args.length, 3);
    var k = args[1];
    guardArgPredicate(env, args[0], vectorProcedures['vector?'], 0, 'procedures', 'vector?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    guardImmutable(env, args[0]);
    var arr = args[0].value;
    if (k > arr.length - 1) {
      raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
    }
    arr[k] = args[2];
    return Unspecified;
  },
  'vector->list': function (args, env) {
    var params = getCopyVectorArgs(args, env, vectorProcedures['vector?'], 'vector?');
    return Pair.createList(params.arr.slice(params.start, params.end));
  },
  'list->vector': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var list = args[0];
    if (args === EmptyList) {
      return new Vector([]);
    }
    var arr = [];
    while (list instanceof Pair) {
      arr.push(list.car);
      list = list.cdr;      
    }
    if (list === EmptyList) {
      return new Vector(arr);
    }
    else {
      raiseRuntimeError(env, 'argument_predicate_false', [0, 'list?']);
    }
  },
  'vector->string': function (args, env) {
    var params = getCopyVectorArgs(args, env, vectorProcedures['vector?'], 'vector?');
    var arr = params.arr;
    var start = params.start;
    var end = params.end;
    var strArr = new Array(end - start);
    var char;
    for (var i = start, j = 0; i < end; i++, j++) {
      char = arr[i];
      guardVectorContentPredicate(env, char, charProcedures['char?'], start, end, 'procedures', 'char?');
      strArr[j] = char.value;
    }
    return new SchemeString(strArr.join(''));
  },
  'string->vector': function (args, env) {
    var params = getCopyVectorArgs(args, env, stringProcedures['string?'], 'string?');
    var charArray = params.arr.split('').map(function (char) {
      return new SchemeChar(char);
    });
    return new Vector(charArray.slice(params.start, params.end));
  },
  'vector-copy': function (args, env) {
    var params = getCopyVectorArgs(args, env, vectorProcedures['vector?'], 'vector?');
    return new Vector(params.arr.slice(params.start, params.end));
  },
  'vector-copy!': function (args, env) {
    guardArgsCountMin(env, args.length, 3);
    guardArgsCountMax(env, args.length, 5);
    guardArgPredicate(env, args[0], vectorProcedures['vector?'], 0, 'procedures', 'vector?');
    guardImmutable(env, args[0]);
    guardArgPredicate(env, args[1], numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    guardArgPredicate(env, args[2], vectorProcedures['vector?'], 2, 'procedures', 'vector?');
    var to = args[0].value;
    var at = args[1]
    var from = args[2].value;
    var start = 0;
    var end = from.length;
    if (args.length >= 4) {
      guardArgPredicate(env, args[3], numberProcedures['nonnegative-integer?'], 3, 'procedures', 'nonnegative-integer?');
      start = args[3];
    }
    if (args.length === 5) {
      guardArgPredicate(env, args[4], numberProcedures['nonnegative-integer?'], 4, 'procedures', 'nonnegative-integer?');
      end = args[4];
    }
    if (at > to.length - 1) {
      raiseRuntimeError(env, 'vector_index_out_range', [at.length]);
    }
    if (start > from.length - 1) {
      raiseRuntimeError(env, 'vector_index_out_range', [from.length]);
    }
    if (end > from.length) {
      raiseRuntimeError(env, 'vector_index_out_range', [from.length]);
    }
    if (start > end) {
      raiseRuntimeError(env, 'start_index_greater_end_index');
    }
    if (to.length - at < end -start) {
      raiseRuntimeError(env, 'vector_copy_space_needed', [end - start, to.length - at]);
    }
    if (to === from && at === start) {
      return;
    }
    if (at < start) {
      for (var i = start, j = at; i < end; i++, j++) {
        to[j] = from[i];
      }
    }
    else { // at >= start
      for (var i = end - 1, j = at + end - start - 1; i >= start; i--, j--) {
        to[j] = from[i];
      }
    }
    return Unspecified;
  },
  'vector-append': function (args, env) {
    var totalLength = 0, i, j, k, argArr;
    for (i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], vectorProcedures['vector?'], i, 'procedures', 'vector?');
      totalLength += args[i].value.length;
    }
    var arr = new Array(totalLength);
    for (i = 0, k = 0; i < args.length; i++) {
      argArr = args[i].value;
      for (j = 0; j < argArr.length; j++, k++) {
        arr[k] = argArr[j];
      }
    }
    return new Vector(arr);
  },
  'vector-fill!': function (args, env) {
    guardArgsCountMin(env, args.length, 2);
    guardArgsCountMax(env, args.length, 4);
    guardArgPredicate(env, args[0], vectorProcedures['vector?'], 0, 'procedures', 'vector?');
    guardImmutable(env, args[0]);
    var arr = args[0].value;
    var fill = args[1];
    var start = 0;
    var end = arr.length;
    if (args.length >= 3) {
      guardArgPredicate(env, args[2], numberProcedures['nonnegative-integer?'], 2, 'procedures', 'nonnegative-integer?');
      start = args[2];
    }
    if (args.length === 4) {
      guardArgPredicate(env, args[3], numberProcedures['nonnegative-integer?'], 3, 'procedures', 'nonnegative-integer?');
      end = args[3];
    }
    if (start > arr.length - 1) {
      raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
    }
    if (end > arr.length) {
      raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
    }
    if (start > end) {
      raiseRuntimeError(env, 'start_index_greater_end_index');
    }
    for (var i = start; i < end; i++) {
      arr[i] = fill;
    }
    return Unspecified;
  },
};

module.exports = vectorProcedures;
},{"../common":3,"../lang-table":16,"../types":26,"./char":19,"./number":22,"./string":24}],26:[function(require,module,exports){
var common = require('./common');
var guardArgsCountExact = common.guardArgsCountExact;
var cloneEnvs = common.cloneEnvs;
var raiseRuntimeError = common.raiseRuntimeError;

function Environment(parent) {
  this.parent = parent || null;
  this.varNames = [];
  this.varValues = [];
  this.expressionStack = [];
  this.opIndex = 0;
  this.ops = null;
  this.procedure = null;
}
Environment.prototype.addVar = function addVar(name, value) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    this.varNames.push(name);
    this.varValues.push(value);
  }
  else {
    this.varValues[idx] = value;
  }
};
Environment.prototype.setVar = function setVar(name, value) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    if (this.parent !== null) {
      this.parent.setVar(name, value);
    }
    else {
      raiseRuntimeError(this, 'undefined_variable', [name]);
    }
  }
  else {
    this.varValues[idx] = value;
  }
};
Environment.prototype.getVar = function getVar(name) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    if (this.parent !== null) {
      return this.parent.getVar(name);
    }
    else {
      raiseRuntimeError(this, 'undefined_variable', [name]);
    }
  }
  else {
    return this.varValues[idx];
  }
};
Environment.prototype.clone = function clone() {
  var env = new Environment(this.parent);
  env.varNames = this.varNames;
  env.varValues = this.varValues;
  env.expressionStack = this.expressionStack.slice();
  env.opIndex = this.opIndex;
  env.ops = this.ops;
  env.calledProcedure = this.calledProcedure;
  return env;
};
function OutputPort(fn) {
  this.fn = fn;
}
OutputPort.prototype.emit = function emit(data) {
  this.fn(data);
};
function Procedure(args, body, env, name) {
  this.args = args;
  this.body = body;
  this.env = env;
  this.name = name;
}
Procedure.prototype.toString = function toString() {
  if (this.name) {
    return '#<procedure ' + this.name + '>';
  }
  else {
    return '#<procedure>';
  }
};
function PrimitiveProcedure(fn, name) {
  this.fn = fn;
  this.name = name || '';
}
PrimitiveProcedure.prototype.execute = function execute(args, env) {
  return this.fn(args, env);
};
PrimitiveProcedure.prototype.toString = function toString() {
  return '#<procedure ' + this.name + '>';
};
function Application(name) {
  this.name = name || '';
}
function ContinuationProcedure(fn, name) {
  this.fn = fn;
  this.name = name || '';
}
ContinuationProcedure.prototype.execute = function execute(args, env, envs) {
  return this.fn(args, env, envs);
};
ContinuationProcedure.prototype.toString = PrimitiveProcedure.prototype.toString;
function Continuation(envs) {
  this.envs = envs;
}
Continuation.prototype.execute = function execute(args, env) {
  guardArgsCountExact(env, args.length, 1);
  return {
    envs: cloneEnvs(this.envs),
    value: args[0]
  };
};
Continuation.prototype.toString = Procedure.prototype.toString;
function Symbol(value) {
  this.value = value;
}
Symbol.prototype.valueOf = function valueOf() {
  return this.value;
};
Symbol.prototype.toString = function toString() {
  return this.value;
};
function SchemeString(value, immutable) {
  this.value = value;
  this.immutable = !!immutable;
}
SchemeString.prototype.valueOf = function valueOf() {
  return this.value;
};
SchemeString.prototype.toString = function toString() {
  // TODO translate nrt to the current language
  var value = this.value.replace(/[\t\n\r]/g, function (txt) {
    switch (txt) {
      case '\t':
        return '\\t';
      case '\n':
        return '\\n';
      case '\r':
        return '\\r';
    }
  });
  return '"' + value + '"';
};
function SchemeChar(value) {
  this.value = value;
}
SchemeChar.prototype.valueOf = function valueOf() {
  return this.value;
};
SchemeChar.prototype.toString = function toString() {
  return '#\\' + this.value;
};
function Vector(items, immutable) {
  this.value = null;
  this.immutable = !!immutable;
  if (Array.isArray(items)) {
    this.value = items;
  }
  else {
    this.value = new Array(length);  
  }
}
Vector.prototype.valueOf = function valueOf() {
  return this.value;
};
Vector.prototype.toString = function toString() {
  return '#(' + this.value.join(' ') + ')';
};
Vector.create = function createVector(args, env, immutable) {
  return new Vector(args, immutable);
};
function Pair(car, cdr, immutable) {
  this.car = car;
  this.cdr = cdr;
  this.immutable = !!immutable;
}
Pair.prototype.toString = function toString() {
  var list = this;
  var str = '(';
  var cdr;
  while (list instanceof Pair) {
    str += list.car.toString();
    cdr = list.cdr;
    if (cdr instanceof Pair) {
      list = cdr;
    }
    else if (cdr === EmptyList) {
      break;
    }
    else {
      str += ' . ' + cdr.toString();
      break;
    }
    str += ' ';
  }
  str += ')';
  return str;
};
Pair.createList = function createList(args, env, immutable) {
  var pair = EmptyList;
  for (var i = args.length - 1; i >= 0; i--) {
    pair = new Pair(args[i], pair, immutable);
  }
  return pair;
};
Pair.isProperList = function isProperList(list) {
  while (list instanceof Pair) {
    list = list.cdr;      
  }
  return list === EmptyList;
};
var EmptyList = Object.create(null);
EmptyList.toString = function toString() {
  return "'()";
};
var Unspecified = Object.create(null);
Unspecified.toString = function toString() {
  return '';
};
function SchemeError(message) {
  this.message = message || '';
  this.stack = null;
}
SchemeError.prototype.toString = function toString() {
  var res = 'Error: '; // TODO localize
  if (this.message) {
    res += this.message + '\n';
  }
  res += this.stack;
  return res;
}

module.exports = {
  Environment: Environment,
  OutputPort: OutputPort,
  Procedure: Procedure,
  PrimitiveProcedure: PrimitiveProcedure,
  Application: Application,
  ContinuationProcedure: ContinuationProcedure,
  Continuation: Continuation,
  Symbol: Symbol,
  SchemeString: SchemeString,
  SchemeChar: SchemeChar,
  Vector: Vector,
  Pair: Pair,
  EmptyList: EmptyList,
  Unspecified: Unspecified,
  SchemeError: SchemeError,
};
},{"./common":3}]},{},[1]);
