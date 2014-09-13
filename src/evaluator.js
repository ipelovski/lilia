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
var numberProcedures = require('./procedures/number');
var pairListProcedures = require('./procedures/pair-list');
var vectorProcedures = require('./procedures/vector');
var stringProcedures = require('./procedures/string');
var ffiProcedures = require('./procedures/ffi');

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
var ContinuationProcedure = types.ContinuationProcedure;
var Continuation = types.Continuation;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var Vector = types.Vector;
var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

var outputPort;
function setOutputPortHandler(fn) {
  outputPort = new OutputPort(fn);
}
function isEquivalent(args, env) {
  guardArgsCountExact(env, args.length, 2);
  function haveConstructor(args, constructor) {
    return args[0] instanceof constructor && args[1] instanceof constructor;
  }
  var obj1 = args[0];
  var obj2 = args[1];
  if (typeof obj1 === 'boolean' && typeof obj2 === 'boolean') {
    return obj1 === obj2;
  }
  if (typeof obj1 === 'number' && typeof obj2 === 'number') {
    return obj1 === obj2;
  }
  if (typeof obj1 === 'string' && typeof obj2 === 'string') {
    return obj1 === obj2; // chars
  }
  if (haveConstructor(args, Symbol)) {
    return obj1.value === obj2.value;
  }
  if (obj1 === EmptyList && obj2 === EmptyList) {
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
    || haveConstructor(args, Continuation)) {
    return obj1 === obj2;
  }
  return false;
}
function listToVector(list) {
  var arr = [];
  while (list instanceof Pair) {
    arr.push(list.car);
    list = list.cdr;
  }
  return new Vector(arr);
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
var procedureTypes = [Procedure, PrimitiveProcedure, ContinuationProcedure, Continuation];
function isProcedure(arg) {
  return procedureTypes.some(function (type) {
    return arg instanceof type;
  });
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
  'procedure?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return isProcedure(args[0]);
  },
  'display': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    if (outputPort) {
      outputPort.emit(args[0].toString());
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
  'eq?': isEquivalent,
  'eqv?': isEquivalent,  
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
  addProceduers(env, lang, numberProcedures);
  addProceduers(env, lang, pairListProcedures);
  addProceduers(env, lang, vectorProcedures);
  addProceduers(env, lang, stringProcedures);
  addProceduers(env, lang, ffiProcedures);
  addContinuationProcedures(env, lang);
}
function callcc(args, env, envs) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], primitiveFunctions['procedure?'], 0, 'procedures', 'procedure?');
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

function evalListLiteral(listItems, env) {
  if (listItems.length === 0) {
    return EmptyList;
  }
  var listLength = listItems.length;
  var improperList = listItems[listLength - 2] === '.';
  var count = improperList ? listLength - 4 : listLength - 1;
  var args = [];
  var pair;
  if (improperList) {
    pair = new Pair(evalLiteral(listItems[listLength - 3], env),
      evalLiteral(listItems[listLength - 1], env), true);
  }
  else {
    pair = EmptyList;
  }
  for (var i = count; i >= 0; i--) {
    pair = new Pair(evalLiteral(listItems[i], env), pair, true);
  }
  return pair;
}
function evalVectorLiteral(vectorItems, env) {
  var count = vectorItems.length;
  var args = new Array(count);
  for (var i = 0; i < count; i++) {
    args[i] = evalLiteral(vectorItems[i], env);
  }
  return Vector.create(args, env, true);
}
function evalLiteral(literal, env) {
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
      return value;
    case TokenTypes.string:
      return new SchemeString(value, true);
    case TokenTypes.identifier:
      return new Symbol(value);
    case 'list':
      return evalListLiteral(value, env);
    case 'vector':
      return evalVectorLiteral(value, env);
    default:
      raiseRuntimeError(env, 'unknown_type', [type]);
  }
}

function peek(arr) {
  return arr[arr.length - 1];
}
function evalOPDefine(op, env) {
  var value = env.expressionStack.pop();
  var identifier = env.expressionStack.pop();
  env.addVar(identifier.value, value);
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
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOPVariable(op, env) {
  var value = env.getVar(op[1]);
  env.expressionStack.push(value);
  return -1;
}
function evalOPLiteral(op, env) {
  var value = evalLiteral(op[1], env);
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
  var procedure = new Procedure(op[1], op[2], env);
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
    case OPTypes.literal:
      return evalOPLiteral(op, env);
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
function evalOPs(ops, env) {
  function applyProcedure(procedure, actualArgs) {
    var formals = procedure.args;
    env = new Environment(procedure.env);
    if (op[0] === OPTypes.tailcall) {
      envs.pop();
    }
    envs.push(env);
    applyArguments(formals, actualArgs, env);
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
      if (envs.length >= maxEnvCount) {
        raiseRuntimeError(env, 'maximum_stack_size_exceeded');
      }
      var procedure = env.expressionStack.pop();
      var argsCount = op[1];
      var actualArgs = new Array(argsCount);
      for (var a = argsCount - 1; a >= 0; a--) {
        actualArgs[a] = env.expressionStack.pop();
      }
      if (procedure instanceof PrimitiveProcedure) {
        value = procedure.execute(actualArgs, env);
        env.expressionStack.push(value);
        idx = -1;
      }
      else if (procedure instanceof Procedure) {
        applyProcedure(procedure, actualArgs);
      }
      else if (procedure instanceof ContinuationProcedure) {
        value = procedure.execute(actualArgs, env, envs);
        // TODO the passed lambda should accept only one argument
        procedure = actualArgs[0];
        applyProcedure(procedure, [value]);
      }
      else if (procedure instanceof Continuation) {
        value = procedure.execute(actualArgs, env);
        envs = value.envs;
        env = peek(envs);
        env.expressionStack.push(value.value);
        ops = env.ops;
        idx = env.opIndex + 1;
      }
    }
    else {
      idx = evalOP(op, env);
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

function evaluate(text, lang) {
  lang = lang || 'en';
  var program = compiler.compile(text, lang);
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  return evalOPs(program, env);
}
function initEval(lang) {
  lang = lang || 'en';
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  return function evalFragment(text) {
    var fragment = compiler.compile(text, lang);
    return evalOPs(fragment, env);
  };
}

exports.evalOPs = evalOPs;
exports.evaluate = evaluate;
exports.initEval = initEval;
exports.setOutputPortHandler = setOutputPortHandler;