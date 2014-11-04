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
var langName = common.langName;

var Environment = types.Environment;
var OutputPort = types.OutputPort;
var Procedure = types.Procedure;
var PrimitiveProcedure = types.PrimitiveProcedure;
var Application = types.Application;
var ContinuationProcedure = types.ContinuationProcedure;
var Continuation = types.Continuation;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Pair = types.Pair;
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
function objectToString(obj, env) {
  if (typeof obj === 'boolean') {
    return '#' + langTable.get(env.getVar(langName), 'tokens', obj.toString());
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
      outputPort.emit(objectToString(obj, env));
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
      outputPort.emit(objectToString(obj, env));
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
    if (procedures.hasOwnProperty(name)) {
      var translatedName = langTable.get(lang, 'procedures', name);
      env.addVar(translatedName,
        new PrimitiveProcedure(procedures[name], translatedName));
    }
  }
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
    if (continuationProcedures.hasOwnProperty(name)) {
      var translatedName = langTable.get(lang, 'procedures', name);
      env.addVar(translatedName,
        new ContinuationProcedure(continuationProcedures[name], translatedName));
    }
  }
}
function addApplication(env, lang) {
  var translatedName = langTable.get(lang, 'procedures', 'apply');
  env.addVar(translatedName, new Application(translatedName));
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

function applyArguments(formals, actualArgs, procEnv) {
  var i;
  if (!Array.isArray(formals)) {
    procEnv.addVar(formals, Pair.createList(actualArgs));
  }
  else if (formals[formals.length - 2] === '.') {
    if (formals.length - 1 > actualArgs.length) {
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
    if (op[0] === OPTypes.call ||
      op[0] === OPTypes.tailcall) {
      var application = false;
      while (true) {
        if (envs.length >= maxEnvCount) {
          raiseRuntimeError(env, 'maximum_stack_size_exceeded');
        }
        var procedure;
        var actualArgs;
        var applicationArgs;
        var a, l;
        if (application) {
          actualArgs = applicationArgs;
          application = false;
        }
        else {
          procedure = env.expressionStack.pop();
          var argsCount = op[1];
          actualArgs = new Array(argsCount);
          for (a = argsCount - 1; a >= 0; a--) {
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
          applicationArgs = [];
          for (a = 1, l = actualArgs.length - 1; a < l; a++) {
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
function applySchemeProcedure(procedure, actualArgs) {
  var formals = procedure.args;
  var env = new Environment(procedure.env);
  applyArguments(formals, actualArgs, env);
  return evalOPs(procedure.body, env);
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
  var program;
  try {
    program = compiler.compile(text, lang);
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