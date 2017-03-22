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
var guardType = common.guardType;
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

var ffiProcedures = ffi.procedures;
var convert = ffi.convert;

var outputPort;
function setOutputPortHandler(fn) {
  outputPort = new OutputPort(fn);
}

function SchemeError(lang, message) {
  this.name = langTable.get(lang, 'common', 'error');
  this.message = message || '';
  this.stack = null;
}
SchemeError.prototype.toString = function toString() {
  var res = this.name + ': ';
  if (this.message) {
    res += this.message + '\n';
  }
  res += this.stack;
  return res;
};

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
    return new SchemeError(env.getVar(langName), args[0].toString());
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
function evalOPAdd(op, env) {
  var value = env.expressionStack.pop();
  var identifier = op.id;
  env.addVar(identifier, value);
  setProcedureName(value, identifier);
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOPSet(op, env) {
  var value = env.expressionStack.pop();
  var identifier = op.id;
  env.setVar(identifier, value);
  setProcedureName(value, identifier);
  env.expressionStack.push(Unspecified);
  return -1;
}
function evalOPGet(op, env) {
  var value = env.getVar(op.id);
  env.expressionStack.push(value);
  return -1;
}
function evalOPLiteral(op, env) {
  var value = op.value;
  if (value === null) {
    value = Unspecified;
  } else if (value.type === 'lambda') {
    var lambda = value.value;
    value = new Procedure(lambda.args, lambda.body, env, lambda.name);
  }
  env.expressionStack.push(value);
  return -1;
}
function evalOPJumpIfFalse(op, env) {
  var value = env.expressionStack.pop();
  if (value === false) {
    return op.index;
  }
  else {
    return -1;
  }
}
function evalOPPop(op, env) {
  var count = op.count;
  if (count > 0) {
    var value = env.expressionStack.pop();
    while (count > 0) {
      env.expressionStack.pop();
      count --;
    }
    env.expressionStack.push(value);
  } else if (count < 0) {
    while (count < 0) {
      env.expressionStack.pop();
      count += 1;
    }
  }
  return -1;
}
function evalOPCopy(op, env) {
  var value = peek(env.expressionStack);
  if (value === undefined) {
    value = Unspecified;
  }
  env.expressionStack.push(value);
  return -1;
}
function evalOP(op, env) {
  switch (op.type) {
    case OPTypes.add:
      return evalOPAdd(op, env);
    case OPTypes.set:
      return evalOPSet(op, env);
    case OPTypes.get:
      return evalOPGet(op, env);
    case OPTypes.literal:
      return evalOPLiteral(op, env);
    case OPTypes.jump:
      return op.index;
    case OPTypes.jumpIfFalse:
      return evalOPJumpIfFalse(op, env);
    case OPTypes.pop:
      return evalOPPop(op, env);
    case OPTypes.copy:
      return evalOPCopy(op, env);
  }
}
function getCodeLocation(env) {
  if (env.ops) { // if there is compiled code
    var procedureCall = env.ops[env.opIndex];
    var position = procedureCall.position;
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
        procedureName = procedure.name || langTable.get(env.getVar(langName), 'common', 'anonymous');
      }
    }
    else { // global env
      procedureName = langTable.get(env.getVar(langName), 'common', 'global');
    }
    stackInfo.push(procedureName + location);
  }
  return stackInfo.join('\n');
}
function evalOPs(ops, env) {
  var idx, op, value;
  var i = 0;
  var envs = [env];
  var maxEnvCount = 1000;
  env.ops = ops; // TODO move it
  var lang = env.getVar(langName);

  main:
  while (true) {
    
    if (i === ops.length) { // the procedure ends
      if (envs.length === 1) { // all code is evaluated, including the global - the first environment
        break;
      } else {
        value = env.expressionStack.pop(); // return value of procedure
        envs.pop();
        env = peek(envs);
        ops = env.ops;
        env.opIndex += 1;
        i = env.opIndex;
        env.expressionStack.push(value);
        continue;
      }
    }

    op = ops[i];
    if (op.type === OPTypes.call ||
        op.type === OPTypes.tailcall) {
      applyProcedure();
    } else {
      idx = evalOP(op, env);
    }
    if (idx !== -1) {
      i = idx;
    } else {
      i += 1;
    }
    env.opIndex = i;
  }
  return env.expressionStack.pop();
  
  function applyProcedure() {
    if (envs.length >= maxEnvCount) {
      raiseRuntimeError(env, 'maximum_stack_size_exceeded');
    }
    var procedure = env.expressionStack.pop();
    
    // TODO this takes too much time
    guardType(env, procedure, primitiveFunctions['procedure?'], 'invalid_proc_call');
    
    // TODO value.stack = getStack(envs);
    var argsCount = op.argsCount;
    var actualArgs = new Array(argsCount);
    for (var a = argsCount - 1; a >= 0; a--) {
      actualArgs[a] = env.expressionStack.pop();
    }

    if (procedure instanceof PrimitiveProcedure) {
      applyPrimitiveProcedure(procedure, actualArgs);
    }
    else if (procedure instanceof Procedure) {
      applyUserProcedure(procedure, actualArgs);
    }
    else if (procedure instanceof Application) {
      applyApplication(procedure, actualArgs);
    }
    else if (procedure instanceof ContinuationProcedure) {
      applyContinuationProcedure(procedure, actualArgs);
    }
    else if (procedure instanceof Continuation) {
      applyContinuation(procedure, actualArgs);
    }
  }

  function applyUserProcedure(procedure, actualArgs) {
    var formals = procedure.args;
    env = new Environment(procedure.env);
    if (op.type === OPTypes.tailcall) {
      envs.pop();
    }
    applyArguments(formals, actualArgs, env);
    envs.push(env);
    env.procedure = procedure;
    ops = env.ops = procedure.body;
    idx = 0;
  }
  
  function createProcedureEnv(procedure) {
    env = new Environment(envs[0]);
    envs.push(env);
    env.procedure = procedure;
  }
  
  function applyPrimitiveProcedure(procedure, actualArgs) {
    createProcedureEnv(procedure);
    var value = procedure.execute(actualArgs, env);
    if (value instanceof SchemeError) {
      value.stack = getStack(envs);
      throw value;
    }
    envs.pop();
    env = peek(envs);
    env.expressionStack.push(value);
    idx = -1;
  }
  
  function applyApplication(procedure, actualArgs) {
    createProcedureEnv(procedure);
    procedure = actualArgs[0];
    guardArgPredicate(env, procedure, primitiveFunctions['procedure?'], 0, 'procedures', 'procedure?');
    var applicationArgs = [];
    if (actualArgs.length > 1) {
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
    }
    applyProcedure(procedure, applicationArgs);
    envs.pop();
    env = peek(envs);
  }
  
  function applyContinuationProcedure(procedure, actualArgs) {
    createProcedureEnv(procedure);
    var value = procedure.execute(actualArgs, env, envs);
    env = peek(envs);
    // TODO the passed lambda should accept only one argument
    procedure = actualArgs[0];
    applyUserProcedure(procedure, [value]);
  }
  
  function applyContinuation(procedure, actualArgs) {
    createProcedureEnv(procedure);
    var value = procedure.execute(actualArgs, env);
    // just clear the old env
    envs.pop();
    env = peek(envs);

    envs = value.envs;
    env = peek(envs);
    env.expressionStack.push(value.value);
    ops = env.ops;
    idx = env.opIndex + 1;
  }
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
    return new SchemeError(lang, e.message);
  }
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  var result;
  try {
    result = evalOPs(program, env);
  }
  catch (e) {
    if (e instanceof SchemeError) {
      return e;
    } else {
      return new SchemeError(lang, e.message);
    }
  }
  return new Result(result, env);
}
function session(lang) {
  lang = lang || 'en';
  var env = new Environment();
  addPrimitivesAndLang(env, lang);
  return {
    environment: env,
    evaluate: function evalFragment(text) {
      var fragment;
      try {
        fragment = compiler.compile(text, lang);
      }
      catch (e) {
        return new SchemeError(lang, e.message);
      }
      var result;
      try {
        result = evalOPs(fragment, env);
      }
      catch (e) {
        if (e instanceof SchemeError) {
          return e;
        } else {
          return new SchemeError(lang, e.message);
        }
      }
      return new Result(result, env);
    },
  };
}

exports.applySchemeProcedure = applySchemeProcedure;
exports.evaluate = evaluate;
exports.session = session;
exports.setOutputPortHandler = setOutputPortHandler;