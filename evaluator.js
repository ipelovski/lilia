/*
A simple evaluator based on the grammer of r7rs small.
Evaluates programming code by transforming it
to an abstract syntax tree and then executs it.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['lexer', 'parser', 'langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lexer'), require('./parser'), require('./lang-table'));
  } else {
    root.evaluator = factory(root.lexer, root.parser, root.langTable);
  }
}(this, function (lexer, parser, langTable) {
  'use strict';
  
  var FormTypes = parser.FormTypes;
  var OPTypes = parser.OPTypes;
  var TokenTypes = lexer.TokenTypes;
  var langName = '@-1Ang-@';

  function Environment(parent) {
    this.parent = parent || null;
    this.varNames = [];
    this.varValues = [];
    this.expressionStack = [];
    this.opIndex = 0;
    this.ops = null;
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
    return env;
  };
  function cloneEnvs(envs) {
    return envs.map(function (env) {
      return env.clone();
    });
  }
  var outputPort;
  function OutputPort(fn) {
    this.fn = fn;
  }
  OutputPort.prototype.emit = function emit(data) {
    this.fn(data);
  };
  function setOutputPortHandler(fn) {
    outputPort = new OutputPort(fn);
  }  
  function Procedure(args, body, env) {
    this.args = args;
    this.body = body;
    this.env = env;
  }
  Procedure.prototype.toString = function toString() {
    return '#<procedure>';
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
  var procedureTypes = [Procedure, PrimitiveProcedure, ContinuationProcedure, Continuation];
  function isProcedure(arg) {
    return procedureTypes.some(function (type) {
      return arg instanceof type;
    });
  }

  function Symbol(value) {
    this.value = value;
  }
  Symbol.prototype.valueOf = function valueOf() {
    return this.value;
  };
  Symbol.prototype.toString = function valueOf() {
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
    return this.value;
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
  var EmptyList = Object.create(null);
  EmptyList.toString = function toString() {
    return "'()";
  };
  var Unspecified = Object.create(null);
  Unspecified.toString = function toString() {
    return '';
  };
  function createList(args, env, immutable) {
    var pair = EmptyList;
    for (var i = args.length - 1; i >= 0; i--) {
      pair = new Pair(args[i], pair, immutable);
    }
    return pair;
  }
  function isProperList(list) {
    while (list instanceof Pair) {
      list = list.cdr;      
    }
    return list === EmptyList;
  }
  function createVector(args, env, immutable) {
    return new Vector(args, immutable);
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
  function guardNumbers(env, args) {
    for (var i = 0; i < args.length; i++) {
      if (typeof args[i] !== 'number') {
        raiseRuntimeError(env, 'number_expected', [typeof args[i]]);
      }
    }
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
  function guardArgPredicate(env, arg, predicate, argPosition, predicateCat, predicateKey) {
    if (!predicate([arg], env)) {
      var lang = env.getVar(langName);
      var predicateName = langTable.get(lang, predicateCat, predicateKey);
      raiseRuntimeError(env, 'argument_predicate_false', [argPosition, predicateName])
    }
  }
  function guardImmutable(env, arg) {
    if (arg.immutable) {
      raiseRuntimeError(env, 'mutating_immutable_object');
    }
  }
  var primitiveFunctions = {
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
    'boolean?': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return typeof args[0] === 'boolean';
    },
    'not': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return args[0] === false;
    },
    'cons': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      return new Pair(args[0], args[1]);
    },
    'pair?': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return args[0] instanceof Pair;
    },
    'car': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?');
      return args[0].car;
    },
    'cdr': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?');
      return args[0].cdr;
    },
    'set-car!': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?');
      guardImmutable(env, args[0]);
      args[0].car = args[1];
      return Unspecified;
    },
    'set-cdr!': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?');
      guardImmutable(env, args[0]);
      args[0].cdr = args[1];
      return Unspecified;
    },
    'null?': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return args[0] === EmptyList;
    },
    'list?': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return isProperList(args[0]);
    },
    'list': createList,
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
        guardArgPredicate(env, args[i], primitiveFunctions['list?'], i, 'procedures', 'list?');
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
    'vector': createVector,
    'vector?': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      return args[0] instanceof Vector;
    },
    'vector-length': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      guardArgPredicate(env, args[0], primitiveFunctions['vector?'], 0, 'procedures', 'vector?');
      return args[0].value.length;
    },
    'vector-ref': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      var k = args[1];
      guardArgPredicate(env, args[0], primitiveFunctions['vector?'], 0, 'procedures', 'vector?');
      guardArgPredicate(env, k, primitiveFunctions['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
      var arr = args[0].value;
      if (k > arr.length - 1) {
        raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
      }
      return arr[k];
    },
    'vector-set!': function (args, env) {
      guardArgsCountExact(env, args.length, 3);
      var k = args[1];
      guardArgPredicate(env, args[0], primitiveFunctions['vector?'], 0, 'procedures', 'vector?');
      guardArgPredicate(env, k, primitiveFunctions['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
      guardImmutable(env, args[0]);
      var arr = args[0].value;
      if (k > arr.length - 1) {
        raiseRuntimeError(env, 'vector_index_out_range', [arr.length]);
      }
      arr[k] = args[2];
      return Unspecified;
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
  };
  function addPrimitivesAndLang(env, lang) {
    env.addVar(langName, lang);
    for (var name in primitiveFunctions) {
      var translatedName = langTable.get(lang, 'procedures', name);
      env.addVar(translatedName,
        new PrimitiveProcedure(primitiveFunctions[name], translatedName));
    }
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
      procEnv.addVar(formals, createList(actualArgs));
    }
    else if (formals[formals.length - 2] === '.') {
      if (formals.length -1 > actualArgs.length) {
        raiseRuntimeError(procEnv, 'min_args_count_expected', [formals.length, actualArgs.length]);
      }
      for (i = 0; i < formals.length - 2; i++) {
        procEnv.addVar(formals[i], actualArgs[i]);
      }
      procEnv.addVar(formals[formals.length - 1], createList(actualArgs.slice(formals.length - 2)));
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
    return createVector(args, env, true);
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
    var program = parser.parse(text, lang);
    var env = new Environment();
    addPrimitivesAndLang(env, lang);
    return evalOPs(program, env);
  }
  function initEval(lang) {
    lang = lang || 'en';
    var env = new Environment();
    addPrimitivesAndLang(env, lang);
    return function evalFragment(text) {
      var fragment = parser.parse(text, lang);
      return evalOPs(fragment, env);
    };
  }
  return {
    evaluate: evaluate,
    initEval: initEval,
    setOutputPortHandler: setOutputPortHandler,
  };
}));