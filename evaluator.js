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
  var FormTypes = parser.FormTypes;
  var TokenTypes = lexer.TokenTypes;
  var langName = '@-1Ang-@';

  function Environment(parent) {
    this.parent = parent || null;
    this.varNames = [];
    this.varValues = [];
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
  function Procedure(args, body, env) {
    this.args = args;
    this.body = body;
    this.env = env;
  }
  function PrimitiveProcedure(fn) {
    this.fn = fn;
  }
  PrimitiveProcedure.prototype.execute = function execute(args, env) {
    return this.fn(args, env);
  };
  function Symbol(value) {
    this.value = value;
  }
  Symbol.prototype.valueOf = function valueOf() {
    return this.value;
  };
  function SchemeString(value, immutable) {
    this.value = value;
    this.immutable = !!immutable;
  }
  SchemeString.prototype.valueOf = function valueOf() {
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
  }
  function createList(args, env, immutable) {
    var pair = EmptyList;
    for (var i = args.length - 1; i >= 0; i--) {
      pair = new Pair(args[i], pair, immutable);
    }
    return pair;
  }
  function isProperList(list) {
    var cdr;
    while (list instanceof Pair) {
      cdr = list.cdr;
      if (cdr instanceof Pair) {
        list = cdr;
      }
      else if (cdr === EmptyList) {
        return true;
      }
      else {
        return false;
      }
    }
    return false;
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
      for (i = 0, l = args.length - 1; i < l; i++) {
        if (args[i] !== args[i + 1]) {
          return false;
        }
      }
      return true;
    },
    '<': function (args, env) {
      guardArgsCountMin(env, args.length, 2);
      guardNumbers(env, args);
      for (i = 0, l = args.length - 1; i < l; i++) {
        if (args[i] >= args[i + 1]) {
          return false;
        }
      }
      return true;
    },
    '>': function (args, env) {
      guardArgsCountMin(env, args.length, 2);
      guardNumbers(env, args);
      for (i = 0, l = args.length - 1; i < l; i++) {
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
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?')
      return args[0].car;
    },
    'cdr': function (args, env) {
      guardArgsCountExact(env, args.length, 1);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?')
      return args[0].cdr;
    },
    'set-car!': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?')
      guardImmutable(env, args[0]);
      args[0].car = args[1];
      return Unspecified;
    },
    'set-cdr!': function (args, env) {
      guardArgsCountExact(env, args.length, 2);
      guardArgPredicate(env, args[0], primitiveFunctions['pair?'], 0, 'procedures', 'pair?')
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
    }
  };
  function addPrimitivesAndLang(env, lang) {
    env.addVar(langName, lang);
    for (var name in primitiveFunctions) {
      var translatedName = langTable.get(lang, 'procedures', name);
      env.addVar(translatedName, new PrimitiveProcedure(primitiveFunctions[name]));
    }
  }

  function evalForms(forms, env) {
    var result, procedureCall;
    for (var i = 0; i < forms.length; i++) {
      result = evalFormFully(forms[i], env);
    }
    return result;
  }
  function evalForm(form, env) {
    if (form.type === FormTypes.definition) {
      return evalDefintion(form, env);
    }
    if (form.type === FormTypes.assignment) {
      return evalAssignment(form, env);
    }
    if (form.type === FormTypes.variable) {
      return evalVariable(form, env);
    }
    if (form.type === FormTypes.literal) {
      return evalLiteral(form, env);
    }
    if (form.type === FormTypes.lambda) {
      return evalLambda(form, env);
    }
    if (form.type === FormTypes.conditional) {
      return evalConditional(form, env);
    }
    if (form.type === FormTypes.conjunction) {
      return evalConjunction(form, env);
    }
    if (form.type === FormTypes.disjunction) {
      return evalDisjunction(form, env);
    }
    if (form.type === FormTypes.begin) {
      return evalBegin(form, env);
    }
    if (form.type === FormTypes.procedureCall) {
      return applyProcedure(form, env);
    }
  }
  function evalFormFully(form, env) {
    var value = evalForm(form, env);
    while (value.procedureCall) {
      procedureCall = {
        type: FormTypes.procedureCall,
        procedure: value.procedureCall.procedure,
        arguments: value.procedureCall.arguments
      };
      value = evalForm(procedureCall, value.env);
    }
    return value;
  }
  function evalDefintion(definition, env) {
    env.addVar(definition.identifier, evalForm(definition.value, env));
    return Unspecified;
  }
  function evalAssignment(assignment, env) {
    env.setVar(assignment.identifier, evalForm(assignment.value, env));
    return Unspecified;
  }
  function evalVariable(variable, env) {
    return env.getVar(variable.identifier);
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
  function evalLambda(form, env) {
    return new Procedure(form.formals, form.body, env);
  }
  function evalConditional(form, env) {
    var test = evalForm(form.test, env);
    if (test !== false) {
      return evalForm(form.consequent, env);
    }
    else {
      return evalForm(form.alternate, env);
    }
  }
  function evalConjunction(form, env) {
    var value = true;
    var tests = form.tests;
    for (var i = 0; i < tests.length; i++) {
      value = evalForm(tests[i], env);
      if (value === false) {
        break;
      }
    }
    return value;
  }
  function evalDisjunction(form, env) {
    var value = false;
    var tests = form.tests;
    for (var i = 0; i < tests.length; i++) {
      value = evalForm(tests[i], env);
      if (value !== false) {
        break;
      }
    }
    return value;
  }
  function evalBegin(form, env) {
    return evalBodyForms(form.forms, env);
  }
  function applyProcedure(form, env) {
    if (form.tail) {
      return {
        procedureCall: form,
        env: env
      };
    }
    else {
      return evalProcedure(form, env);
    }
  }
  function evalBodyForms(forms, env) {
    var result, procedureCall;
    for (var i = 0; i < forms.length; i++) {
      result = evalForm(forms[i], env);    
    }
    return result;
  }
  function evalProcedure(form, env) {
    var procedure = evalFormFully(form.procedure, env);
    if (procedure instanceof Procedure ||
        procedure instanceof PrimitiveProcedure) {
      var actualArgs = [];
      var procArgs = form.arguments;
      for (var i = 0; i < procArgs.length; i++) {
        actualArgs.push(evalFormFully(procArgs[i], env));
      }
      if (procedure instanceof Procedure) {
        var formals = procedure.args;
        var procEnv = new Environment(procedure.env);
        if (!Array.isArray(formals)) {
          procEnv.addVar(formals, createList(actualArgs));
        }
        else if (formals[formals.length - 2] === '.') {
          if (formals.length -1 > actualArgs.length) {
            raiseRuntimeError(env, 'min_args_count_expected', [formals.length, actualArgs.length]);
          }
          for (i = 0; i < formals.length - 2; i++) {
            procEnv.addVar(formals[i], actualArgs[i]);
          }
          procEnv.addVar(formals[formals.length - 1], createList(actualArgs.slice(formals.length - 2)));
        }
        else {
          if (formals.length !== actualArgs.length) {
            raiseRuntimeError(env, 'exact_args_count_expected', [formals.length, actualArgs.length]);
          }
          for (i = 0; i < formals.length; i++) {
            procEnv.addVar(formals[i], actualArgs[i]);
          }
        }
        return evalBodyForms(procedure.body, procEnv);
      }
      else {
        return procedure.execute(actualArgs, env);
      }
    }
    else {
      raiseRuntimeError(env, 'invalid_proc');
    }
  }

  function evalProgram(forms, lang) {
    var env = new Environment();
    addPrimitivesAndLang(env, lang);
    return evalForms(forms, env);
  }
  function evaluate(text, lang) {
    lang = lang || 'en';
    var forms = parser.parse(text, lang);
    return evalProgram(forms, lang);
  }
  function initEval(lang) {
    lang = lang || 'en';
    var env = new Environment();
    addPrimitivesAndLang(env, lang);
    return function evalFragment(text) {
      var forms = parser.parse(text, lang);
      return evalForms(forms, env);
    };
  }
  return {
    evaluate: evaluate,
    initEval: initEval
  };
}));