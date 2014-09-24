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