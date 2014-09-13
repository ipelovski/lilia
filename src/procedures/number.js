var common = require('../common');
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var raiseRuntimeError = common.raiseRuntimeError;

function guardNumbers(env, args) {
  for (var i = 0; i < args.length; i++) {
    if (typeof args[i] !== 'number') {
      raiseRuntimeError(env, 'number_expected', [typeof args[i]]);
    }
  }
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
};

module.exports = numberProcedures;