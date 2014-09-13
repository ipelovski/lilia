var common = require('../common');
var types = require('../types');
var Vector = types.Vector;
var Unspecified = types.Unspecified;
var numberProcedures = require('./number');
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;
var raiseRuntimeError = common.raiseRuntimeError;

var vectorProcedures = {
  'vector': Vector.create,
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
};

module.exports = vectorProcedures;