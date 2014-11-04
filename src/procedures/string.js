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
var raiseRuntimeError = common.raiseRuntimeError;

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
    var i;
    for (i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], charProcedures['char?'], i, 'procedures', 'char?');
    }
    var arr = new Array(args.length);
    for (i = 0; i < arr.length; i++) {
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
    var i, l;
    for (i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], stringProcedures['string?'], i, 'procedures', 'string?');
    }
    for (i = 0, l = args.length - 1; i < l; i++) {
      if (args[i].value !== args[i + 1].value) {
        return false;
      }
    }
    return true;
  },
  'string-append': function (args, env) {
    var i;
    for (i = 0; i < args.length; i++) {
      guardArgPredicate(env, args[i], stringProcedures['string?'], i, 'procedures', 'string?');
    }
    var arr = [];
    for (i = 0; i < args.length; i++) {
      arr.push(args[i].value);
    }
    return new SchemeString(arr.join(''));
  },
  'substring': function (args, env) {
    guardArgsCountExact(env, args.length, 3);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    guardArgPredicate(env, args[1], numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    guardArgPredicate(env, args[2], numberProcedures['nonnegative-integer?'], 2, 'procedures', 'nonnegative-integer?');
    var string = args[0].value;
    var start = args[1];
    var end = args[2];
    if (start > string.length - 1) {
      raiseRuntimeError(env, 'string_index_out_range', [string.length]);
    }
    if (end > string.length) {
      raiseRuntimeError(env, 'string_index_out_range', [string.length]);
    }
    if (start > end) {
      raiseRuntimeError(env, 'start_index_greater_end_index');
    }
    return new SchemeString(string.substring(start, end));
  },
};

module.exports = stringProcedures;