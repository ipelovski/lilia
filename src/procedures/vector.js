'use strict';

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
    var at = args[1];
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
    if (to.length - at < end - start) {
      raiseRuntimeError(env, 'vector_copy_space_needed', [end - start, to.length - at]);
    }
    if (to === from && at === start) {
      return;
    }
    var i, j;
    if (at < start) {
      for (i = start, j = at; i < end; i++, j++) {
        to[j] = from[i];
      }
    }
    else { // at >= start
      for (i = end - 1, j = at + end - start - 1; i >= start; i--, j--) {
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