'use strict';

var common = require('../common');
var types = require('../types');
var numberProcedures = require('./number');
var equivalenceProcedures = require('./equivalence');

var guardArgsCountExact = common.guardArgsCountExact;
var guardArgsCountMin = common.guardArgsCountMin;
var guardArgsCountMax = common.guardArgsCountMax;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;
var raiseRuntimeError = common.raiseRuntimeError;

var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

var isEq = equivalenceProcedures['eq?'];
var isEqv = equivalenceProcedures['eqv?'];
var isEqual = equivalenceProcedures['equal?'];

function car(args, env) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
  return args[0].car;
}
function cdr(args, env) {
  guardArgsCountExact(env, args.length, 1);
  guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
  return args[0].cdr;
}
function findMember(compare) {
  return function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[1], pairListProcedures['pair?'], 1, 'procedures', 'pair?');
    var obj = args[0];
    var pair = args[1];
    while (pair instanceof Pair) {
      if (compare([pair.car, obj], env)) {
        return pair;
      }
      pair = pair.cdr;
    }
    if (pair === EmptyList) {
      return false;
    }
    else {
      raiseRuntimeError(env, 'improper_list');
    }
  };
}
function findPair(compare) {
  return function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[1], pairListProcedures['pair?'], 1, 'procedures', 'pair?');
    var obj = args[0];
    var pair = args[1];
    var element;
    while (pair instanceof Pair) {
      element = pair.car;
      if (!pairListProcedures['pair?']([element], env)) {
        raiseRuntimeError(env, 'improper_alist');
      }
      if (compare([element.car, obj], env)) {
        return element;
      }
      pair = pair.cdr;
    }
    if (pair === EmptyList) {
      return false;
    }
    else {
      raiseRuntimeError(env, 'improper_list');
    }
  };
}
var pairListProcedures = {
  'cons': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    return new Pair(args[0], args[1]);
  },
  'pair?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof Pair;
  },
  'car': car,
  'cdr': cdr,
  'set-car!': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardImmutable(env, args[0]);
    args[0].car = args[1];
    return Unspecified;
  },
  'set-cdr!': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardImmutable(env, args[0]);
    args[0].cdr = args[1];
    return Unspecified;
  },
  'caar': function (args, env) {
    return car([car(args, env)], env);
  },
  'cadr': function (args, env) {
    return car([cdr(args, env)], env);
  },
  'cdar': function (args, env) {
    return cdr([car(args, env)], env);
  },
  'cddr': function (args, env) {
    return cdr([cdr(args, env)], env);
  },
  'null?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] === EmptyList;
  },
  'list?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return Pair.isProperList(args[0]);
  },
  'list': Pair.createList,
  'make-list': function (args, env) {
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
    return Pair.createList(arr, env);
  },
  'length': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var list = args[0];
    if (list === EmptyList) {
      return 0;
    }
    var length = 0;
    while (list instanceof Pair) {
      length += 1;
      list = list.cdr;      
    }
    if (list === EmptyList) {
      return length;
    }
    else {
      raiseRuntimeError(env, 'argument_predicate_false', [0, 'list?']);
    }
  },
  'list-ref': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      return pair.car;
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
  },
  'list-set!': function (args, env) {
    guardArgsCountExact(env, args.length, 3);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];    
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      guardImmutable(env, pair);
      pair.car = args[2];
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
    return Unspecified;
  },
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
      guardArgPredicate(env, args[i], pairListProcedures['list?'], i, 'procedures', 'list?');
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
  'reverse': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var list = args[0];
    if (list === EmptyList) {
      return EmptyList;
    }
    var newList = EmptyList;
    while (list instanceof Pair) {
      newList = new Pair(list.car, newList);
      list = list.cdr;      
    }
    if (list === EmptyList) {
      return newList;
    }
    else {
      raiseRuntimeError(env, 'argument_predicate_false', [0, 'list?']);
    }
  },
  'list-tail': function (args, env) {
    guardArgsCountExact(env, args.length, 2);
    var k = args[1];
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    guardArgPredicate(env, k, numberProcedures['nonnegative-integer?'], 1, 'procedures', 'nonnegative-integer?');
    var pair = args[0];
    var idx = 0;
    while (pair instanceof Pair && idx < k) {
      idx += 1;
      pair = pair.cdr;
    }
    if (idx === k && pair instanceof Pair) {
      return pair;
    }
    else {
      var errorMessage = pair === EmptyList ? 'list_index_out_range' : 'list_index_reached_non_pair';
      raiseRuntimeError(env, errorMessage, [idx]);
    }
  },
  'memq': findMember(equivalenceProcedures['eq?']),
  'memv': findMember(equivalenceProcedures['eqv?']),
  'member': findMember(equivalenceProcedures['equal?']), // TODO implement (member obj list compare)
  'assq': findPair(equivalenceProcedures['eq?']),
  'assv': findPair(equivalenceProcedures['eqv?']),
  'assoc': findPair(equivalenceProcedures['equal?']), // TODO implement (assoc obj alist compare)
  'list-copy': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    var obj = args[0];
    if (!(obj instanceof Pair)) {
      return obj;
    }
    var pair, nextPair, list;
    list = pair = nextPair = new Pair(obj.car, obj.cdr);
    obj = obj.cdr;
    while (obj instanceof Pair) {
      nextPair = new Pair(obj.car, obj.cdr);
      if (pair) {
        pair.cdr = nextPair;
      }
      pair = nextPair;
      obj = obj.cdr;
    }
    return list;
  },
};

module.exports = pairListProcedures;