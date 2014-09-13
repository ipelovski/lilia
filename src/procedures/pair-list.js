var common = require('../common');
var types = require('../types');

var guardArgsCountExact = common.guardArgsCountExact;
var guardArgPredicate = common.guardArgPredicate;
var guardImmutable = common.guardImmutable;

var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

var pairListProcedures = {
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
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    return args[0].car;
  },
  'cdr': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], pairListProcedures['pair?'], 0, 'procedures', 'pair?');
    return args[0].cdr;
  },
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
  'null?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] === EmptyList;
  },
  'list?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return Pair.isProperList(args[0]);
  },
  'list': Pair.createList,
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
};

module.exports = pairListProcedures;