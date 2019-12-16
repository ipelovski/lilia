'use strict';

var common = require('../common');
var types = require('../types');
var stringProcedures = require('./string');

var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgPredicate = common.guardArgPredicate;

var symbolProcedures = {
  'symbol?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof Symbol;
  },
  'symbol->string': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], symbolProcedures['symbol?'], 0, 'procedures', 'symbol?');
    return new SchemeString(args[0].valueOf(), true);
  },
  'string->symbol': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');
    return new Symbol(args[0].valueOf());
  },
};

module.exports = symbolProcedures;