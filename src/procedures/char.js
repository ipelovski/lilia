'use strict';

var common = require('../common');
var types = require('../types');

var guardArgsCountExact = common.guardArgsCountExact;
var SchemeChar = types.SchemeChar;

var charProcedures = {
  'char?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof SchemeChar;
  },
};

module.exports = charProcedures;