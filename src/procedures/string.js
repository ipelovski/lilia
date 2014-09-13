var common = require('../common');
var types = require('../types');

var SchemeString = types.SchemeString;
var guardArgsCountExact = common.guardArgsCountExact;

var stringProcedures = {
  'string?': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    return args[0] instanceof SchemeString;
  },
};

module.exports = stringProcedures;