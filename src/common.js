/*
A module containing common functions.
*/
// Formats a string similar to the .net String.Format() method.
var langTable = require('./lang-table');

var langName = '@-1Ang-@';

if (!String.format) {
  String.format = function(format, args) {
    if (!Array.isArray(args)) {
      args = Array.prototype.slice.call(arguments, 1);
    }
    return format.replace(/{(\d+)}/g, function(match, number) { 
      return typeof args[number] !== 'undefined'
        ? args[number] 
        : match;
    });
  };
}
function guardArgsCountExact(env, argsCount, expected) {
  if (argsCount !== expected) {
    raiseRuntimeError(env, 'exact_args_count_expected', [expected, argsCount]);
  }
}
function guardArgsCountMin(env, argsCount, expected) {
  if (argsCount < expected) {
    raiseRuntimeError(env, 'min_args_count_expected', [expected, argsCount]);
  }
}
function guardArgPredicate(env, arg, predicate, argPosition, predicateCat, predicateKey) {
  if (!predicate([arg], env)) {
    var lang = env.getVar(langName);
    var predicateName = langTable.get(lang, predicateCat, predicateKey);
    raiseRuntimeError(env, 'argument_predicate_false', [argPosition, predicateName])
  }
}
function guardImmutable(env, arg) {
  if (arg.immutable) {
    raiseRuntimeError(env, 'mutating_immutable_object');
  }
}
function cloneEnvs(envs) {
  return envs.map(function (env) {
    return env.clone();
  });
}
// Raises an error by throwing it.
// This function is used only in the parser.
function raiseSyntaxError(parsingInfo, messageKey, messageParams) {
  var message = langTable.get(parsingInfo.lang, 'syntax-errors', messageKey);
  if (messageParams) {
    message = String.format(message, messageParams);
  }
  var err = new Error(message);
  var position = parsingInfo.tokenStream.getPosition();
  err.line = position.line;
  err.column = position.column;
  err.toString = function() {
    return Error.prototype.toString.call(this) +
      ' Line: ' + this.line + ', column: ' + this.column + '.';
  };
  throw err;
}
function raiseRuntimeError(env, messageKey, messageParams) {
  var lang = env.getVar(langName);
  var message = langTable.get(lang, 'runtime-errors', messageKey);
  if (messageParams) {
    message = String.format(message, messageParams);
  }
  var err = new Error(message);
  throw err;
}
module.exports = {
  langName: langName,
  guardArgsCountExact: guardArgsCountExact,
  guardArgsCountMin: guardArgsCountMin,
  guardArgPredicate: guardArgPredicate,
  guardImmutable: guardImmutable,
  cloneEnvs: cloneEnvs,
  raiseSyntaxError: raiseSyntaxError,
  raiseRuntimeError: raiseRuntimeError,
};