'use strict';

var common = require('../common');
var types = require('../types');

var guardArgsCountExact = common.guardArgsCountExact;
var Procedure = types.Procedure;
var PrimitiveProcedure = types.PrimitiveProcedure;
var Application = types.Application;
var ContinuationProcedure = types.ContinuationProcedure;
var Continuation = types.Continuation;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Vector = types.Vector;
var Pair = types.Pair;
var EmptyList = types.EmptyList;
var Unspecified = types.Unspecified;

function haveConstructor(args, constructor) {
  return args[0] instanceof constructor && args[1] instanceof constructor;
}
function isEquivalent(args, env) {
  guardArgsCountExact(env, args.length, 2);
  var obj1 = args[0];
  var obj2 = args[1];
  if (typeof obj1 === 'boolean' && typeof obj2 === 'boolean') {
    return obj1 === obj2;
  }
  if (typeof obj1 === 'number' && typeof obj2 === 'number') {
    return obj1 === obj2;
  }
  if (haveConstructor(args, SchemeChar)) {
    return obj1.value === obj2.value;
  }
  if (haveConstructor(args, Symbol)) {
    return obj1.value === obj2.value;
  }
  if (obj1 === EmptyList && obj2 === EmptyList) {
    return true;
  }
  if (obj1 === Unspecified && obj2 === Unspecified) {
    return true;
  }
  // TODO bytevectors, records, ports, promises
  if (haveConstructor(args, Pair)
    || haveConstructor(args, Vector)
    || haveConstructor(args, SchemeString)
    // TODO not sure for the procedures
    || haveConstructor(args, PrimitiveProcedure)
    || haveConstructor(args, Procedure)
    || haveConstructor(args, ContinuationProcedure)
    || haveConstructor(args, Continuation)
    || haveConstructor(args, Application)) {
    return obj1 === obj2;
  }
  return false;
}
function isEqual(args, env) {
  guardArgsCountExact(env, args.length, 2);
  var eqv = isEquivalent(args, env);
  if (eqv) {
    return true;
  }
  // TODO bytevectors
  if (haveConstructor(args, Pair)) {
    var pair1 = args[0];
    var pair2 = args[0];
    while (pair1 instanceof Pair && pair2 instanceof Pair) {
      if (!isEqual([pair1.car, pair2.car], env)) {
        return false;
      }
      pair1 = pair1.cdr;
      pair2 = pair2.cdr;
    }
    return pair1 === EmptyList && pair2 === EmptyList;
  }
  if (haveConstructor(args, Vector)) {
    var array1 = args[0].value;
    var array2 = args[1].value;
    if (array1.length !== array2.length) {
      return false;
    }
    for (var i = 0; i < array1.length; i++) {
      if (!isEqual([array1[i], array2[i]], env)) {
        return false;
      }
    }
    return true;
  }
  if (haveConstructor(args, SchemeString)) {
    return args[0].value === args[1].value;
  }
  return false;
}
var equivalenceProcedures = {
  'eq?': isEquivalent,
  'eqv?': isEquivalent,
  'equal?': isEqual,
};

module.exports = equivalenceProcedures;