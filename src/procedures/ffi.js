'use strict';

var types = require('../types');
var evaluator = require('../evaluator');
var common = require('../common');
var stringProcedures = require('./string');

var Environment = types.Environment;
var Symbol = types.Symbol;
var SchemeString = types.SchemeString;
var SchemeChar = types.SchemeChar;
var Vector = types.Vector;
var Pair = types.Pair;
var guardArgsCountExact = common.guardArgsCountExact;
var guardArgPredicate = common.guardArgPredicate;
var applySchemeProcedure = evaluator.applySchemeProcedure;

function convertSchemeObjectToJs(obj, env) {
  if (typeof obj === 'number'
    || typeof obj === 'boolean') {
    return obj;
  }
  if (obj instanceof SchemeString
    || obj instanceof SchemeChar
    || obj instanceof Symbol) {
    return obj.value;
  }
  if (obj instanceof Vector) {
    return obj.value.map(function (item) {
      return convertSchemeObjectToJs(item);
    });
  }
  if (obj instanceof Pair) {
    if (isProperList(obj)) {
      var isDict = obj.every(function (item) {
        return item instanceof Pair;
      });
      if (isDict) {
        return convertSchemeDictToJsObject(obj);
      }
      else {
        return convertSchemeObjectToJs(listToVector(obj));
      }
    }
  }
  if (obj instanceof PrimitiveProcedure) {
    return function () {
      var args = Array.prototype.map.apply(arguments, function (arg) {
        return convertSchemeObjectToJs(arg, env);
      });
      obj.fn(args, env);
    }
  }
  if (obj instanceof Procedure) {
    return function () {
      var args = Array.prototype.map.apply(arguments, function (arg) {
        return convertSchemeObjectToJs(arg, env);
      });
      var value = applySchemeProcedure(obj, args);
      return convertSchemeObjectToJs(value);
    };
  }
}
function convertJsObjectToDcit(obj) {
  var pairs = Object.keys(obj).map(function (prop) {
    return new Pair(convertJsObjectToScheme(prop), convertJsObjectToScheme(obj[prop]));
  });
  return Pair.createList(pairs);
}
function convertJsObjectToScheme(obj) {
  if (typeof obj === 'number'
    || typeof obj === 'boolean') {
    return obj;
  }
  if (typeof obj === 'string') {
    return new SchemeString(obj);
  }
  if (typeof obj === 'function') {
    return new PrimitiveProcedure(function (args, env) {
      var value = obj.apply(null, args.map(function (arg) {
        return convertSchemeObjectToJs(arg);
      }))
      return convertJsObjectToScheme(value);
    })
  }
  if (typeof obj === 'object' && obj !== null) {
    if (Array.isArray(obj)) {
      return new Vector(obj, false);
    }
    if (obj instanceof Date) {
      return new SchemeString(obj.toString());
    }
    if (typeof obj.constructor === 'undefined'
      || obj.constructor.name === 'Object') {
      return convertJsObjectToDcit(obj);
    }
  }
  return Unspecified;
}
function convertSchemeDictToJsObject(dict) {
  var obj = {};
  var list = dict, pair;
  while (list instanceof Pair) {
    pair = list.car;
    obj[pair.car.toString()] = convertSchemeObjectToJs(pair.cdr);
    list = list.cdr;      
  }
  return obj;
}
function listToVector(list) {
  var arr = [];
  while (list instanceof Pair) {
    arr.push(list.car);
    list = list.cdr;
  }
  return new Vector(arr);
}
var ffiProcedures = {
  'js-eval': function (args, env) {
    guardArgsCountExact(env, args.length, 1);
    guardArgPredicate(env, args[0], stringProcedures['string?'], 0, 'procedures', 'string?');      
    var value = eval(args[0].value);
    return convertJsObjectToScheme(value);
  },
};

module.exports = ffiProcedures;