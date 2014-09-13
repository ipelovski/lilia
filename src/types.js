var common = require('./common');
var guardArgsCountExact = common.guardArgsCountExact;
var cloneEnvs = common.cloneEnvs;
var raiseRuntimeError = common.raiseRuntimeError;

function Environment(parent) {
  this.parent = parent || null;
  this.varNames = [];
  this.varValues = [];
  this.expressionStack = [];
  this.opIndex = 0;
  this.ops = null;
}
Environment.prototype.addVar = function addVar(name, value) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    this.varNames.push(name);
    this.varValues.push(value);
  }
  else {
    this.varValues[idx] = value;
  }
};
Environment.prototype.setVar = function setVar(name, value) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    if (this.parent !== null) {
      this.parent.setVar(name, value);
    }
    else {
      raiseRuntimeError(this, 'undefined_variable', [name]);
    }
  }
  else {
    this.varValues[idx] = value;
  }
};
Environment.prototype.getVar = function getVar(name) {
  var idx = this.varNames.indexOf(name);
  if (idx === -1) {
    if (this.parent !== null) {
      return this.parent.getVar(name);
    }
    else {
      raiseRuntimeError(this, 'undefined_variable', [name]);
    }
  }
  else {
    return this.varValues[idx];
  }
};
Environment.prototype.clone = function clone() {
  var env = new Environment(this.parent);
  env.varNames = this.varNames;
  env.varValues = this.varValues;
  env.expressionStack = this.expressionStack.slice();
  env.opIndex = this.opIndex;
  env.ops = this.ops;
  return env;
};
function OutputPort(fn) {
  this.fn = fn;
}
OutputPort.prototype.emit = function emit(data) {
  this.fn(data);
};
function Procedure(args, body, env) {
  this.args = args;
  this.body = body;
  this.env = env;
}
Procedure.prototype.toString = function toString() {
  return '#<procedure>';
};
function PrimitiveProcedure(fn, name) {
  this.fn = fn;
  this.name = name || '';
}
PrimitiveProcedure.prototype.execute = function execute(args, env) {
  return this.fn(args, env);
};
PrimitiveProcedure.prototype.toString = function toString() {
  return '#<procedure ' + this.name + '>';
};
function ContinuationProcedure(fn, name) {
  this.fn = fn;
  this.name = name || '';
}
ContinuationProcedure.prototype.execute = function execute(args, env, envs) {
  return this.fn(args, env, envs);
};
ContinuationProcedure.prototype.toString = PrimitiveProcedure.prototype.toString;
function Continuation(envs) {
  this.envs = envs;
}
Continuation.prototype.execute = function execute(args, env) {
  guardArgsCountExact(env, args.length, 1);
  return {
    envs: cloneEnvs(this.envs),
    value: args[0]
  };
};
Continuation.prototype.toString = Procedure.prototype.toString;
function Symbol(value) {
  this.value = value;
}
Symbol.prototype.valueOf = function valueOf() {
  return this.value;
};
Symbol.prototype.toString = function valueOf() {
  return this.value;
};
function SchemeString(value, immutable) {
  this.value = value;
  this.immutable = !!immutable;
}
SchemeString.prototype.valueOf = function valueOf() {
  return this.value;
};
SchemeString.prototype.toString = function toString() {
  return '"' + this.value + '"';
};
function Vector(items, immutable) {
  this.value = null;
  this.immutable = !!immutable;
  if (Array.isArray(items)) {
    this.value = items;
  }
  else {
    this.value = new Array(length);  
  }
}
Vector.prototype.valueOf = function valueOf() {
  return this.value;
};
Vector.prototype.toString = function toString() {
  return '#(' + this.value.join(' ') + ')';
};
Vector.create = function createVector(args, env, immutable) {
  return new Vector(args, immutable);
};
function Pair(car, cdr, immutable) {
  this.car = car;
  this.cdr = cdr;
  this.immutable = !!immutable;
}
Pair.prototype.toString = function toString() {
  var list = this;
  var str = '(';
  var cdr;
  while (list instanceof Pair) {
    str += list.car.toString();
    cdr = list.cdr;
    if (cdr instanceof Pair) {
      list = cdr;
    }
    else if (cdr === EmptyList) {
      break;
    }
    else {
      str += ' . ' + cdr.toString();
      break;
    }
    str += ' ';
  }
  str += ')';
  return str;
};
Pair.createList = function createList(args, env, immutable) {
  var pair = EmptyList;
  for (var i = args.length - 1; i >= 0; i--) {
    pair = new Pair(args[i], pair, immutable);
  }
  return pair;
};
Pair.isProperList = function isProperList(list) {
  while (list instanceof Pair) {
    list = list.cdr;      
  }
  return list === EmptyList;
};
var EmptyList = Object.create(null);
EmptyList.toString = function toString() {
  return "'()";
};
var Unspecified = Object.create(null);
Unspecified.toString = function toString() {
  return '';
};

module.exports = {
  Environment: Environment,
  OutputPort: OutputPort,
  Procedure: Procedure,
  PrimitiveProcedure: PrimitiveProcedure,
  ContinuationProcedure: ContinuationProcedure,
  Continuation: Continuation,
  Symbol: Symbol,
  SchemeString: SchemeString,
  Vector: Vector,
  Pair: Pair,
  EmptyList: EmptyList,
  Unspecified: Unspecified,
};