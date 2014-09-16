'use strict';

var lexer = require('./lexer');
var parser = require('./parser');
var ast = require('./ast');
var types = require('./types');

var TokenTypes = lexer.TokenTypes;
var FormTypes = ast.FormTypes;
var Symbol = types.Symbol;
var Pair = types.Pair;
var Vector = types.Vector;
var SchemeString = types.SchemeString;
var EmptyList = types.EmptyList;

var OPTypes = {
  define: 'define',
  set: 'set',
  internaldefine: 'internaldefine',
  constant: 'constant',
  variable: 'variable',
  jump: 'jump',
  jumpiffalse: 'jumpiffalse',
  jumpiffalsekeep: 'jumpiffalsekeep',
  jumpifnotfalsekeep: 'jumpifnotfalsekeep',
  lambda: 'lambda',
  call: 'call',
  tailcall: 'tailcall',
  discard: 'discard',
  void: 'void',
};

function evalListLiteral(listItems) {
  if (listItems.length === 0) {
    return EmptyList;
  }
  var listLength = listItems.length;
  var improperList = listItems[listLength - 2] === '.';
  var count = improperList ? listLength - 4 : listLength - 1;
  var args = [];
  var pair;
  if (improperList) {
    pair = new Pair(evalLiteral(listItems[listLength - 3]),
      evalLiteral(listItems[listLength - 1]), true);
  }
  else {
    pair = EmptyList;
  }
  for (var i = count; i >= 0; i--) {
    pair = new Pair(evalLiteral(listItems[i]), pair, true);
  }
  return pair;
}
function evalVectorLiteral(vectorItems) {
  var count = vectorItems.length;
  var args = new Array(count);
  for (var i = 0; i < count; i++) {
    args[i] = evalLiteral(vectorItems[i]);
  }
  return Vector.create(args, null, true);
}
function evalLiteral(literal) {
  var value = literal.value.value;
  var type = literal.value.type;
  switch (type) {
    case TokenTypes.boolean:
      return value;
    case TokenTypes.number:
      if (value.indexOf('.') === -1) {
        return parseInt(value);
      }
      else {
        return parseFloat(value);
      }
    case TokenTypes.character:
      return value;
    case TokenTypes.string:
      return new SchemeString(value, true);
    case TokenTypes.identifier:
      return new Symbol(value);
    case 'list':
      return evalListLiteral(value);
    case 'vector':
      return evalVectorLiteral(value);
    default:
      throw new Error('Unkown type'); // TODO should raise a more user frienly error, is this code reachable?
  }
}
function createCallOP(opType, form) {
  // form.nodes[0] are the procedure arguments
  // the op argument is the number of procedure arguments
  var args = form.nodes[0];
  var argsCount;
  if (args.nodes) {
    argsCount = args.nodes.length;
  }
  else {
    argsCount = args.value;
  }
  return [opType, argsCount];
}
function analyzeForm(form, idx, conds) {
  var op, argsCount, i, bodyCode;
  switch (form.type) {
    case FormTypes.definition:
      return [OPTypes.define];
    case FormTypes.internalDefinition:
      return [OPTypes.internaldefine, form.value];
    case FormTypes.assignment:
      return [OPTypes.set];
    case FormTypes.literal:
      return [OPTypes.constant, evalLiteral(form)];
    case FormTypes.variable:
      return [OPTypes.variable, form.value];
    case FormTypes.test:
      op = [OPTypes.jumpiffalse, -1];
      conds.push(op);
      return op;
    case FormTypes.ifthen:
      op = conds.pop(); // test op, i.e. jumpiffalse
      // jump to the next op
      // since there is an op for this form
      // the idx is incremented to point to the right op
      op[1] = idx + 1;
      op = [OPTypes.jump, -1];
      conds.push(op);
      return op;
    case FormTypes.ifelse:
      op = conds.pop(); // ifthen op, i.e. jump
      if (!form.nodes) { // the 'if' has no 'else' clause
        op[1] = idx + 1;
        return [OPTypes.void];
      }
      else {
        op[1] = idx; // jump to the next op
        return null;
      }
    case FormTypes.ifexpr:
      return null;
    case FormTypes.cond:
      argsCount = form.nodes.length;
      for (i = 0; i < argsCount; i++) {
        op = conds.pop(); // ifthen op, i.e. jump
        op[1] = idx + 1;
      }
      return [OPTypes.void];
    case FormTypes.condarrowthen:
      op = conds.pop(); // test op, i.e. jumpiffalse
      op[0] = OPTypes.jumpiffalsekeep;
      // jump to the next op
      // since there is an op for this form
      // the idx is incremented to point to the right op
      op[1] = idx + 1;
      op = [OPTypes.jump, -1];
      conds.push(op);
      return op;
    case FormTypes.condclause:
      if (form.nodes.length === 1) { // only a test clause, no then
        op = conds[conds.length - 1]; // test op, i.e. jumpiffalse
        op[0] = OPTypes.jumpifnotfalsekeep;
      }
      return null;
    case FormTypes.conjunction:
      argsCount = form.nodes.length;
      if (argsCount === 0) {
        return [OPTypes.constant, true];
      }
      else {
        while (argsCount > 0) {
          op = conds.pop();
          op[0] = OPTypes.jumpiffalsekeep;
          op[1] = idx;
          argsCount -= 1;
        }
        return null;
      }
    case FormTypes.disjunction:
      argsCount = form.nodes.length;
      if (argsCount === 0) {
        return [OPTypes.constant, false];
      }
      else {
        while (argsCount > 0) {
          op = conds.pop();
          op[0] = OPTypes.jumpifnotfalsekeep;
          op[1] = idx;
          argsCount -= 1;
        }
        return null;
      }
    case FormTypes.arguments:
      return null;
    case FormTypes.procedureCall:
      return createCallOP(OPTypes.call, form);
    case FormTypes.tailCall:
      return createCallOP(OPTypes.tailcall, form);
    case FormTypes.program:
    case FormTypes.lambdaBody:
      return [OPTypes.discard, form.nodes.length - 1];
    case FormTypes.lambda:
      bodyCode = analyze([form.nodes[1]]);
      return [OPTypes.lambda,
        form.nodes[0].value, // lambda formals
        bodyCode, // analyzed lambda body
        form.nodes[2].value // the name of the lambda
      ];
    case FormTypes.void: // TODO is it used?
      return [OPTypes.void];
    case FormTypes.begin:
      if (form.nodes.length > 0) {
        return [OPTypes.discard, form.nodes.length - 1];
      }
      else {
        return [OPTypes.void];
      }
    default:
      return null;
  }
}
function traverse(forms, ops, conds) {
  var form, op;
  for (var i = 0; i < forms.length; i++) {
    form = forms[i];
    if (form.nodes && form.type !== FormTypes.lambda) {
      traverse(form.nodes, ops, conds);
    }
    op = analyzeForm(form, ops.length, conds);
    if (op) {
      if (Array.isArray(op[0])) {
        ops.push.apply(ops, op);
      }
      else {
        ops.push(op);
      }
    }        
  }
}
function printOps(ops) {
  ops.forEach(function (op, idx) {
    var type = op[0];
    var value = '';
    for (var i = 1; i < op.length; i++) {
      value += op[i] + ' ';
    }
    console.log(idx + ' ' + type + ' ' + value);
  });
}
function analyze(forms) {
  var ops = [];
  var conds = [];
  traverse(forms, ops, conds);   
  // printOps(ops);
  return ops;
}

function compile(text, lang) {
  var program = parser.parse(text, lang);
  return analyze([program]);
}

module.exports = {
  OPTypes: OPTypes,
  compile: compile,
};