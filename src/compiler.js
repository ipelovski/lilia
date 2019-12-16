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
var SchemeChar = types.SchemeChar;
var EmptyList = types.EmptyList;

var OPTypes = {
  add: 'add',
  set: 'set',
  get: 'get',
  jump: 'jump',
  jumpIfFalse: 'jumpIfFalse',
  call: 'call',
  tailcall: 'tailcall',
  literal: 'literal',
  pop: 'pop',
  copy: 'copy'
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
      return new SchemeChar(value);
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

function analyzeForm(form, idx) {
  var op, argsCount, i;
  switch (form.type) {
    case FormTypes.definition:
      return analyzeDefinition(form, idx);
    case FormTypes.assignment:
      return analyzeAssignment(form, idx);
    case FormTypes.internalDefinition:
      return analyzeInternalDefinition(form, idx);
    case FormTypes.literal:
      return analyzeLiteral(form, idx);
    case FormTypes.variable:
      return analyzeVariable(form, idx);
    case FormTypes.ifexpr:
      return analyzeIf(form, idx);
    case FormTypes.cond:
      return analyzeCond(form, idx);
    case FormTypes.conjunction:
      return analyzeConjunction(form, idx);
    case FormTypes.disjunction:
      return analyzeDisjunction(form, idx);
    case FormTypes.lambda:
      return analyzeLambda(form, idx);
    case FormTypes.program:
    case FormTypes.begin:
      return analyzeSequence(form, idx);
    case FormTypes.procedureCall:
      return analyzeCall(OPTypes.call, form, idx);
    case FormTypes.tailCall:
      return analyzeCall(OPTypes.tailcall, form, idx);
    default:
      throw new Error('Unkown type'); // TODO should raise a more user frienly error, is this code reachable?
  }
}

function analyzeDefinition(form, idx) {
  var identifier = form.nodes[0];
  var value = analyzeForm(form.nodes[1], idx + 1);
  return value.concat({
    type: OPTypes.add,
    id: identifier.value.value
  });
}

function analyzeAssignment(form, idx) {
  var identifier = form.nodes[0];
  var value = analyzeForm(form.nodes[1], idx + 1);
  return value.concat({
    type: OPTypes.set,
    id: identifier.value.value
  });
}

function analyzeInternalDefinition(form, idx) {
  var ids = form.nodes;
  if (ids.length > 0) {
    var definitions = new Array(ids.length * 2 + 1);
    for (var i = 0, j = 0; j < ids.length; i += 2, j++) {
      definitions[i] = {
        type: OPTypes.literal,
        value: null
      };
      definitions[i + 1] = {
        type: OPTypes.add,
        id: ids[j].value.value
      };
    }
    definitions[definitions.length - 1] = {
      type: OPTypes.pop,
      count: ids.length
    };
    return definitions;
  } else {
    return [];
  }
}

function analyzeLiteral(form, idx) {
  return [{
    type: OPTypes.literal,
    value: evalLiteral(form)
  }];
}

function analyzeVariable(form, idx) {
  return [{
    type: OPTypes.get,
    id: form.value
  }];
}

function analyzeIf(form, idx) {
  // TODO simplify the form by removing unnecessary nodes
  // form.nodes[0].nodes[0] => form.nodes[0]
  var test = analyzeForm(form.nodes[0].nodes[0], idx);
  var consequent = analyzeForm(form.nodes[1].nodes[0],
    idx + test.length + 1);
  var alternate;
  if (form.nodes[2].nodes !== null) {
    alternate = analyzeForm(form.nodes[2].nodes[0],
      idx + test.length + 1 + consequent.length + 1);
  } else {
    alternate = [{
      type: OPTypes.literal,
      value: null
    }];
  }

  var testJumpIndex = idx + test.length +
    1 + // the operation for test jump has two elements
    consequent.length +
    1; // the operation for consequent jump has two elements
  var consequentJumpIndex = testJumpIndex + alternate.length;
  return test
    .concat({
      type: OPTypes.jumpIfFalse,
      index: testJumpIndex
    })
    .concat(consequent)
    .concat({
      type: OPTypes.jump,
      index: consequentJumpIndex
    })
    .concat(alternate);
}

function analyzeCond(form, idx) {
  // TODO simplify the form by removing unnecessary nodes
  // clauses[i].nodes[0].nodes[0] => clauses[i].nodes[0]
  var clauses = form.nodes;
  var ops = [{
    type: OPTypes.literal,
    value: null
  }];
  var jumps = [];
  for (var i = 0; i < clauses.length; i++) {

    var clauseNodes = clauses[i].nodes;
    var test = analyzeForm(clauseNodes[0].nodes[0], idx + ops.length);
    ops = ops.concat(test);
    if (clauseNodes.length === 1) {
      ops.push({
        type: OPTypes.copy
      });
    }
    var testJump = {
      type: OPTypes.jumpIfFalse,
      index: -1
    };
    ops.push(testJump);
    if (clauses[i].type === FormTypes.condarrow) {
      var call = analyzeForm(clauseNodes[1], idx + ops.length);
      ops = ops.concat(call);
    } else {
      if (clauseNodes.length > 1) {
        var forms = analyzeSequence(clauseNodes[1].nodes[0], idx + ops.length + 1); // + 1 because of the pop op
        ops = ops
          .concat({
            type: OPTypes.pop,
            count: 1
          })
          .concat(forms);
      }
    }
    var jump = {
      type: OPTypes.jump,
      index: -1
    };
    jumps.push(jump);
    ops.push(jump);
    testJump.index = idx + ops.length;
    if (clauseNodes.length === 1) {
      // discard the copied value at the expression stack
      // if the test evaluates to false
      ops.push({
        type: OPTypes.pop,
        count: -1
      });
    }
  }
  var jumpToEndIndex = idx + ops.length;
  jumps.forEach(jump => jump.index = jumpToEndIndex);
  return ops;
}

function analyzeConjunction(form, idx) {
  var argsCount = form.nodes.length;
  if (argsCount === 0) {
    return [{
      type: OPTypes.literal,
      value: true
    }];
  } else {
    var ops = [];
    var jumps = [];
    for (var i = 0; i < argsCount; i++) {
      var test = analyzeForm(form.nodes[i], idx + ops.length);
      ops = ops.concat(test);
      ops.push({
        type: OPTypes.copy
      });
      var jump = {
        type: OPTypes.jumpIfFalse,
        index: -1
      };
      jumps.push(jump);
      ops.push(jump);
    }
    var jumpToEndIndex = idx + ops.length;
    jumps.forEach(jump => jump.index = jumpToEndIndex);
    return ops;
  }
}

function analyzeDisjunction(form, idx) {
  var argsCount = form.nodes.length;
  if (argsCount === 0) {
    return [{
      type: OPTypes.literal,
      value: false
    }];
  } else {
    var ops = [];
    var jumps = [];
    for (var i = 0; i < argsCount; i++) {
      var test = analyzeForm(form.nodes[i], idx + ops.length);
      ops = ops.concat(test);
      ops.push({
        type: OPTypes.copy
      });
      ops.push({
        type: OPTypes.jumpIfFalse,
        index: idx + ops.length + 2 // the current op and the next jump op
      });
      var jump = {
        type: OPTypes.jump,
        index: -1
      };
      jumps.push(jump);
      ops.push(jump);
    }
    var jumpToEndIndex = idx + ops.length;
    jumps.forEach(jump => jump.index = jumpToEndIndex);
    return ops;
  }
}

function analyzeLambda(form, idx) {
  var bodyCode = analyzeSequence(form.nodes[1], 0);
  return [{
    type: OPTypes.literal,
    value: {
      type: 'lambda',
      value: {
        args: form.nodes[0].value, // lambda formals
        body: bodyCode, // analyzed lambda body
        name: form.nodes[2].value // the name of the lambda
      }
    }
  }];
}

function analyzeSequence(form, idx) {
  var nodes = form.nodes;
  var nodesCount = nodes.length;
  if (nodesCount > 0) {
    var ops = traverse(nodes, idx);
    if (nodesCount > 1) {
      ops.push({
        type: OPTypes.pop,
        count: nodesCount - 1
      });
    }
    return ops;
  } else {
    return [{
      type: OPTypes.literal,
      value: null
    }];
  }
}

function analyzeCall(callType, form, idx) {
  var args = form.nodes[0].nodes;
  var argValues = [];
  for (var i = 0; i < args.length; i++) {
    argValues = argValues.concat(analyzeForm(args[i], idx + argValues.length));
  }
  var argsCount = args.length;
  var procedure = analyzeForm(form.nodes[1], idx + argValues.length);
  // the position of the procedure call in the code
  var position = form.nodes[2].value;
  return argValues
    .concat(procedure)
    .concat({
      type: callType,
      argsCount: argsCount,
      position: position
    });
}

function traverse(forms, idx) {
  var form, op;
  var ops = [];
  for (var i = 0; i < forms.length; i++) {
    form = forms[i];
    op = analyzeForm(form, idx + ops.length);
    ops = ops.concat(op);
  }
  return ops;
}

function printOps(ops) {
  ops.forEach(function (op, idx) {
    console.log(idx, op.type, op);
  });
}

function analyze(forms) {
  var ops = traverse(forms, 0);
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