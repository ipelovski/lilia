'use strict';

var TokenTypes = require('./lexer').TokenTypes;
var raiseSyntaxError = require('./common').raiseSyntaxError;

// The values used to tag special forms.
var FormTypes = {
  program: 'program',
  ops: 'ops',
  variable: 'variable',
  literal: 'literal',
  procedureCall: 'procedureCall',
  arguments: 'arguments',
  tailCall: 'tailCall',
  lambda: 'lambda',
  lambdaFormals: 'lambdaFormals',
  lambdaBody: 'lambdaBody',    
  ifexpr: 'ifexpr',
  test: 'test',
  ifthen: 'ifthen',
  ifelse: 'ifelse',
  cond: 'cond',
  condclause: 'condclause',
  condelse: 'condelse',
  condarrow: 'condarrow',
  condarrowthen: 'condarrowthen',
  assignment: 'assignment',
  definition: 'definition',
  internalDefinition: 'internalDefinition',
  conjunction: 'conjunction',
  disjunction: 'disjunction',
  begin: 'begin',
  void: 'void',
};

function createValueNode(type, value) {
  return {
    type: type,
    value: value,
    nodes: null
  };
}

function createInnerNode(type, children) {
  return {
    type: type,
    value: null,
    nodes: children
  };
}

function cloneNode(node) {
  return {
    type: node.type,
    value: node.value,
    nodes: (node.nodes ? node.nodes.map(function(node) {
      return cloneNode(node);
    }) : null)
  };
}
// Traverses a subtree of an AST and marks the nodes
// in a tail context by adding a boolean property to them.
function markTailContext(expression) {
  if (expression.type === FormTypes.ifexpr) {
    markTailContext(expression.nodes[1]);
    if (expression.nodes[2]) {
      markTailContext(expression.nodes[2]);
    }
  }
  else if ((expression.type === FormTypes.ifthen
    || expression.type === FormTypes.ifelse
    || expression.type === FormTypes.test) // the 'test' expressions in conjunctions and disjunctions
    && expression.nodes) { // ifelse can have nodes === null
    markTailContext(expression.nodes[0]);
  }
  else if (expression.type === FormTypes.conjunction
    || expression.type === FormTypes.disjunction
    || expression.type === FormTypes.begin) {
    var tests = expression.nodes;
    if (tests.length > 0) {
      markTailContext(tests[tests.length - 1]);
    }
  }
  else if (expression.type === FormTypes.procedureCall) {
    var procedure = expression.nodes[1];
    // tail 'let' expression
    if (procedure.type === FormTypes.lambda) {
      var procedureBody = procedure.nodes[1];
      markTailContext(procedureBody.nodes[procedureBody.nodes.length - 1]);
    }
    else {
      expression.type = FormTypes.tailCall;
    }
  }
}
// A constructor for an AST node containing a definition.
function createDefinition(identifier, value) {
  return createInnerNode(FormTypes.definition,
    [createLiteral(TokenTypes.identifier, identifier), value]);
}
// A constructor for an AST node containing an internal definition.
function createInternalDefinition(identifiers) {
  return createInnerNode(FormTypes.internalDefinition, identifiers);
}
// A constructor for an AST node containing an assignment.
function createAssignment(identifier, value) {
  return createInnerNode(FormTypes.assignment,
    [createLiteral(TokenTypes.identifier, identifier), value]);
}
// A constructor for an AST node containing an 'if' conditional.
function createIf(test, consequent, alternate) {
  return createInnerNode(FormTypes.ifexpr,
    [createInnerNode(FormTypes.test, [test]),
     createInnerNode(FormTypes.ifthen, [consequent]),
     createInnerNode(FormTypes.ifelse, alternate ? [alternate] : null)]);
}  
// A constructor for an AST node containing an 'cond' 'else' clause.
function createCondClause(test, expressions) {
  var nodes = [createInnerNode(FormTypes.test, [test])];
  if (expressions.length > 0) {
    nodes.push(createInnerNode(FormTypes.ifthen,
      [createBegin(expressions)]));
  }
  return createInnerNode(FormTypes.condclause, nodes);
}
// A constructor for an AST node containing an 'cond' 'else' clause.
function createCondElse(expressions) {
  return createInnerNode(FormTypes.condelse,
    [createInnerNode(FormTypes.test, [createLiteral(TokenTypes.boolean, true)]),
     createInnerNode(FormTypes.ifthen, [createBegin(expressions)])]);
}
// A constructor for an AST node containing an 'cond' '=>' clause.
function createCondArrow(test, expression) {
  return createInnerNode(FormTypes.condarrow,
    [createInnerNode(FormTypes.test, [test]),
     createProcedureCall(expression, [test])]); // TODO `test` should not be reevaluated
}
// A constructor for an AST node containing an 'cond' conditional.
function createCond(clauses) {
  // TODO mark clauses for tail call if necessary
  return createInnerNode(FormTypes.cond, clauses);
}
// A constructor for an AST node containing a procedure call.
function createProcedureCall(procedure, args, position) {
  return createInnerNode(FormTypes.procedureCall,
    [createInnerNode(FormTypes.arguments, args), procedure, createValueNode('position', position || null)]);
}
// Converts definitions into internal definitions and assignments.
// Checks for duplicate definitions.
function transformAndCheckDefinitions(parsingInfo, forms) {
  var identifiers = [];
  var exprFound = false;
  function traverse(forms) {
    var form, identifier;
    for (var i = 0; i < forms.length; i++) {
      form = forms[i];
      if (form.type === FormTypes.begin) {
        traverse(form.nodes);
      }
      if (form.type === FormTypes.definition) {
        // checks for expressions mixed with definitions
        if (exprFound) {
          // TODO the position of the token stream will be incorrect
          raiseSyntaxError(parsingInfo, 'definition_expected_expr_found');
        }
        identifier = form.nodes[0].value.value;
        if (identifiers.indexOf(identifier) !== -1) {
          // TODO the position of the token stream will be incorrect
          raiseSyntaxError(parsingInfo, 'duplicate_definitions', [identifier]);
        }
        else {
          identifiers.push(identifier);
          form.type = FormTypes.assignment;
        }
      }
      else if (form.type !== FormTypes.begin) {
        exprFound = true;
      }
    }
  }
  traverse(forms);
  forms.unshift(createInternalDefinition(
    identifiers.map(identifier =>
      createLiteral(TokenTypes.identifier, identifier)
    )
  ));
  return forms;
}
// A constructor for an AST node containing a lambda definition.
function createLambda(parsingInfo, formals, body, name) {
  body = transformAndCheckDefinitions(parsingInfo, body);
  if (body.length > 0) {
    markTailContext(body[body.length - 1]);
  }
  return createInnerNode(FormTypes.lambda,
    [createValueNode(FormTypes.lambdaFormals, formals),
     createInnerNode(FormTypes.lambdaBody, body),
     createValueNode(FormTypes.variable, name)]);
}
// A constructor for an AST node containing an 'and' expression.
function createConjunction(tests) {
  return createInnerNode(FormTypes.conjunction, tests);
}
// A constructor for an AST node containing an 'or' expression.
function createDisjunction(tests) {
  return createInnerNode(FormTypes.disjunction, tests);
}
// A constructor for an AST node containing a variable evaluation.
function createVariable(identifier) {
  return createValueNode(FormTypes.variable, identifier);
}
// A constructor for an AST node containing a self-evaluating literal.
function createLiteral(type, value) {
  return createValueNode(FormTypes.literal, {
    type: type,
    value: value
  });
}
// A constructor for an AST node containing a sequence of forms.
function createBegin(forms) {
  return createInnerNode(FormTypes.begin, forms);
}
// A constructor for an AST node containing a whole program.
function createProgram(forms) {
  return createInnerNode(FormTypes.program, forms);  
}

module.exports = {
  FormTypes: FormTypes,
  createDefinition: createDefinition,
  createAssignment: createAssignment,
  createIf: createIf,
  createCond: createCond,
  createCondArrow: createCondArrow,
  createCondElse: createCondElse,
  createCondClause: createCondClause,
  createProcedureCall: createProcedureCall,
  createLambda: createLambda,
  createConjunction: createConjunction,
  createDisjunction: createDisjunction,
  createVariable: createVariable,
  createLiteral: createLiteral,
  createBegin: createBegin,
  createProgram: createProgram,
};