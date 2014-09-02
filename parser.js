/*
A simple parser based on the grammer of r7rs small.
Transforms a stream of tokens to an abstract syntax tree.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['lexer'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lexer'), require('./lang-table'));
  } else {
    root.parser = factory(root.lexer, root.langTable);
  }
}(this, function (lexer, langTable) {
  'use strict';
  
  var TokenTypes = lexer.TokenTypes;

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
    derived: 'derived', // TODO
  };
  var OPTypes = {
    define: 'define',
    set: 'set',
    internaldefine: 'internaldefine',
    literal: 'literal',
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

  // A cache for syntax keywords for a given natural language.
  var syntax = null;
  // A cache for syntaxes of all natural languages.
  var syntaxCache = {};
  // Holds the currently used natural language. A string.
  var currentLang = null;
  // Extracts values from the language tables and stores them
  // in the syntax caches.
  function populateSyntax(lang) {
    if (currentLang === lang) {
      return;
    }
    var existingSyntax = syntaxCache[lang];
    if (existingSyntax) {
      syntax = existingSyntax;
    }
    else {
      var get = function get(key) {
        return langTable.get(lang, 'syntax', key);
      };
      syntax = syntaxCache[lang] = {
        'quote': get('quote'),
        'lambda': get('lambda'),
        'if': get('if'),
        'set!': get('set!'),
        'begin': get('begin'),
        'cond': get('cond'),
        'and': get('and'),
        'or': get('or'),
        'case': get('case'),
        'let': get('let'),
        'let*': get('let*'),
        'letrec': get('letrec'),
        'do': get('do'),
        'delay': get('delay'),
        'quasiquote': get('quasiquote'),
        'else': get('else'),
        '=>': get('=>'),
        'define': get('define'),
        'unquote': get('unquote'),
        'unquote-splicing': get('unquote-splicing'),
      };
    }
    currentLang = lang;
  }
  // Checks if a token object is tagged as an identifier.
  function isIdentifier(token) {
    return token.type === TokenTypes.identifier;
  }
  // Checks if a token object contains a syntax keyword.
  function isSyntaticKeyword(token) {
    return isIdentifier(token) && token.value in syntax;
  }
  // Checks if a token object contains a variable.
  function isVariable(token) {
    return isIdentifier(token) && !(token.value in syntax);
  }

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
    var op, argsCount, i;
    switch (form.type) {
      case FormTypes.definition:
        return [OPTypes.define];
      case FormTypes.internalDefinition:
        return [OPTypes.internaldefine, form.value];
      case FormTypes.assignment:
        return [OPTypes.set];
      case FormTypes.literal:
        // TODO now it uses the ast nodes
        return [OPTypes.literal, form];
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
          return [OPTypes.literal, createLiteral(TokenTypes.boolean, true)];
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
          return [OPTypes.literal, createLiteral(TokenTypes.boolean, false)];
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
        return [OPTypes.lambda,
          form.nodes[0].value, // lambda formals
          form.nodes[1].value // analyzed lambda body
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
      if (type === OPTypes.literal) {
        value = op[1].value.value;
      }
      else {
        for (var i = 1; i < op.length; i++) {
          value += op[i] + ' ';
        }
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

  // A constructor for an AST node containing a definition.
  function createDefinition(identifier, value) {
    return createInnerNode(FormTypes.definition,
      [createLiteral(TokenTypes.identifier, identifier), value]);
  }
  // A constructor for an AST node containing an internal definition.
  function createInternalDefinition(identifiers) {
    return createValueNode(FormTypes.internalDefinition, identifiers);
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
       createInnerNode(FormTypes.condarrowthen,
         [createProcedureCall(expression, 1)])]);
  }
  // A constructor for an AST node containing an 'cond' conditional.
  function createCond(clauses) {
    // TODO mark clauses for tail call if necessary
    return createInnerNode(FormTypes.cond, clauses);
  }
  // A constructor for an AST node containing a procedure call.
  function createProcedureCall(procedure, args) {
    if (Array.isArray(args)) {
      return createInnerNode(FormTypes.procedureCall,
        [createInnerNode(FormTypes.arguments, args), procedure]);
    }
    else {
      return createInnerNode(FormTypes.procedureCall,
        [createValueNode(FormTypes.arguments, args), procedure]);
    }
  }
  // Converts definitions into internal definitions and assignments.
  // Checks for duplicate definitions.
  function transformAndCheckDefinitions(forms) {
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
            raiseError(null, 'definition_expected_expr_found');
          }
          identifier = form.nodes[0].value.value;
          if (identifiers.indexOf(identifier) !== -1) {
            raiseError(null, 'duplicate_definitions', [identifier]);
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
    forms.unshift(createInternalDefinition(identifiers));
    return forms;
  }
  // A constructor for an AST node containing a lambda definition.
  function createLambda(formals, body) {
    body = transformAndCheckDefinitions(body);
    if (body.length > 0) {
      markTailContext(body[body.length - 1]);
    }
    var bodyCode = analyze([createInnerNode(FormTypes.lambdaBody, body)]);
    return createInnerNode(FormTypes.lambda,
      [createValueNode(FormTypes.lambdaFormals, formals),
       createValueNode(FormTypes.lambdaBody, bodyCode)]);
  }
  // A constructor for an AST node containing a whole program.
  function createProgram(forms) {
    if (forms.length === 0) {
      forms.push(createValueNode(FormTypes.void, null));
    }
    var node = createInnerNode(FormTypes.program, forms);
    return analyze([node]);
  }
  // A constructor for an AST node containing an 'and' expression.
  function createConjunction(tests) {
    return createInnerNode(FormTypes.conjunction,
      tests.map(function(test) {
        return createInnerNode(FormTypes.test, [test]);
      }));
  }
  // A constructor for an AST node containing an 'or' expression.
  function createDisjunction(tests) {
    return createInnerNode(FormTypes.disjunction,
      tests.map(function(test) {
        return createInnerNode(FormTypes.test, [test]);
      }));
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

  // Raises an error by throwing it.
  // This function is used only in the parser.
  function raiseError(tokenStream, messageKey, messageParams) {
    var message = langTable.get(currentLang, 'syntax-errors', messageKey);
    if (messageParams) {
      message = String.format(message, messageParams);
    }
    var err = new Error(message);
    var position;
    if (tokenStream) {
      position = tokenStream.getPosition();
    }
    else { // TODO should find exact position
      position = {
        line: 0,
        column: 0
      };
    }
    err.line = position.line;
    err.column = position.column;
    err.toString = function() {
      return Error.prototype.toString.call(this) +
        ' Line: ' + this.line + ', column: ' + this.column + '.';
    };
    throw err;
  }

  // Reads the alternate expression in an 'if' form
  // from the given token stream.
  function readAlternate(tokenStream) {
    var token = tokenStream.peek();
    if (!token) {
      raiseError(tokenStream, 'if_expr_end_unexpected');
    }
    if (token.type === TokenTypes.rightParen) {
      return null;
    }
    return readExpression(tokenStream);
  }
  // Reads an 'if' expression from the given token stream.
  function readIf(tokenStream) {
    var token = tokenStream.peek();
    if (!token) {
      raiseError(tokenStream, 'if_expr_end_unexpected');
    }
    var test = readExpression(tokenStream);
    var consequent = readExpression(tokenStream);
    var alternate = readAlternate(tokenStream);
    token = tokenStream.advance();
    if (! token || token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'if_expression_end_expected');
    }
    return createIf(test, consequent, alternate);
  }
  // Reads a clause for a 'cond' expression from the given token stream.
  function readCondClause(tokenStream) {
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream); // TODO
    }
    if (token.type === TokenTypes.leftParen) {
      token = tokenStream.peek();
      var test, expression;
      var elseFound = false;
      if (isIdentifier(token) && token.value === syntax['else']) {
        tokenStream.advance();
        elseFound = true;
      }
      else {
        test = readExpression(tokenStream);
        if (!test) {
          raiseError(tokenStream); // TODO
        }
      }
      token = tokenStream.peek();
      if (isIdentifier(token) && token.value === syntax['=>']) {
        if (elseFound) {
          raiseError(tokenStream); // TODO
        }
        tokenStream.advance();
        expression = readExpression(tokenStream);
        if (!expression) {
          raiseError(tokenStream); // TODO
        }
        token = tokenStream.advance();
        if (token.type === TokenTypes.rightParen) {
          return createCondArrow(test, expression);
        }
        else {
          raiseError(tokenStream); // TODO
        }
      }
      else {
        var expressions = [];
        expression = readExpression(tokenStream);
        while (expression) {
          expressions.push(expression);
          expression = readExpression(tokenStream);
        }
        token = tokenStream.advance();
        if (token.type === TokenTypes.rightParen) {
          if (elseFound) {
            return createCondElse(expressions);
          }
          else {
            return createCondClause(test, expressions);
          }
        }
        else {
          raiseError(tokenStream); // TODO
        }
      }
    }
  }
  // Reads a 'cond' expression from the given token stream.
  function readCond(tokenStream) {
    var clauses = [];
    var clause = readCondClause(tokenStream);
    if (!clause) {
      raiseError(); // TODO
    }
    var elseFound = false;
    while (clause) {
      if (elseFound) {
        raiseError(tokenStream, 'cond_else_last');
      }
      if (clause.type === FormTypes.condelse) {
        elseFound = true;
      }
      clauses.push(clause);
      clause = readCondClause(tokenStream);
    }    
    return createCond(clauses);
  }
  // Reads procedure call from the given token stream.
  function readProcedureCall(tokenStream) {
    var token;
    var procedure = readExpression(tokenStream);
    var args = [];
    var expression = readExpression(tokenStream);
    while(expression) {
      args.push(expression);
      expression = readExpression(tokenStream);
    }
    token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'proc_call_end_unexpected');
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'invalid_proc_call');
    }
    return createProcedureCall(procedure, args);
  }
  // Reads the formal parameters of a lambda expression
  // from the given token stream.
  function readLambdaFormals(tokenStream) {
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'lambda_expr_end_unexpected');
    }
    if (isVariable(token)) {
      return token.value;
    }
    if (token.type === TokenTypes.leftParen) {
      var formals = [];
      token = tokenStream.advance();
      while(token && isVariable(token)) {
        formals.push(token.value);
        token = tokenStream.advance();
      }
      if (!token) {
        raiseError(tokenStream, 'lambda_formals_end_unexpected');
      }
      if (token.type === TokenTypes.dot) {
        formals.push('.');
        token = tokenStream.advance();
        if (!token) {
          raiseError(tokenStream, 'lambda_formals_end_unexpected');
        }
        if (!isVariable(token)) {
          raiseError(tokenStream, 'lambda_expr_variable_expected');
        }
        formals.push(token.value);
        token = tokenStream.advance();
        if (!token) {
          raiseError(tokenStream, 'lambda_formals_end_unexpected');
        }
      }
      if (token.type === TokenTypes.rightParen) {
        return formals;
      }
      else {
        raiseError(tokenStream, 'expected_lambda_formals_end');
      }
    }
    else {
      raiseError(tokenStream, 'invalid_lambda_formals');
    }
  }
  // Reads a sequence of forms that are a part of another form
  // from the given token stream.
  function readBody(tokenStream, missingBodyExpressionsErrorKey) {
    var forms = [];
    var definition = readDefintion(tokenStream);
    while (definition) {
      forms.push(definition);
      definition = readDefintion(tokenStream);
    }
    var expression = readExpression(tokenStream);
    if (!expression) {
      raiseError(tokenStream, missingBodyExpressionsErrorKey);
    }
    while (expression) {
      forms.push(expression);
      expression = readExpression(tokenStream);
    }
    return forms;
  }
  // Reads the body forms of a lambda expression
  // from the given token stream.
  function readLambdaBody(tokenStream) {
    return readBody(tokenStream, 'no_lambda_body_exprs');
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
    else if (expression.type === FormTypes.conjunction ||
      expression.type === FormTypes.disjunction) {
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
        markTailContext(procedureBody.value[procedureBody.value.length - 1]);
      }
      else {
        expression.type = FormTypes.tailCall;
      }
    }
  }
  // Reads a lambda expression
  // from the given token stream.
  function readLambda(tokenStream) {
    var formals = readLambdaFormals(tokenStream);
    var body = readLambdaBody(tokenStream);
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'lambda_body_end_unexpected');
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'invalid_lambda_body_end');
    }
    // markTailContext(body[body.length - 1]);
    return createLambda(formals, body);
  }
  // Checks if a token object represents a self-evaluating expression.
  function isSelfEvaluating(token) {
    var tokenType = token.type;
    return tokenType === TokenTypes.boolean ||
       tokenType === TokenTypes.number ||
       tokenType === TokenTypes.character ||
       tokenType === TokenTypes.string;
  }
  // Reads the storing of a value to a location
  // from the given token stream.
  function readValueAssignment(tokenStream, constructor, formName) {
    var token = tokenStream.advance();
    if (!token || !isVariable(token)) {
      raiseError(tokenStream, 'variable_expected', [formName]);
    }
    var variable = token.value;
    token = tokenStream.peek(); // TODO maybe move the reading in the readExpression
    if (!token) {
      raiseError(tokenStream, 'expr_expected', [formName]);
    }
    var expression = readExpression(tokenStream);
    if (!expression) {
      raiseError(tokenStream, 'expr_expected', [formName]);
    }
    token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [formName]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [formName]);
    }
    return constructor(variable, expression);
  }
  // Reads the variable name and the formal parameters of a lambda definition
  // from the given token stream.
  function readDefintionHeader(tokenStream) {
    var token = tokenStream.advance();
    if (!token || !isVariable(token)) {
      raiseError(tokenStream, 'variable_expected', [formName]);
    }
    var variable = token.value;
    var formals = [];
    token = tokenStream.advance();
    while(token && isVariable(token)) {
      formals.push(token.value);
      token = tokenStream.advance();
    }
    if (!token) {
      raiseError(tokenStream, 'definition_formals_end_unexpected');
    }
    if (token.type === TokenTypes.dot) {
      formals.push('.');
      token = tokenStream.advance();
      if (!token) {
        raiseError(tokenStream, 'definition_formals_end_unexpected');
      }
      if (!isVariable(token)) {
        raiseError(tokenStream, 'definition_expr_variable_expected');
      }
      formals.push(token.value);
      token = tokenStream.advance();
      if (!token) {
        raiseError(tokenStream, 'definition_formals_end_unexpected');
      }
    }
    if (token.type === TokenTypes.rightParen) {
      if (formals[0] === '.') {
        formals = formals[1];
      }
      return {
        identifier: variable,
        formals: formals
      };
    }
    else {
      raiseError(tokenStream, 'expected_lambda_formals_end');
    }
  }
  // Reads a defintion form from the given token stream.
  function readDefintion(tokenStream) {
    var token = tokenStream.peek();
    var nextToken;
    if (!token) {
      return null;
    }
    var formName = langTable.get(currentLang, 'syntax-common', 'definition'); // TODO cache
    if (token.type === TokenTypes.leftParen) {
      nextToken = tokenStream.peek(1);
      if (!nextToken) {
        raiseError(tokenStream, 'list_end_unexpected');
      }
      if (isIdentifier(nextToken) && nextToken.value === syntax['define']) {
        tokenStream.advance(2);
        token = tokenStream.peek();
        if (!token) {
          raiseError(tokenStream, 'definition_end_unexpected');
        }
        if (isVariable(token)) {
          return readValueAssignment(tokenStream, createDefinition, formName);
        }
        else if (token.type === TokenTypes.leftParen) {
          tokenStream.advance();
          var header = readDefintionHeader(tokenStream);
          var body = readBody(tokenStream, 'no_definition_exprs');
          token = tokenStream.advance();
          if (!token) {
            raiseError(tokenStream, 'definition_body_end_unexpected');
          }
          if (token.type !== TokenTypes.rightParen) {
            raiseError(tokenStream, 'invalid_definition_body_end');
          }
          // markTailContext(body[body.length - 1]);
          return createDefinition(header.identifier, createLambda(header.formals, body));
        }
      }
    }
    return null;
  }
  // Reads a 'set!' expression
  // from the given token stream.
  function readAssignment(tokenStream) {
    return readValueAssignment(tokenStream, createAssignment, langTable.get(currentLang, 'syntax-common', 'assignment')); // TODO cache
  }
  // Reads an 'and' or an 'or' expressions
  // from the given token stream.
  function readBoolOperations(tokenStream, syntaxKey, constructor) {
    var tests = []
    var expression = readExpression(tokenStream);
    while (expression) {
      tests.push(expression);
      expression = readExpression(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [syntax[syntaxKey]]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [syntax[syntaxKey]]);
    }
    return constructor(tests);
  }
  // Reads an 'and' expressions from the given token stream.
  function readConjunction(tokenStream) {
    return readBoolOperations(tokenStream, 'and', createConjunction);
  }
  // Reads an 'or' expressions from the given token stream.
  function readDisjunction(tokenStream) {
    return readBoolOperations(tokenStream, 'or', createDisjunction);
  }
  function guardLetBinding(tokenStream, token) {
    if (!token) {
      raiseError(tokenStream, 'let_binding_end_unexpected');
    }
  }
  // Reads a binding in a 'let' expression
  // from the given token stream.
  function readBinding(tokenStream) {
    var token = tokenStream.advance();
    guardLetBinding(tokenStream, token);
    if (token.type === TokenTypes.rightParen) {
      return null;
    }
    if (token.type !== TokenTypes.leftParen) {
      raiseError(tokenStream, 'expected_let_binding');
    }
    token = tokenStream.advance();
    guardLetBinding(tokenStream, token);
    if (!isVariable(token)) {
      raiseError(tokenStream, 'expected_let_variable');
    }
    var variable = token.value;
    var init = readExpression(tokenStream);
    if (!init) {
      raiseError(tokenStream, 'expected_let_init');
    }
    token = tokenStream.advance();
    guardLetBinding(tokenStream, token);
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'expected_let_binding_end');
    }
    return {
      variable: variable,
      init: init
    };
  }
  // Reads the bindings in a 'let' expression
  // from the given token stream.
  function readBindings(tokenStream) {
    var bindings = [];
    var binding = readBinding(tokenStream);
    while (binding) {
      bindings.push(binding);
      binding = readBinding(tokenStream);
    }
    return bindings;
  }
  // Reads the body forms of a 'let' expression
  // from the given token stream.
  function readLetBody(tokenStream) {
    return readBody(tokenStream, 'no_let_body_exprs');
  }
  function guardLet(tokenStream, token) {
    if (!token) {
      raiseError(tokenStream, 'let_end_unexpected');
    }
  }
  // Reads the content of let forms
  // from the given token stream
  // since they have common syntax.
  function readLetContent(tokenStream) {
    // TODO parameterize the error messages for different let forms
    var bindings;
    var token = tokenStream.advance();
    guardLet(tokenStream, token);
    if (token.type === TokenTypes.leftParen) {
      bindings = readBindings(tokenStream);
    }
    else {
      raiseError(tokenStream, 'let_bindings_exptected');
    }

    var body = readLetBody(tokenStream);
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'let_body_end_unexpected');
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'invalid_let_body_end');
    }
    return [bindings, body];
  }
  // Reads a 'let' expression
  // from the given token stream.
  function readLet(tokenStream) {
    var token = tokenStream.peek();
    guardLet(tokenStream, token);
    var name;    
    if (isVariable(token)) {
      name = token.value;
      tokenStream.advance();
    }    
    var letContent = readLetContent(tokenStream);
    var bindings = letContent[0];
    var body = letContent[1];
    var bindingsCount = bindings.length;
    var formals = new Array(bindingsCount);
    var inits = new Array(bindingsCount);
    for (var i = 0; i < bindingsCount; i++) {
      formals[i] = bindings[i].variable;
      inits[i] = bindings[i].init;
    }
    var procedure = createLambda(formals, body);
    if (!name) {
      /*
      (let ((a 0)) (+ a 1)) =>
      ((lambda (a) (+ a 1)) 0)
      */
      return createProcedureCall(procedure, inits);
    }
    else {
      /*
      (let foo ((a 0))
        (if (< a 10)
            (foo (+ a 1))
          a))
      =>
      ((lambda ()
        (define foo
          (lambda (a)
            (if (< a 10)
                (foo (+ a 1))
              a)))
        (foo 0)))
      */
      var lambdaDefinition = createDefinition(name, procedure);
      var lambdaApplication = createProcedureCall(createVariable(name), inits);
      var enclosingProcedure = createLambda([], [lambdaDefinition, lambdaApplication]);
      return createProcedureCall(enclosingProcedure, []);
    }
  }
  // Reads a 'letrec' expression
  // from the given token stream.
  function readLetrec(tokenStream) {
    var letContent = readLetContent(tokenStream);
    var bindings = letContent[0];
    var body = letContent[1];
    /*
    (letrec ((a 0) (b 0)) (+ a b)) =>
    ((lambda () (define a 0) (define b 0) (+ a b)))
    */
    var bindingsCount = bindings.length;
    var definitions = new Array(bindingsCount);
    var binding;
    for (var i = 0; i < bindingsCount; i++) {
      binding = bindings[i];
      definitions[i] = createDefinition(binding.variable, binding.init);
    }
    body = definitions.concat(body);
    var procedure = createLambda([], body);
    return createProcedureCall(procedure, []);
  }
  // Reads a 'let*' expression
  // from the given token stream.
  function readLetstar(tokenStream) {
    var letContent = readLetContent(tokenStream);
    var bindings = letContent[0];
    var body = letContent[1];
    /*
    (let* ((a 0) (b 0)) (+ a b)) =>
    ((lambda (a)
      ((lambda (b)
        (+a b)) 0)) 0)
    */
    var bindingsCount = bindings.length;
    var formals, inits, procedure;
    if (bindingsCount === 0) {
      procedure = createLambda([], body);
      return createProcedureCall(procedure, []);
    }
    else {
      for (var i = bindingsCount - 1; i >= 0; i--) {
        formals = [bindings[i].variable];
        inits = [bindings[i].init];
        procedure = createLambda(formals, body);
        body = [createProcedureCall(procedure, inits)];
      }
      return body[0];
    }
  }
  // Reads a 'begin' expression
  // from the given token stream.
  function readBegin(tokenStream) {
    var forms = [];
    var form = readDefintion(tokenStream);
    var readFunc;
    if (form) {
      readFunc = readDefintion;
    }
    else {
      readFunc = readExpression;
      form = readFunc(tokenStream);
    }
    while (form) {
      forms.push(form);
      form = readFunc(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [syntax['begin']]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [syntax['begin']]);
    }
    return createBegin(forms);
  }
  // A constant containing the types of simple data
  var simpleDatums = [
    TokenTypes.boolean,
    TokenTypes.number,
    TokenTypes.character,
    TokenTypes.string,
    TokenTypes.identifier
    // TODO byte vector
  ];
  // Reads a simple datum from the given token stream.
  function readSimpleDatum(tokenStream) {
    var token = tokenStream.peek();
    if (simpleDatums.indexOf(token.type) !== -1) {
      tokenStream.advance();
      return createLiteral(token.type, token.value);
    }
    return null;
  }
  // Reads a list datum from the given token stream.
  function readListDatum(tokenStream) {
    var listItems = [];
    var literal = readDatum(tokenStream);
    while (literal) {
      listItems.push(literal);
      literal = readDatum(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'list_end_unexpected');
    }
    if (token.type === TokenTypes.dot) {
      listItems.push('.');
      token = tokenStream.peek();
      if (!token) {
        raiseError(tokenStream, 'list_end_unexpected');
      }
      literal = readDatum(tokenStream);
      if (!literal) {
        raiseError(tokenStream, 'datum_expected');
      }
      listItems.push(literal);
      token = tokenStream.peek();
      if (!token) {
        raiseError(tokenStream, 'list_end_unexpected');
      }
      tokenStream.advance();
    }
    if (token.type === TokenTypes.rightParen) {      
      return createLiteral('list', listItems);
    }
    else {
      raiseError(tokenStream, 'expected_list_end');
    }
  }
  // Reads a vector datum from the given token stream.
  function readVectorDatum(tokenStream) {
    var vectorItems = [];
    var literal = readDatum(tokenStream);
    while (literal) {
      vectorItems.push(literal);
      literal = readDatum(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'vector_end_unexpected');
    }
    if (token.type === TokenTypes.rightParen) {
      return createLiteral('vector', vectorItems);
    }
    else {
      raiseError(tokenStream, 'expected_vector_end');
    }
  }
  // Reads a list or a vector datum from the given token stream.
  function readCompoundDatum(tokenStream) {
    var token = tokenStream.peek();
    if (token.type === TokenTypes.leftParen) {
      tokenStream.advance();
      return readListDatum(tokenStream);
    }
    if (token.type === TokenTypes.vectorParen) {
      tokenStream.advance();
      return readVectorDatum(tokenStream);
    }
    return null;
    // TODO read abbreviations
  }
  // Reads a single datum from the given token stream.
  function readDatum(tokenStream, raise) {
    var datum = readSimpleDatum(tokenStream) || readCompoundDatum(tokenStream);
    if (!datum && raise) {
      raiseError(tokenStream, 'invalid_datum');
    }
    return datum;
  }
  // Reads a quote literal from the given token stream.
  function readQuote(tokenStream) {
    var datum = readDatum(tokenStream);
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [syntax['quote']]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [syntax['quote']]);
    }
    return datum;
  }
  // Reads any expression from the given token stream.
  function readExpression(tokenStream) {
    var token = tokenStream.peek();
    var nextToken;
    if (!token) {
      return null;
    }
    // If it is a lone variable then it will be evaluated.
    if (isIdentifier(token)) {
      if (!isSyntaticKeyword(token)) {
        tokenStream.advance();
        return createVariable(token.value);
      }
      else {
        raiseError(tokenStream, 'syntax_keywords_as_variables');
      }
    }
    // Checks if it is a self-evaluating expression, e.g. a string literal.
    if (isSelfEvaluating(token)) {
      tokenStream.advance();
      return createLiteral(token.type, token.value);
    }
    // If it is an openning vector parenthesis then reads the whole vector.
    if (token.type === TokenTypes.vectorParen) {
      tokenStream.advance();
      return readVectorDatum(tokenStream);
    }
    // If it is a quotation then reads the quoted datum.
    if (token.type === TokenTypes.quote) {
      tokenStream.advance();
      return readDatum(tokenStream);
    }
    // If it is an openning parenthesis
    // then tries to read a specific expression.
    if (token.type === TokenTypes.leftParen) {
      tokenStream.advance();
      nextToken = tokenStream.peek();
      if (!nextToken) {
        raiseError(tokenStream, 'list_end_unexpected');
      }
      if (isIdentifier(nextToken)) {
        if (nextToken.value === syntax['if']) {
          tokenStream.advance();
          return readIf(tokenStream);
        }
        if (nextToken.value === syntax['cond']) {
          tokenStream.advance();
          return readCond(tokenStream);
        }
        if (nextToken.value === syntax['lambda']) {
          tokenStream.advance();
          return readLambda(tokenStream);
        }
        if (nextToken.value === syntax['set!']) {
          tokenStream.advance();
          return readAssignment(tokenStream);
        }
        if (nextToken.value === syntax['and']) {
          tokenStream.advance();
          return readConjunction(tokenStream);
        }
        if (nextToken.value === syntax['or']) {
          tokenStream.advance();
          return readDisjunction(tokenStream);
        }
        if (nextToken.value === syntax['let']) {
          tokenStream.advance();
          return readLet(tokenStream);
        }
        if (nextToken.value === syntax['letrec']) {
          tokenStream.advance();
          return readLetrec(tokenStream);
        }
        if (nextToken.value === syntax['let*']) {
          tokenStream.advance();
          return readLetstar(tokenStream);
        }
        if (nextToken.value === syntax['begin']) {
          tokenStream.advance();
          return readBegin(tokenStream);
        }
        if (nextToken.value === syntax['quote']) {
          tokenStream.advance();
          return readQuote(tokenStream);
        }
        if (nextToken.value === syntax['define']) {
          raiseError(tokenStream, 'expr_expected_definition_found');
        }
      }
      if (nextToken.type === TokenTypes.leftParen || isVariable(nextToken)) {        
        return readProcedureCall(tokenStream);
      }
      if (nextToken.type === TokenTypes.boolean
        || nextToken.type === TokenTypes.number
        || nextToken.type === TokenTypes.character
        || nextToken.type === TokenTypes.string) {
        raiseError(tokenStream, 'data_not_procedure');
      }
      raiseError(tokenStream, 'unknown_expr');
    }
    return null;
  }
  // Reads an expression or a definition from the given token stream.
  function readCommandOrDefinition(tokenStream) {
    // The defintions should be processed first
    // otherwise they will be treated as unknown expressions.
    return readDefintion(tokenStream) || readExpression(tokenStream);
  }
  // Reads the whole program from the given token stream.
  // While iterating the token stream the parser tries
  // to read whole forms and create AST nodes for each of them.
  function readProgram(tokenStream) {
    // TODO read the import declaration
    var forms = [];
    var form = readCommandOrDefinition(tokenStream);
    while (form) {
      forms.push(form);
      form = readCommandOrDefinition(tokenStream);
    }
    return createProgram(forms);
  }

  // The main function in the module.
  // Creats an AST from programming code.
  function parse(text, lang) {
    lang = lang || 'en';
    populateSyntax(lang, lang);
    var tokenStream = new lexer.TokenStream(text, lang);
    return readProgram(tokenStream);
  }

  // Exports the main function and the FormTypes tag values.
  return {
    parse: parse,
    FormTypes: FormTypes,
    OPTypes: OPTypes,
    cloneNode: cloneNode,
  };
}));