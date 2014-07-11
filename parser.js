(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['lexer'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lexer'), require('./lang-table'));
  } else {
    root.parser = factory(root.lexer, root.langTable);
  }
}(this, function (lexer, langTable) {
  var TokenTypes = lexer.TokenTypes;

  var FormTypes = {
    program: 'program',
    variable: 'variable',
    literal: 'literal',
    procedureCall: 'procedureCall',
    lambda: 'lambda',
    conditional: 'conditional',
    assignment: 'assignment',
    definition: 'definition',
    conjunction: 'conjunction',
    disjunction: 'disjunction',
    begin: 'begin',
    derived: 'derived', // TODO
  };

  var Syntax = null;
  var syntaxCache = {};
  var currentLang = null;
  function populateSyntax(lang) {
    if (currentLang === lang) {
      return;
    }
    var existingSyntax = syntaxCache[lang];
    if (existingSyntax) {
      Syntax = existingSyntax;
    }
    else {
      var get = function get(key) {
        return langTable.get(lang, 'syntax', key);
      };
      Syntax = syntaxCache[lang] = {
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
    populateExpressionKeywords();
    populateSyntaxKeywords();
    currentLang = lang;
  }

  var expressionKeywords = null;
  function populateExpressionKeywords () {
    expressionKeywords = 
      [Syntax['quote'],
       Syntax['lambda'],
       Syntax['if'],
       Syntax['set!'],
       Syntax['begin'],
       Syntax['cond'],
       Syntax['and'],
       Syntax['or'],
       Syntax['case'],
       Syntax['let'],
       Syntax['let*'],
       Syntax['letrec'],
       Syntax['do'],
       Syntax['delay'],
       Syntax['quasiquote']];
  }
  function isExpressionKeyword(token) {
    return token.type === TokenTypes.identifier &&
      expressionKeywords.indexOf(token.value) !== -1;
  }
  var syntaticKeywords = null;
  function populateSyntaxKeywords() {
    syntaticKeywords = 
      [Syntax['else'],
       Syntax['=>'],
       Syntax['define'],
       Syntax['unquote'], 
       Syntax['unquote-splicing']];
  }

  function createDefinition(identifier, value) {
    return {
      type: FormTypes.definition,
      identifier: identifier,
      value: value
    };
  }
  function createAssignment(identifier, value) {
    return {
      type: FormTypes.assignment,
      identifier: identifier,
      value: value
    };
  }
  function createConditional(test, consequent, alternate) {
    return {
      type: FormTypes.conditional,
      test: test,
      consequent: consequent,
      alternate: alternate
    };
  }
  function createProcedureCall(procedure, args) {
    return {
      type: FormTypes.procedureCall,
      procedure: procedure,
      arguments: args
    };
  }
  function createLambda(formals, body) {
    return {
      type: FormTypes.lambda,
      formals: formals,
      body: body
    };
  }
  function createConjunction(tests) {
    return {
      type: FormTypes.conjunction,
      tests: tests
    };
  }
  function createDisjunction(tests) {
    return {
      type: FormTypes.disjunction,
      tests: tests
    };
  }
  function createVariable(identifier) {
    return {
      type: FormTypes.variable,
      identifier: identifier
    };
  }
  function createLiteral(type, value) {
    return {
      type: FormTypes.literal,
      value: {
        type: type,
        value: value
      }
    };
  }
  function createBegin(forms) {
    return {
      type: FormTypes.begin,
      forms: forms
    };
  }

  function isSyntaticKeyword(token) {
    return isExpressionKeyword(token) ||
      (token.type === TokenTypes.identifier &&
      syntaticKeywords.indexOf(token.value) !== -1);
  }
  function isIdentifier(token) {
    return token.type === TokenTypes.identifier;
  }
  function isVariable(token) {
    return isIdentifier(token) && !isSyntaticKeyword(token);
  }

  function raiseError(tokenStream, messageKey, messageParams) {
    var message = langTable.get(currentLang, 'syntax-errors', messageKey);
    if (messageParams) {
      message = String.format(message, messageParams);
    }
    var err = new Error(message);
    var position = tokenStream.getPosition();
    err.line = position.line;
    err.column = position.column;
    err.toString = function() {
      return Error.prototype.toString.call(this) +
        ' Line: ' + this.line + ', column: ' + this.column + '.';
    };
    throw err;
  }

  function analyze(text, lang) {
    lang = lang || 'en';
    populateSyntax(lang, lang);
    var tokenStream = new lexer.TokenStream(text, lang);
    return readProgram(tokenStream);
  }
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
  function readConditional(tokenStream) {
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
    return createConditional(test, consequent, alternate);
  }
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
      if (formals.length === 0) {
        raiseError(tokenStream, 'lambda_formals_variable_expected');
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
  function readLambdaBody(tokenStream) {
    return readBody(tokenStream, 'no_lambda_body_exprs');
  }
  function markTailContext(expression) {
    if (expression.type === FormTypes.conditional) {
      markTailContext(expression.consequent);
      if (expression.alternate) {
        markTailContext(expression.alternate);
      }
    }
    else if (expression.type === FormTypes.conjunction ||
      expression.type === FormTypes.disjunction) {
      var tests = expression.tests;
      if (tests.length > 0) {
        markTailContext(tests[tests.length - 1]);
      }
    }
    else if (expression.type === FormTypes.procedureCall) {
      var procedure = expression.procedure;
      // tail 'let' expression
      if (procedure.type === FormTypes.lambda) {
        markTailContext(procedure.body[procedure.body.length - 1]);
      }
      else {
        expression.tail = true;
      }
    }
  }
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
    markTailContext(body[body.length - 1]);
    return createLambda(formals, body);
  }
  function isSelfEvaluating(token) {
    var tokenType = token.type;
    return tokenType === TokenTypes.boolean ||
       tokenType === TokenTypes.number ||
       tokenType === TokenTypes.character ||
       tokenType === TokenTypes.string;
  }
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
      if (isIdentifier(nextToken) && nextToken.value === Syntax['define']) {
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
          markTailContext(body[body.length - 1]);
          return createDefinition(header.identifier, createLambda(header.formals, body));
        }
      }
    }
    return null;
  }
  function readAssignment(tokenStream) {
    return readValueAssignment(tokenStream, createAssignment, langTable.get(currentLang, 'syntax-common', 'assignment')); // TODO cache
  }
  function readBoolOperations(tokenStream, syntaxKey, constructor) {
    var tests = []
    var expression = readExpression(tokenStream);
    while (expression) {
      tests.push(expression);
      expression = readExpression(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [Syntax[syntaxKey]]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [Syntax[syntaxKey]]);
    }
    return constructor(tests);
  }
  function readConjunction(tokenStream) {
    return readBoolOperations(tokenStream, 'and', createConjunction);
  }
  function readDisjunction(tokenStream) {
    return readBoolOperations(tokenStream, 'or', createDisjunction);
  }
  function guardLetBinding(tokenStream, token) {
    if (!token) {
      raiseError(tokenStream, 'let_binding_end_unexpected');
    }
  }
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
  function readBindings(tokenStream) {
    var bindings = [];
    var binding = readBinding(tokenStream);
    while (binding) {
      bindings.push(binding);
      binding = readBinding(tokenStream);
    }
    return bindings;
  }
  function readLetBody(tokenStream) {
    return readBody(tokenStream, 'no_let_body_exprs');
  }
  function guardLet(tokenStream, token) {
    if (!token) {
      raiseError(tokenStream, 'let_end_unexpected');
    }
  }
  function readLet(tokenStream) {
    var token = tokenStream.advance();
    guardLet(tokenStream, token);
    var name;
    var bindings;
    if (isVariable(token)) {
      name = token.value;
      token = tokenStream.advance();
      guardLet(tokenStream, token);
    }
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

    var bindingsCount = bindings.length;
    var formals = new Array(bindingsCount);
    var inits = new Array(bindingsCount);
    for (var i = 0; i < bindingsCount; i++) {
      formals[i] = bindings[i].variable;
      inits[i] = bindings[i].init;
    }
    var procedure = createLambda(formals, body);
    if (!name) {
      return createProcedureCall(procedure, inits);
    }
    else {
      var lambdaDefinition = createDefinition(name, procedure);
      var lambdaApplication = createProcedureCall(createVariable(name), inits);
      var enclosingProcedure = createLambda([], [lambdaDefinition, lambdaApplication]);
      return createProcedureCall(enclosingProcedure, []);
    }
  }
  function readBegin(tokenStream) {
    var forms = [];
    var expression = readExpression(tokenStream);
    if (!expression) {
      raiseError(tokenStream, 'no_begin_exprs');
    }
    while (expression) {
      forms.push(expression);
      expression = readExpression(tokenStream);
    }
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [Syntax['begin']]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [Syntax['begin']]);
    }
    return createBegin(forms);
  }
  var simpleDatums = [
    TokenTypes.boolean,
    TokenTypes.number,
    TokenTypes.character,
    TokenTypes.string,
    TokenTypes.identifier
    // TODO byte vector
  ];
  function readSimpleDatum(tokenStream) {
    var token = tokenStream.peek();
    if (simpleDatums.indexOf(token.type) !== -1) {
      tokenStream.advance();
      return createLiteral(token.type, token.value);
    }
    return null;
  }
  function readListDatum(tokenStream) {
    var listItems = [];
    var literal = readDatum(tokenStream);
    while (literal) {
      listItems.push(literal);
      literal = readDatum(tokenStream);
    }
    token = tokenStream.advance();
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
  function readVectorDatum(tokenStream) {
    var vectorItems = [];
    var literal = readDatum(tokenStream);
    while (literal) {
      vectorItems.push(literal);
      literal = readDatum(tokenStream);
    }
    token = tokenStream.advance();
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
  function readDatum(tokenStream, raise) {
    var datum = readSimpleDatum(tokenStream) || readCompoundDatum(tokenStream);
    if (!datum && raise) {
      raiseError(tokenStream, 'invalid_datum');
    }
    return datum;
  }
  function readData(tokenStream) {
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'datum_end_unexpected');
    }
    return readDatum(tokenStream, true);
  }
  function readQuote(tokenStream) {
    var datum = readDatum(tokenStream);
    var token = tokenStream.advance();
    if (!token) {
      raiseError(tokenStream, 'expr_end_unexpected', [Syntax['quote']]);
    }
    if (token.type !== TokenTypes.rightParen) {
      raiseError(tokenStream, 'right_paren_expected', [Syntax['quote']]);
    }
    return datum;
  }
  function readExpression(tokenStream) {
    var token = tokenStream.peek();
    var nextToken;
    if (!token) {
      return null;
    }
    if (isIdentifier(token)) {
      if (!isSyntaticKeyword(token)) {
        tokenStream.advance();
        return createVariable(token.value);
      }
      else {
        raiseError(tokenStream, 'syntax_keywords_as_variables');
      }
    }
    if (isSelfEvaluating(token)) {
      tokenStream.advance();
      return createLiteral(token.type, token.value);
    }
    if (token.type === TokenTypes.vectorParen) {
      tokenStream.advance();
      return readVectorDatum(tokenStream);
    }
    if (token.type === TokenTypes.quote) {
      tokenStream.advance();
      return readDatum(tokenStream);
    }
    if (token.type === TokenTypes.leftParen) {
      tokenStream.advance();
      nextToken = tokenStream.peek();
      if (!nextToken) {
        raiseError(tokenStream, 'list_end_unexpected');
      }
      if (isIdentifier(nextToken)) {
        if (nextToken.value === Syntax['if']) {
          tokenStream.advance();
          return readConditional(tokenStream);
        }
        if (nextToken.value === Syntax['lambda']) {
          tokenStream.advance();
          return readLambda(tokenStream);
        }
        if (nextToken.value === Syntax['set!']) {
          tokenStream.advance();
          return readAssignment(tokenStream);
        }
        if (nextToken.value === Syntax['and']) {
          tokenStream.advance();
          return readConjunction(tokenStream);
        }
        if (nextToken.value === Syntax['or']) {
          tokenStream.advance();
          return readDisjunction(tokenStream);
        }
        if (nextToken.value === Syntax['let']) {
          tokenStream.advance();
          return readLet(tokenStream);
        }
        if (nextToken.value === Syntax['begin']) {
          tokenStream.advance();
          return readBegin(tokenStream);
        }
        if (nextToken.value === Syntax['quote']) {
          tokenStream.advance();
          return readQuote(tokenStream);
        }
      }
      if (nextToken.type === TokenTypes.leftParen || isVariable(nextToken)) {        
        return readProcedureCall(tokenStream);
      }
      raiseError(tokenStream, 'unknown_expr');
    }
    return null;
  }
  function readCommandOrDefinition(tokenStream) {
    return readDefintion(tokenStream) || readExpression(tokenStream);
  }
  function readProgram(tokenStream) {
    var forms = [];
    var form = readCommandOrDefinition(tokenStream);
    while (form) {
      forms.push(form);
      form = readCommandOrDefinition(tokenStream);
    }
    return forms;
  }

  return {
    analyze: analyze,
    FormTypes: FormTypes
  };
}));