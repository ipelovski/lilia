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
  var TokenTypes = lexer.TokenTypes;

  // The values used to tag special forms.
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

  // A constructor for an AST node containing a definition.
  function createDefinition(identifier, value) {
    return {
      type: FormTypes.definition,
      identifier: identifier,
      value: value
    };
  }
  // A constructor for an AST node containing an assignment.
  function createAssignment(identifier, value) {
    return {
      type: FormTypes.assignment,
      identifier: identifier,
      value: value
    };
  }
  // A constructor for an AST node containing an 'if' conditional.
  function createConditional(test, consequent, alternate) {
    return {
      type: FormTypes.conditional,
      test: test,
      consequent: consequent,
      alternate: alternate
    };
  }
  // A constructor for an AST node containing a procedure call.
  function createProcedureCall(procedure, args) {
    return {
      type: FormTypes.procedureCall,
      procedure: procedure,
      arguments: args
    };
  }
  // A constructor for an AST node containing a lambda definition.
  function createLambda(formals, body) {
    return {
      type: FormTypes.lambda,
      formals: formals,
      body: body
    };
  }
  // A constructor for an AST node containing an 'and' expression.
  function createConjunction(tests) {
    return {
      type: FormTypes.conjunction,
      tests: tests
    };
  }
  // A constructor for an AST node containing an 'or' expression.
  function createDisjunction(tests) {
    return {
      type: FormTypes.disjunction,
      tests: tests
    };
  }
  // A constructor for an AST node containing a variable evaluation.
  function createVariable(identifier) {
    return {
      type: FormTypes.variable,
      identifier: identifier
    };
  }
  // A constructor for an AST node containing a self-evaluating literal.
  function createLiteral(type, value) {
    return {
      type: FormTypes.literal,
      value: {
        type: type,
        value: value
      }
    };
  }
  // A constructor for an AST node containing a sequence of forms.
  function createBegin(forms) {
    return {
      type: FormTypes.begin,
      forms: forms
    };
  }

  // Raises an error by throwing it.
  // This function is used only in the parser.
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
    markTailContext(body[body.length - 1]);
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
          markTailContext(body[body.length - 1]);
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
  // Reads a 'let' expression
  // from the given token stream.
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
  // Reads a 'begin' expression
  // from the given token stream.
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
  // Reads a vector datum from the given token stream.
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
          return readConditional(tokenStream);
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
        if (nextToken.value === syntax['begin']) {
          tokenStream.advance();
          return readBegin(tokenStream);
        }
        if (nextToken.value === syntax['quote']) {
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
    return forms;
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
    FormTypes: FormTypes
  };
}));