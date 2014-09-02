(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lang-table'));
  } else {
    root['lang-en'] = factory(root.langTable);
  }
}(this, function (langTable) {
  var data = {
    'common': {
      'missing_entry': 'Missing translation for language "{0}", category "{1}", key "{2}".',
    },
    'syntax': {
      'define': 'define',
      'set!': 'set!',
      'lambda': 'lambda',
      'if': 'if',
      'quote': 'quote',
      'begin': 'begin',
      'cond': 'cond',
      'and': 'and',
      'or': 'or',
      'case': 'case',
      'let': 'let',
      'let*': 'let*',
      'letrec': 'letrec',
      'do': 'do',
      'delay': 'delay',
      'quasiquote': 'quasiquote',
      'else': 'else',
      '=>': '=>',
      'unquote': 'unquote',
      'unquote-splicing': 'unquote-splicing',
    },

    'syntax-common': {
      'definition': 'definition',
      'assignment': 'assignment'
    },

    'syntax-errors': {
      'if_expression_end_expected': 'Expected end of "if" expression.',
      'if_expr_end_unexpected': 'Unexpected end of "if" expression.',
      'proc_call_end_unexpected': 'Unexpected end of a procedure call. Expected a closing parentheses.',
      'invalid_proc_call': 'Invalid procedure call. Expected a right parentheses.',
      'lambda_expr_end_unexpected': 'Unexpected end of a lambda expression.',
      'lambda_formals_end_unexpected': 'Unexpected end of a lambda formals.',
      'lambda_expr_variable_expected': 'Expected a variable after a dot in lambda formals.',
      'expected_lambda_formals_end': 'Expected a right parentheses to end the lambda formals.',
      'invalid_lambda_formals': 'Expected a list of formals or a single variable.',
      'no_lambda_body_exprs': 'The lambda body needs at least one expression.',
      'lambda_body_end_unexpected': 'Unexpected end of a lambda body.',
      'invalid_lambda_body_end': 'The lambda body should be closed with a right parentheses.',
      'variable_expected': 'Invalid "{0}". Expected a variable.',
      'expr_expected': 'Invalid "{0}". Expected an expression.',
      'expr_end_unexpected': 'Unexpected end of a {0}. Expected a right parentheses.',
      'right_paren_expected': 'Invalid {0}. Expected a right parentheses.',
      'list_end_unexpected': 'Unexpected end of a list.',
      'unknown_expr': 'Unknown expression.',
      'expected_let_variable': 'Expected a variable for a let binding.',
      'expected_let_init': 'Expected an expression for a let binding initialization.',
      'expected_let_binding': 'Expected a let binding. A binding is (variable init-expression).',
      'expected_let_binding_end': 'Expected the end of a let binding.',
      'let_binding_end_unexpected': 'Unexpected end of a let binding.',
      'let_end_unexpected': 'Unexpected end of let expression.',
      'let_bindings_exptected': 'Expected a list of let bindings.',
      'let_body_end_unexpected': 'Unexpected end of a let body.',
      'no_let_body_exprs': 'The let body needs at least one expression.',
      'invalid_let_body_end': 'The let body should be closed with a right parentheses.',
      'no_definition_exprs': 'The definition needs at least one subexpression.',
      'definition_formals_end_unexpected': 'Unexpected end of a defintion formals.',
      'definition_expr_variable_expected': 'Expected a variable after a dot in definition formals.',
      'definition_body_end_unexpected': 'Unexpected end of a definition body.',
      'invalid_definition_body_end': 'The definition body should be closed with a right parentheses.',
      'definition_end_unexpected': 'Unexpected end of a definition.',
      'syntax_keywords_as_variables': 'Syntax keywords cannot be used as variables.',
      'datum_end_unexpected': 'Unexpected end of datum.',
      'datum_expected': 'Expected a datum.',
      'expected_list_end': 'Expected a right parentheses to end the list.',
      'vector_end_unexpected': 'Unexpected end of a vector.',
      'expected_vector_end': 'Expected a right parentheses to end the vector.',
      'invalid_datum': 'Invalid datum representation.',
      'expr_expected_definition_found': 'Expected an expression but found a definition. ' +
        'Definitions should be placed at the beginning of the lambda body.',
      'definition_expected_expr_found': 'Expected a definition but found an expression. ' +
        'Definitions should be placed at the beginning of the lambda body.',
      'duplicate_definitions': 'Found multiple definitions for variable "{0}".',
      'data_not_procedure': 'Simple data cannot be used as a procedure.',
      'cond_else_last': '"else" can be used only at the last part of a "cond" expression.',
    },

    'tokens': {
      'true': 't',
      'false': 'f',
      'newline': 'newline',
      'space': 'space',
      'tab': 'tab',
      'valid_escaped_chars': 'nrt',
    },

    'token-errors': {
      'invalid_token': 'Invalid token.',
      'invalid_char': 'Invalid character.',
      'invalid_identifier': 'Invalid identifier.',
      'invalid_string': 'Invalid string.',
      'invalid_escaped_char': 'Invalid escaped character.',
      'invalid_number': 'Invalid number.',      
    },

    'procedures': {
      '+': '+',
      '-': '-',
      '*': '*',
      '/': '/',
      '=': '=',
      '>': '>',
      '<': '<',
      'number?': 'number?',
      'nonnegative-integer?': 'nonnegative-integer?',
      'boolean?': 'boolean?',
      'not': 'not',
      'list': 'list',
      'cons': 'cons',
      'pair?': 'pair?',
      'car': 'car',
      'cdr': 'cdr',
      'set-car!': 'set-car!',
      'set-cdr!': 'set-cdr!',
      'null?': 'null?',
      'list?': 'list?',
      'vector': 'vector',
      'vector?': 'vector?',
      'vector-length': 'vector-length',
      'vector-ref': 'vector-ref',
      'vector-set!': 'vector-set!',
      'procedure?': 'procedure?',
      'call-with-current-continuation': 'call-with-current-continuation',
      'call/cc': 'call/cc',
      'display': 'display',
      'newline': 'newline',
      'append': 'append',
    },

    'runtime-errors': {
      'number_expected': 'Expected a number but received {0}.',
      'exact_args_count_expected': 'Expected {0} arguments, but got {1}.',
      'min_args_count_expected': 'Expected at least {0} arguments, but got {1}.',
      'unknown_type': 'Unknown literal type "{0}".',
      'invalid_proc': 'Invalid procedure.',
      'undefined_variable': 'Undefined variable with name "{0}".',
      'argument_predicate_false': 'Expected argument on position {0} to satisfy predicate {1}.',
      'mutating_immutable_object': 'Cannot mutate an immutable object.',
      'vector_index_out_range': 'Index is out of range. The length of the vector is {0}.',
    },
  };

  langTable.register('en', function get(category, key) {
    var cat = data[category];
    if (cat) {
      var value = cat[key];
      if (value) {
        return value;
      }
    }
    return null;
  });

  return data;
}));