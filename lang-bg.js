(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lang-table'));
  } else {
    root['lang-bg'] = factory(root.langTable);
  }
}(this, function (langTable) {
  var data = {
    'common': {
      'missing_entry': 'Липсва превод за език "{0}", кетегория "{1}", ключ "{2}".',
    },
    'syntax': {
      'define': 'определи',
      'set!': 'смени!',
      'lambda': 'функция',
      'if': 'ако',
      'quote': 'цитирай',
      'begin': 'почни',
      'cond': 'условие',
      'and': 'и',
      'or': 'или',
      'case': 'случай',
      'let': 'нека',
      'let*': 'нека*',
      'letrec': 'нека-възвратно',
      'do': 'прави',
      'delay': 'отложи',
      'quasiquote': 'почтицитирай',
      'else': 'иначе',
      '=>': '=>',
      'unquote': 'нецитирай',
      'unquote-splicing': 'нецитирай-съединяване',
    },

    'syntax-common': {
      'definition': 'определение',
      'assignment': 'присвояване'
    },

    'syntax-errors': {
      'if_expression_end_expected': 'Expected end of "if" expression.',
      'invalid': 'Invalid conditional "if".',
      'if_expr_end_unexpected': 'Unexpected end of "if" expression.',
      'proc_call_end_unexpected': 'Unexpected end of a procedure call. Expected a right parentheses.',
      'invalid_proc_call': 'Invalid procedure call. Expected a right parentheses.',
      'lambda_expr_end_unexpected': 'Unexpected end of a lambda expression.',
      'lambda_formals_end_unexpected': 'Unexpected end of a lambda formals.',
      'lambda_expr_variable_expected': 'Expected a variable after a dot in lambda formals.',
      'expected_lambda_formals_end': 'Expected a right parentheses to end the lambda formals.',
      'invalid_lambda_formals': 'Expected a list of formals or a single variable.',
      'lambda_formals_variable_expected': 'Expected at least one variable in lambda formals.',
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
      'no_begin_exprs': 'The begin expression needs at least one subexpression.',
      'no_definition_exprs': 'The definition needs at least one subexpression.',
      'definition_formals_end_unexpected': 'Unexpected end of a defintion formals.',
      'definition_expr_variable_expected': 'Expected a variable after a dot in definition formals.',
      'definition_body_end_unexpected': 'Unexpected end of a definition body.',
      'invalid_definition_body_end': 'The definition body should be closed with a right parentheses.',
      'definition_end_unexpected': 'Unexpected end of a definition.',
    },

    'tokens': {
      'true': 'в',
      'false': 'л',
      'newline': 'новред',
      'space': 'интервал',
      'tab': 'табулация',
      'valid_escaped_chars': 'нрт',
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
      'number?': 'число?',
      'boolean?': 'булева?',
      'not': 'не',
      'list': 'списък',
      'cons': 'конс',
      'car': 'първо',
      'cdr': 'останало',
      'vector-ref': 'вектор-вземи',
    },

    'runtime-errors': {
      'number_expected': 'Expected a number but received {0}.',
      'exact_args_count_expected': 'Expected {0} arguments, but got {1}.',
      'min_args_count_expected': 'Expected at least {0} arguments, but got {1}.',
      'unknown_type': 'Unknown literal type "{0}".',
      'wrong_args_count': 'Expected {0} arguments, but received {1}.',
      'wrong_min_args_count': 'Expected at least {0} arguments, but received {1}.',
      'invalid_proc': 'Invalid procedure.',
      'undefined_variable': 'Undefined variable with name "{0}".',
    },
};

  langTable.register('bg', function get(category, key) {
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