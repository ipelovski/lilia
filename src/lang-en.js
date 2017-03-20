'use strict';

var langTable = require('./lang-table');

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
    'letrec*': 'letrec*',
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
    'cond_clause_end_unexpected': 'Unexpected end of a "cond" clause.',
    'cond_clause_test_expected': 'Expected a test expression for a "cond" clause.',
    'unnecessary_right_paren': 'Unnecessary usage of right parentheses.',
    'bad_syntax': 'Bad syntax.',
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
    'invalid_token': 'Invalid content.',
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
    'exp': 'exp',
    'log': 'log',
    'sin': 'sin',
    'cos': 'cos',
    'tan': 'tan',
    'asin': 'asin',
    'acos': 'acos',
    'atan': 'atan',
    'sqrt': 'sqrt',
    'expt': 'expt',

    'boolean?': 'boolean?',
    'not': 'not',
    'boolean=?': 'boolean=?',
    'list': 'list',
    'make-list': 'make-list',
    'cons': 'cons',
    'pair?': 'pair?',
    'car': 'car',
    'cdr': 'cdr',
    'set-car!': 'set-car!',
    'set-cdr!': 'set-cdr!',
    'caar': 'caar',
    'cadr': 'cadr',
    'cdar': 'cdar',
    'cddr': 'cddr',
    'null?': 'null?',
    'list?': 'list?',
    'length': 'length',
    'list-ref': 'list-ref',
    'list-set!': 'list-set!',
    'append': 'append',
    'reverse': 'reverse',
    'list-tail': 'list-tail',
    'memq': 'memq',
    'memv': 'memv',
    'member': 'member',
    'assq': 'assq',
    'assv': 'assv',
    'assoc': 'assoc',
    'list-copy': 'list-copy',

    'vector': 'vector',
    'make-vector': 'make-vector',
    'vector?': 'vector?',
    'vector-length': 'vector-length',
    'vector-ref': 'vector-ref',
    'vector-set!': 'vector-set!',
    'vector->list': 'vector->list',
    'list->vector': 'list->vector',
    'vector->string': 'vector->string',
    'string->vector': 'string->vector',
    'vector-copy': 'vector-copy',
    'vector-copy!': 'vector-copy!',
    'vector-append': 'vector-append',
    'vector-fill!': 'vector-fill!',

    'procedure?': 'procedure?',
    'call-with-current-continuation': 'call-with-current-continuation',
    'call/cc': 'call/cc',
    'write': 'write',
    'write-char': 'write-char',
    'write-string': 'write-string',
    'display': 'display',
    'newline': 'newline',
    'eq?': 'eq?',
    'eqv?': 'eqv?',
    'equal?': 'equal?',
    'string?': 'string?',
    'char?': 'char?',
    'make-string': 'make-string',
    'string': 'string',
    'string-length': 'string-length',
    'string-ref': 'string-ref',
    'string-set!': 'string-set!',
    'string=?': 'string=?',
    'string-append': 'string-append',
    'substring': 'substring',
    'js-eval': 'js-eval',
    'raise': 'raise',
    'apply': 'apply',
  },

  'runtime-errors': {
    'number_expected': 'Expected a number but received {0}.',
    'exact_args_count_expected': 'Expected {0} arguments, but got {1}.',
    'min_args_count_expected': 'Expected at least {0} arguments, but got {1}.',
    'max_args_count_expected': 'Expected at most {0} arguments, but got {1}.',
    'invalid_proc': 'Invalid procedure.',
    'undefined_variable': 'Undefined variable with name "{0}".',
    'argument_predicate_false': 'Expected argument on position {0} to satisfy predicate {1}.',
    'mutating_immutable_object': 'Cannot mutate an immutable object.',
    'vector_index_out_range': 'Index is out of range. The length of the vector is {0}.',
    'list_index_out_range': 'Index is out of range. The length of the list is {0}.',
    'list_index_reached_non_pair': 'Index is out of range. A non-pair is reached at position {0}.',
    'string_index_out_range': 'Index is out of range. The length of the string is {0}.',
    'maximum_stack_size_exceeded': 'Too much recursive calls with non-tail calls.',
    'start_index_greater_end_index': 'The start index is greater than the end index.',
    'vector_argument_predicate_false': 'Expected the elements of the vector from index {0} to {1} to satisfy predicate {2}.',
    'vector_copy_space_needed': 'Not enough space to copy elements from the second vector to the first. ' +
      'The amount of elements to copy is {0} and the available space is for {1} elements.',
    'improper_list': 'The given list is improper, it does not end with the empty list.',
    'improper_alist': 'The given association list is improper, it contains non-pair elements.',
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

module.exports = data;