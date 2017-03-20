'use strict';

var langTable = require('./lang-table');

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
    'letrec': 'некарек',
    'letrec*': 'некарек*',
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
    'if_expression_end_expected': 'Очаква се края на израза "ако".',
    'if_expr_end_unexpected': 'Неочакван край на израза "ако".',
    'proc_call_end_unexpected': 'Неочакван край на извикване на функция. Очаква се затваряща скоба.',
    'invalid_proc_call': 'Невалидно извикване на функция. Очаква се затваряща скоба.',
    'lambda_expr_end_unexpected': 'Неочакван край на израза "функция"',
    'lambda_formals_end_unexpected': 'Неочакван край на формалните параметри на функцията.',
    'lambda_expr_variable_expected': 'Очаква се променлива след точката в списъка с формални параметри.',
    'expected_lambda_formals_end': 'Очаква се затваряща скоба след края на формалните параметри.',
    'invalid_lambda_formals': 'Очаква се списък с формални параметри или единствена променлива.',
    'no_lambda_body_exprs': 'Тялото на функцията трябва да има поне един израз.',
    'lambda_body_end_unexpected': 'Неочакван край на тялото на функцията.',
    'invalid_lambda_body_end': 'Тялото на функцията трябва да завършва със затваряща скоба.',
    'variable_expected': 'Невалидно "{0}". Очаква се променлива.',
    'expr_expected': 'Невалидно "{0}". Очаква се израз.',
    'expr_end_unexpected': 'Неочакван край на {0}. Очаква се затваряща скоба.',
    'right_paren_expected': 'Невалидно {0}. Очаква се затваряща скоба.',
    'list_end_unexpected': 'Неочакван край на списъка.',
    'unknown_expr': 'Непознат израз.',
    'expected_let_variable': 'Очаква се променлива за израза "нека".',
    'expected_let_init': 'Очаква се израз при инициализирането на променливата.',
    'expected_let_binding': 'Очаква се инициализация за израза "нека". Инициализацията трябва да е във вида (променлива израз).',
    'expected_let_binding_end': 'Очаква се края на инициализацията за израза "нека".',
    'let_binding_end_unexpected': 'Неочакван край на инициализацията за израза "нека".',
    'let_end_unexpected': 'Неочакван край на израза "нека".',
    'let_bindings_exptected': 'Очаква се списък от инициализации на израза "нека".',
    'let_body_end_unexpected': 'Неочакван край на тялото на израза "нека".',
    'no_let_body_exprs': 'Изразът "нека" трябва да има поне един израз в тялото си.',
    'invalid_let_body_end': 'Тялото на израза "нека" трябва да завършва със затваряща скоба.',
    'no_definition_exprs': 'Определението трябва да съдържа поне един подизраз.',
    'definition_formals_end_unexpected': 'Неочакван край на формалните параметри на определението.',
    'definition_expr_variable_expected': 'Очаква се променлива след точката в списъка с формални параметри.',
    'definition_body_end_unexpected': 'Неочакван край на тялото на определението.',
    'invalid_definition_body_end': 'Тялото на определението трябва да завършва със затваряща скоба.',
    'definition_end_unexpected': 'Неочакван край на определението.',
    'syntax_keywords_as_variables': 'Синтактичните ключови думи не могат да се използват като променливи..',
    'datum_end_unexpected': 'Неочакван край на данните.',
    'datum_expected': 'Очакват се данни.',
    'expected_list_end': 'Очаква се списъка да завършва със затваряща скоба.',
    'vector_end_unexpected': 'Неочакван край на вектор.',
    'expected_vector_end': 'Очаква се вектора да завършва със затваряща скоба.',
    'invalid_datum': 'Невалидно представяне на данните.',
    'expr_expected_definition_found': 'Очаква се израз но е открито определение. ' +
      'Опрделенията трябва да се поставят в началото на тялото на фукция.',
    'definition_expected_expr_found': 'Очаква се определение но е открит израз. ' +
      'Опрделенията трябва да се поставят в началото на тялото на фукция.',
    'duplicate_definitions': 'Открити са повече от едно определения за променлива "{0}".',
    'data_not_procedure': 'Прости данни не могат да се използват като фукция.',
    'cond_else_last': '"иначе" може да се изполва единствено в последната част на израза "условие".',
    'unnecessary_right_paren': 'Ненужно използване на затваряща скоба.',
    'bad_syntax': 'Грешен синтаксис.',
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
    'invalid_token': 'Невалидно съдържание.',
    'invalid_char': 'Невалиден знак.',
    'invalid_identifier': 'Невалиден символ.',
    'invalid_string': 'Невалиден низ от знаци.',
    'invalid_escaped_char': 'Невалиден знак.',
    'invalid_number': 'Невалидно число.',
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
    'nonnegative-integer?': 'неотрицателно-цяло-число?',
    'exp': 'експ',
    'log': 'лог',
    'sin': 'син',
    'cos': 'кос',
    'tan': 'тан',
    'asin': 'асин',
    'acos': 'акос',
    'atan': 'атан',
    'sqrt': 'квкорен',
    'expt': 'степен',

    'boolean?': 'булева?',
    'not': 'не',
    'boolean=?': 'булеви=?',
    'list': 'списък',
    'make-list': 'създай-списък',
    'cons': 'конс',
    'pair?': 'двойка?',
    'car': 'първо',
    'cdr': 'второ',
    'set-car!': 'смени-първо!',
    'set-cdr!': 'смени-второ!',
    'caar': 'пърпърво',
    'cadr': 'първторо',
    'cdar': 'втопърво',
    'cddr': 'втовторо',
    'null?': 'нищо?',
    'list?': 'списък?',
    'length': 'дължина',
    'list-ref': 'списък-вземи',
    'list-set!': 'списък-смени!',
    'append': 'добави',
    'reverse': 'обърни',
    'list-tail': 'списък-остатък',
    'memq': 'част-ра',
    'memv': 'част-рав',
    'member': 'част',
    'assq': 'елемент-ра',
    'assv': 'елемент-рав',
    'assoc': 'елемент',
    'list-copy': 'списък-повтори',

    'vector': 'вектор',
    'make-vector': 'създай-вектор',
    'vector?': 'вектор?',
    'vector-length': 'вектор-дължина',
    'vector-ref': 'вектор-вземи',
    'vector-set!': 'вектор-смени!',
    'vector->list': 'вектор->списък',
    'list->vector': 'списък->вектор',
    'vector->string': 'вектор->низ',
    'string->vector': 'низ->вектор',
    'vector-copy': 'вектор-повтори',
    'vector-copy!': 'вектор-повтори!',
    'vector-append': 'вектор-добави',
    'vector-fill!': 'вектор-попълни!',

    'procedure?': 'функция?',
    'call-with-current-continuation': 'извикай-със-сегашното-продължение',
    'call/cc': 'извикай/сп',
    'write': 'напиши',
    'write-char': 'напиши-знак',
    'write-string': 'напиши-низ',
    'display': 'покажи',
    'newline': 'новред',
    'eq?': 'ра?',
    'eqv?': 'рав?',
    'equal?': 'равно?',
    'string?': 'низ?',
    'char?': 'знак?',
    'make-string': 'създай-низ',
    'string': 'низ',
    'string-length': 'низ-дължина',
    'string-ref': 'низ-вземи',
    'string-set!': 'низ-смени!',
    'string=?': 'низ=?',
    'string-append': 'низ-добави',
    'substring': 'подниз',
    'js-eval': 'джс-изчисли',
    'raise': 'предизвикай',
    'apply': 'приложи',
  },

  'runtime-errors': {
    'number_expected': 'Очаква се число но е подадено {0}.',
    'exact_args_count_expected': 'Очакват се {0} на брой аргументи, но са подадени {1}.',
    'min_args_count_expected': 'Очакват се поне {0} на брой аргументи, но са подадени {1}.',
    'max_args_count_expected': 'Очакват се не повече от {0} на брой аргументи, но са подадени {1}.',
    'invalid_proc': 'Невалидна функция.',
    'undefined_variable': 'Неопределена променлива с име "{0}".',
    'argument_predicate_false': 'Очаква се аргументът на позиция {0} да изпълнява условието {1}.',
    'mutating_immutable_object': 'Неизменими данни не могат да се променят..',
    'vector_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на вектора, която е {0}.',
    'list_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на списъка, която е {0}.',
    'list_index_reached_non_pair': 'Индексът е извън позволения обхват. На позиция {0} е намерена стойност, която не е двойка.',
    'string_index_out_range': 'Индексът е извън позволения обхват. Трябва да е между 0 и дължината на низа, която е {0}.',
    'maximum_stack_size_exceeded': 'Твърде много рекурсивни извиквания без да се използват крайни извиквания.',
    'start_index_greater_end_index': 'Началният индекс е по-голям от крайния.',
    'vector_argument_predicate_false': 'Очаква се елементите на вектора от позиция {0} до позиция {1} да изпълняват условието {2}.',
    'vector_copy_space_needed': 'Няма достатъчно място, за да се запишат елементите от втория вектор в първия. ' +
      'Броят на елементите, които трябва да се запишат, е {0}, а свободното място е за {1} елемента.',
    'improper_list': 'Подаденият списък е неправилен, той не завършва с празен списък.',
    'improper_alist': 'Подаденият асоциативен списък е неправилен, той съдържа елементи, които не са двойки.',
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

module.exports = data;