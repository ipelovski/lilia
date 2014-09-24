var tests = require('../test-fns');

var testOK = tests.testOK, testFail = tests.testFail;

module.exports = {
  'pair procedures': {
    'cons': {
      'should raise error for calling with wrong number of args':
        testFail('(cons 1)', 'Expected 2 arguments, but got 1'),
      'should return a pair':
        testOK('(cons 1 2)', '(1 . 2)'),
    },

    'make-list': {
      'should raise error for calling with too little args':
        testFail('(make-list)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(make-list 1 1 1)', 'Expected at most 2 arguments, but got 3'),
      'should raise error for calling with wrong type of args':
        testFail('(make-list (quote (1 2)))', 'Expected argument on position 0 to satisfy predicate nonnegative-integer?.'),
      'should create a vector with given length':
        testOK('(length (make-list 5))', 5),
      'should create a vector with default fill 0':
        testOK('(list-ref (make-list 5) 2)', 0),
      'should create a vector with the given fill':
        testOK('(list-ref (make-list 5 3) 2)', 3),
    },

    'pair?': {
      'should raise error for calling with wrong number of args':
        testFail('(pair?)', 'Expected 1 arguments, but got 0'),
      'should return true for a pair':
        testOK('(pair? (cons 1 2))', true),
      'should return false for a non pair':
        testOK('(pair? 1)', false),
    },

    'car': {
      'should raise error for calling with wrong number of args':
        testFail('(car)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(car 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the first element in a pair':
        testOK('(car (cons 1 2))', 1),
    },

    'cdr': {
      'should raise error for calling with wrong number of args':
        testFail('(cdr)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(cdr 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the second element in a pair':
        testOK('(cdr (cons 1 2))', 2),
    },

    'set-car!': {
      'should raise error for calling with wrong number of args':
        testFail('(set-car!)', 'Expected 2 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(set-car! (list) 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should set the first element in a pair':
        testOK('(define a (cons 1 2))(set-car! a 3) a', '(3 . 2)'),
    },

    'set-cdr!': {
      'should raise error for calling with wrong number of args':
        testFail('(set-cdr!)', 'Expected 2 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(set-cdr! (list) 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should set the second element in a pair':
        testOK('(define a (cons 1 2))(set-cdr! a 3) a', '(1 . 3)'),
    },

    'caar': {
      'should raise error for calling with wrong number of args':
        testFail('(caar)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(caar 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with with a cons with non pair car':
        testFail('(caar (cons 1 (cons 2 3)))', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the first element in a pair':
        testOK('(caar \'((a . b) . (c . d)))', 'a'),
    },

    'cadr': {
      'should raise error for calling with wrong number of args':
        testFail('(cadr)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(cadr 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with with a cons with non pair car':
        testFail('(cadr (cons (cons 2 3) 1))', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the first element in a pair':
        testOK('(cadr \'((a . b) . (c . d)))', 'c'),
    },

    'cdar': {
      'should raise error for calling with wrong number of args':
        testFail('(cdar)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(cdar 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with with a cons with non pair car':
        testFail('(cdar (cons 1 (cons 2 3)))', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the first element in a pair':
        testOK('(cdar \'((a . b) . (c . d)))', 'b'),
    },

    'cddr': {
      'should raise error for calling with wrong number of args':
        testFail('(cddr)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(cddr 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with with a cons with non pair car':
        testFail('(cddr (cons (cons 2 3) 1))', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should return the first element in a pair':
        testOK('(cddr \'((a . b) . (c . d)))', 'd'),
    },

    'null?': {
      'should raise error for calling with wrong number of args':
        testFail('(null?)', 'Expected 1 arguments, but got 0'),
      'should return true for the empty list':
        testOK('(null? (list))', true),
      'should return false for something different from the empty list':
        testOK('(null? (list 1))', false),
    },

    'list?': {
      'should raise error for calling with wrong number of args':
        testFail('(list?)', 'Expected 1 arguments, but got 0'),
      'should return true for a proper list':
        testOK('(list? (list 1 2 3))', true),
      'should return false for a non proper list':
        testOK('(list? (cons 1 (cons 2 3)))', false),
      'should return true for the empty list':
        testOK('(list? (list))', true),
      'should return false for a non list':
        testOK('(list? (vector))', false),
    },

    'length': {
      'should raise error for calling with wrong number of args':
        testFail('(length)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(length #(1 2 3))', 'Expected argument on position 0 to satisfy predicate list?.'),
      'should raise error for calling with non proper list':
        testFail('(length \'(a . (b . c)))', 'Expected argument on position 0 to satisfy predicate list?.'),
      'should return length for a literal list':
        testOK('(length (quote (1 2 3)))', 3),
      'should return length for a new list':
        testOK('(length (list 1 2 3))', 3),
      'should return length for the empty list':
        testOK('(length (list))', 0),
    },

    'list-ref': {
      'should raise error for calling with wrong number of args':
        testFail('(list-ref)', 'Expected 2 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(list-ref #(1 2) 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with wrong type of args 2':
        testFail('(list-ref (list 1 2) #t)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?.'),
      'should raise error for accessing invalid index':
        testFail('(list-ref (list 1 2) 2)', 'Index is out of range. The length of the list is 2.'),
      'should raise error for indexing a non-pair':
        testFail('(list-ref (quote (a . (b . c))) 2)', 'Index is out of range. A non-pair is reached at position 2.'),
      'should return k-th element of a literal list':
        testOK('(list-ref (quote (1 2 3)) 1)', 2),
      'should return k-th element of a new list':
        testOK('(list-ref (list 1 2 3) 1)', 2),
    },

    'list-set!': {
      'should raise error for calling with wrong number of args':
        testFail('(list-set!)', 'Expected 3 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(list-set! #(1 2) 1 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with wrong type of args 2':
        testFail('(list-set! (list 1) #t 1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?.'),
      'should raise error for accessing invalid index':
        testFail('(list-set! (list 1 2) 2 3)', 'Index is out of range. The length of the list is 2.'),
      'should raise error for indexing a non-pair':
        testFail('(list-set! (quote (a . (b . c))) 2 3)', 'Index is out of range. A non-pair is reached at position 2.'),
      'should raise error for mutating an immutable list':
        testFail('(list-set! (quote (1 2 3)) 2 4)', 'Cannot mutate an immutable object.'),
      'should set k-th element of a list':
        testOK('(let ((l (list 1 2 3)))(list-set! l 2 4) l)', '(1 2 4)'),
    },

    'append': {
      // TODO check for the exceptions
      'should return empty list for no arguments':
        testOK('(append)', "'()"),
      'should return the only argument for one argument':
        testOK('(append 1)', 1),
      'should return concatenated lists for 2 list arguments':
        testOK('(append \'(x) \'(y))', '(x y)'),
      'should return concatenated lists for 3 list arguments':
        testOK('(append \'(x) \'(y) \'(z))', '(x y z)'),
      'sample 1':
        testOK('(append \'(a) \'(b c d))', '(a b c d)'),
      'sample 2':
        testOK('(append \'(a (b)) \'((c)))', '(a (b) (c))'),
      'sample 3':
        testOK('(append \'(a b) \'(c . d))', '(a b c . d)'),
      'sample 4':
        testOK('(append \'() \'a)', 'a'),
    },

    'reverse': {
      'should raise error for calling with wrong number of args':
        testFail('(reverse)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(reverse #(1 2))', 'Expected argument on position 0 to satisfy predicate list?.'),
      'should raise error for calling with inproper list':
        testFail('(reverse \'(1 . (2 . (3 . 4))))', 'Expected argument on position 0 to satisfy predicate list?.'),
      'should return a reversed list':
        testOK('(reverse (list 1 2 3))', '(3 2 1)'),
    },

    'list-tail': {
      'should raise error for calling with wrong number of args':
        testFail('(list-tail)', 'Expected 2 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(list-tail #(1 2) 1)', 'Expected argument on position 0 to satisfy predicate pair?.'),
      'should raise error for calling with wrong type of args 2':
        testFail('(list-tail (list 1 2) #t)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?.'),
      'should raise error for accessing invalid index':
        testFail('(list-tail (list 1 2) 2)', 'Index is out of range. The length of the list is 2.'),
      'should raise error for indexing a non-pair':
        testFail('(list-tail (quote (a . (b . c))) 2)', 'Index is out of range. A non-pair is reached at position 2.'),
      'should return k-th sublist of a literal list':
        testOK('(list-tail (quote (1 2 3)) 1)', '(2 3)'),
      'should return k-th sublist of a new list':
        testOK('(list-tail (list 1 2 3) 1)', '(2 3)'),
    },
  }
};