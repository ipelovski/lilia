var tests = require('../test-fns');

var testOK = tests.testOK, testFail = tests.testFail;

module.exports = {
  'vector procedures': {

    'vector?': {
      'should raise error for calling with wrong number of args':
        testFail('(vector?)', 'Expected 1 arguments, but got 0'),
      'should return true for a literal vector':
        testOK('(vector? #(1 2 3))', true),
      'should return true for a new vector':
        testOK('(vector? (vector 1 2 3))', true),
      'should return false for a non vector':
        testOK('(vector? #t)', false),
    },

    'make-vector': {
      'should raise error for calling with too little args':
        testFail('(make-vector)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(make-vector 1 1 1)', 'Expected at most 2 arguments, but got 3'),
      'should raise error for calling with wrong type of args':
        testFail('(make-vector (quote (1 2)))', 'Expected argument on position 0 to satisfy predicate nonnegative-integer?.'),
      'should create a vector with given length':
        testOK('(vector-length (make-vector 5))', 5),
      'should create a vector with default fill 0':
        testOK('(vector-ref (make-vector 5) 2)', 0),
      'should create a vector with the given fill':
        testOK('(vector-ref (make-vector 5 3) 2)', 3),
    },

    'vector-length': {
      'should raise error for calling with wrong number of args':
        testFail('(vector-length)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(vector-length (quote (1 2)))', 'Expected argument on position 0 to satisfy predicate vector?.'),
      'should return length for a literal vector':
        testOK('(vector-length #(1 2 3))', 3),
      'should return length for a new vector':
        testOK('(vector-length (vector 1 2 3))', 3),
    },

    'vector-ref': {
      'should raise error for calling with wrong number of args':
        testFail('(vector-ref)', 'Expected 2 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(vector-ref (quote (1 2)) 1)', 'Expected argument on position 0 to satisfy predicate vector?.'),
      'should raise error for calling with wrong type of args 2':
        testFail('(vector-ref #(1 2) #t)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?.'),
      'should raise error for accessing invalid index':
        testFail('(vector-ref #(1 2) 2)', 'Index is out of range.'),
      'should return k-th element of a literal vector':
        testOK('(vector-ref #(1 2 3) 1)', 2),
      'should return k-th element of a new vector':
        testOK('(vector-ref (vector 1 2 3) 1)', 2),
    },

    'vector-set!': {
      'should raise error for calling with wrong number of args':
        testFail('(vector-set!)', 'Expected 3 arguments, but got 0'),
      'should raise error for calling with wrong type of args':
        testFail('(vector-set! (quote (1 2)) 1 1)', 'Expected argument on position 0 to satisfy predicate vector?.'),
      'should raise error for calling with wrong type of args 2':
        testFail('(vector-set! (vector 1) #t 1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?.'),
      'should raise error for accessing invalid index':
        testFail('(vector-set! (vector 1 2) 2 3)', 'Index is out of range.'),
      'should raise error for mutating an immutable vector':
        testFail('(vector-set! #(1 2 3) 2 4)', 'Cannot mutate an immutable object.'),
      'should set k-th element of a vector':
        testOK('(let ((v (vector 1 2 3)))(vector-set! v 2 4) v)', '#(1 2 4)'),
    },

    'vector->list': {
      'should raise error for calling with too few args':
        testFail('(vector->list)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(vector->list #(1 2 3) 0 1 2)', 'Expected at most 3 arguments, but got 4'),
      'should raise error for passing not a vector':
        testFail('(vector->list \'(1 2 3))', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for start':
        testFail('(vector->list #(1 2 3) -1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(vector->list #(1 2 3) 1 -2)', 'Expected argument on position 2 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing start greater length':
        testFail('(vector->list #(1 2 3) 3)', 'Index is out of range'),
      'should raise error for passing end greater length':
        testFail('(vector->list #(1 2 3) 0 4)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(vector->list #(1 2 3) 2 1)', 'The start index is greater than the end index'),
      'should create list from vector':
        testOK('(vector->list #(1 2 3))', '(1 2 3)'),
      'should create list from vector from start':
        testOK('(vector->list #(1 2 3) 1)', '(2 3)'),
      'should create list from vector from start to end':
        testOK('(vector->list #(1 2 3 4) 1 3)', '(2 3)'),
    },

    'list->vector': {
      'should raise error for calling with too few args':
        testFail('(list->vector)', 'Expected 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(list->vector (list 1 2 3) 0 1 2)', 'Expected 1 arguments, but got 4'),
      'should raise error for passing not a list':
        testFail('(list->vector #(1 2 3))', 'Expected argument on position 0 to satisfy predicate list?'),
      'should create vector from list':
        testOK('(list->vector (list 1 2 3))', '#(1 2 3)'),
    },

    'vector->string': {
      'should raise error for calling with too few args':
        testFail('(vector->string)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(vector->string #(#\\a #\\b #\\c) 0 1 2)', 'Expected at most 3 arguments, but got 4'),
      'should raise error for passing not a vector':
        testFail('(vector->string \'(1 2 3))', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for start':
        testFail('(vector->string #(#\\a #\\b #\\c) -1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(vector->string #(#\\a #\\b #\\c) 1 -2)', 'Expected argument on position 2 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing start greater length':
        testFail('(vector->string #(#\\a #\\b #\\c) 3)', 'Index is out of range'),
      'should raise error for passing end greater length':
        testFail('(vector->string #(#\\a #\\b #\\c) 0 4)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(vector->string #(#\\a #\\b #\\c) 2 1)', 'The start index is greater than the end index'),
      'should raise error for passing vector with non chars':
        testFail('(vector->string #(#\\a #\\b 1) 1)', 'Expected the elements of the vector from index 1 to 3 to satisfy predicate char?'),
      'should create string from vector':
        testOK('(vector->string #(#\\a #\\b #\\c))', '"abc"'),
      'should create string from vector from start':
        testOK('(vector->string #(#\\a #\\b #\\c) 1)', '"bc"'),
      'should create string from vector from start to end':
        testOK('(vector->string #(#\\a #\\b #\\c #\\d) 1 3)', '"bc"'),
    },

    'string->vector': {
      'should raise error for calling with too few args':
        testFail('(string->vector)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(string->vector "abc" 0 1 2)', 'Expected at most 3 arguments, but got 4'),
      'should raise error for passing not a string':
        testFail('(string->vector \'(1 2 3))', 'Expected argument on position 0 to satisfy predicate string?'),
      'should raise error for passing non positive integer for start':
        testFail('(string->vector "abc" -1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(string->vector "abc" 1 -2)', 'Expected argument on position 2 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing start greater length':
        testFail('(string->vector "abc" 3)', 'Index is out of range'),
      'should raise error for passing end greater length':
        testFail('(string->vector "abc" 0 4)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(string->vector "abc" 2 1)', 'The start index is greater than the end index'),
      'should create vector from string':
        testOK('(string->vector "abc")', '#(#\\a #\\b #\\c)'),
      'should create vector from string from start':
        testOK('(string->vector "abc" 1)', '#(#\\b #\\c)'),
      'should create vector from string from start to end':
        testOK('(string->vector "abcd" 1 3)', '#(#\\b #\\c)'),
    },

    'vector-copy': {
      'should raise error for calling with too few args':
        testFail('(vector-copy)', 'Expected at least 1 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(vector-copy #(1 2 3) 0 1 2)', 'Expected at most 3 arguments, but got 4'),
      'should raise error for passing not a vector':
        testFail('(vector-copy \'(1 2 3))', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for start':
        testFail('(vector-copy #(1 2 3) -1)', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(vector-copy #(1 2 3) 1 -2)', 'Expected argument on position 2 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing start greater length':
        testFail('(vector-copy #(1 2 3) 3)', 'Index is out of range'),
      'should raise error for passing end greater length':
        testFail('(vector-copy #(1 2 3) 0 4)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(vector-copy #(1 2 3) 2 1)', 'The start index is greater than the end index'),
      'should create fresh copy from vector':
        testOK('(vector-copy #(1 2 3))', '#(1 2 3)'),
      'should create fresh copy from vector from start':
        testOK('(vector-copy #(1 2 3) 1)', '#(2 3)'),
      'should create fresh copy from vector from start to end':
        testOK('(vector-copy #(1 2 3 4) 1 3)', '#(2 3)'),
    },

    'vector-copy!': {
      'should raise error for calling with too few args':
        testFail('(vector-copy!)', 'Expected at least 3 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(vector-copy! (vector 1 2 3) 1 #(1 2 3) 0 1 2)', 'Expected at most 5 arguments, but got 6'),
      'should raise error for passing not a vector as a first argument':
        testFail('(vector-copy! \'(1 2 3) 1 #(2 3))', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for at':
        testFail('(vector-copy! (vector 1 2 3) -1 #(2 3))', 'Expected argument on position 1 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing not a vector as a third argument':
        testFail('(vector-copy! (vector 1 2 3) 1 \'(2 3))', 'Expected argument on position 2 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for start':
        testFail('(vector-copy! (vector 1 2 3) 1 #(2 3) -1)', 'Expected argument on position 3 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(vector-copy! (vector 1 2 3) 1 #(2 3) 1 -1)', 'Expected argument on position 4 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing at greater length of first argument':
        testFail('(vector-copy! (vector 1 2 3) 3 #(2 3))', 'Index is out of range'),
      'should raise error for passing start greater length of third argument':
        testFail('(vector-copy! (vector 1 2 3) 1 #(2 3) 3)', 'Index is out of range'),
      'should raise error for passing end greater length of third argument':
        testFail('(vector-copy! (vector 1 2 3) 1 #(2 3) 0 3)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(vector-copy! (vector 1 2 3) 1 #(2 3) 1 0)', 'The start index is greater than the end index'),
      'should raise error if there is not enough space to copy things':
        testFail('(vector-copy! (vector 1 2 3) 2 #(2 3))',
          'Not enough space to copy elements from the second vector to the first. ' +
          'The amount of elements to copy is 2 and the available space is for 1 elements'),
      'should raise error if the first argument is immutable':
        testFail('(vector-copy! #(1 2 3) 0 #(2 3))', 'Cannot mutate an immutable object'),
      'should overwrite content of the first argument':
        testOK('(define v1 (vector 1 2 3))(vector-copy! v1 1 #(3 2)) v1', '#(1 3 2)'),
      'should overwrite content of the first argument from start':
        testOK('(define v1 (vector 1 2 3))(vector-copy! v1 1 #(3 4) 1) v1', '#(1 4 3)'),
      'should overwrite content of the first argument from start to end':
        testOK('(define v1 (vector 1 2 3))(vector-copy! v1 1 #(3 2) 0 1) v1', '#(1 3 3)'),
      'should not overwrite content when vectors overlap and at is less than start':
        testOK('(define v1 (vector 1 2 3 4 5))(vector-copy! v1 1 v1 2) v1', '#(1 3 4 5 5)'),
      'should not overwrite content when vectors overlap and start is less than at':
        testOK('(define v1 (vector 1 2 3 4 5))(vector-copy! v1 2 v1 1 3) v1', '#(1 2 2 3 5)'),
      'should not overwrite content when vectors overlap and at equals start':
        testOK('(define v1 (vector 1 2 3 4 5))(vector-copy! v1 1 v1 1 3) v1', '#(1 2 3 4 5)'),
    },

    'vector-append': {
      // TODO check for the exceptions
      'should return empty vector for no arguments':
        testOK('(vector-append)', '#()'),
      'should raise error for passing non vector':
        testFail('(vector-append \'(1 2 3))', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should return new vector for one argument':
        testOK('(vector-append #(1 2 3))', '#(1 2 3)'),
      'should return concatenated vector for 2 vectors':
        testOK('(vector-append #(x) #(y))', '#(x y)'),
      'should return concatenated vector for 3 vectors':
        testOK('(vector-append #(x) #(y) #(z))', '#(x y z)'),
      'sample 1':
        testOK('(vector-append #(a b c) #(d e f))', '#(a b c d e f)'),
    },

    'vector-fill!': {
      'should raise error for calling with too few args':
        testFail('(vector-fill!)', 'Expected at least 2 arguments, but got 0'),
      'should raise error for calling with too many args':
        testFail('(vector-fill! (list 1 2 3) 0 1 2 3)', 'Expected at most 4 arguments, but got 5'),
      'should raise error for passing not a vector':
        testFail('(vector-fill! \'(1 2 3) 0)', 'Expected argument on position 0 to satisfy predicate vector?'),
      'should raise error for passing non positive integer for start':
        testFail('(vector-fill! (vector 1 2 3) 0 -1)', 'Expected argument on position 2 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing non positive integer for end':
        testFail('(vector-fill! (vector 1 2 3) 0 1 -2)', 'Expected argument on position 3 to satisfy predicate nonnegative-integer?'),
      'should raise error for passing start greater length':
        testFail('(vector-fill! (vector 1 2 3) 0 3)', 'Index is out of range'),
      'should raise error for passing end greater length':
        testFail('(vector-fill! (vector 1 2 3) 0 0 4)', 'Index is out of range'),
      'should raise error for passing start greater than end':
        testFail('(vector-fill! (vector 1 2 3) 0 2 1)', 'The start index is greater than the end index'),
      'should raise error for passing immutable vector':
        testFail('(vector-fill! #(1 2 3) 0)', 'Cannot mutate an immutable object'),
      'should fill a vector':
        testOK('(define v1 (vector 1 2 3))(vector-fill! v1 0) v1', '#(0 0 0)'),
      'should fill a vector from start':
        testOK('(define v1 (vector 1 2 3))(vector-fill! v1 0 1) v1', '#(1 0 0)'),
      'should fill a vector from start to end':
        testOK('(define v1 (vector 1 2 3))(vector-fill! v1 0 1 2) v1', '#(1 0 3)'),
    },
  }
};