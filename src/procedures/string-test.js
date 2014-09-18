var tests = require('../test-fns');

var testOK = tests.testOK, testFail = tests.testFail;

module.exports = {
  'String procedurs': {

    'string?': {

      'should raise error for calling with wrong number of args':
        testFail('(string?)', 'Expected 1 arguments, but got 0'),

      'should return true for a literal string':
        testOK('(string? "abc")', true),

      'should return true for a new string':
        testOK('(string? (make-string 5 #\\a))', true),

      'should return false for a non string':
        testOK('(string? #(#\\a))', false),
    }
  }
};