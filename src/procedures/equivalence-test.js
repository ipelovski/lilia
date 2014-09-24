var tests = require('../test-fns');

var testOK = tests.testOK, testFail = tests.testFail;

module.exports = {
  'equivalence predicates': {

    'eq?': {

      'should raise an error for calling with wrong number of arguments':
        testFail('(eq? 1)', 'Expected 2 arguments, but got 1'),
      'should return true for #t and #t':
        testOK('(eq? #t #t)', true),
      'should return true for #f and #f':
        testOK('(eq? #f #f)', true),
      'should return true for symbols with the same value':
        testOK('(eq? \'abc \'abc)', true),
      'should return true for numbers with the same value':
        testOK('(eq? 1.01 1.01)', true),
      'should return true for characters with the same value':
        testOK('(eq? #\\a #\\a)', true),
      'should return true for comparing empty lists':
        testOK('(eq? \'() \'())', true),
      'should return true for the same pair':
        testOK('(let ((a (cons 1 2)))(eq? a a))', true),
      'should return true for the same vectors':
        testOK('(let ((a (vector 1 2)))(eq? a a))', true),
      'should return true for the same strings':
        testOK('(let ((a "abc"))(eq? a a))', true),
      'should return true for the same lambdas':
        testOK('(let ((id (lambda (x) x)))(eq? id id))', true),
      'should return false for arguments of different types':
        testOK('(eq? 1 #t)', false),
      'should return false for #t and #f':
        testOK('(eq? #f #t)', false),
      'should return false for symbols with different values':
        testOK('(eq? \'abc \'abcd)', false),
      'should return false for numbers with different values':
        testOK('(eq? 1.01 1)', false),
      'should return false for characters with different values':
        testOK('(eq? #\\a #\\b)', false),
      'should return false for comparing empty list with non empty list':
        testOK('(eq? \'() (list 1 2))', false),
      'should return false for different pairs':
        testOK('(eq? (cons 1 2) (cons 1 2))', false),
      'should return false for different vectors':
        testOK('(eq? (vector 1 2) (vector 1 2))', false),
      'should return false for different strings':
        testOK('(eq? "abc" "abc")', false),
      'should return false for different lambdas':
        testOK('(eq? (lambda (x) x) (lambda (x) x))', false),
      'specification samples': [
        testOK('(eq? \'a \'a)', true),
        testOK('(eq? (list \'a) (list \'a))', false),
        testOK('(eq? car car)', true),
        testOK('(let ((x \'(a)))(eq? x x))', true),
        testOK('(let ((x \'#()))(eq? x x))', true),
        testOK('(let ((p (lambda (x) x)))(eq? p p))', true)
      ],
    },
    'eqv?': {

      'specification samples': [
        testOK('(eqv? \'a \'a)', true),
        testOK('(eqv? \'a \'b)', false),
        testOK('(eqv? 2 2)', true),
        //testOK('(eqv? 2 2.0)', false),
        testOK('(eqv? \'() \'())', true),
        testOK('(eqv? 100000000 100000000)', true),
        testOK('(eqv? (cons 1 2) (cons 1 2))', false),
        testOK('(eqv? (lambda () 1) (lambda () 2))', false),
        testOK('(let ((p (lambda (x) x)))(eqv? p p))', true),
        testOK('(eqv? #f \'nil)', false),
        testOK('\n\
          (define gen-counter\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) n))))\n\
          (let ((g (gen-counter)))\n\
            (eqv? g g))', true),
        testOK('\n\
          (define gen-counter\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) n))))\n\
          (eqv? (gen-counter) (gen-counter))', false),
        testOK('\n\
          (define gen-loser\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) 27))))\n\
          (let ((g (gen-loser)))\n\
            (eqv? g g))', true),
        testOK('\n\
          (letrec ((f (lambda () (if (eqv? f g) \'f \'both)))\n\
                   (g (lambda () (if (eqv? f g) \'g \'both))))\n\
            (eqv? f g))', false),
        testOK('(let ((x \'(a)))(eqv? x x))', true),
      ],
    },

    'equal?': {
      'specification samples': [
        testOK('(equal? \'a \'a)', true),
        testOK('(equal? \'(a) \'(a))', true),
        testOK('(equal? \'(a (b) c) \'(a (b) c))', true),
        testOK('(equal? "abc" "abc")', true),
        testOK('(equal? 2 2)', true),
        testOK('(equal? (make-vector 5 \'a) (make-vector 5 \'a))', true),
        testOK('(equal? (lambda (x) x) (lambda (y) y))', false),
      ]
    },
  },
};