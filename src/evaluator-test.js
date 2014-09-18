var tests = require('./test-fns');

var testOK = tests.testOK, testFail = tests.testFail, testSkip = tests.testSkip;

module.exports = {
  'evaluation': {
    'should eval nothing':
      testOK('', ''),
    'should eval comments, i.e. nothing':
      testOK(';aaaa', ''),
    'should eval definition':
      testOK('(define a 1)', ''),
    'should eval assignment':
      testOK('(define a 1)(set! a 2)', ''),
    'should raise error for extra right parentheses':
      testFail('(define a 1))', 'Unnecessary usage of right parentheses'),
    'should raise error for invalid tokens':
      testFail('\\#a', 'Invalid content'),
    'simple staff':
      testOK('\n\
        (define a 1)\n\
        (set! a 2)\n\
        (define foo (lambda (x) (+ a x)))\n\
        (foo 4)\n\
        ((lambda (x) x) #t)', true),
    'should generate stack info':
      testOK('\n\
        (define (foo)\n\
          (bar)1)\n\
        (define (bar)\n\
          (baz)1)\n\
        (define (baz)\n\
          (raise (quote opala)))\n\
        (foo)', 'opala\nraise\nbaz\nbar\nfoo'),
      
    // TODO
    // 'should generate stack info 2':
    //   testSkip('\n\
    //     (define (foo)\n\
    //       (let ()\n\
    //       (bar))1)\n\
    //     (define (bar)\n\
    //       (baz)1)\n\
    //     (define (baz)\n\
    //       (raise (quote opala)))\n\
    //     (foo)', 'opala\nraise\nbaz\nbar\nfoo'),
    // },
    'begin': {
      'begin':
        testOK('\n\
          (define foo (lambda (x)\n\
            (define y 1)\n\
            (begin\n\
              (set! y (+ x y))\n\
              y)))\n\
          (foo 5)', 6),
      'nested begins':
        testOK('\n\
          (begin\n\
            (begin\n\
              (begin 1)))', 1),
      'empty begin':
        testOK('(begin)', ''),
    },
    'conditionals': {
      'if': {
        'should eval if then':
          testOK('(if #t 1 2)', 1),
        'should eval if else':
          testOK('(if #f 1 2)', 2),
        'should eval nested if':
          testOK('(if #f (if #t 1 2) (if #f 3 4))', 4),
        'should eval if then no else':
          testOK('(if #t 1)', 1),
        'should eval if else no else':
          testOK('(if #f 1)', ''),
      },
      'cond': {
        'should eval simple cond':
          testOK('(cond (#t 1))', 1),
        'should eval cond no true':
          testOK('(cond (#f 1))', ''),
        'should eval cond no else':
          testOK('\n\
            (define a 1)\n\
            (cond\n\
              ((< a 0) (set! a -1) (+ a 1))\n\
              ((> a 0) (- a 1) a)\n\
              ((= a 0) #f))', 1),
        'should eval cond with only test':
          testOK('\n\
            (define a 1)\n\
            (cond\n\
              ((< a 0) (set! a -1) (+ a 1))\n\
              ((> a 0))\n\
              ((= a 0) #f))', true),
        'should eval cond with else':
          testOK('\n\
            (define a 1)\n\
            (cond\n\
              ((< a 0) (set! a -1) (+ a 1))\n\
              ((= a 0) (- a 1) a)\n\
              (else (set! a 10) 5))', 5),
        'should raise error for cond with else in the middle':
          testFail('\n\
            (define a 1)\n\
            (cond\n\
              ((< a 0) (set! a -1) (+ a 1))\n\
              (else (set! a 10) 5)\n\
              ((= a 0) (- a 1) a))', '"else" can be used only at the last part of a "cond" expression'),
        'should eval cond with =>':
          testOK('\n\
            (define a 1)\n\
            (cond\n\
              ((< a 0) => number?)\n\
              ((> a 0) => (lambda (x) x))\n\
              (else (set! a 10) 5))', true),
      },
    },
    'let': {
      'simple let':
        testOK('\n\
          (let ((x 1))\n\
            (+ x 1))', 2),
      'let':
        testOK('\n\
          (define foo (lambda (x)\n\
            (let ((y 1))\n\
              (+ x y))))\n\
          (foo 5)', 6),
      'named let':
        testOK('\n\
          (define foo (lambda (x)\n\
            (let bar ((y 0))\n\
              (if (= x y)\n\
                y\n\
                (bar (+ y 1))))))\n\
          (foo 5)', 5),
      'named let should not exaust stack':
        testOK('\n\
          (let foo ((x 0))\n\
            (if (= x 2000)\n\
              x\n\
              (foo (+ x 1))))', 2000),
      'let variables scope':
        testOK('\n\
          (let ((x 2) (y 3))\n\
            (let ((x 7)\n\
                  (z (+ x y)))\n\
              (* z x)))', 35),
      'let internal definitions':
        testOK('\n\
          (let ((x 5))\n\
            (define foo (lambda (y) (bar x y)))\n\
            (define bar (lambda (a b) (+ (* a b) a)))\n\
            (foo (+ x 3)))', 45),
      'letrec lambdas':
        testOK('\n\
          (letrec ((even?\n\
                    (lambda (n)\n\
                      (if (= 0 n)\n\
                        #t\n\
                        (odd? (- n 1)))))\n\
                   (odd?\n\
                    (lambda (n)\n\
                      (if (= 0 n)\n\
                      #f\n\
                      (even? (- n 1))))))\n\
            (even? 88))', true),
      'should raise error for letrec':
        testFail('\n\
          (let ((x 2) (y 3))\n\
            (letrec ((z (+ x y))(x 7))\n\
              (* z x)))', 'Expected a number'),
      'should eval let*':
        testOK('\n\
          (let ((x 2) (y 3))\n\
            (let* ((x 7)\n\
              (z (+ x y)))\n\
              (* z x)))', 70),
      'should eval let* with no bindings':
        testOK('(let* () 0 (+ 1 2))', 3),
    },
    'definitions': {
      'should raise error for incorrect usage of "define"':
        testFail('define', 'Syntax keywords cannot be used as variables'),
      'should raise error for accessing unbound variable':
        testFail('(+ 1 a)', 'Undefined variable with name "a"'),
      'should raise error for setting unbound variable':
        testFail('(set! a 5)', 'Undefined variable with name "a"'),
      'should set a variable to a parent environment':
        testOK('\n\
          (define a 1)\n\
          (define foo (lambda (x) (set! a x)))\n\
          (foo 4)\n\
          a', 4),
      'should set a variable to an already defined one':
        testOK('\n\
          (define a 1)\n\
          (define a 2)\n\
          a', 2),
      'should raise error for invalid boolean token.':
        testFail('(if #t1)', 'Invalid content'), // TODO return a better message
      'should raise error for improper position of definitions in begin':
        testFail('\n\
          (define (foo)\n\
            (define x 1)\n\
            ((lambda ()\n\
               (begin\n\
                 (+ 1 2)\n\
                 (begin\n\
                   (define x 2)\n\
                   (define y 2)))\n\
               y)))\n\
          (foo)', 'Expected a definition but found an expression'),
      'should raise error for improper position of definitions in lambda':
        testFail('\n\
          (define (foo)\n\
            (define x 1)\n\
            (+ 1 2)\n\
            (define y 2)\n\
            y)\n\
          (foo)', 'Expected an expression but found a definition'),
      'should eval duplicate global definitions':
        testOK('\n\
          (define x 1)\n\
          (define x 2)\n\
          x', 2),
      'should raise error for duplicate definitions in lambda':
        testFail('\n\
          (define (foo)\n\
            (define x 1)\n\
            (define x 2)\n\
            x)\n\
          (foo)', 'Found multiple definitions for variable'),
      'should eval definitions before assignments':
        testOK('\n\
          (define (foo)\n\
            (define x 1)\n\
            ((lambda ()\n\
               (define y x)\n\
               (define x 2)\n\
               y)))\n\
          (foo)', ''),
    },  
    'lambda and calls': {
      'should eval procedure call':
        testOK('(define a 1)(+ a 1)', 2),
      'tail call':
        testOK('\n\
          (define foo (lambda (x)\n\
            (if (= x 1001)\n\
              x\n\
              (foo (+ x 1)))\n\
          ))\n\
          (foo 0)', 1001),
      'lambda list formal':
        testOK('\n\
          (define foo (lambda x\n\
              x))\n\
          (foo 5 6)', "(5 6)"),
      'lambda rest formals':
        testOK('\n\
          (define foo (lambda (x . y)\n\
              y))\n\
          (foo 4 5 6)', '(5 6)'),
      'define procedure':
        testOK('\n\
          (define (foo x)\n\
              x)\n\
          (foo 5)', 5),
      'define procedure list formal':
        testOK('\n\
          (define (foo . x)\n\
              x)\n\
          (foo 5 6)', '(5 6)'),
      'define procedure rest formals':
        testOK('\n\
          (define (foo x . y)\n\
              y)\n\
          (foo 4 5 6)', '(5 6)'),
    },
    'quotations': {
      'quote pair':
        testOK('(quote (1 . 2))', '(1 . 2)'),
      'quote abbr pair':
        testOK("'(1 . 2)", '(1 . 2)'),
      'quote list':
        testOK('(quote (1 2))', '(1 2)'),
      'quote vector':
        testOK('(quote #(1 2))', '#(1 2)'),
      'quote nested lists':
        testOK('(quote ((1 2) (3 4) 5))', '((1 2) (3 4) 5)'),
      'should quote quotations':
        testOK("(car (car '('(1 2 5) '(1 3 4) '(1 2 4))))", 'quote'),
    },
    'call/cc': {
      'simple call/cc':
        testOK('(+ 1 (call/cc (lambda (x) (x 1) 2)))', 2),
      'simple call/cc no call':
        testOK('(+ 1 (call/cc (lambda (x) 2)))', 3),
      'simple call/cc in lambda':
        testOK('\n\
          (define (foo a)\n\
            (call/cc (lambda (return)\n\
                       (if (> a 0)\n\
                           (return a)\n\
                         (return (- a)) ) )) )\n\
          (foo -5)', 5),
      // TODO
      // it.skip('simple recurring call/cc', function () {
      //   testOK('\n\
      //   (define r #f)\n\
      //     (+ 1 (call/cc\n\
      //            (lambda (k)\n\
      //              (set! r k)\n\
      //              (+ 2 (k 3)))))\n\
      //   (r 5)', 6),
      'call/cc sample 1':
        testOK('\n\
          (define list-product\n\
            (lambda (s)\n\
              (call/cc\n\
                (lambda (exit)\n\
                  (let recur ((s s))\n\
                    (if (null? s) 1\n\
                      (if (= (car s) 0) (exit 0)\n\
                        (* (car s) (recur (cdr s))))))))))\n\
          (list-product (list 1 2 3 4))', 24),
    },
    'do': {
      'should evaluate simple do':
        testOK('\n\
          (do ((vec (vector 0 0 0 0 0))\n\
               (i 0 (+ i 1)))\n\
              ((= i 5) vec)\n\
            (vector-set! vec i i))', '#(0 1 2 3 4)'),
      'should evaluate simple do 2':
        testOK('\n\
          (let ((x \'(1 3 5 7 9)))\n\
            (do ((x x (cdr x))\n\
                 (sum 0 (+ sum (car x))))\n\
                ((null? x) sum)))', 25),
      'should not exaust stack':
        testOK('\n\
          (do ((i 0 (+ i 1)))\n\
              ((= i 2000) i))', 2000),
      'should not intermingle internal symbols used for do forms': [
        testOK('\n\
          (do ((i 0 (+ i 1))\n\
               (j 0))\n\
              ((= i 10) (* i j))\n\
              (set! j (do ((j 0 (+ j 1)))\n\
                  ((= j 10) j))))', 100),
        testOK('\n\
          (do ((i 0 (+ i 1))\n\
               (j 0 (do ((j 0 (+ j 1)))\n\
                       ((= j 10) j))))\n\
              ((= i 10) (* i j)))', 100)
      ],
    },

    'conjunction': {
      'should eval with 0 args':
        testOK('(and)', true),
      'should eval with 1 args':
        testOK('(and 1)', 1),
      'should eval with 2 args: false, true':
        testOK('(and #f 1)', false),
      'should eval with 2 args: true, false':
        testOK('(and 1 #f)', false),
      'should eval with 2 args: true, true':
        testOK('(and #t 1)', 1),
      'should eval with 2 args: false, false':
        testOK('(and #f #f)', false),
    },

    'disjunction': {
      'should eval with 0 args':
        testOK('(or)', false),
      'should eval with 1 args':
        testOK('(or 1)', 1),
      'should eval with 2 args: false, true':
        testOK('(or #f 1)', 1),
      'should eval with 2 args: true, false':
        testOK('(or 1 #f)', 1),
      'should eval with 2 args: true, true':
        testOK('(or #t 1)', true),
      'should eval with 2 args: false, false':
        testOK('(or #f #f)', false),
    },

    'primitives': {

      'number procedures': {

        '"+"': {
          'should raise error for calling with non-numbers':
            testFail('(+ #t #f)', 'Expected a number'),
              'should return result for 0 args':
            testOK('(+)', 0),
              'should return result for 1 arg':
            testOK('(+ 5)', 5),
              'should return result for 2 args':
            testOK('(+ 5 1)', 6),
              'should return result for more than 2 args':
            testOK('(+ 5 1 2)', 8),
        },

        '"-"': {
          'should raise error for calling with non-numbers':
            testFail('(- #t #f)', 'Expected a number'),
          'should raise error for calling with wrong number of args':
            testFail('(-)', 'Expected at least 1 arguments, but got 0'),
          'should return result for 1 arg':
            testOK('(- 5)', -5),
          'should return result for 2 args':
            testOK('(- 5 1)', 4),
          'should return result for more than 2 args':
            testOK('(- 5 1 2)', 2),        
        },

        '"*"': {
          'should raise error for calling with non-numbers':
            testFail('(* #t #f)', 'Expected a number'),
          'should return result for 0 args':
            testOK('(*)', 1),
          'should return result for 1 arg':
            testOK('(* 5)', 5),
          'should return result for 2 args':
            testOK('(* 5 2)', 10),
          'should return result for more than 2 args':
            testOK('(* 5 2 2)', 20),        
        },

        '"/"': {
          'should raise error for calling with non-numbers':
            testFail('(/ #t #f)', 'Expected a number'),
          'should raise error for calling with wrong number of args':
            testFail('(/)', 'Expected at least 1 arguments, but got 0'),
          'should return result for 1 arg':
            testOK('(/ 5)', 0.2),
          'should return result for 2 args':
            testOK('(/ 6 2)', 3),
          'should return result for more than 2 args':
            testOK('(/ 20 2 2)', 5),        
        },

        'number?': {
          'should raise error for calling with wrong number of args':
            testFail('(number?)', 'Expected 1 arguments, but got 0'),
          'should return true for a number value':
            testOK('(number? 1)', true),
          'should return false for a non-number value':
            testOK('(number? #t)', false),
        },

        'nonnegative-integer?': {
          'should raise error for calling with wrong number of args':
            testFail('(nonnegative-integer?)', 'Expected 1 arguments, but got 0'),
          'should return true for a non-negative integer':
            testOK('(nonnegative-integer? 1)', true),
          'should return false for a non-number value':
            testOK('(nonnegative-integer? #t)', false),
          'should return false for a non-integer value':
            testOK('(nonnegative-integer? 1.5)', false),
          'should return false for a negative value':
            testOK('(nonnegative-integer? -1)', false),
        },
      },
        
      'boolean procedures': {

        'boolean?': {
          'should raise error for calling with wrong number of args':
            testFail('(boolean?)', 'Expected 1 arguments, but got 0'),
          'should return true for a boolean value':
            testOK('(boolean? #t)', true),
          'should return false for a non-boolean value':
            testOK('(boolean? 1)', false),
        },

        'not': {
          'should raise error for calling with wrong number of args':
            testFail('(not)', 'Expected 1 arguments, but got 0'),
          'should return true for a false value':
            testOK('(not #f)', true),
          'should return false for a truthy value':
            testOK('(not 1)', false),
        },

        'boolean=?': {
          'should raise error for calling with wrong number of args':
            testFail('(boolean=?)', 'Expected at least 2 arguments, but got 0'),
          'should raise error for calling with wrong type of args':
            testFail('(boolean=? #t #f 1)', 'Expected argument on position 2 to satisfy predicate boolean?'),
          'should return true if all arguments are true':
            testOK('(boolean=? #t #t #t)', true),
          'should return true if all arguments are false':
            testOK('(boolean=? #f #f)', true),
          'should return false some arguments are different':
            testOK('(boolean=? #t #f #f)', false),
        },
      },      
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
      },
    },
    'procedure application': {
      'should raise error for calling a procedure with wrong number of args':
        testFail('(define foo (lambda (x) x))(foo 5 6)', 'Expected 1 arguments, but got 2'),
    },
    'scheme to js ffi': {
      'should execute simple script':
        testOK('(js-eval "1")', 1),
      'should return vector':
        testOK('(js-eval "[1, 2]")', '#(1 2)'),
      'should return alist':
        testOK(
          '(js-eval "({ a: 1, b: 2, c: { d: 3, e: 4 } })")',
          '(("a" . 1) ("b" . 2) ("c" ("d" . 3) ("e" . 4)))'),
    },
    'lambda names': {
      'should name lambda definition':
        testOK('(define (foo) 1)foo', '#<procedure foo>'),
      'should name lambda definition by value 1':
        testOK('(define foo (lambda () 1))foo', '#<procedure foo>'),
      'should name lambda definition by value 2':
        testOK('(define foo ((lambda()(lambda()1))))foo', '#<procedure foo>'),
    },
    
    // it.only('ops', function () {
    //   testOK('\n\
    //   (define foo (lambda (x)\n\
    //     (if (= x 100)\n\
    //       x\n\
    //       (foo (+ x 1)))\n\
    //   ))\n\
    //   (foo 0)';
    //   var program = parser.parse(text),
    //   console.log(program),
    // }),
  }
};