describe('evaluation', function () {
  it('should eval nothing', function () {
    var res = evaluator.evaluate('');
    expect(res + '').to.equal('');
  });
  it('should eval comments, i.e. nothing', function () {
    var res = evaluator.evaluate(';aaaa');
    expect(res + '').to.equal('');
  });
  it('should eval definition', function () {
    var res = evaluator.evaluate('(define a 1)');
    expect(res + '').to.equal('');
  });
  it('should eval assignment', function () {
    var res = evaluator.evaluate('(define a 1)(set! a 2)');
    expect(res + '').to.equal('');
  });
  it('simple staff', function () {
    var text = '\n\
    (define a 1)\n\
    (set! a 2)\n\
    (define foo (lambda (x) (+ a x)))\n\
    (foo 4)\n\
    ((lambda (x) x) #t)';
    var res = evaluator.evaluate(text);
    expect(res).to.equal(true);
  });
  describe('begin', function () {
    it('begin', function () {
      var text = '\n\
      (define foo (lambda (x)\n\
        (define y 1)\n\
        (begin\n\
          (set! y (+ x y))\n\
          y)))\n\
      (foo 5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(6);
    });
    it('nested begins', function () {
      var text = '\n\
      (begin\n\
        (begin\n\
          (begin 1)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(1);
    });
    it('empty begin', function () {
      var res = evaluator.evaluate('(begin)');
      expect(res + '').to.equal('');
    });
  });
  describe('conditionals', function () {
    describe('if', function () {
      it('should eval if then', function () {
        var res = evaluator.evaluate('(if #t 1 2)');
        expect(res).to.equal(1);
      });
      it('should eval if else', function () {
        var res = evaluator.evaluate('(if #f 1 2)');
        expect(res).to.equal(2);
      });
      it('should eval nested if', function () {
        var res = evaluator.evaluate('(if #f (if #t 1 2) (if #f 3 4))');
        expect(res).to.equal(4);
      });
      it('should eval if then no else', function () {
        var res = evaluator.evaluate('(if #t 1)');
        expect(res).to.equal(1);
      });
      it('should eval if else no else', function () {
        var res = evaluator.evaluate('(if #f 1)');
        expect(res + '').to.equal('');
      });
    });
    describe('cond', function () {
      it('should eval simple cond', function () {
        var res = evaluator.evaluate('(cond (#t 1))');
        expect(res).to.equal(1);
      });
      it('should eval cond no true', function () {
        var res = evaluator.evaluate('(cond (#f 1))');
        expect(res + '').to.equal('');
      });
      it('should eval cond no else', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((> a 0) (- a 1) a)\n\
          ((= a 0) #f))';
        var res = evaluator.evaluate(text);
        expect(res).to.equal(1);
      });
      it('should eval cond with only test', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((> a 0))\n\
          ((= a 0) #f))';
        var res = evaluator.evaluate(text);
        expect(res).to.equal(true);
      });
      it('should eval cond with else', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((= a 0) (- a 1) a)\n\
          (else (set! a 10) 5))';
        var res = evaluator.evaluate(text);
        expect(res).to.equal(5);
      });
      it('should raise error for cond with else in the middle', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          (else (set! a 10) 5)\n\
          ((= a 0) (- a 1) a))';
        expect(function () {
          evaluator.evaluate(text)
        }).to.throw(/"else" can be used only at the last part of a "cond" expression/);
      });
      it('should eval cond with =>', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) => number?)\n\
          ((> a 0) => (lambda (x) x))\n\
          (else (set! a 10) 5))';
        var res = evaluator.evaluate(text);
        expect(res).to.equal(true);
      });
    });
  });
  describe('let', function () {
    it('simple let', function () {
      var text = '\n\
      (let ((x 1))\n\
        (+ x 1))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(2);
    });
    it('let', function () {
      var text = '\n\
      (define foo (lambda (x)\n\
        (let ((y 1))\n\
          (+ x y))))\n\
      (foo 5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(6);
    });
    it('named let', function () {
      var text = '\n\
      (define foo (lambda (x)\n\
        (let bar ((y 0))\n\
          (if (= x y)\n\
            y\n\
            (bar (+ y 1))))))\n\
      (foo 5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(5);
    });
    it('let variables scope', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (let ((x 7)\n\
              (z (+ x y)))\n\
          (* z x)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(35);
    });
    it('let internal definitions', function () {
      var text = '\n\
      (let ((x 5))\n\
        (define foo (lambda (y) (bar x y)))\n\
        (define bar (lambda (a b) (+ (* a b) a)))\n\
        (foo (+ x 3)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(45);
    });
    it('letrec lambdas', function () {
      var text = '\n\
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
        (even? 88))'
      var res = evaluator.evaluate(text);
      expect(res).to.equal(true);
    });
    it('should raise error for letrec', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (letrec ((z (+ x y))(x 7))\n\
          (* z x)))';
      expect(function () {
        evaluator.evaluate(text);
      }).to.throw(Error);
    });
    it('should eval let*', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (let* ((x 7)\n\
          (z (+ x y)))\n\
          (* z x)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(70);
    });
    it('should eval let* with no bindings', function () {
      var res = evaluator.evaluate('(let* () 0 (+ 1 2))');
      expect(res).to.equal(3);
    });
  });
  describe('definitions', function () {
    it('should raise error for incorrect usage of "define"', function () {
      expect(function () {
        evaluator.evaluate('define');
      }).to.throw(/Syntax keywords cannot be used as variables/);
    });
    it('should raise error for accessing unbound variable', function () {
      expect(function () {
        evaluator.evaluate('(+ 1 a)');
      }).to.throw(/Undefined variable with name "a"/);
    });
    it('should raise error for setting unbound variable', function () {
      expect(function () {
        evaluator.evaluate('(set! a 5)');
      }).to.throw(/Undefined variable with name "a"/);
    });
    it('should set a variable to a parent environment', function () {
      var text = '\n\
      (define a 1)\n\
      (define foo (lambda (x) (set! a x)))\n\
      (foo 4)\n\
      a';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(4);
    });
    it('should set a variable to an already defined one', function () {
      var text = '\n\
      (define a 1)\n\
      (define a 2)\n\
      a';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(2);
    });
    it('should raise error for invalid boolean token.', function () {
      expect(function () {
        evaluator.evaluate('(if #t1)');
      }).to.throw(/Invalid token/);
    });
    it('should raise error for improper position of definitions in begin', function () {
      var text = '\n\
      (define (foo)\n\
        (define x 1)\n\
        ((lambda ()\n\
           (begin\n\
             (+ 1 2)\n\
             (begin\n\
               (define x 2)\n\
               (define y 2)))\n\
           y)))\n\
      (foo)';
      expect(function () {
        evaluator.evaluate(text);
      }).to.throw(/Expected a definition but found an expression/);
    });
    it('should raise error for improper position of definitions in lambda', function () {
      var text = '\n\
      (define (foo)\n\
        (define x 1)\n\
        (+ 1 2)\n\
        (define y 2)\n\
        y)\n\
      (foo)';
      expect(function () {
        evaluator.evaluate(text);
      }).to.throw(/Expected an expression but found a definition/);
    });
    it('should eval duplicate global definitions', function () {
      var text = '\n\
      (define x 1)\n\
      (define x 2)\n\
      x)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(2);
    });
    it('should raise error for duplicate definitions in lambda', function () {
      var text = '\n\
      (define (foo)\n\
        (define x 1)\n\
        (define x 2)\n\
        x)\n\
      (foo)';
      expect(function () {
        evaluator.evaluate(text);
      }).to.throw(/Found multiple definitions for variable/);
    });
    it('should eval definitions before assignments', function () {
      var text = '\n\
      (define (foo)\n\
        (define x 1)\n\
        ((lambda ()\n\
           (define y x)\n\
           (define x 2)\n\
           y)))\n\
      (foo)';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal('');
    });
  });  
  describe('lambda and calls', function () {
    it('should eval procedure call', function () {
      var res = evaluator.evaluate('(define a 1)(+ a 1)');
      expect(res).to.equal(2);
    });
    it('tail call', function () {
      var text = '\n\
      (define foo (lambda (x)\n\
        (if (= x 1001)\n\
          x\n\
          (foo (+ x 1)))\n\
      ))\n\
      (foo 0)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(1001);
    });
    it('lambda list formal', function () {
      var text = '\n\
      (define foo (lambda x\n\
          x))\n\
      (foo 5 6)';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('lambda rest formals', function () {
      var text = '\n\
      (define foo (lambda (x . y)\n\
          y))\n\
      (foo 4 5 6)';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('define procedure', function () {
      var text = '\n\
      (define (foo x)\n\
          x)\n\
      (foo 5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(5);
    });
    it('define procedure list formal', function () {
      var text = '\n\
      (define (foo . x)\n\
          x)\n\
      (foo 5 6)';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('define procedure rest formals', function () {
      var text = '\n\
      (define (foo x . y)\n\
          y)\n\
      (foo 4 5 6)';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
  });
  describe('quotations', function () {
    it('quote pair', function () {
      var text = '\n\
      (quote (1 . 2))';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(1 . 2)");
    });
    it('quote abbr pair', function () {
      var text = "\n\
      '(1 . 2)";
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(1 . 2)");
    });
    it('quote list', function () {
      var text = '\n\
      (quote (1 2))';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("(1 2)");
    });
    it('quote vector', function () {
      var text = '\n\
      (quote #(1 2))';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("#(1 2)");
    });
    it('quote nested lists', function () {
      var text = '\n\
      (quote ((1 2) (3 4) 5))';
      var res = evaluator.evaluate(text);
      expect(res + '').to.equal("((1 2) (3 4) 5)");
    });
  });
  describe('call/cc', function () {
    it('simple call/cc', function () {
      var text = '\n\
      (+ 1 (call/cc (lambda (x) (x 1) 2)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(2);
    });
    it('simple call/cc no call', function () {
      var text = '\n\
      (+ 1 (call/cc (lambda (x) 2)))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(3);
    });
    it('simple call/cc in lambda', function () {
      var text = '\n\
      (define (foo a)\n\
        (call/cc (lambda (return)\n\
                   (if (> a 0)\n\
                       (return a)\n\
                     (return (- a)) ) )) )\n\
      (foo -5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(5);
    });
    it.skip('simple recurring call/cc', function () {
      var text = '\n\
      (define r #f)\n\
        (+ 1 (call/cc\n\
               (lambda (k)\n\
                 (set! r k)\n\
                 (+ 2 (k 3)))))\n\
      (r 5)';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(6);
    });
    it('call/cc sample 1', function () {
      var text = '\n\
      (define list-product\n\
        (lambda (s)\n\
          (call/cc\n\
            (lambda (exit)\n\
              (let recur ((s s))\n\
                (if (null? s) 1\n\
                  (if (= (car s) 0) (exit 0)\n\
                    (* (car s) (recur (cdr s))))))))))\n\
      (list-product (list 1 2 3 4))';
      var res = evaluator.evaluate(text);
      expect(res).to.equal(24);
    });
  });

  describe('conjunction', function () {
    it('should eval with 0 args', function () {
      expect(evaluator.evaluate('(and)')).to.equal(true);
    });
    it('should eval with 1 args', function () {
      expect(evaluator.evaluate('(and 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, true', function () {
      expect(evaluator.evaluate('(and #f 1)')).to.equal(false);
    });
    it('should eval with 2 args: true, false', function () {
      expect(evaluator.evaluate('(and 1 #f)')).to.equal(false);
    });
    it('should eval with 2 args: true, true', function () {
      expect(evaluator.evaluate('(and #t 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, false', function () {
      expect(evaluator.evaluate('(and #f #f)')).to.equal(false);
    });
  });

  describe('disjunction', function () {
    it('should eval with 0 args', function () {
      expect(evaluator.evaluate('(or)')).to.equal(false);
    });
    it('should eval with 1 args', function () {
      expect(evaluator.evaluate('(or 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, true', function () {
      expect(evaluator.evaluate('(or #f 1)')).to.equal(1);
    });
    it('should eval with 2 args: true, false', function () {
      expect(evaluator.evaluate('(or 1 #f)')).to.equal(1);
    });
    it('should eval with 2 args: true, true', function () {
      expect(evaluator.evaluate('(or #t 1)')).to.equal(true);
    });
    it('should eval with 2 args: false, false', function () {
      expect(evaluator.evaluate('(or #f #f)')).to.equal(false);
    });
  });

  describe('primitives', function () {
    describe('number procedures', function () {
      describe('"+"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(function () {
            evaluator.evaluate('(+ #t #f)');
          }).to.throw(/Expected a number/);
        });
        it('should return result for 0 args', function () {
          expect(evaluator.evaluate('(+)')).to.equal(0);
        });
        it('should return result for 1 arg', function () {
          expect(evaluator.evaluate('(+ 5)')).to.equal(5);
        });
        it('should return result for 2 args', function () {
          expect(evaluator.evaluate('(+ 5 1)')).to.equal(6);
        });
        it('should return result for more than 2 args', function () {
          expect(evaluator.evaluate('(+ 5 1 2)')).to.equal(8);
        });
      });

      // eval primitives number procedures
      describe('"-"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(function () {
            evaluator.evaluate('(- #t #f)');
          }).to.throw(/Expected a number/);
        });
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(-)');
          }).to.throw(/Expected at least 1 arguments, but got 0/);
        });
        it('should return result for 1 arg', function () {
          expect(evaluator.evaluate('(- 5)')).to.equal(-5);
        });
        it('should return result for 2 args', function () {
          expect(evaluator.evaluate('(- 5 1)')).to.equal(4);
        });
        it('should return result for more than 2 args', function () {
          expect(evaluator.evaluate('(- 5 1 2)')).to.equal(2);
        });
      });

      // eval primitives number procedures
      describe('"*"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(function () {
            evaluator.evaluate('(* #t #f)');
          }).to.throw(/Expected a number/);
        });
        it('should return result for 0 args', function () {
          expect(evaluator.evaluate('(*)')).to.equal(1);
        });
        it('should return result for 1 arg', function () {
          expect(evaluator.evaluate('(* 5)')).to.equal(5);
        });
        it('should return result for 2 args', function () {
          expect(evaluator.evaluate('(* 5 2)')).to.equal(10);
        });
        it('should return result for more than 2 args', function () {
          expect(evaluator.evaluate('(* 5 2 2)')).to.equal(20);
        });
      });

      // eval primitives number procedures
      describe('"/"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(function () {
            evaluator.evaluate('(/ #t #f)');
          }).to.throw(/Expected a number/);
        });
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(/)');
          }).to.throw(/Expected at least 1 arguments, but got 0/);
        });
        it('should return result for 1 arg', function () {
          expect(evaluator.evaluate('(/ 5)')).to.equal(0.2);
        });
        it('should return result for 2 args', function () {
          expect(evaluator.evaluate('(/ 6 2)')).to.equal(3);
        });
        it('should return result for more than 2 args', function () {
          expect(evaluator.evaluate('(/ 20 2 2)')).to.equal(5);
        });
      });

      // eval primitives number procedures
      describe('number?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(number?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a number value', function () {
          expect(evaluator.evaluate('(number? 1)')).to.equal(true);
        });
        it('should return false for a non-number value', function () {
          expect(evaluator.evaluate('(number? #t)')).to.equal(false);
        });
      });
      // eval primitives number procedures
      describe('nonnegative-integer?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(nonnegative-integer?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a non-negative integer', function () {
          expect(evaluator.evaluate('(nonnegative-integer? 1)')).to.equal(true);
        });
        it('should return false for a non-number value', function () {
          expect(evaluator.evaluate('(nonnegative-integer? #t)')).to.equal(false);
        });
        it('should return false for a non-integer value', function () {
          expect(evaluator.evaluate('(nonnegative-integer? 1.5)')).to.equal(false);
        });
        it('should return false for a negative value', function () {
          expect(evaluator.evaluate('(nonnegative-integer? -1)')).to.equal(false);
        });
      });
    });

    // eval primitives procedures
    describe('boolean procedures', function () {
      describe('boolean?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(boolean?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a boolean value', function () {
          expect(evaluator.evaluate('(boolean? #t)')).to.equal(true);
        });
        it('should return false for a non-boolean value', function () {
          expect(evaluator.evaluate('(boolean? 1)')).to.equal(false);
        });
      });
      // eval primitives boolean procedures
      describe('not', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(not)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a false value', function () {
          expect(evaluator.evaluate('(not #f)')).to.equal(true);
        });
        it('should return false for a truthy value', function () {
          expect(evaluator.evaluate('(not 1)')).to.equal(false);
        });
      });
    });

    // eval primitive procedures
    describe('pair procedures', function () {
      describe('cons', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(cons 1)');
          }).to.throw(/Expected 2 arguments, but got 1/);
        });
        it('should return a pair', function () {
          expect(evaluator.evaluate('(cons 1 2)') + '').to.equal('(1 . 2)');
        });
      });
      // eval primitives pair procedures
      describe('pair?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(pair?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a pair', function () {
          expect(evaluator.evaluate('(pair? (cons 1 2))')).to.equal(true);
        });
        it('should return false for a non pair', function () {
          expect(evaluator.evaluate('(pair? 1)')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('car', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(car)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(car 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate pair?./);
        });
        it('should return the first element in a pair', function () {
          expect(evaluator.evaluate('(car (cons 1 2))')).to.equal(1);
        });
      });
      // eval primitives pair procedures
      describe('cdr', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(cdr)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(cdr 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate pair?./);
        });
        it('should return the second element in a pair', function () {
          expect(evaluator.evaluate('(cdr (cons 1 2))')).to.equal(2);
        });
      });
      // eval primitives pair procedures
      describe('set-car!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(set-car!)');
          }).to.throw(/Expected 2 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(set-car! (list) 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate pair?./);
        });
        it('should set the first element in a pair', function () {
          expect(evaluator.evaluate('(define a (cons 1 2))(set-car! a 3) a') + '').to.equal('(3 . 2)');
        });
      });
      // eval primitives pair procedures
      describe('set-cdr!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(set-cdr!)');
          }).to.throw(/Expected 2 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(set-cdr! (list) 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate pair?./);
        });
        it('should set the second element in a pair', function () {
          expect(evaluator.evaluate('(define a (cons 1 2))(set-cdr! a 3) a') + '').to.equal('(1 . 3)');
        });
      });
      // eval primitives pair procedures
      describe('null?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(null?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for the empty list', function () {
          expect(evaluator.evaluate('(null? (list))')).to.equal(true);
        });
        it('should return false for something different from the empty list', function () {
          expect(evaluator.evaluate('(null? (list 1))')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('list?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(list?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a proper list', function () {
          expect(evaluator.evaluate('(list? (list 1 2 3))')).to.equal(true);
        });
        it('should return false for a non proper list', function () {
          expect(evaluator.evaluate('(list? (cons 1 (cons 2 3)))')).to.equal(false);
        });
        it('should return true for the empty list', function () {
          expect(evaluator.evaluate('(list? (list))')).to.equal(true);
        });
        it('should return false for a non list', function () {
          expect(evaluator.evaluate('(list? (vector))')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('append', function () {
        // TODO check for the exceptions
        it('should return empty list for no arguments', function () {
          expect(evaluator.evaluate('(append)') + '').to.equal("'()");
        });
        it('should return the only argument for one argument', function () {
          expect(evaluator.evaluate('(append 1)')).to.equal(1);
        });
        it('should return concatenated lists for 2 list arguments', function () {
          expect(evaluator.evaluate('(append \'(x) \'(y))') + '').to.equal('(x y)');
        });
        it('should return concatenated lists for 3 list arguments', function () {
          expect(evaluator.evaluate('(append \'(x) \'(y) \'(z))') + '').to.equal('(x y z)');
        });
        it('sample 1', function () {
          expect(evaluator.evaluate('(append \'(a) \'(b c d))') + '').to.equal('(a b c d)');
        });
        it('sample 2', function () {
          expect(evaluator.evaluate('(append \'(a (b)) \'((c)))') + '').to.equal('(a (b) (c))');
        });
        it('sample 3', function () {
          expect(evaluator.evaluate('(append \'(a b) \'(c . d))') + '').to.equal('(a b c . d)');
        });
        it('sample 4', function () {
          expect(evaluator.evaluate('(append \'() \'a)') + '').to.equal('a');
        });
      });
    });
    // eval primitives procedures  
    describe('vector procedures', function () {
      describe('vector?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(vector?)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should return true for a literal vector', function () {
          expect(evaluator.evaluate('(vector? #(1 2 3))')).to.equal(true);
        });
        it('should return true for a new vector', function () {
          expect(evaluator.evaluate('(vector? (vector 1 2 3))')).to.equal(true);
        });
        it('should return false for a non vector', function () {
          expect(evaluator.evaluate('(vector? #t)')).to.equal(false);
        });
      });
      // eval primitives vector procedures
      describe('vector-length', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-length)');
          }).to.throw(/Expected 1 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-length (quote (1 2)))');
          }).to.throw(/Expected argument on position 0 to satisfy predicate vector?./);
        });
        it('should return length for a literal vector', function () {
          expect(evaluator.evaluate('(vector-length #(1 2 3))')).to.equal(3);
        });
        it('should return length for a new vector', function () {
          expect(evaluator.evaluate('(vector-length (vector 1 2 3))')).to.equal(3);
        });
      });
      // eval primitives vector procedures
      describe('vector-ref', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-ref)');
          }).to.throw(/Expected 2 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-ref (quote (1 2)) 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate vector?./);
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(function () {
            evaluator.evaluate('(vector-ref #(1 2) #t)');
          }).to.throw(/Expected argument on position 1 to satisfy predicate nonnegative-integer?./);
        });
        it('should raise error for accessing invalid index', function () {
          expect(function () {
            evaluator.evaluate('(vector-ref #(1 2) 2)');
          }).to.throw(/Index is out of range./);
        });
        it('should return k-th element of a literal vector', function () {
          expect(evaluator.evaluate('(vector-ref #(1 2 3) 1)')).to.equal(2);
        });
        it('should return k-th element of a new vector', function () {
          expect(evaluator.evaluate('(vector-ref (vector 1 2 3) 1)')).to.equal(2);
        });
      });
      // eval primitives vector procedures
      describe('vector-set!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-set!)');
          }).to.throw(/Expected 3 arguments, but got 0/);
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(function () {
            evaluator.evaluate('(vector-set! (quote (1 2)) 1 1)');
          }).to.throw(/Expected argument on position 0 to satisfy predicate vector?./);
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(function () {
            evaluator.evaluate('(vector-set! (vector 1) #t 1)');
          }).to.throw(/Expected argument on position 1 to satisfy predicate nonnegative-integer?./);
        });
        it('should raise error for accessing invalid index', function () {
          expect(function () {
            evaluator.evaluate('(vector-set! (vector 1 2) 2 3)');
          }).to.throw(/Index is out of range./);
        });
        it('should raise error for mutating an immutable vector', function () {
          expect(function () {
            evaluator.evaluate('(vector-set! #(1 2 3) 2 4)');
          }).to.throw(/Cannot mutate an immutable object./);
        });
        it('should set k-th element of a vector', function () {
          expect(evaluator.evaluate('(let ((v (vector 1 2 3)))(vector-set! v 2 4) v)') + '').to.equal('#(1 2 4)');
        });
      });
    });
  });
  describe('procedure application', function () {
    it('should raise error for calling a procedure with wrong number of args', function () {
      expect(function () {
        evaluator.evaluate('(define foo (lambda (x) x))(foo 5 6)');
      }).to.throw(/Expected 1 arguments, but got 2/);
    });
  });
  
  // it.only('ops', function () {
  //   var text = '\n\
  //   (define foo (lambda (x)\n\
  //     (if (= x 100)\n\
  //       x\n\
  //       (foo (+ x 1)))\n\
  //   ))\n\
  //   (foo 0)';
  //   var program = parser.parse(text);
  //   console.log(program);
  // });
});