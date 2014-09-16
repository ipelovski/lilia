describe('evaluation', function () {
  it('should eval nothing', function () {
    var res = lilia.evaluate('');
    expect(res + '').to.equal('');
  });
  it('should eval comments, i.e. nothing', function () {
    var res = lilia.evaluate(';aaaa');
    expect(res + '').to.equal('');
  });
  it('should eval definition', function () {
    var res = lilia.evaluate('(define a 1)');
    expect(res + '').to.equal('');
  });
  it('should eval assignment', function () {
    var res = lilia.evaluate('(define a 1)(set! a 2)');
    expect(res + '').to.equal('');
  });
  it('should raise error for extra right parentheses', function () {
    expect(function () {
      lilia.evaluate('(define a 1))');
    }).to.throw(/Unnecessary usage of right parentheses/);
  });
  it('simple staff', function () {
    var text = '\n\
    (define a 1)\n\
    (set! a 2)\n\
    (define foo (lambda (x) (+ a x)))\n\
    (foo 4)\n\
    ((lambda (x) x) #t)';
    var res = lilia.evaluate(text);
    expect(res).to.equal(true);
  });
  it('should generate stack info', function () {
    var text = '\n\
    (define (foo)\n\
      (bar)1)\n\
    (define (bar)\n\
      (baz)1)\n\
    (define (baz)\n\
      (raise (quote opala)))\n\
    (foo)';
    var res = lilia.evaluate(text);
    expect(res.toString()).to.equal('opala\nraise\nbaz\nbar\nfoo');
  });
  it.skip('should generate stack info 2', function () {
    var text = '\n\
    (define (foo)\n\
      (let ()\n\
      (bar))1)\n\
    (define (bar)\n\
      (baz)1)\n\
    (define (baz)\n\
      (raise (quote opala)))\n\
    (foo)';
    var res = lilia.evaluate(text);
    expect(res.toString()).to.equal('opala\nraise\nbaz\nbar\nfoo');
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
      var res = lilia.evaluate(text);
      expect(res).to.equal(6);
    });
    it('nested begins', function () {
      var text = '\n\
      (begin\n\
        (begin\n\
          (begin 1)))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(1);
    });
    it('empty begin', function () {
      var res = lilia.evaluate('(begin)');
      expect(res + '').to.equal('');
    });
  });
  describe('conditionals', function () {
    describe('if', function () {
      it('should eval if then', function () {
        var res = lilia.evaluate('(if #t 1 2)');
        expect(res).to.equal(1);
      });
      it('should eval if else', function () {
        var res = lilia.evaluate('(if #f 1 2)');
        expect(res).to.equal(2);
      });
      it('should eval nested if', function () {
        var res = lilia.evaluate('(if #f (if #t 1 2) (if #f 3 4))');
        expect(res).to.equal(4);
      });
      it('should eval if then no else', function () {
        var res = lilia.evaluate('(if #t 1)');
        expect(res).to.equal(1);
      });
      it('should eval if else no else', function () {
        var res = lilia.evaluate('(if #f 1)');
        expect(res + '').to.equal('');
      });
    });
    describe('cond', function () {
      it('should eval simple cond', function () {
        var res = lilia.evaluate('(cond (#t 1))');
        expect(res).to.equal(1);
      });
      it('should eval cond no true', function () {
        var res = lilia.evaluate('(cond (#f 1))');
        expect(res + '').to.equal('');
      });
      it('should eval cond no else', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((> a 0) (- a 1) a)\n\
          ((= a 0) #f))';
        var res = lilia.evaluate(text);
        expect(res).to.equal(1);
      });
      it('should eval cond with only test', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((> a 0))\n\
          ((= a 0) #f))';
        var res = lilia.evaluate(text);
        expect(res).to.equal(true);
      });
      it('should eval cond with else', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) (set! a -1) (+ a 1))\n\
          ((= a 0) (- a 1) a)\n\
          (else (set! a 10) 5))';
        var res = lilia.evaluate(text);
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
          lilia.evaluate(text)
        }).to.throw(/"else" can be used only at the last part of a "cond" expression/);
      });
      it('should eval cond with =>', function () {
        var text = '\n\
        (define a 1)\n\
        (cond\n\
          ((< a 0) => number?)\n\
          ((> a 0) => (lambda (x) x))\n\
          (else (set! a 10) 5))';
        var res = lilia.evaluate(text);
        expect(res).to.equal(true);
      });
    });
  });
  describe('let', function () {
    it('simple let', function () {
      var text = '\n\
      (let ((x 1))\n\
        (+ x 1))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(2);
    });
    it('let', function () {
      var text = '\n\
      (define foo (lambda (x)\n\
        (let ((y 1))\n\
          (+ x y))))\n\
      (foo 5)';
      var res = lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
      expect(res).to.equal(5);
    });
    it('named let should not exaust stack', function () {
      var text = '\n\
      (let foo ((x 0))\n\
        (if (= x 2000)\n\
          x\n\
          (foo (+ x 1))))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(2000);
    });
    it('let variables scope', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (let ((x 7)\n\
              (z (+ x y)))\n\
          (* z x)))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(35);
    });
    it('let internal definitions', function () {
      var text = '\n\
      (let ((x 5))\n\
        (define foo (lambda (y) (bar x y)))\n\
        (define bar (lambda (a b) (+ (* a b) a)))\n\
        (foo (+ x 3)))';
      var res = lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
      expect(res).to.equal(true);
    });
    it('should raise error for letrec', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (letrec ((z (+ x y))(x 7))\n\
          (* z x)))';
      expect(lilia.evaluate(text).message).to.contain('Expected a number');
    });
    it('should eval let*', function () {
      var text = '\n\
      (let ((x 2) (y 3))\n\
        (let* ((x 7)\n\
          (z (+ x y)))\n\
          (* z x)))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(70);
    });
    it('should eval let* with no bindings', function () {
      var res = lilia.evaluate('(let* () 0 (+ 1 2))');
      expect(res).to.equal(3);
    });
  });
  describe('definitions', function () {
    it('should raise error for incorrect usage of "define"', function () {
      expect(function () {
        lilia.evaluate('define');
      }).to.throw(/Syntax keywords cannot be used as variables/);
    });
    it('should raise error for accessing unbound variable', function () {
      expect(function () {
        lilia.evaluate('(+ 1 a)');
      }).to.throw(/Undefined variable with name "a"/);
    });
    it('should raise error for setting unbound variable', function () {
      expect(function () {
        lilia.evaluate('(set! a 5)');
      }).to.throw(/Undefined variable with name "a"/);
    });
    it('should set a variable to a parent environment', function () {
      var text = '\n\
      (define a 1)\n\
      (define foo (lambda (x) (set! a x)))\n\
      (foo 4)\n\
      a';
      var res = lilia.evaluate(text);
      expect(res).to.equal(4);
    });
    it('should set a variable to an already defined one', function () {
      var text = '\n\
      (define a 1)\n\
      (define a 2)\n\
      a';
      var res = lilia.evaluate(text);
      expect(res).to.equal(2);
    });
    it('should raise error for invalid boolean token.', function () {
      expect(function () {
        lilia.evaluate('(if #t1)');
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
        lilia.evaluate(text);
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
        lilia.evaluate(text);
      }).to.throw(/Expected an expression but found a definition/);
    });
    it('should eval duplicate global definitions', function () {
      var text = '\n\
      (define x 1)\n\
      (define x 2)\n\
      x';
      var res = lilia.evaluate(text);
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
        lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
      expect(res + '').to.equal('');
    });
  });  
  describe('lambda and calls', function () {
    it('should eval procedure call', function () {
      var res = lilia.evaluate('(define a 1)(+ a 1)');
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
      var res = lilia.evaluate(text);
      expect(res).to.equal(1001);
    });
    it('lambda list formal', function () {
      var text = '\n\
      (define foo (lambda x\n\
          x))\n\
      (foo 5 6)';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('lambda rest formals', function () {
      var text = '\n\
      (define foo (lambda (x . y)\n\
          y))\n\
      (foo 4 5 6)';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('define procedure', function () {
      var text = '\n\
      (define (foo x)\n\
          x)\n\
      (foo 5)';
      var res = lilia.evaluate(text);
      expect(res).to.equal(5);
    });
    it('define procedure list formal', function () {
      var text = '\n\
      (define (foo . x)\n\
          x)\n\
      (foo 5 6)';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
    it('define procedure rest formals', function () {
      var text = '\n\
      (define (foo x . y)\n\
          y)\n\
      (foo 4 5 6)';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(5 6)");
    });
  });
  describe('quotations', function () {
    it('quote pair', function () {
      var text = '\n\
      (quote (1 . 2))';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(1 . 2)");
    });
    it('quote abbr pair', function () {
      var text = "\n\
      '(1 . 2)";
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(1 . 2)");
    });
    it('quote list', function () {
      var text = '\n\
      (quote (1 2))';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("(1 2)");
    });
    it('quote vector', function () {
      var text = '\n\
      (quote #(1 2))';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("#(1 2)");
    });
    it('quote nested lists', function () {
      var text = '\n\
      (quote ((1 2) (3 4) 5))';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal("((1 2) (3 4) 5)");
    });
    it('should quote quotations', function () {
      var text = "(car (car '('(1 2 5) '(1 3 4) '(1 2 4))))";
      var res = lilia.evaluate(text);
      expect(res.toString()).to.equal('quote');
    });
  });
  describe('call/cc', function () {
    it('simple call/cc', function () {
      var text = '\n\
      (+ 1 (call/cc (lambda (x) (x 1) 2)))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(2);
    });
    it('simple call/cc no call', function () {
      var text = '\n\
      (+ 1 (call/cc (lambda (x) 2)))';
      var res = lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
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
      var res = lilia.evaluate(text);
      expect(res).to.equal(24);
    });
  });
  describe('do', function () {
    it('should evaluate simple do', function () {
      var text = '\n\
      (do ((vec (vector 0 0 0 0 0))\n\
           (i 0 (+ i 1)))\n\
          ((= i 5) vec)\n\
        (vector-set! vec i i))';
      var res = lilia.evaluate(text);
      expect(res + '').to.equal('#(0 1 2 3 4)');
    });
    it('should evaluate simple do 2', function () {
      var text = '\n\
      (let ((x \'(1 3 5 7 9)))\n\
        (do ((x x (cdr x))\n\
             (sum 0 (+ sum (car x))))\n\
            ((null? x) sum)))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(25);
    });
    it('should not exaust stack', function () {
      var text = '\n\
      (do ((i 0 (+ i 1)))\n\
          ((= i 2000) i))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(2000);
    });
    it('should not intermingle internal symbols used for do forms', function () {
      var text = '\n\
      (do ((i 0 (+ i 1))\n\
           (j 0))\n\
          ((= i 10) (* i j))\n\
          (set! j (do ((j 0 (+ j 1)))\n\
              ((= j 10) j))))';
      var res = lilia.evaluate(text);
      expect(res).to.equal(100);
      var text = '\n\
      (do ((i 0 (+ i 1))\n\
           (j 0 (do ((j 0 (+ j 1)))\n\
                   ((= j 10) j))))\n\
          ((= i 10) (* i j)))';
      res = lilia.evaluate(text);
      expect(res).to.equal(100);
    });
  });

  describe('conjunction', function () {
    it('should eval with 0 args', function () {
      expect(lilia.evaluate('(and)')).to.equal(true);
    });
    it('should eval with 1 args', function () {
      expect(lilia.evaluate('(and 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, true', function () {
      expect(lilia.evaluate('(and #f 1)')).to.equal(false);
    });
    it('should eval with 2 args: true, false', function () {
      expect(lilia.evaluate('(and 1 #f)')).to.equal(false);
    });
    it('should eval with 2 args: true, true', function () {
      expect(lilia.evaluate('(and #t 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, false', function () {
      expect(lilia.evaluate('(and #f #f)')).to.equal(false);
    });
  });

  describe('disjunction', function () {
    it('should eval with 0 args', function () {
      expect(lilia.evaluate('(or)')).to.equal(false);
    });
    it('should eval with 1 args', function () {
      expect(lilia.evaluate('(or 1)')).to.equal(1);
    });
    it('should eval with 2 args: false, true', function () {
      expect(lilia.evaluate('(or #f 1)')).to.equal(1);
    });
    it('should eval with 2 args: true, false', function () {
      expect(lilia.evaluate('(or 1 #f)')).to.equal(1);
    });
    it('should eval with 2 args: true, true', function () {
      expect(lilia.evaluate('(or #t 1)')).to.equal(true);
    });
    it('should eval with 2 args: false, false', function () {
      expect(lilia.evaluate('(or #f #f)')).to.equal(false);
    });
  });

  describe('primitives', function () {
    describe('number procedures', function () {
      describe('"+"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(lilia.evaluate('(+ #t #f)').message).to.contain('Expected a number');
        });
        it('should return result for 0 args', function () {
          expect(lilia.evaluate('(+)')).to.equal(0);
        });
        it('should return result for 1 arg', function () {
          expect(lilia.evaluate('(+ 5)')).to.equal(5);
        });
        it('should return result for 2 args', function () {
          expect(lilia.evaluate('(+ 5 1)')).to.equal(6);
        });
        it('should return result for more than 2 args', function () {
          expect(lilia.evaluate('(+ 5 1 2)')).to.equal(8);
        });
      });

      // eval primitives number procedures
      describe('"-"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(lilia.evaluate('(- #t #f)').message).to.contain('Expected a number');
        });
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(-)').message).to.contain('Expected at least 1 arguments, but got 0');
        });
        it('should return result for 1 arg', function () {
          expect(lilia.evaluate('(- 5)')).to.equal(-5);
        });
        it('should return result for 2 args', function () {
          expect(lilia.evaluate('(- 5 1)')).to.equal(4);
        });
        it('should return result for more than 2 args', function () {
          expect(lilia.evaluate('(- 5 1 2)')).to.equal(2);
        });
      });

      // eval primitives number procedures
      describe('"*"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(lilia.evaluate('(* #t #f)').message).to.contain('Expected a number');
        });
        it('should return result for 0 args', function () {
          expect(lilia.evaluate('(*)')).to.equal(1);
        });
        it('should return result for 1 arg', function () {
          expect(lilia.evaluate('(* 5)')).to.equal(5);
        });
        it('should return result for 2 args', function () {
          expect(lilia.evaluate('(* 5 2)')).to.equal(10);
        });
        it('should return result for more than 2 args', function () {
          expect(lilia.evaluate('(* 5 2 2)')).to.equal(20);
        });
      });

      // eval primitives number procedures
      describe('"/"', function () {
        it('should raise error for calling with non-numbers', function () {
          expect(lilia.evaluate('(/ #t #f)').message).to.contain('Expected a number');
        });
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(/)').message).to.contain('Expected at least 1 arguments, but got 0');
        });
        it('should return result for 1 arg', function () {
          expect(lilia.evaluate('(/ 5)')).to.equal(0.2);
        });
        it('should return result for 2 args', function () {
          expect(lilia.evaluate('(/ 6 2)')).to.equal(3);
        });
        it('should return result for more than 2 args', function () {
          expect(lilia.evaluate('(/ 20 2 2)')).to.equal(5);
        });
      });

      // eval primitives number procedures
      describe('number?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(number?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a number value', function () {
          expect(lilia.evaluate('(number? 1)')).to.equal(true);
        });
        it('should return false for a non-number value', function () {
          expect(lilia.evaluate('(number? #t)')).to.equal(false);
        });
      });
      // eval primitives number procedures
      describe('nonnegative-integer?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(nonnegative-integer?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a non-negative integer', function () {
          expect(lilia.evaluate('(nonnegative-integer? 1)')).to.equal(true);
        });
        it('should return false for a non-number value', function () {
          expect(lilia.evaluate('(nonnegative-integer? #t)')).to.equal(false);
        });
        it('should return false for a non-integer value', function () {
          expect(lilia.evaluate('(nonnegative-integer? 1.5)')).to.equal(false);
        });
        it('should return false for a negative value', function () {
          expect(lilia.evaluate('(nonnegative-integer? -1)')).to.equal(false);
        });
      });
    });

    // eval primitives procedures
    describe('boolean procedures', function () {
      describe('boolean?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(boolean?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a boolean value', function () {
          expect(lilia.evaluate('(boolean? #t)')).to.equal(true);
        });
        it('should return false for a non-boolean value', function () {
          expect(lilia.evaluate('(boolean? 1)')).to.equal(false);
        });
      });
      // eval primitives boolean procedures
      describe('not', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(not)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a false value', function () {
          expect(lilia.evaluate('(not #f)')).to.equal(true);
        });
        it('should return false for a truthy value', function () {
          expect(lilia.evaluate('(not 1)')).to.equal(false);
        });
      });
    });

    // eval primitive procedures
    describe('pair procedures', function () {
      describe('cons', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(cons 1)').message).to.contain('Expected 2 arguments, but got 1');
        });
        it('should return a pair', function () {
          expect(lilia.evaluate('(cons 1 2)') + '').to.equal('(1 . 2)');
        });
      });
      // eval primitives pair procedures
      describe('make-list', function () {
        it('should raise error for calling with too little args', function () {
          expect(lilia.evaluate('(make-list)').message).to.contain('Expected at least 1 arguments, but got 0');
        });
        it('should raise error for calling with too many args', function () {
          expect(lilia.evaluate('(make-list 1 1 1)').message).to.contain('Expected at most 2 arguments, but got 3');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(make-list (quote (1 2)))').message).to.contain('Expected argument on position 0 to satisfy predicate nonnegative-integer?.');
        });
        it('should create a vector with given length', function () {
          expect(lilia.evaluate('(length (make-list 5))')).to.equal(5);
        });
        it('should create a vector with default fill 0', function () {
          expect(lilia.evaluate('(list-ref (make-list 5) 2)')).to.equal(0);
        });
        it('should create a vector with the given fill', function () {
          expect(lilia.evaluate('(list-ref (make-list 5 3) 2)')).to.equal(3);
        });
      });
      // eval primitives pair procedures
      describe('pair?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(pair?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a pair', function () {
          expect(lilia.evaluate('(pair? (cons 1 2))')).to.equal(true);
        });
        it('should return false for a non pair', function () {
          expect(lilia.evaluate('(pair? 1)')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('car', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(car)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(car 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should return the first element in a pair', function () {
          expect(lilia.evaluate('(car (cons 1 2))')).to.equal(1);
        });
      });
      // eval primitives pair procedures
      describe('cdr', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(cdr)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(cdr 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should return the second element in a pair', function () {
          expect(lilia.evaluate('(cdr (cons 1 2))')).to.equal(2);
        });
      });
      // eval primitives pair procedures
      describe('set-car!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(set-car!)').message).to.contain('Expected 2 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(set-car! (list) 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should set the first element in a pair', function () {
          expect(lilia.evaluate('(define a (cons 1 2))(set-car! a 3) a') + '').to.equal('(3 . 2)');
        });
      });
      // eval primitives pair procedures
      describe('set-cdr!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(set-cdr!)').message).to.contain('Expected 2 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(set-cdr! (list) 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should set the second element in a pair', function () {
          expect(lilia.evaluate('(define a (cons 1 2))(set-cdr! a 3) a') + '').to.equal('(1 . 3)');
        });
      });
      // eval primitives pair procedures
      describe('null?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(null?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for the empty list', function () {
          expect(lilia.evaluate('(null? (list))')).to.equal(true);
        });
        it('should return false for something different from the empty list', function () {
          expect(lilia.evaluate('(null? (list 1))')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('list?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(list?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a proper list', function () {
          expect(lilia.evaluate('(list? (list 1 2 3))')).to.equal(true);
        });
        it('should return false for a non proper list', function () {
          expect(lilia.evaluate('(list? (cons 1 (cons 2 3)))')).to.equal(false);
        });
        it('should return true for the empty list', function () {
          expect(lilia.evaluate('(list? (list))')).to.equal(true);
        });
        it('should return false for a non list', function () {
          expect(lilia.evaluate('(list? (vector))')).to.equal(false);
        });
      });
      // eval primitives pair procedures
      describe('length', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(length)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(length #(1 2 3))').message).to.contain('Expected argument on position 0 to satisfy predicate list?.');
        });
        it('should return length for a literal list', function () {
          expect(lilia.evaluate('(length (quote (1 2 3)))')).to.equal(3);
        });
        it('should return length for a new list', function () {
          expect(lilia.evaluate('(length (list 1 2 3))')).to.equal(3);
        });
      });
      // eval primitives pair procedures
      describe('list-ref', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(list-ref)').message).to.contain('Expected 2 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(list-ref #(1 2) 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(lilia.evaluate('(list-ref (list 1 2) #t)').message).to.contain('Expected argument on position 1 to satisfy predicate nonnegative-integer?.');
        });
        it('should raise error for accessing invalid index', function () {
          expect(lilia.evaluate('(list-ref (list 1 2) 2)').message).to.contain('Index is out of range. The length of the list is 2.');
        });
        it('should raise error for indexing a non-pair', function () {
          expect(lilia.evaluate('(list-ref (quote (a . (b . c))) 2)').message).to.contain('Index is out of range. A non-pair is reached at position 2.');
        });
        it('should return k-th element of a literal list', function () {
          expect(lilia.evaluate('(list-ref (quote (1 2 3)) 1)')).to.equal(2);
        });
        it('should return k-th element of a new list', function () {
          expect(lilia.evaluate('(list-ref (list 1 2 3) 1)')).to.equal(2);
        });
      });
      // eval primitives pair procedures
      describe('list-set!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(list-set!)').message).to.contain('Expected 3 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(list-set! #(1 2) 1 1)').message).to.contain('Expected argument on position 0 to satisfy predicate pair?.');
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(lilia.evaluate('(list-set! (list 1) #t 1)').message).to.contain('Expected argument on position 1 to satisfy predicate nonnegative-integer?.');
        });
        it('should raise error for accessing invalid index', function () {
          expect(lilia.evaluate('(list-set! (list 1 2) 2 3)').message).to.contain('Index is out of range. The length of the list is 2.');
        });
        it('should raise error for indexing a non-pair', function () {
          expect(lilia.evaluate('(list-set! (quote (a . (b . c))) 2 3)').message).to.contain('Index is out of range. A non-pair is reached at position 2.');
        });
        it('should raise error for mutating an immutable list', function () {
          expect(lilia.evaluate('(list-set! (quote (1 2 3)) 2 4)').message).to.contain('Cannot mutate an immutable object.');
        });
        it('should set k-th element of a list', function () {
          expect(lilia.evaluate('(let ((l (list 1 2 3)))(list-set! l 2 4) l)') + '').to.equal('(1 2 4)');
        });
      });
      // eval primitives pair procedures
      describe('append', function () {
        // TODO check for the exceptions
        it('should return empty list for no arguments', function () {
          expect(lilia.evaluate('(append)') + '').to.equal("'()");
        });
        it('should return the only argument for one argument', function () {
          expect(lilia.evaluate('(append 1)')).to.equal(1);
        });
        it('should return concatenated lists for 2 list arguments', function () {
          expect(lilia.evaluate('(append \'(x) \'(y))') + '').to.equal('(x y)');
        });
        it('should return concatenated lists for 3 list arguments', function () {
          expect(lilia.evaluate('(append \'(x) \'(y) \'(z))') + '').to.equal('(x y z)');
        });
        it('sample 1', function () {
          expect(lilia.evaluate('(append \'(a) \'(b c d))') + '').to.equal('(a b c d)');
        });
        it('sample 2', function () {
          expect(lilia.evaluate('(append \'(a (b)) \'((c)))') + '').to.equal('(a (b) (c))');
        });
        it('sample 3', function () {
          expect(lilia.evaluate('(append \'(a b) \'(c . d))') + '').to.equal('(a b c . d)');
        });
        it('sample 4', function () {
          expect(lilia.evaluate('(append \'() \'a)') + '').to.equal('a');
        });
      });
    });
    // eval primitives procedures  
    describe('vector procedures', function () {
      describe('vector?', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(vector?)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should return true for a literal vector', function () {
          expect(lilia.evaluate('(vector? #(1 2 3))')).to.equal(true);
        });
        it('should return true for a new vector', function () {
          expect(lilia.evaluate('(vector? (vector 1 2 3))')).to.equal(true);
        });
        it('should return false for a non vector', function () {
          expect(lilia.evaluate('(vector? #t)')).to.equal(false);
        });
      });
      describe('make-vector', function () {
        it('should raise error for calling with too little args', function () {
          expect(lilia.evaluate('(make-vector)').message).to.contain('Expected at least 1 arguments, but got 0');
        });
        it('should raise error for calling with too many args', function () {
          expect(lilia.evaluate('(make-vector 1 1 1)').message).to.contain('Expected at most 2 arguments, but got 3');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(make-vector (quote (1 2)))').message).to.contain('Expected argument on position 0 to satisfy predicate nonnegative-integer?.');
        });
        it('should create a vector with given length', function () {
          expect(lilia.evaluate('(vector-length (make-vector 5))')).to.equal(5);
        });
        it('should create a vector with default fill 0', function () {
          expect(lilia.evaluate('(vector-ref (make-vector 5) 2)')).to.equal(0);
        });
        it('should create a vector with the given fill', function () {
          expect(lilia.evaluate('(vector-ref (make-vector 5 3) 2)')).to.equal(3);
        });
      });
      // eval primitives vector procedures
      describe('vector-length', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(vector-length)').message).to.contain('Expected 1 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(vector-length (quote (1 2)))').message).to.contain('Expected argument on position 0 to satisfy predicate vector?.');
        });
        it('should return length for a literal vector', function () {
          expect(lilia.evaluate('(vector-length #(1 2 3))')).to.equal(3);
        });
        it('should return length for a new vector', function () {
          expect(lilia.evaluate('(vector-length (vector 1 2 3))')).to.equal(3);
        });
      });
      // eval primitives vector procedures
      describe('vector-ref', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(vector-ref)').message).to.contain('Expected 2 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(vector-ref (quote (1 2)) 1)').message).to.contain('Expected argument on position 0 to satisfy predicate vector?.');
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(lilia.evaluate('(vector-ref #(1 2) #t)').message).to.contain('Expected argument on position 1 to satisfy predicate nonnegative-integer?.');
        });
        it('should raise error for accessing invalid index', function () {
          expect(lilia.evaluate('(vector-ref #(1 2) 2)').message).to.contain('Index is out of range.');
        });
        it('should return k-th element of a literal vector', function () {
          expect(lilia.evaluate('(vector-ref #(1 2 3) 1)')).to.equal(2);
        });
        it('should return k-th element of a new vector', function () {
          expect(lilia.evaluate('(vector-ref (vector 1 2 3) 1)')).to.equal(2);
        });
      });
      // eval primitives vector procedures
      describe('vector-set!', function () {
        it('should raise error for calling with wrong number of args', function () {
          expect(lilia.evaluate('(vector-set!)').message).to.contain('Expected 3 arguments, but got 0');
        });
        it('should raise error for calling with wrong type of args', function () {
          expect(lilia.evaluate('(vector-set! (quote (1 2)) 1 1)').message).to.contain('Expected argument on position 0 to satisfy predicate vector?.');
        });
        it('should raise error for calling with wrong type of args 2', function () {
          expect(lilia.evaluate('(vector-set! (vector 1) #t 1)').message).to.contain('Expected argument on position 1 to satisfy predicate nonnegative-integer?.');
        });
        it('should raise error for accessing invalid index', function () {
          expect(lilia.evaluate('(vector-set! (vector 1 2) 2 3)').message).to.contain('Index is out of range.');
        });
        it('should raise error for mutating an immutable vector', function () {
          expect(lilia.evaluate('(vector-set! #(1 2 3) 2 4)').message).to.contain('Cannot mutate an immutable object.');
        });
        it('should set k-th element of a vector', function () {
          expect(lilia.evaluate('(let ((v (vector 1 2 3)))(vector-set! v 2 4) v)') + '').to.equal('#(1 2 4)');
        });
      });
    });
    describe('equivalence predicates', function () {
      describe('eq?', function () {
        it('should raise an error for calling with wrong number of arguments', function () {
          expect(lilia.evaluate('(eq? 1)').message).to.contain('Expected 2 arguments, but got 1');
        });
        it('should return true for #t and #t', function () {
          expect(lilia.evaluate('(eq? #t #t)')).to.equal(true);
        });
        it('should return true for #f and #f', function () {
          expect(lilia.evaluate('(eq? #f #f)')).to.equal(true);
        });
        it('should return true for symbols with the same value', function () {
          expect(lilia.evaluate('(eq? \'abc \'abc)')).to.equal(true);
        });
        it('should return true for numbers with the same value', function () {
          expect(lilia.evaluate('(eq? 1.01 1.01)')).to.equal(true);
        });
        it('should return true for characters with the same value', function () {
          expect(lilia.evaluate('(eq? #\\a #\\a)')).to.equal(true);
        });
        it('should return true for comparing empty lists', function () {
          expect(lilia.evaluate('(eq? \'() \'())')).to.equal(true);
        });
        it('should return true for the same pair', function () {
          expect(lilia.evaluate('(let ((a (cons 1 2)))(eq? a a))')).to.equal(true);
        });
        it('should return true for the same vectors', function () {
          expect(lilia.evaluate('(let ((a (vector 1 2)))(eq? a a))')).to.equal(true);
        });
        it('should return true for the same strings', function () {
          expect(lilia.evaluate('(let ((a "abc"))(eq? a a))')).to.equal(true);
        });
        it('should return true for the same lambdas', function () {
          expect(lilia.evaluate('(let ((id (lambda (x) x)))(eq? id id))')).to.equal(true);
        });
        it('should return false for arguments of different types', function () {
          expect(lilia.evaluate('(eq? 1 #t)')).to.equal(false);
        });
        it('should return false for #t and #f', function () {
          expect(lilia.evaluate('(eq? #f #t)')).to.equal(false);
        });
        it('should return false for symbols with different values', function () {
          expect(lilia.evaluate('(eq? \'abc \'abcd)')).to.equal(false);
        });
        it('should return false for numbers with different values', function () {
          expect(lilia.evaluate('(eq? 1.01 1)')).to.equal(false);
        });
        it('should return false for characters with different values', function () {
          expect(lilia.evaluate('(eq? #\\a #\\b)')).to.equal(false);
        });
        it('should return false for comparing empty list with non empty list', function () {
          expect(lilia.evaluate('(eq? \'() (list 1 2))')).to.equal(false);
        });
        it('should return false for different pairs', function () {
          expect(lilia.evaluate('(eq? (cons 1 2) (cons 1 2))')).to.equal(false);
        });
        it('should return false for different vectors', function () {
          expect(lilia.evaluate('(eq? (vector 1 2) (vector 1 2))')).to.equal(false);
        });
        it('should return false for different strings', function () {
          expect(lilia.evaluate('(eq? "abc" "abc")')).to.equal(false);
        });
        it('should return false for different lambdas', function () {
          expect(lilia.evaluate('(eq? (lambda (x) x) (lambda (x) x))')).to.equal(false);
        });
        it('specification samples', function () {
          expect(lilia.evaluate('(eq? \'a \'a)')).to.equal(true);
          expect(lilia.evaluate('(eq? (list \'a) (list \'a))')).to.equal(false);
          expect(lilia.evaluate('(eq? car car)')).to.equal(true);
          expect(lilia.evaluate('(let ((x \'(a)))(eq? x x))')).to.equal(true);
          expect(lilia.evaluate('(let ((x \'#()))(eq? x x))')).to.equal(true);
          expect(lilia.evaluate('(let ((p (lambda (x) x)))(eq? p p))')).to.equal(true);
        });
      });
      describe('eqv?', function () {
        it('specification samples', function () {
          expect(lilia.evaluate('(eqv? \'a \'a)')).to.equal(true);
          expect(lilia.evaluate('(eqv? \'a \'b)')).to.equal(false);
          expect(lilia.evaluate('(eqv? 2 2)')).to.equal(true);
          //expect(lilia.evaluate('(eqv? 2 2.0)')).to.equal(false);
          expect(lilia.evaluate('(eqv? \'() \'())')).to.equal(true);
          expect(lilia.evaluate('(eqv? 100000000 100000000)')).to.equal(true);
          expect(lilia.evaluate('(eqv? (cons 1 2) (cons 1 2))')).to.equal(false);
          expect(lilia.evaluate('(eqv? (lambda () 1) (lambda () 2))')).to.equal(false);
          expect(lilia.evaluate('(let ((p (lambda (x) x)))(eqv? p p))')).to.equal(true);
          expect(lilia.evaluate('(eqv? #f \'nil)')).to.equal(false);
          var text = '\n\
          (define gen-counter\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) n))))\n\
          (let ((g (gen-counter)))\n\
            (eqv? g g))';
          expect(lilia.evaluate(text)).to.equal(true);
          text = '\n\
          (define gen-counter\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) n))))\n\
          (eqv? (gen-counter) (gen-counter))';
          expect(lilia.evaluate(text)).to.equal(false);
          text = '\n\
          (define gen-loser\n\
            (lambda ()\n\
              (let ((n 0))\n\
                (lambda () (set! n (+ n 1)) 27))))\n\
          (let ((g (gen-loser)))\n\
            (eqv? g g))';
          expect(lilia.evaluate(text)).to.equal(true);
          text = '\n\
          (letrec ((f (lambda () (if (eqv? f g) \'f \'both)))\n\
                   (g (lambda () (if (eqv? f g) \'g \'both))))\n\
            (eqv? f g))';
          expect(lilia.evaluate(text)).to.equal(false);
          expect(lilia.evaluate('(let ((x \'(a)))(eqv? x x))')).to.equal(true);
        });
      });
    });
  });
  describe('procedure application', function () {
    it('should raise error for calling a procedure with wrong number of args', function () {
      expect(function () {
        lilia.evaluate('(define foo (lambda (x) x))(foo 5 6)');
      }).to.throw(/Expected 1 arguments, but got 2/);
    });
  });
  describe('scheme to js ffi', function () {
    it('should execute simple script', function () {
      expect(lilia.evaluate('(js-eval "1")')).to.equal(1);
    });
    it('should return vector', function () {
      expect(lilia.evaluate('(js-eval "[1, 2]")').toString()).to.equal('#(1 2)');
    });
    it('should return alist', function () {
      expect(lilia.evaluate('(js-eval "({ a: 1, b: 2, c: { d: 3, e: 4 } })")').toString())
        .to.equal('(("a" . 1) ("b" . 2) ("c" ("d" . 3) ("e" . 4)))');
    });
  });
  describe('lambda names', function () {
    it('should name lambda definition', function () {
      expect(lilia.evaluate('(define (foo) 1)foo').toString()).to.equal('#<procedure foo>');
    });
    it('should name lambda definition by value 1', function () {
      expect(lilia.evaluate('(define foo (lambda () 1))foo').toString()).to.equal('#<procedure foo>');
    });
    it('should name lambda definition by value 2', function () {
      expect(lilia.evaluate('(define foo ((lambda()(lambda()1))))foo').toString()).to.equal('#<procedure foo>');
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
