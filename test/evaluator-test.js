describe('eval', function () {
  it('should eval definition', function () {
    evaluator.evaluate('(define a 1)');
  });
  it('tail call', function () {
    var text = '\n\
    (define foo (lambda (x)\n\
      (if (= x 100)\n\
        x\n\
        (foo (+ x 1)))\n\
    ))\n\
    (foo 0)';
    var res = evaluator.evaluate(text);
    expect(res).to.equal(100);
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
        it('should return false for a non list', function () {
          expect(evaluator.evaluate('(list? (list))')).to.equal(false);
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
        it('should return true for a new vector', function () {
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
      }).to.throw(/Expected 1 arguments, but received 2/);
    });
  });
});