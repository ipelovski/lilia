(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        define(['evaluator'], factory);
    } else if (typeof exports === 'object') {
        factory(require('./evaluator'), require('readline'));
    } else {
        factory(root.evaluator);
    }
}(this, function (evaluator, readline) {
  require('./common');
  require('./lang-en');

  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  rl.setPrompt('lilia>');
  rl.prompt();

  var evalInput = evaluator.initEval();

  rl.on('line', function (line) {
    try {
      var res = evalInput(line);
      console.log(res.toString());
      rl.prompt();
    }
    catch (err) {
      console.log(err.toString());
      rl.prompt();
    }
  }).on('close', function () {
    process.exit(0);
  });

  return;

  function evaluate(text, lang) {
    try {
      var start = +new Date();
      var result = evaluator.evaluate(text, lang);
      console.log(result);
      console.log('time elapsed:', new Date() - start);
    }
    catch (err) {
      console.log(err.toString());
    }
  }

  var text1 = '\
  (define a 1)\n\
  (set! a 2)\n\
  (define foo (lambda (x) (+ a x)))\n\
  (foo 4)\n\
  ((lambda (x) x) #t)';

  var text2 = '\n\
  (define foo (lambda (x)\n\
    (if (= x 100000)\n\
      x\n\
      (foo (+ x 1)))\n\
  ))\n\
  (foo 0)';
  var text3 = '\
  (define factorial\n\
    (lambda (n)\n\
      (if (= n 0) 1\n\
          (* n (factorial (- n 1))))))\n\
  (factorial 10)';

  var textbg1 = '\
  (определи а 1)\n\
  (тури! а 2)\n\
  (определи опа (правило (ш) (+ а ш)))\n\
  (опа 4)';

  var textbg2 = '\
  (определи а 1)\n\
  (присвои! а 2)\n\
  (определи опа (функция (ш) (+ а ш)))\n\
  (опа 4)';

  var textbg3 = '\
  (определи а 1)\n\
  (ако #в 1 2)';

  evaluate(textbg3, 'bg2');
}));