require('./src/common');
var langEn = require('./src/lang-en');
var langBg = require('./src/lang-bg');
var evaluator = require('./src/evaluator');

if (typeof window !== 'undefined'){
  window.lilia = {
    evaluate: evaluator.evaluate,
    session: evaluator.session,
    setOutputPortHandler: evaluator.setOutputPortHandler,
    string: evaluator.string,
    'lang-en': langEn,
    'lang-bg': langBg,
  }
}