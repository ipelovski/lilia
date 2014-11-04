'use strict';

require('./src/common');
var langEn = require('./src/lang-en');
var langBg = require('./src/lang-bg');
var evaluator = require('./src/evaluator');

if (typeof window !== 'undefined') {
  window.lilia = {
    evaluate: evaluator.evaluate,
    session: evaluator.session,
    setOutputPortHandler: evaluator.setOutputPortHandler,
    string: evaluator.string,
    'lang-en': langEn,
    'lang-bg': langBg,
  };

  var lineContent = '';
  var output = function (data) {
    if (data[data.length - 1] !== '\n') {
      lineContent += data;
    }
    else {
      console.log(lineContent + data.substring(0, data.length - 1));
      lineContent = '';
    }
  };
  evaluator.setOutputPortHandler(output);

  // borrowed the code from processing.js
  var init = function () {
    document.removeEventListener('DOMContentLoaded', init, false);

    var scripts = document.getElementsByTagName('script');
    for (var i = 0; i < scripts.length; i++) {
      var script = scripts[i];
      var type = script.getAttribute('type').toLowerCase();
      var process = false;
      var lang = 'en';
      if (type === 'text/scheme' || type === 'text/lilia') {
        process = true;
      }
      else if (type.indexOf('text/lilia-') === 0) {
        process = true;
        lang = type.substring('text/lilia-'.length);
      }
      if (process) {
        evaluator.evaluate(script.textContent || script.text, lang);
        if (lineContent) {
          output('\n');
        }
      }
    }
  };
  document.addEventListener('DOMContentLoaded', init, false);
}