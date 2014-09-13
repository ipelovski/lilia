(function () {
  if (typeof require === 'undefined' && typeof window !== 'undefined') {
    var currentScript = document.currentScript || (function() {
      var scripts = document.getElementsByTagName('script');
      return scripts[scripts.length - 1];
    })();
    var a = document.createElement('a');
    a.href = currentScript.src;
    var basePath = a.pathname;
    var baseDir = basePath.substring(0, basePath.lastIndexOf('/') + 1);
    var modules = {};
    var loadFile = function loadFile(path) {
      var request = new XMLHttpRequest();
      request.open('GET', path, false);  // `false` makes the request synchronous
      request.send(null);

      if (request.status === 200) {
        return request.responseText;
      }
      else {
        throw new Error('cannot load file');
      }
    };
    var loadModule = function loadModule(path, module) {
      var code = loadFile(path);
      // adds the code to the source view of chrome dev tools
      code = code + '//@ sourceURL=' + path + '\n//# sourceURL=' + path;
      var fn = new Function('require', 'module', 'exports', code);
      fn.displayName = path; // sets a friendly name for the call stack
      var exports = module.exports = {};
      // change the base directory to the one of the module currently being loaded
      var oldBaseDir = baseDir;
      baseDir = path.substring(0, path.lastIndexOf('/') + 1);
      fn(require, module, exports);
      // return the base directory to its old value
      baseDir = oldBaseDir;
      module.path = path;
    };
    var requireFn = function require(path) {
      if (path.substr(-3) !== '.js') {
        path = path + '.js';
      }
      var modulePathSegments;
      if (path[0] === '/') {
        modulePathSegments = ['']; // '/'.split().pop()
      }
      else {
        modulePathSegments = baseDir.split('/');
        modulePathSegments.pop();
      }
      var segment;
      var segments = path.split('/');
      for (var i = 0; i < segments.length; i++) {
        segment = segments[i];
        if (segment === '..') {
          modulePathSegments.pop();
        }
        else if (segment !== '' && segment !== '.') {
          modulePathSegments.push(segment);
        }
      }
      var modulePath = modulePathSegments.join('/');
      var module;
      if (modulePath in modules) {
        module = modules[modulePath];
      }
      else {
        // this fixes cyclic modules
        module = modules[modulePath] = {};
        loadModule(modulePath, module);
      }
      return module.exports;
    };
    window.require = requireFn;
  }

  require('./src/common');
  var langEn = require('./src/lang-en');
  var langBg = require('./src/lang-bg');
  var evaluator = require('./src/evaluator');

  if (typeof window !== 'undefined'){
    window.lilia = {
      evaluate: evaluator.evaluate,
      initEval: evaluator.initEval,
      setOutputPortHandler: evaluator.setOutputPortHandler,
      'lang-en': langEn,
      'lang-bg': langBg,
    }
  }
}());