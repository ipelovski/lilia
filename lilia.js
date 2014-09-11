(function () {
  if (typeof require === 'undefined' && typeof window !== 'undefined') {
    var basePath = window.location.pathname;    
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
      var fn = new Function('require', 'module', 'exports', code);
      var exports = module.exports = {};
      fn(require, module, exports);
      return {
        exports: module.exports,
        path: path
      };
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
        else if (segment !== '') {
          modulePathSegments.push(segment);
        }
      }
      var modulePath = modulePathSegments.join('/');
      var module;
      if (modulePath in modules) {
        module = modules[modulePath];
      }
      else {
        // this should fix cyclic modules, not tested
        module = modules[modulePath] = {};
        module = modules[modulePath] = loadModule(path, module);
      }
      return module.exports;
    };
    window.require = requireFn;
  }

  require('./common');
  require('./lang-en');
  var evaluator = require('./evaluator');

  if (typeof window !== 'undefined'){
    window.evaluator = evaluator;
  }
}());