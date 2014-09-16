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
    var resolve = function resolve(basePath, path) {
      if (path.substr(-3) !== '.js') {
        path = path + '.js';
      }
      var pathSegments;
      if (path[0] === '/') {
        pathSegments = ['']; // '/'.split().pop()
      }
      else {
        pathSegments = basePath.split('/');
        pathSegments.pop();
      }
      var segment;
      var segments = path.split('/');
      for (var i = 0; i < segments.length; i++) {
        segment = segments[i];
        if (segment === '..') {
          pathSegments.pop();
        }
        else if (segment !== '' && segment !== '.') {
          pathSegments.push(segment);
        }
      }
      return pathSegments.join('/');
    };
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
      var moduleRequire = baseRequire(path);
      fn(moduleRequire, module, exports);
      module.path = path;
    };
    var baseRequire = function baseRequire(basePath) {
      return function require(path) {
        var modulePath = resolve(basePath, path);
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
    };
    window.require = baseRequire(baseDir);
  }
}());