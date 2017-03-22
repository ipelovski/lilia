(function () {
  if (typeof require === 'undefined' && typeof window !== 'undefined') {
    var currentScript = document.currentScript || (function() {
      var scripts = document.getElementsByTagName('script');
      return scripts[scripts.length - 1];
    })();
    var a = document.createElement('a');
    a.href = window.location.href;
    var origin = a.origin;
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
        throw new Error('cannot load file ' + path);
      }
    };
    var loadScript = function loadScript(path) {
      var script = document.createElement("script")
      script.type = "text/javascript";
      script.src = path;
      document.getElementsByTagName("head")[0].appendChild(script);
    };
    var loadModule = function loadModule(path, module) {
      var code = loadFile(path);
      // adds the code to the source view of chrome dev tools
      code = code + '//# sourceURL=' + origin + path;
      var fn;
      try {
        fn = new Function('require', 'module', 'exports', code);
      }
      catch (e) {
        if (e instanceof SyntaxError) {
          // there is a syntax error in the file
          // load the file as a script so the browser should display the location of the syntax error
          loadScript(path);
          return;
        }
        else {
          throw e;
        }
      }
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