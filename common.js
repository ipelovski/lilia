/*
A module containing common functions.
*/
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define([], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory();
  } else {
    factory();
  }
}(this, function () {
  // Formats a string similar to the .net String.Format() method.
  if (!String.format) {
    String.format = function(format, args) {
      if (!Array.isArray(args)) {
        args = Array.prototype.slice.call(arguments, 1);
      }
      return format.replace(/{(\d+)}/g, function(match, number) { 
        return typeof args[number] !== 'undefined'
          ? args[number] 
          : match;
      });
    };
  }
}));