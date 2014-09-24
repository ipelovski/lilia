function testOK(code, expected) {
  var fn = function () {
    var value = lilia.evaluate(code);
    if (typeof expected === 'string') {
      value = value.toString();
    }
    expect(value).to.equal(expected);
  };
  fn.toString = function toString() {
    return code;
  };
  return fn;
}

function testFail(code, errorMessage) {
  var fn = function () {
    var value = lilia.evaluate(code);
    expect(value).to.have.property('message');
    expect(value.message).to.contain(errorMessage);
  };
  fn.toString = function toString() {
    return code;
  };
  return fn;
}

function testSkip(code, expected) {
  return function () {
    var value = lilia.evaluate(code);
    if (typeof expected === 'string') {
      value = value.toString();
    }
    expect(value).to.equal(expected);
  };
}

function loadSuite(data) {
  var prop, tests = [], suites = [];
  for (var key in data) {
    prop = data[key];
    if (typeof prop === 'object') {
      if (Array.isArray(prop)) {
        prop.forEach(function (test, i) {
          tests.push({ title: key + ' ' + i, fn: test });
        });
      }
      else {
        suites.push({ title: key, fn: loadSuite(prop) });
      }
    }
    if (typeof prop === 'function') {
      tests.push({ title: key, fn: prop });
    }
  }
  return function () {
    suites.forEach(function (suite) {
      describe(suite.title, suite.fn);
    });
    tests.forEach(function (test) {
      it(test.title, test.fn);
    });
  }
}

exports.testOK = testOK;
exports.testFail = testFail;
exports.loadSuite = loadSuite;

// var root = {};
// var suites = [root];
// exports.suite = function suite(title) {
//   var suite = {};
//   suites[0][title] = suite;
//   suites.unshift(suite);
// };
// exports.endSuite = function endSuite(title) {
//   if (typeof title === 'undefined') {
//     throw new Error('suite title expected');
//   }
//   suites.shift();
//   if (suites.length === 1) {
//     loadSuite(root)();
//     root = {};
//     suites = [root];
//   }
// };
// exports.testOK = function testOK(title, code, expected) {
//   suites[0][title] = function () {
//     var value = lilia.evaluate(code);
//     if (typeof expected === 'string') {
//       value = value.toString();
//     }
//     expect(value).to.equal(expected);
//   };
// };
// exports.testFail = function testFail(title, code, errorMessage) {
//   suites[0][title] = function () {
//     var value = lilia.evaluate(code);
//     expect(value).to.have.property('message');
//     expect(value.message).to.contain(errorMessage);
//   };
// };