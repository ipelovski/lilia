var loadSuite = require('./src/test-fns').loadSuite;

loadSuite(require('./src/evaluator-test'))();
loadSuite(require('./src/procedures/pair-list-test'))();
loadSuite(require('./src/procedures/string-test'))();
loadSuite(require('./src/procedures/vector-test'))();