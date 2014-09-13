// function readCaseClause(tokenStream) {
//   var token = tokenStream.advance();
//   if (!token) {
//     raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//   }
//   if (token.type === TokenTypes.leftParen) {
//     token = tokenStream.advance();
//     if (!token) {
//       raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//     }
//     var elseFound = false;
//     var data = [];
//     if (isIdentifier(token) && token.value === syntax['else']) {
//       elseFound = true;
//     }
//     if (token.type === TokenTypes.leftParen) {
//       var datum = readExpression(tokenStream);
//       while (datum) {
//         if (datum.type !== FormTypes.identifier) {
//           raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//         }
//         else {
//           data.push(datum);
//           datum = readExpression(tokenStream);
//         }
//       }
//       token = tokenStream.advance();
//       if (!token) {
//         raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//       }
//       if (token.type !== TokenTypes.rightParen) {
//         raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//       }
//     }
//     var expressions = [];
//   }
// }
// function readCase(tokenStream) {
//   var key = readExpression(tokenStream);
//   if (!key) {
//     raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//   }
//   var clause = readCaseClause(tokenStream);
//   if (!clause) {
//     raiseSyntaxError(tokenStream, 'bad_syntax'); // TODO make it specific
//   }
//   var clauses = [clause];
//   clause = readCaseClause(tokenStream);
//   while (clause) {
//     clauses.push(clause);
//     clause = readCaseClause(tokenStream);
//   }
// }