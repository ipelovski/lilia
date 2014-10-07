/*
A simple lexer based on the grammer of r7rs small.
Transforms programming code to a stream of tokens.
NOTE: the implementation was first based on r4rs
so it needs a revision for r7rs correctness.
*/
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lang-table'));
  } else {
    root.lexer = factory(root.langTable);
  }
}(this, function (langTable) {
  'use strict';
  
  // The values used to tag tokens.
  // It seems there is no panalty for comparing
  // strings compared to numbers.
  // String values are easier to debug.
  var TokenTypes = {
    identifier: 'identifier',
    boolean: 'boolean',
    number: 'number',
    character: 'character',
    string: 'string',
    leftParen: 'leftParen',
    rightParen: 'rightParen',
    vectorParen: 'vectorParen',
    quote: 'quote',
    backquote: 'backquote',
    unquote: 'unquote',
    unquoteSplicing: 'unquoteSplicing',
    dot: 'dot'
  };
  // A constructor for iterators of tokens.
  // It reads a programming code character by character
  // and produces a token when a valid sequence is
  // recognised. Otherwise raises an error.
  function tokenize(text, lang) {
    // The index of the current character in the text.
    var idx = 0;
    // Sometimes the iterator needs to look at the next
    // character in order to recognise a token.
    // The previously read character is kept here
    // by explicitly assinging a value to the variable.
    var oldchar = null;
    // The line of the current character.
    var charLine = 1;
    // The column of the current character.
    var charColumn = 0;
    // The line of the current token.
    var line = 0;
    // The column of the current token.
    var column = 0;

    // The natural language used in the programming code.
    // The default value is English.
    lang = lang || 'en';
    // A utility function for getting translations of
    // tokens in the current natural language.
    var get = function get(key) {
      return langTable.get(lang, 'tokens', key);
    }
    // Constants containing the translations of several tokens.
    var charTrue = get('true');
    var charFalse = get('false');
    var charSpace = get('space');
    var charNewLine = get('newline')
    var charTab = get('tab');
    var whitespaceChars  = [charSpace[0], charNewLine[0], charTab[0]];

    // A constructor for token objects.
    // When the token iterator reads a valid token
    // it packs the token information in a token object.
    function createToken(type, value) {
      return {
        // The line on which the token starts.
        line: line,
        // TODO
        // Well if the token is composed by several characters
        // then the column will not be correct because it is
        // incremented on reading every character.
        column: column,
        // A tag describing the token.
        // Must be one of the values in TokenTypes.
        type: type,
        // The value of the token object.
        // Optional. A string or a boolean.
        value: value
      };
    }
    // When the current character is of type undefined
    // then this counter is incremented.
    // It is used for capturing unknown errors.
    var undefchar = 0;
    // Retrieves the next character in the text.
    function getNextChar() {
      var char;
      // If there is a previously read character then it is returned.
      if (oldchar !== null) {
        char = oldchar;
        oldchar = null;
        return char;
      }
      char = text[idx];
      idx += 1;
      if (char) {
        if (char === '\n') {
          charLine += 1;
          charColumn = 0;
        }
        else {
          charColumn += 1;
        }
      }
      // Checking for unknown errors.
      if (!char) {
        undefchar += 1;
      }
      if (undefchar > 1) {
        throw new Error('Parse error');
      }
      return char;
    }
    // Raises an error by throwing it.
    // This function is used only in the lexer.
    function raiseError(messageKey) {
      var message = langTable.get(lang, 'token-errors', messageKey);
      var err = new Error(message);
      err.line = line;
      err.column = column;
      err.toString = function() {
        return Error.prototype.toString.call(this) +
          ' (line: ' + this.line + ', column: ' + this.column + ')';
      };
      throw err;
    }
    // Reads all characters until a delimiter character is read.
    // Returns a string.
    function getCharsTillDelimeter() {
      var char = getNextChar();
      var charBuffer = [];
      while (char && !isDelimeter(char)) {
        charBuffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return charBuffer.join('');
    }
    // Reads the text charcter by character until a valid token is read.
    // Constructs a token object and returns it.
    function getNextToken() {
      // Holds the current character.
      var char = getNextChar();
      // If the token is composed by several characters
      // then these characters are kept in this variable as a string.
      var chars,
      // When the next character is needed to be read it is 
      // sometimes kept in this variable.
        nextChar;
      // Clears the previously read undefined characters.
      undefchar = 0;

      // Skips characters while they are atmosphere (whitespace or comment).
      while (true) {
        if (isWhitespace(char)) {
          char = getNextChar();
          continue;
        }
        if (char === ';') {
          while (char && char !== '\n') {
            char = getNextChar();
          }
          continue;
        }
        break;
      }
      // If no character is read after the atmosphere then there is no token.
      if (!char) {
        return null;
      }
      // 'char' holds the first character of the token,
      // so the character line and column
      // and token line and column are the same.
      line = charLine;
      column = charColumn;
      // Reads single character tokens.
      var token = getTokenFromChar(char);
      // If such a single character token is read then it is returned.
      if (token) {
        return token;
      }
      // Tokens starting with a '#'' are vectors, booleans and characters.
      if (char === '#') {
        char = getNextChar();
        if (!char) {
          raiseError('invalid_token');
        }
        // '#(' is the beginning of a vector literal
        if (char === '(') {
          return createToken(TokenTypes.vectorParen);
        }
        // '#t' and '#f' are the boolean literals
        if (char === charTrue || char === charFalse) {
          nextChar = getNextChar();
          // TODO #true and #false are also boolean literals
          if (nextChar && !isDelimeter(nextChar)) {
            raiseError('invalid_token');
          }
          oldchar = nextChar;
          return createToken(TokenTypes.boolean, char === charTrue);
        }
        // '#\character' are character literals
        if (char === '\\') {
          char = getNextChar();
          if (!char) {
            raiseError('invalid_char');
          }
          // '#\character name', etc. are special character literals
          // TODO not all character names are implemented
          if (whitespaceChars.indexOf(char) !== -1) {
            chars = char + getCharsTillDelimeter();
            if (chars.length > 1) {
              if (chars === charSpace || chars === charNewLine || chars === charTab) {
                return createToken(TokenTypes.character, chars);
              }
              else {
                raiseError('invalid_char');
              }
            }
          }
          nextChar = getNextChar();
          if (nextChar && !isDelimeter(nextChar)) {
            raiseError('invalid_char');
          }
          // TODO '#\xhex' is not implemented
          oldchar = nextChar;
          return createToken(TokenTypes.character, char);
        }
        raiseError('invalid_token');
        // TODO get number
      }
      // Tokens starting with '"' are string literals.
      if (char === '"') {
        return readString();
      }
      // Tokens starting with a digit are number literals.
      // TODO not all number literal formats are implemented.
      if (isDigit(char)) {
        return readNumber(char);
      }
      // If the character is a valid first character for an identifier
      // then the token is treated as an identifier.
      if (isInitial(char)) {
        return readIdentifier(char);
      }
      // Tokens starting with a dot are '.' (used in definitions),
      // '...' (used in macros),
      // float numbers (TODO).
      if (char === '.') {
        chars = getCharsTillDelimeter();
        if (chars.length === 0) {
          return createToken(TokenTypes.dot);
        }
        if (chars === '..') {
          return createToken(TokenTypes.identifier, '...');
        }
        raiseError('invalid_identifier');
      }
      // Tokens starting with a sign are numbers or arithmetic operations.
      if (char === '+' || char === '-') {
        nextChar = getNextChar();
        oldchar = nextChar;
        if (isDigit(nextChar)) {
          return readNumber(char);
        }
        else {
          return readPeculiarIdentifier(char);
        }
      }
      raiseError('invalid_token');
    }
    // A constant holding valid escaped characters in strings like \n, \r, \t.
    var validEscapedChars = get('valid_escaped_chars');
    var validEscapedCharCodes = [10, 13, 9]
    function guardString(char) {
      if (!char) {
        raiseError('invalid_string');
      }
    }
    // Reads a token representing a string literal and returns its object.
    function readString() {
      var buffer = [];
      var char = getNextChar();
      guardString(char);
      while(char !== '"') {
        if (char === '\\') {
          // TODO add also escaped ", \, etc
          char = getNextChar();
          guardString(char);
          var idx = validEscapedChars.indexOf(char);
          if (idx === -1) {
            raiseError('invalid_escaped_char');
          }
          buffer.push(String.fromCharCode(validEscapedCharCodes[idx]));
        }
        else {
          buffer.push(char);
        }
        char = getNextChar();
        guardString(char);
      }
      return createToken(TokenTypes.string, buffer.join(''));
    }
    // Reads a token representing a number literal and returns its object.
    function readNumber(char) {
      var buffer = [];
      var dot = false;
      if (char === '-' || char === '+') {
        buffer.push(char);
        char = getNextChar();
      }
      while (char && !isDelimeter(char)) {
        if (char === '.') {
          // If there is more than one dot in a number then it is invalid.
          if (dot) {
            raiseError('invalid_number');
          }
          dot = true;
        }
        // If there are non digit characters in the number then it is invalid.
        // NOTE: actually there are such cases and they need to be implemented.
        else if (!isDigit(char)) {
          raiseError('invalid_number');
        }
        buffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return createToken(TokenTypes.number, buffer.join(''));
    }
    // Reads a token representing an identifier, i.e. symbol.
    // TODO implement identifiers enclosed in vertical lines.
    function readIdentifier(char) {
      var buffer = [];
      while (char && !isDelimeter(char)) {
        if (!isSubsequent(char)) {
          raiseError('invalid_identifier');
        }
        buffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return createToken(TokenTypes.identifier, buffer.join(''));
    }
    // Reads a token representing a peculiar identifier, i.e. '+' or '-'.
    function readPeculiarIdentifier(char) {
      var nextChar = getNextChar();
      if (!nextChar || isDelimeter(nextChar)) {
        oldchar = nextChar;
        return createToken(TokenTypes.identifier, char);
      }
      else {
        raiseError('invalid_identifier');
      }
    }
    // Checks if a character is an initial one.
    function isInitial(char) {
      return isLetter(char) || isSpecialInitial(char);
    }
    // A hack function checking if a character is a letter.
    function isLetter(char) {
      return char.toLowerCase() !== char.toUpperCase();
    }
    // A constant containing the list of special initial characters.
    var specialInitials = /[!$%&*\/:<=>?^_~]/;
    // Checks if a character is a special initial one.
    function isSpecialInitial(char) {
      return specialInitials.test(char);
    }
    // A constant containing the list of digits.
    var digits = /[0-9]/;
    // Checks if a character is a digit.
    function isDigit(char) {
      return digits.test(char);
    }
    // A constant containing the list of special subsequent characters.
    var specialSubsequent = /[+-.@]/;
    // Checks if a character is a special subsequent one.
    function isSpecialSubsequent(char) {
      return specialSubsequent.test(char);
    }
    // Checks if a character is a subsequent one.
    function isSubsequent(char) {
      return isInitial(char) || isDigit(char) || isSpecialSubsequent(char);
    }

    // Checks if the character is a token and returns the token object.
    function getTokenFromChar(char) {
      if (char === '(') {
        return createToken(TokenTypes.leftParen);
      }
      if (char === ')') {
        return createToken(TokenTypes.rightParen);
      }
      if (char === '\'') {
        return createToken(TokenTypes.quote);
      }
      if (char === '`') {
        return createToken(TokenTypes.backquote);
      }
      if (char === ',') {
        return createToken(TokenTypes.unquote);
      }
      return null;
    }
    // Checks if a character is a whitespace.
    function isWhitespace(char) {
      return char === ' ' ||
        char === '\n' ||
        char === '\t';
    }
    // Checks if a character is a delimeter.
    function isDelimeter(char) {
      return isWhitespace(char) ||
        char === '(' ||
        char === ')' ||
        char === '"' ||
        char === ';';
    }
    // Returns the position of the last read token.
    function getPosition() {
      return {
        line: line,
        column: column
      }
    }
    // Returns a token iterator.
    return {
      getNextToken: getNextToken,
      getPosition: getPosition
    };
  }
  // The construction of the TokenStream type.
  // It wraps a token iterator and provides
  // functionality for peeking next tokens without consuming them.
  function TokenStream(text, lang) {
    // A cahce holding the peeked tokens.
    // If the cache is not empty then tokens are
    // retrieved from it one by one and consumed.
    this.cache = [];
    // The token iterator that is wrapped.
    this.tokenIterator = tokenize(text, lang);
  }
  // Peeks a token without consuming it.
  // Peeked tokens are kept in a local cache.
  // 'idx' argument specifies how many tokens
  // to be skipped before returning a token.
  // Default is 0. An integer.
  TokenStream.prototype.peek = function peek(idx) {
    idx = idx || 0;
    if (this.cache && this.cache.length > idx) {
      return this.cache[idx];
    }
    var token;
    do {
      token = this.tokenIterator.getNextToken();
      this.cache.push(token);
      if (!token) {
        break;
      }      
    }
    while (this.cache.length < idx);
    return token;
  };
  // Consumes a token and returs it.
  // If the local cache of tokens is not empty
  // then it is traversed first.
  // 'howMany' argument specifies how many tokens
  // to be consumed. The last consumed token is returned.
  // Default is 1. An integer.
  TokenStream.prototype.advance = function advance(howMany) {
    howMany = howMany || 1;
    var token;
    while (howMany) {
      if (this.cache.length) {
        token = this.cache.shift();
      }
      else {
        token = this.tokenIterator.getNextToken();
      }
      howMany -= 1;
    }
    return token;
  };
  // Returns the position of the last consumed token.
  TokenStream.prototype.getPosition = function() {
    return this.tokenIterator.getPosition();
  };

  // Exports the TokenStream type and the TokenTypes tag values.
  return {
    TokenStream: TokenStream,
    TokenTypes: TokenTypes
  };
}));