(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['langTable'], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory(require('./lang-table'));
  } else {
    root.lexer = factory(root.langTable);
  }
}(this, function (langTable) {
  var TokenTypes = {
    identifier: 'identifier',
    boolean: 'boolean',
    number: 'number',
    character: 'character',
    string: 'string',
    leftParan: 'leftParan',
    rightParen: 'rightParen',
    vectorParen: 'vectorParen',
    quote: 'quote',
    backquote: 'backquote',
    unquote: 'unquote',
    unquoteSplicing: 'unquoteSplicing',
    dot: 'dot'
  };
  function tokenize(text, lang) {
    var idx = 0;
    var oldchar = null;
    var line = 1;
    var column = 0;

    lang = lang || 'en';
    var get = function get(key) {
      return langTable.get(lang, 'tokens', key);
    }
    var charTrue = get('true');
    var charFalse = get('false');
    var charSpace = get('space');
    var charNewLine = get('newline')
    var charTab = get('tab');
    var whitespaceChars  = [charSpace[0], charNewLine[0], charTab[0]];

    function createToken(type, value) {
      return {
        line: line,
        column: column,
        type: type,
        value: value
      };
    }
    var undefchar = 0;
    function getNextChar() {
      if (oldchar !== null) {
        var char = oldchar;
        oldchar = null;
        return char;
      }
      var char = text[idx];
      idx += 1;
      if (char) {
        if (char === '\n') {
          line += 1;
          column = 1;
        }
        else {
          column += 1;
        }
      }
      // console.log('next char', char);
      if (!char) undefchar++;
      if (undefchar > 10) {
        throw new Error('Parse error');
      }
      return char;
    }
    function raiseError(messageKey) {
      var message = langTable.get(lang, 'token-errors', messageKey);
      var err = new Error(message);
      err.line = line;
      err.column = column;
      err.toString = function() {
        return Error.prototype.toString.call(this) +
          ' Line: ' + this.line + ', column: ' + this.column + '.';
      };
      throw err;
    }
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
    function getNextToken() {
      var char = getNextChar();
      var chars, nextChar;
      oldchar = null;

      // skip while is atmosphere (whitespace or comment)
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
      if (!char) {
        return null;
      }
      var token = getTokenFromChar(char);
      if (token) {
        return token;
      }
      if (char === '#') {
        char = getNextChar();
        if (!char) {
          raiseError('invalid_token');
        }
        if (char === '(') {
          return createToken(TokenTypes.vectorParen);
        }
        if (char === charTrue || char === charFalse) {
          nextChar = getNextChar();
          if (!isDelimeter(nextChar)) {
            raiseError('invalid_token');
          }
          oldchar = nextChar;
          return createToken(TokenTypes.boolean, char === charTrue);
        }
        if (char === '\\') {
          char = getNextChar();
          if (!char) {
            raiseError('invalid_char');
          }
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
          oldchar = nextChar;
          return createToken(TokenTypes.character, char);
        }
        raiseError('invalid_token');
        // TODO get number
      }
      if (char === '"') {
        return readString();
      }
      if (isDigit(char)) {
        return readNumber(char);
      }
      if (isInitial(char)) {
        return readIdentifier(char);
      }
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
      return null;
    }
    var validEscapedChars = new RegExp('["\\' + get('valid_escaped_chars') + ']');
    function guardString(char) {
      if (!char) {
        raiseError('invalid_string');
      }
    }
    function readString() {
      var buffer = [];
      var char = getNextChar();
      guardString(char);
      while(char !== '"') {
        buffer.push(char);
        if (char === '\\') {
          char = getNextChar();
          guardString(char);
          if (!validEscapedChars.test(char)) {
            raiseError('invalid_escaped_char');
          }
          buffer.push(char);
        }
        char = getNextChar();
        guardString(char);
      }
      return createToken(TokenTypes.string, buffer.join(''));
    }
    function readNumber(char) {
      var buffer = [];
      var dot = false;
      if (char === '-' || char === '+') {
        buffer.push(char);
        char = getNextChar();
      }
      while (char && !isDelimeter(char)) {
        if (char === '.') {
          if (dot) {
            raiseError('invalid_number');
          }
          dot = true;
        }
        else if (!isDigit(char)) {
          raiseError('invalid_number');
        }
        buffer.push(char);
        char = getNextChar();
      }
      oldchar = char;
      return createToken(TokenTypes.number, buffer.join(''));
    }
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
    function isInitial(char) {
      return isLetter(char) || isSpecialInitial(char);
    }
    function isLetter(char) {
      return char.toLowerCase() !== char.toUpperCase();
    }
    var specialInitials = /[!$%&*\/:<=>?^_~]/;
    function isSpecialInitial(char) {
      return specialInitials.test(char);
    }
    var digits = /[0-9]/;
    function isDigit(char) {
      return digits.test(char);
    }
    var specialSubsequent = /[+-.@]/;
    function isSpecialSubsequent(char) {
      return specialSubsequent.test(char);
    }
    function isSubsequent(char) {
      return isInitial(char) || isDigit(char) || isSpecialSubsequent(char);
    }

    function getTokenFromChar (char) {
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
    function isWhitespace(char) {
      return char === ' ' ||
        char === '\n' ||
        char === '\t';
    }
    function isDelimeter(char) {
      return isWhitespace(char) ||
        char === '(' ||
        char === ')' ||
        char === '"' ||
        char === ';';
    }
    function getPosition() {
      return {
        line: line,
        column: column
      }
    }
    return {
      getNextToken: getNextToken,
      getPosition: getPosition
    };
  }
  function TokenStream(text, lang) {
    this.cache = [];
    this.tokenIterator = tokenize(text, lang);
  }
  TokenStream.prototype.peek = function peek(idx) {
    idx = idx || 0;
    if (this.cache && this.cache.length > idx) {
      return this.cache[idx];
    }
    var token;
    do {
      token = this.tokenIterator.getNextToken();
      if (!token) {
        break;
      }
      this.cache.push(token);
    }
    while (this.cache.length < idx);
    return token;
  };
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
      // console.log('token', token);
    }
    return token;
  };
  TokenStream.prototype.getPosition = function() {
    return this.tokenIterator.getPosition();
  };

  return {
    TokenStream: TokenStream,
    TokenTypes: TokenTypes
  };
}));