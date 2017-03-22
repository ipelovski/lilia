if (typeof lilia === 'undefined') {
  require('../lilia');
}
var keywords = [];
function setKeywords(lang) {
  var langData = lilia['lang-' + lang];
  keywords = Object.keys(langData.syntax)
    .map(function (key) {
      return langData.syntax[key];
    })
    .concat(Object.keys(langData.procedures)
      .map(function (key) {
        return langData.procedures[key];
      }));
}
function runSchemeAndPrint(input) {
  var result;
  try {
    result = session.evaluate(input);
    if (result !== null || result !== undefined) {
      if (result.valueOf().stack) {
        consoleError(result.toString());
      }
      else {
        consoleLog(result.toString());
      }
    }
  }
  catch (err) {
    result = err.toString();
    if (typeof err.line === 'number') {
      codeEditor.setCursor(err.line - 1, err.column - 1);
    }
    consoleError(result);
  }
}

var symbolRegex = '[\\wа-яА-Я_\\-=\\/]+';
// console initialization
var consoleSuccessClassName = 'jquery-console-message-success';
var consoleErrorClassName = 'jquery-console-message-error';
var customConsoleElement = jQuery('#console');
var consoleRegex = new RegExp(symbolRegex + '$', 'g');
var customConsole = customConsoleElement.console({
  promptLabel: '> ',
  commandValidate: function(line) {
    //return line !== '';
    return true;
  },
  commandHandle: function (line) {
    runSchemeAndPrint(line);
    return [];
  },
  autofocus: false,
  animateScroll: true,
  promptHistory: true,
  fadeOnReset: false,
  //welcomeMessage: 'Enter some Scheme expressions to evaluate. You can use Tab for autocomplete.',
  cols: 90,
  completeHandle: function (input) {
    var match = input.match(consoleRegex);
    if (!match) {
      return [];
    }
    var word = match[0];        
    var matched = keywords.filter(function (keyword) {
      return keyword.indexOf(word) === 0;
    });
    if (matched.length === 1) {
      return [matched[0].substr(word.length)];
    }
    else {
      return matched;
    }
  },
});
function innerConsoleLog(text, cssClass) {
  if (text) {
    customConsole.commandResult(text, cssClass);
  }
  else {
    customConsole.commandResult([]);
  }
}
function consoleLog(text) {
  text = text.replace(/</g, '&lt;').replace(/>g/, '&gt;');
  innerConsoleLog(text, consoleSuccessClassName);
}
function setEditorCursor(line, column) {
  codeEditor.setCursor(line - 1, column - 1);
  codeEditor.focus();
}
var errorStackRegex = /\(line: (\d+), column: (\d+)\)/g;
function consoleError(text) {     
  text = text.replace(errorStackRegex, function (txt) {
    errorStackRegex.lastIndex = 0;
    var match = errorStackRegex.exec(txt);
    return '<a href="javascript:setEditorCursor(' + match[1] + ', ' + match[2] + ')">' + txt + '</a>';
  });
  innerConsoleLog(text, consoleErrorClassName);
}

var editorRegex = new RegExp(symbolRegex);
function showCursorPosition() {
  var pos = codeEditor.getCursor();
  document.getElementById('cursor-position').innerText = 'Line ' + (pos.line + 1) + ', Column ' + (pos.ch + 1);
} 

var codeEditor = CodeMirror.fromTextArea(document.getElementById('editor'), {
  theme: 'monokai',
  indentUnit: 4,
  keyMap: 'extra',
  lineNumbers: true,
  matchBrackets: true,
  autoCloseBrackets: true,
  autofocus: true,
  mode: {
    name: 'scheme',
    keywords: (function () {
      var keywords = [];
      ['en', 'bg'].forEach(function (lang) {
        var langData = lilia['lang-' + lang];
        keywords = keywords
          .concat(Object.keys(langData.syntax)
            .map(function (key) {
              return langData.syntax[key];
            }))
          .concat(Object.keys(langData.procedures)
            .map(function (key) {
              return langData.procedures[key];
            }));
      });
      return keywords;
    }()),
    indentKeys: (function () {
      var keys = [];
      ['en', 'bg'].forEach(function (lang) {
        var langData = lilia['lang-' + lang];
        keys = keys.concat([
          langData.syntax['define'],
          langData.syntax['let'],
          langData.syntax['let*'],
          langData.syntax['letrec'],
          langData.syntax['letrec*'],
          langData.syntax['lambda']
        ]);
      });
      return keys;
    }()),
  },
  extraKeys: { 'Ctrl-Space': 'autocomplete' },
  autoCloseBrackets: {
    pairs: '()""'
  },
  codeInfo: {
    tooltips: false
  },     
});
codeEditor.setSize(800, 400);
codeEditor.on('cursorActivity', showCursorPosition);
codeEditor.setValue('');
codeEditor.setCursor(1, 0);

var signatures = {
  'define': '<i>(define variable expression)</i><br/>or<br/><i>(define (name argument ...)<br/>&nbsp;&nbsp;&nbsp;&nbsp;expression ...)</i>',
  'list-copy': '<i>(list-copy obj)</i></br>Returns a new list containing the elements of the given list.',
};

showCursorPosition();

lilia.setOutputPortHandler(function (data) {
  var lines = data.split('\n');
  var line;
  for (var i = 0, l = lines.length - 1; i < l; i++) {
    line = lines[i];
    customConsole.message(line);
    customConsole.message('', 'jquery-console-message-break');
  }
  line = lines[lines.length - 1];
  // '\n'.split('\n') => ['', ''], so we don't need to output the last empty string
  if (line !== '') {
    customConsole.message(line);
  }
});
var editorLang; // the human language used for the evaluator
var session; // the evaluation session
function init(lang) {
  editorLang = lang;
  session = lilia.session(lang);
  setKeywords(lang);
}
init('en');

function runScheme() {
  var input = codeEditor.getValue();
  customConsole.promptText('Evaluating...');
  runSchemeAndPrint(input);
  codeEditor.focus();
}
document.getElementById('button-run').addEventListener('click', runScheme);
document.getElementById('en-en-lang').addEventListener('click', function() {
  init('en');
});
document.getElementById('bg-bg-lang').addEventListener('click', function() {
  init('bg');
});
document.getElementById('button-clear').addEventListener('click', function () {
  customConsole.reset();
});