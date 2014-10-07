// triggering autocomplete when pressing 'tab'
CodeMirror.keyMap.extra = {
  Tab: function(cm) {
    var pos = cm.getCursor();
    var line = cm.getLine(pos.line);
    var ch = line[pos.ch - 1];
    if (ch !== undefined && editorRegex.test(ch)) {
      CodeMirror.showHint(cm);
      var newPos = cm.getCursor();
      if (pos.ch === newPos.ch // nothing is inserted
        && cm.state.completionActive === null) { // no hint widget is displayed
        cm.execCommand('insertTab');
      }
    }
    else {
      cm.execCommand('insertTab');
    }
  },
  fallthrough: 'default'
};

// autocomplete for scheme syntax and procedures

// widget displaying a description for a syntax or a procedure
function DescWidget(cm, hintsWidget) {
  var desc = document.createElement('div');
  desc.className = 'hint-description';

  var widgetBox = hintsWidget.hints.getBoundingClientRect();
  var left = widgetBox.right;
  var top = widgetBox.top;
  desc.style.left = left + 'px';
  desc.style.top = top + 'px';
  document.body.appendChild(desc);

  var startScroll = cm.getScrollInfo();
  cm.on('scroll', this.onScroll = function() {
    var curScroll = cm.getScrollInfo(), editor = cm.getWrapperElement().getBoundingClientRect();
    var newTop = top + startScroll.top - curScroll.top;
    var point = newTop - (window.pageYOffset || (document.documentElement || document.body).scrollTop);
    if (point <= editor.top || point >= editor.bottom) return this.close();
    desc.style.top = newTop + 'px';
    desc.style.left = (left + startScroll.left - curScroll.left) + 'px';
  });

  var descStyle = window.getComputedStyle(desc);
  var borderTopWidth = parseInt(descStyle['border-top-width']);
  borderTopWidth += parseInt(descStyle['margin-top']);
  borderTopWidth += parseInt(descStyle['padding-top']);
  
  this.move = function (el) {
    var elBox = el.getBoundingClientRect();
    desc.style.top = (elBox.top - borderTopWidth) + 'px';
  };
  this.show = function (text) {
    if (!text) {
      desc.style.display = 'none';
      desc.innerHTML = '';
      return;
    }
    else {
      if (desc.style.display === 'none') {
        desc.style.display = 'block';
      }
      text = signatures[text] || text;
      text = text.replace(/\n\r|\n|\r|\t/g, function (match) {
        switch (match) {
          case '\n\r':
          case '\n':
          case '\r':
            return '<br />';
          case '\t':
            return '&nbsp;&nbsp;&nbsp;&nbsp;';
        }
        return '';
      });
      desc.innerHTML = text;
    }
  };
  this.close = function () {
    desc.parentNode.removeChild(desc);
  };
}
CodeMirror.hint.scheme = function(cm) {
  var inner = CodeMirror.hint.anyword(cm);
  var list = inner.list;
  var token = cm.getTokenAt(cm.getCursor());
  if (token && (token.type === 'builtin'
    || token.type === 'variable' || token.type === 'string')) {
    var word = token.string;
    keywords.forEach(function (keyword) {
      if (keyword.indexOf(word) === 0 && list.indexOf(keyword) === -1) {
        list.push(keyword);
        // list.push({
        //  text: keyword,
        //  render: function (elt, data, cur){
        //    elt.appendChild(document.createTextNode(keyword));
        //    var desc = document.createElement('span');
        //    desc.appendChild(document.createTextNode('(call-with-current-continuation procedure)'));
        //    desc.className = 'hint-description';
        //    elt.appendChild(desc);
        //  }
        // });
      }
    });
  }
  else {
    list.push.apply(list, keywords);
  }
  var selectedWord = null;
  var descWidget = null;
  CodeMirror.on(inner, 'shown', function () {
    var widget = cm.state.completionActive.widget;
    descWidget = new DescWidget(cm, widget);
    if (selectedWord) {
      descWidget.show(selectedWord);
    }
  });
  CodeMirror.on(inner, 'select', function (word, element) {
    if (!signatures[word]) {
      word = '';
    }
    if (descWidget) {
      descWidget.show(word);
      descWidget.move(element);
    }
    else {
      selectedWord = word;          
    }
  });
  CodeMirror.on(inner, 'close', function () {       
    descWidget.close();
    descWidget = null;
  });
  inner.list = list.sort();
  return inner;
};

// code tooltips
(function () {
  'use strict';
  var word = /[\wа-яА-Я\$_\-!$%&*+\.\/:<=>?@\^~]/;
  function showTooltip(cm, e, content, node, curpos) {
    var tt = document.createElement("div");
    tt.className = "CodeMirror-lint-tooltip";
    tt.appendChild(content.cloneNode(true));
    document.body.appendChild(tt);

    function position(e) {
      // var cur = curpos;//cm.getCursor()
      // var curLine = cm.getLine(cur.line);
      // var start = cur.ch, end = start;
      // while (end < curLine.length && word.test(curLine.charAt(end))) ++end;
      // while (start && word.test(curLine.charAt(start - 1))) --start;

      // var pos = cm.cursorCoords(CodeMirror.Pos(cur.line, start));
      // var left = pos.left, top = pos.bottom, below = true;
      // tt.style.left = left + "px";
      // tt.style.top = top + "px";
      // // If we're at the edge of the screen, then we want the menu to appear on the left of the cursor.
      // var winW = window.innerWidth || Math.max(document.body.offsetWidth, document.documentElement.offsetWidth);
      // var winH = window.innerHeight || Math.max(document.body.offsetHeight, document.documentElement.offsetHeight);
      // document.body.appendChild(tt);
      // var box = tt.getBoundingClientRect(), overlapY = box.bottom - winH;
      // if (overlapY > 0) {
      //   var height = box.bottom - box.top, curTop = pos.top - (pos.bottom - box.top);
      //   if (curTop - height > 0) { // Fits above cursor
      //     tt.style.top = (top = pos.top - height) + "px";
      //     below = false;
      //   } else if (height > winH) {
      //     tt.style.height = (winH - 5) + "px";
      //     tt.style.top = (top = pos.bottom - box.top) + "px";
      //     var cursor = cm.getCursor();
      //     if (data.from.ch != cursor.ch) {
      //       pos = cm.cursorCoords(cursor);
      //       tt.style.left = (left = pos.left) + "px";
      //       box = tt.getBoundingClientRect();
      //     }
      //   }
      // }
      // var overlapX = box.left - winW;
      // if (overlapX > 0) {
      //   if (box.right - box.left > winW) {
      //     tt.style.width = (winW - 5) + "px";
      //     overlapX -= (box.right - box.left) - winW;
      //   }
      //   tt.style.left = (left = pos.left - overlapX) + "px";
      // }

      //if (!tt.parentNode) return CodeMirror.off(document, "mousemove", position);
      tt.style.top = Math.max(0, e.clientY - tt.offsetHeight - 5) + "px";
      tt.style.left = (e.clientX + 5) + "px";
    }
    //CodeMirror.on(document, "mousemove", position);
    position(e);
    if (tt.style.opacity != null) tt.style.opacity = 1;
    return tt;
  }
  function rm(elt) {
    if (elt.parentNode) elt.parentNode.removeChild(elt);
  }
  function hideTooltip(tt) {
    if (!tt.parentNode) return;
    if (tt.style.opacity == null) rm(tt);
    tt.style.opacity = 0;
    setTimeout(function() { rm(tt); }, 600);
    // rm(tt);
  }
  function showTooltipFor(cm, e, content, node, pos) {
    var tooltip = showTooltip(cm, e, content, node, pos);
    function hide() {
      CodeMirror.off(node, "mouseout", hide);
      if (tooltip) { hideTooltip(tooltip); tooltip = null; }
    }
    var poll = setInterval(function() {
      if (tooltip) for (var n = node;; n = n.parentNode) {
        if (n == document.body) return;
        if (!n) { hide(); break; }
      }
      if (!tooltip) return clearInterval(poll);
    }, 400);
    CodeMirror.on(node, "mouseout", hide);
  }
  function annotationTooltip(word) {
    var severity;
    if (!severity) severity = "error";
    var tip = document.createElement("div");
    tip.className = "CodeMirror-lint-message-" + severity;
    var text = document.createElement('div');
    text.innerHTML = signatures[word] || word;
    tip.appendChild(text);
    return tip;
  }
  function popupSpanTooltip(cm, word, e, pos) {
    var target = e.target || e.srcElement;
    showTooltipFor(cm, e, annotationTooltip(word), target, pos);
  }
  function onMouseOver(cm, e) {
    var node = e.target || e.srcElement;
    if (!/\bcm-builtin/.test(node.className)) {
      return;
    }

    var box = node.getBoundingClientRect();
    var x = (box.left + box.right) / 2;
    var y = (box.top + box.bottom) / 2;
    var pos = cm.coordsChar({left: x, top: y}, "client");
    var token = cm.getTokenAt(pos);
    if (token && (token.type === 'builtin'
      || token.type === 'variable' || token.type === 'string')) {
      var word = token.string;
      if (keywords.indexOf(word) !== -1) {
        var cancelTooltip = function () {
          CodeMirror.off(node, "mousemove", moveTooltip);
          CodeMirror.off(node, "mouseout", cancelTooltip);
          clearTimeout(displayTooltip);
        }
        var displayTooltip = setTimeout(function () {
          CodeMirror.off(node, "mousemove", moveTooltip);
          CodeMirror.off(node, "mouseout", cancelTooltip);
          popupSpanTooltip(cm, word, e, pos);
        }, 500)
        CodeMirror.on(node, "mouseout", cancelTooltip);
        var moveTooltip = function (me) {
          e = me;
        }
        CodeMirror.on(node, "mousemove", moveTooltip);
        return //popupSpanTooltip(cm, word, e, pos);
      }
    }
  }

  function CodeInfo(cm, options) {
    this.marked = [];
    this.options = options;
    this.timeout = null;
    this.onMouseOver = function(e) { onMouseOver(cm, e); };
  }

  function parseOptions(cm, options) {
    if (!options || options === true) options = {};
    return options;
  }
  CodeMirror.defineOption("codeInfo", false, function(cm, val, old) {
    if (old && old != CodeMirror.Init) {
      CodeMirror.off(cm.getWrapperElement(), "mouseover", cm.state.codeInfo.onMouseOver);
      delete cm.state.codeInfo;
    }

    if (val) {
      var state = cm.state.codeInfo = new CodeInfo(cm, parseOptions(cm, val));
      if (state.options.tooltips !== false) {
        CodeMirror.on(cm.getWrapperElement(), "mouseover", state.onMouseOver);
      }
    }
  });
}());