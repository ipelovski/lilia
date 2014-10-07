Λилия / Lilia
=============

Lilia is a Scheme-like programming language running in the browser. The main goal of the language is to be easily translated to non-English languages. Since I'm a Bulgarian, the second language for the interpreter is Bulgarian. You can try the demo [here](http://ipelovski.github.io/lilia/editor/editor.html). Another goal is to provide descriptive error messages but this is still work in progress.

How to use
==========

Add Lilia to your page by adding the following markup to your HTML:
```
<script src="path/to/lilia-packed.js"></script>
```
The code adds a global varialbe `lilia` which is an entry point for the JavaScript API.

The JavaScript API is:
lilia.session([language='en'])
------------------------------
Starts a new session of the interpreter in the given language. The default language is English (en). All code, evaluated in a single session, shares a common global environment.
```
var session = lilia.session('en');
var result = session.evaluate('(list 1 2 3)');
```
Every session has the following method:

session.evaluate(code)
----------------------
Evaluates the given code in the common environment of the session and returns the result of the evaluation. The result is an object with the following methods:

result.toString()
-----------------
Converts a result from an evaluation to Scheme external representation.
```
var session = lilia.session('en');
var result = session.evaluate('(list 1 2 3)');
console.log(result.toString());
// prints (1 2 3)
```

result.toJS()
-------------
Converts recursively a result from an evaluation to JavaScript objects. Primitive types are reused (booleans, numbers). Proper lists and vectors are converted to arrays. Association lists are converted to plain objects.
```
var session = lilia.session('en');
var result = session.evaluate('(list 1 2 3)');
console.log(result.toJS());
// prints [1, 2, 3]
```

Implementation status
=====================
Lilia is based on the R7RS(small) Scheme report. There are still some inconsistencies and missing parts from the specification. The main are:
* no numeric tower, only the floats used in JavaScript
* no macros
* still some unimplemented procedures
* no bytevectors and records yet

What is in Lilia?
* All vector, pair, list procedures
* Most of the number procedures
* tail calls
* call/cc

License
=======
MIT
