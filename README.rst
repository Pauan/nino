To install
==========

::

  git clone --recursive git://github.com/Pauan/nino.git nino

If you're using a browser::

  <script src="lib/esprima/esprima.js"></script>
  <script src="nino.js"></script>

If you're using AMD ``define``::

  define(["nino"], function (NINO) {
    ...
  })

If you're using CommonJS (including Node.js)::

  var NINO = require("nino")

Usage
=====

First, you must generate a Nino AST using any of the following functions::

  var code = NINO.op("+", ...)
  var code = NINO.opArray("+", [...])
  var code = NINO.fromJSON(["+", ...])
  var code = NINO.parse("...")

In addition, you can use ``NINO.fromAST`` and ``NINO.toAST`` to convert from/to the `SpiderMonkey AST <https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API>`_::

  var code = NINO.fromAST({ ... })
  NINO.toAST(code)

Now that you have a Nino AST, call either ``NINO.expression`` or ``NINO.statement``::

  code = NINO.expression(code)
  code = NINO.statement(code)

REPLs will generally want to use ``NINO.expression`` because they return a value. On the other hand, if you're putting the string into a file which will be loaded later, you should use ``NINO.statement``.

Now, you **must** call ``NINO.traverse`` on **all** of the files you want to compile::

  var scope = { foo: true, bar: true }
  code = NINO.traverse(code, scope)

In the above example, Nino will treat the variables ``foo`` and ``bar`` as being defined. You must **first** call ``NINO.traverse`` on **all** the files, so that uniques [#uniques]_ do not collide with normal variables.

Now you must call ``NINO.replace`` to replace the uniques with variables::

  code = NINO.replace(code, scope)

Lastly, you call ``NINO.compile`` which returns a string::

  NINO.compile(code)

Nino also has a small utility to generate an HTML string::

  NINO.html("foo", ["bar.js", "qux.js"])

In addition, there are some optional properties::

  NINO.minified = false
  NINO.warnings = true
  NINO.builtins = { ... }
  NINO.mangle   = function (s) { ... }
  NINO.error    = function (x, s) { ... }

* ``NINO.minified`` controls whether the output is minified or not.

* ``NINO.warnings`` controls whether to display warning messages or not.

* ``NINO.builtins`` is an object that contains all the builtin JavaScript variables. It can be used as the second argument to ``NINO.traverse`` and ``NINO.replace``.

* ``NINO.mangle`` controls variable mangling. JavaScript variables can only contain certain characters, so if you have a variable which contains illegal characters, you have to mangle it to make it legal. The default behavior is as follows::

    break    ->  _break
    0foo     ->  _0foo
    foo&bar  ->  foo_38_bar
    foo_bar  ->  foo__bar

* ``NINO.error`` lets you specify a custom function for errors. The first argument is the AST object that caused the error, and the second argument is the error message. Whatever the function returns is thrown.

Why use it?
===========

* You can use ``NINO.parse`` and ``NINO.compile`` to create minified code: no need for a separate minifier. Parsing is courtesy of Esprima.

* In addition to a ``variable`` datatype, Nino also has a ``unique`` datatype. The only difference is that a ``unique`` is guaranteed to never collide with any other variable [#uniques]_. If you've used Lisps, a ``unique`` is exactly the same as a gensym.

* `Destructuring assignments <http://wiki.ecmascript.org/doku.php?id=harmony:destructuring>`_ are supported::

    NINO.parse("var [a, b] = [1, 2]")

    var c = [1, 2]
      , a = c[0]
      , b = c[1]

  ::

    NINO.parse("(function ([a, ...b]) { return b })")

    (function (c) {
      var a = c[0]
        , b = [].slice.call(c, 1)
      return b
    })

  ::

    NINO.parse("var { a } = { a: 1 }")

    var b = { a: 1 }
      , a = b.a

  ::

    NINO.parse("(function (...a, b) { return a })")

    (function () {
      var c = arguments
        , a = [].slice.call(c, 0, -1)
        , b = c[c.length - 1]
      return a
    })

  As you can see, it even supports ``...`` in the middle of the argument list, rather than only at the end. This is something even ECMAScript Harmony does not do.

* The `spread <http://wiki.ecmascript.org/doku.php?id=harmony:spread>`_ ``...`` operator is mostly supported::

    NINO.parse("[1, ...a, 2, 3]")

    [1].concat(a, [2], [3])

  ::

    NINO.parse("foo(1, ...bar, 2)")

    foo.apply(null, [1].concat(bar, [2]))

  But it doesn't work with the ``"new"`` operator. In addition, because it always uses ``null``, the value of ``this`` will be broken.

* `Object shorthand <http://wiki.ecmascript.org/doku.php?id=strawman:object_initialiser_shorthand>`_::

    NINO.parse("{ x, y }")

    ({ x: x, y: y })

* The ``NINO.op``, ``NINO.opArray``, and ``NINO.fromJSON`` functions have some conveniences:

  * ``+``, ``-``, ``*``, ``/``, ``&&``, and ``||`` support 1 or more arguments::

      NINO.fromJSON(["+", 1, 2, 3, 4, 5])

      1 + 2 + 3 + 4 + 5

  * ``++`` and ``--`` support either 1 or 2 arguments::

      NINO.fromJSON(["++", NINO.variable("foo")])

      ++foo

    ::

      NINO.fromJSON(["++", NINO.variable("foo"), 2])

      foo += 2

  * ``"if"`` supports 1 to 3 arguments::

      NINO.fromJSON(["if", 1])

      1

    ::

      NINO.fromJSON(["if", 1, 2])

      1 && 2

    ::

      NINO.fromJSON(["if", 1, 2, 3])

      1 ? 2 : 3

  * ``"<"``, ``"<="``, ``">"``, ``">="``, ``"=="``, ``"!="``, ``"==="``, and ``"!=="`` support more than 2 arguments with the following behavior::

      NINO.fromJSON(["<", 1, 2, 3, 4, 5])

      1 < 2 && 3 < 4 && 4 < 5

    ::

      NINO.fromJSON(["==", 1, 2, 3, 4, 5])

      1 == 2 && 2 == 3 && 3 == 4 && 4 == 5

    ::

      NINO.fromJSON(["==", 1, ["call", NINO.variable("foo"), 2], ["call", NINO.variable("bar"), 3], 4, 5])

      var a = foo(2)
        , b = bar(3)
      1 == a && a == b && b == 4 && 4 == 5

* *All* statements can be used in expression position::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1],
                        ["if", 1, ["throw", 2]]])

    var a = foo(1)
      , b
    if (1) {
      throw 2
      b = void 0
    }
    a + b

  ::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1],
                        ["throw", 2]])

    foo(1);
    throw 2;
    void 0 + void 0

  ::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1]
                        ["debugger"]])

    var a = foo(1);
    debugger;
    a + void 0

  ::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1],
                        ["try", 2, ["finally", 3]]])

    var a = foo(1),
        b;
    try {
      b = 2
    } finally {
      3
    }
    a + b

  ::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1],
                        ["while", 2, 3]])

    var a = foo(1);
    while (2)
      3;
    a + void 0

  ::

    NINO.fromJSON(["+", ["call", NINO.variable("foo"), 1],
                        ["var", ["=", NINO.variable("a"), ["call", NINO.variable("bar"), 2]]]])

    var b = foo(1),
        a = bar(2);
    b + a

.. [#uniques]
   There are two important caveats regarding uniques. Nino prevents uniques from colliding with other variables by *renaming the uniques*. This means that as long as Nino is aware of *all* the variables that are defined, then everything will work correctly.

   But let's suppose you wrote some code which is compiled with the Nino compiler. In addition, you load a third-party JavaScript library which Nino does not know about. In this case, it is entirely possible that uniques could collide with variables defined by the third-party library.

   There are two ways to solve this:

   1. You can use ``NINO.parse`` followed by ``NINO.traverse`` on the JavaScript file. You don't need to compile it, only traverse it. This is the recommended approach.

   2. You can manually add the global variables to the second argument to ``NINO.traverse`` and ``NINO.replace``. This runs the risk that you may miss some variables, but is sometimes necessary.

   This only applies to *global uniques*: local uniques (defined inside of a function) are *always* guaranteed to *never* collide.

   Secondly, Nino provides a way to *completely bypass* the compiler and *insert arbitrary JavaScript code*. **Any** variables defined in this way could potentially collide with uniques.

   In practice, however, as long as you properly call ``NINO.traverse`` on all the JavaScript files, uniques should not collide.
