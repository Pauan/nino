How to use
==========

First, you must create a Nino AST by calling the ``NINO.op``, ``NINO.opArray``, or ``NINO.fromJSON`` functions::

  var code = NINO.op("+", ...)
  var code = NINO.opArray("+", [...])
  var code = NINO.fromJSON(["+", ...])

To see which operators are defined, just search for ``n.makeOp`` in the files ``compile.js`` and ``literals.js``. They generally follow JavaScript names and semantics.

Now that you have an AST, you need to pass it to the ``NINO.traverse`` function [#traverse]_ along with a scope, which is simply an object that says which global variables are defined::

  var scope = { foo: true, bar: true }
  var ast   = NINO.traverse(code, scope)

In the above example, Nino will treat the variables ``foo`` and ``bar`` as being defined, so that they won't collide with any uniques [#uniques]_.

Lastly, you call ``NINO.compile`` which returns a string::

  NINO.compile(ast, scope)

In addition, you can pass an object as the third argument, which supports the following properties::

  NINO.compile(ast, scope, {
    type: "expression",
    minified: false,
    warnings: true
  })

The ``type`` property determines whether the top level is treated as an ``"expression"`` (the default), or a ``"statement"``. REPLs will generally want to use ``"expression"`` since they return a value. On the other hand, if you're putting the string into a file which will be loaded later, you should use ``"statement"``.

Why use it?
===========

If you're designing a language that compiles to JavaScript, you have to deal with some annoying problems:

* Distinction between statements and expressions

* Operator precedence

* Left/right associativity

* Funky variable behavior (var hoisting, among other things)

Instead, let Nino handle all of that. Some cool features that Nino has:

* With a single line of code you can choose whether to output a normal or minified string: no need for a separate minifier.

* Generates *very* short and *very* fast JavaScript.

* In addition to a ``"symbol"`` datatype, Nino also has a ``"unique"`` datatype. The only difference is that a ``"unique"`` is guaranteed to never collide with any other variable [#uniques]_. If you've used Lisps, a ``"unique"`` is exactly the same as a gensym.

* The Nino AST is generally fairly simple and intelligent. It has some conveniences:

  * ``"if"`` supports 1 or more arguments::

      NINO.fromJSON(["if", 1])

      1

    ::

      NINO.fromJSON(["if", 1, 2])

      1 && 2

    ::

      NINO.fromJSON(["if", 1, 2, 3])

      1 ? 2 : 3

    ::

      NINO.fromJSON(["if", 1, 2, 3, 4])

      1 ? 2 : 3 && 4

  * ``"<"``, ``"<="``, ``">"``, ``">="``, ``"=="``, ``"!="``, ``"==="``, and ``"!=="`` all support more than 2 arguments with the following behavior::

      NINO.fromJSON(["<", 1, 2, 3, 4, 5])

      1 < 2 && 3 < 4 && 4 < 5

    ::

      NINO.fromJSON(["==", 1, 2, 3, 4, 5])

      1 == 2 && 2 == 3 && 3 == 4 && 4 == 5

  * *All* statements can be used in expression position::

      NINO.fromJSON(["+", ["call", 1, 2], ["throw", 3]])

      1(2);
      throw 3;
      void 0 + void 0

    ::

      NINO.fromJSON(["+", ["call", 1, 2], ["debugger"]])

      var a = 1(2);
      debugger;
      a + void 0

    ::

      NINO.fromJSON(["+", ["call", 1, 2], ["try", 3, ["finally", 4]]])

      var a = 1(2),
          b;
      try {
        b = 3
      } finally {
        4
      }
      a + b

    ::

      NINO.fromJSON(["+", ["call", 1, 2], ["while", 3, 4]])

      var a = 1(2);
      while (3)
        4;
      a + void 0

    ::

      NINO.fromJSON(["+", ["call", 1, 2],
                          ["var", ["=", ["symbol", "a"], ["call", 3, 4]]]])

      var b = 1(2),
          a = 3(4);
      b + a

  * Can generate helpful warnings, e.g. about useless expressions::

      NINO.fromJSON(["function", [","],
                      [",", ["return", 1], 2]])

      warning: useless expression: 2
      (function () {
        return 1;
        2
      })

  * All JavaScript operators and statements are supported *except* for the following:

    * `block <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/block>`_
    * `do...while <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/do...while>`_
    * `label <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/label>`_
    * `switch <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/switch>`_
    * `with <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/with>`_

    * `const <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/const>`_
    * `export <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/export>`_
    * `for each...in <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for_each...in>`_
    * `for...of <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for...of>`_
    * `import <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/import>`_
    * `let <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/let>`_
    * `yield <https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Operators/yield>`_

.. [#traverse]
   Why can't you just call ``NINO.compile`` directly?

   Let's suppose you wanted to compile multiple files using the Nino compiler. If you naively compiled each file separately, then it wouldn't work, because Nino needs to know about *all* the variables that are defined.

   So instead, you first call ``NINO.traverse`` on all of the files, and then afterwards you call ``NINO.compile``.

.. [#uniques]
   There are two important caveats regarding uniques. Nino prevents uniques from colliding with other variables by *renaming the uniques*. This means that as long as Nino is aware of *all* the variables that are defined, then everything will work correctly.

   But let's suppose you wrote some code which is compiled with the Nino compiler. In addition, you load a third-party JavaScript library which Nino does not know about. In this case, it is entirely possible that uniques could collide with variables defined by the third-party library.

   The answer to this is to let Nino know about the symbols defined in the third-party library. This only applies to *global uniques*: local uniques (defined inside of a function) are *always* guaranteed to *never* collide.

   Secondly, Nino provides a way to *completely bypass* the compiler and *insert arbitrary JavaScript code*. *Any* variables defined in this way could potentially collide with uniques.
