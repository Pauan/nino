What is it?
===========

If you're designing a language that compiles to JavaScript, you have to deal with quite a few problems:

* Distinction between statements and expressions

* Operator precedence

* Left/right associativity

* Funky variable behavior (var hoisting, among other things)

Instead, let Nino handle all of that. You simply design your compiler so that it compiles to a Nino AST, then you call ``NINO.traverse``, and lastly ``NINO.compile`` which returns a string for the program.

Some more cool features that Nino has:

* With a single line of code you can choose whether to output a normal or minified string: no need for a separate minifier.

* Generates *very* short and *very* fast JavaScript.

* In addition to a "symbol" datatype, Nino also has a "box" datatype. The only difference is that a "box" is guaranteed to never collide with any other variable [#boxes]_. If you've used Lisps, a "box" is exactly the same as a gensym.

* The Nino AST is generally fairly simple and intelligent. It has some conveniences:

  * "if" supports 1 or more arguments::

      NINO.op("if", NINO.op("number", 1))

      1

    ::

      NINO.op("if", NINO.op("number", 1), NINO.op("number", 2))

      1 && 2

    ::

      NINO.op("if", NINO.op("number", 1), NINO.op("number", 2), NINO.op("number", 3))

      1 ? 2 : 3

    ::

      NINO.op("if", NINO.op("number", 1), NINO.op("number", 2), NINO.op("number", 3), NINO.op("number", 4))

      1 ? 2 : 3 && 4

  * "<", "<=", ">", ">=", "==", "!=", "===", and "!==" all support more than 2 arguments with the following behavior::

      NINO.op("<", NINO.op("number", 1),
                   NINO.op("number", 2),
                   NINO.op("number", 3),
                   NINO.op("number", 4),
                   NINO.op("number", 5))

      1 < 2 && 3 < 4 && 4 < 5

    ::

      NINO.op("==", NINO.op("number", 1),
                    NINO.op("number", 2),
                    NINO.op("number", 3),
                    NINO.op("number", 4),
                    NINO.op("number", 5))

      1 == 2 && 2 == 3 && 3 == 4 && 4 == 5

  * *All* statements can be used in expression position::

      NINO.op("+", NINO.op("number", 1),
                   NINO.op("return", NINO.op("number", 2)))

      return 2;
      1 + void 0

    ::

      NINO.op("+", NINO.op("number", 1),
                   NINO.op("try", NINO.op("number", 2),
                                  NINO.op("finally", NINO.op("number", 3))))

      var a;
      try {
        a = 2
      } finally {
        3
      }
      1 + a

    ::

      NINO.op("+", NINO.op("number", 1),
                   NINO.op("while", NINO.op("number", 2),
                                    NINO.op("number", 3)))

      while (2)
        3;
      1 + void 0

    ::

      NINO.op("+", NINO.op("number", 1),
                   NINO.op("var", NINO.op("=", NINO.op("symbol", "a"), NINO.op("number", 1))))

      var a = 1
      1 + a

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

.. [#boxes]
   There are two important caveats regarding boxes. Nino prevents boxes from colliding with other variables by *renaming the boxes*. This means that as long as Nino is aware of *all* the variables that are defined, then everything will work correctly.

   But let's suppose you wrote some code which is compiled with the Nino compiler. In addition, you load a third-party JavaScript library which Nino does not know about. In this case, it is entirely possible that boxes could collide with variables defined by the third-party library.

   The answer to this is to let Nino know about the symbols defined in the third-party library. This only applies to *global boxes*: local boxes (defined inside of a function) are *always* guaranteed to *never* collide.

   Secondly, Nino provides a way to *completely bypass* the compiler and *insert arbitrary JavaScript code*. *Any* variables defined in this way could potentially collide with boxes.
