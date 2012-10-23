@>>>
  @var
    @
      @a
        @number 5
      @b
        @number 10
  > var a = 5
      , b = 10

@>>>
  @string
    foobar"\quxcorge
  > "foobar\"\\quxcorge"

@>>>
  @+
    @number 5
    @number 10
  5 + 10

@>>>
  @=
    @name foobar
    @number 10
  foobar = 10

@>>>
  @*
    @+
      @number 1
      @number 2
    @number 3
  (1 + 2) * 3

@>>>
  @+
    @-
      @number 1
      @number 2
    @number 3
  1 - 2 + 3

@>>>
  @+
    @+
      @number 1
      @number 2
    @number 3
  1 + 2 + 3

@>>>
  @call
    @call
      @name foo
      @
        @name bar
    @
      @number 1
      @number 2
      @number 3
  foo(bar)(1, 2, 3)

@>>>
  @call
    @name foo
    @
      @,
        @number 1
        @number 2
      @number 2
      @number 3
  foo((1, 2), 2, 3)

@>>>
  @call
    @function
      >
      @foo
      @
        @return
          @name bar
    @
      @number 1
      @number 2
      @number 3
  > (function (foo) {
      return bar
    })(1, 2, 3)

@>>>
  @return
    @call
      @function
        >
        @foo
        @
          @return
            @name bar
      @
        @number 1
        @number 2
        @number 3
  > return function (foo) {
      return bar
    }(1, 2, 3)

@>>>
  @call
    @function
      >
      @
      @
        @var
          @
            @a
              @,
                @name b
                @,
                  @name c
                  @name d
        @return
          @array
            @
              @name a
              @name b
    @
  > (function () {
      var a = (b, c, d);
      return [a, b]
    })()

@>>>
  @array
    @
      @,
        @name a
        @name b
      @name c
  [(a, b), c]

@>>>
  @if
    @number 1
    @
    @
  if (1) ;

@>>>
  @if
    @number 1
    @
      @number 2
    @
  if (1) 2

@>>>
  @if
    @number 1
    @
    @
      @number 2
  if (1) ; else 2

@>>>
  @if
    @number 1
    @
      @number 2
      @number 3
    @
  > if (1) {
      2;
      3
    }

@>>>
  @if
    @number 1
    @
    @
      @number 2
      @number 3
  > if (1) ; else {
      2;
      3
    }

@>>>
  @if
    @number 1
    @
      @number 2
    @
      @number 3
  if (1) 2; else 3

@>>>
  @if
    @number 1
    @
      @number 2
      @number 4
    @
      @number 3
      @number 4
  > if (1) {
      2;
      4
    } else {
      3;
      4
    }

@>>>
  @if
    @number 1
    @
      @number 2
    @
      @if
        @number 3
        @
          @number 4
        @
  if (1) 2; else if (3) 4

@>>>
  @[]
    @name foo
    @name bar
  foo[bar]

@>>>
  @[]
    @function
      >
      @foo
      @
        @return
          @name foo
    @string bar
  > (function (foo) {
      return foo
    })["bar"]

@>>>
  @[]
    @new
      @name Number
      @
    @string bar
  new Number()["bar"]

@>>>
  @.
    @name foo
    bar
  foo.bar

@>>>
  @.
    @number 1
    qux
  1..qux

@>>>
  @.
    @number 1.0
    qux
  1.0.qux

@>>>
  @.
    @+
      @number 1
      @number 2
    corge
  (1 + 2).corge

@>>>
  @+
    @number 1
    @.
      @number 2
      corge
  1 + 2..corge

@>>>
  @.
    @function
      >
      @foo
      @
        @return
          @name foo
    bar
  > (function (foo) {
      return foo
    }).bar

@>>>
  @.
    @new
      @name Number
      @
    bar
  new Number().bar

@>>>
  @.
    @new
      @name Number
      @
        @number 5
    bar
  new Number(5).bar

@>>>
  @.
    @new
      @name Number
      @
    bar
  new Number().bar

@>>>
  @new
    @name Number
    @
  new Number()

@>>>
  @new
    @+
      @number 1
      @number 2
    @
  new (1 + 2)()

@>>>
  @new
    @.
      @name Foo
      bar
    @
  new Foo.bar()

@>>>
  @new
    @call
      @name foo
      @
        @name bar
    @
  new (foo(bar))()

@>>>
  @call
    @new
      @name foo
      @
        @name bar
    @
  new foo(bar)()

@>>>
  @object
    @
      @foo
        @number 5
      @bar
        @number 10
      @qux
        @number 20
  > {
      foo: 5,
      bar: 10,
      qux: 20
    }

@>>>
  @object
    @
      @
        foobar \"quxcorge
        @,
          @,
            @,
              @,
                @number 1
                @number 2
              @number 3
            @number 4
          @number 5
      @bar
        @number 10
      @qux
        @number 20
  > {
      "foobar \\\"quxcorge": (1, 2, 3, 4, 5),
      bar: 10,
      qux: 20
    }

@>>>
  @function
    foo
    @
    @
      @array
        @
          @number 1
          @number 2
          @number 3
      @return
        @object
          @
            @foo
              @number 1
            @bar
              @number 2
            @qux
              @number 3
  > (function foo() {
      [1, 2, 3];
      return {
        foo: 1,
        bar: 2,
        qux: 3
      }
    })

@>>>
  @function-statement
    foo
    @
    @
      @array
        @
          @number 1
          @number 2
          @number 3
      @return
        @object
          @
            @foo
              @number 1
            @bar
              @number 2
            @qux
              @number 3
  > function foo() {
      [1, 2, 3];
      return {
        foo: 1,
        bar: 2,
        qux: 3
      }
    }

@>>>
  @call
    @name yesno
    @
      @object
        @
          @foo
            @function
              >
              @
              @
                @return @name foo
          @bar
            @function
              >
              @
              @
                @return @name bar
  > yesno({
      foo: function () {
        return foo
      },
      bar: function () {
        return bar
      }
    })

@>>>
  @var
    @
      @a
        @object
          @
            @foo
              @number 1
            @bar
              @number 2
  > var a = {
      foo: 1,
      bar: 2
    }

@>>>
  @array
    @
      @function
        >
        @
        @
          @name foo
      @function
        >
        @
        @
          @name bar
  > [function () {
      foo
    }, function () {
      bar
    }]

@>>>
  @array
    @
      @object
        @
          @foo
            @function
              >
              @
              @
                @name foo
          @bar
            @function
              >
              @
              @
                @name bar
  > [{
      foo: function () {
        foo
      },
      bar: function () {
        bar
      }
    }]

@>>>
  @,
    @,
      @function
        >
        @
        @
          @name foo
      @function
        >
        @
        @
          @name bar
    @function
      >
      @
      @
        @name qux
  > (function () {
      foo
    }), function () {
      bar
    }, function () {
      qux
    }

@>>>
  @typeof
    @function
      >
      @
      @
        @name foo
  > typeof function () {
      foo
    }

@>>>
  @call
    @function
      >
      @
      @
        @return
          @.
            @function
              >
              @
              @
            bar
    @
  > (function () {
      return function () {}.bar
    })()

@>>>
  @+
    @function
      >
      @
      @
    @number 1
  (function () {}) + 1

@>>>
  @+
    @+
      @+
        @function
          >
          @
          @
        @number 1
      @number 2
    @number 3
  (function () {}) + 1 + 2 + 3

@>>>
  @+
    @number 1
    @function
      >
      @
      @
  1 + function () {}

@>>>
  @?:
    @function
      >
      @
      @
    @number 1
    @number 2
  (function () {}) ? 1 : 2

@>>>
  @?:
    @number 1
    @function
      >
      @
      @
    @number 2
  1 ? function () {} : 2
