@>>>
  @array
    @name do
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  > (function () {
      return function () {
        return function () {
          return function () {
            return 5
          }(4)
        }(3)
      }(2)
    })(1)

@>>>
  @array
    @name fn
    @array
      @name a
      @name b
      @name c
    @array
      @name +
      @number 1
      @number 2
      @number 3
      @number 4
      @number 5
  > (function (a, b, c) {
      return 1 + 2 + 3 + 4 + 5
    })

@>>>
  @array
    @name <
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  1 < 2 && 2 < 3 && 3 < 4 && 4 < 5

@>>>
  @array
    @name > >
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  1 > 2 && 2 > 3 && 3 > 4 && 4 > 5

@>>>
  @array
    @name is
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  1 === 2 && 2 === 3 && 3 === 4 && 4 === 5

@>>>
  @array
    @name isnt
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  1 !== 2 && 2 !== 3 && 3 !== 4 && 4 !== 5

@>>>
  @array
    @name or-is
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  1 === 2 || 1 === 3 || 1 === 4 || 1 === 5

@>>>
  @array
    @name if
  void 0

@>>>
  @array
    @name if
    @number 1
  1

@>>>
  @array
    @name if
    @number 1
    @number 2
  if (1) 2

@>>>
  @array
    @name if
    @number 1
    @number 2
    @number 3
  if (1) 2; else 3

@>>>
  @array
    @name if
    @number 1
    @number 2
    @number 3
    @number 4
  if (1) 2; else if (3) 4

@>>>
  @array
    @name if
    @number 1
    @number 2
    @number 3
    @number 4
    @number 5
  if (1) 2; else if (3) 4; else 5

@>>>
  @array
    @name let
    @name a
    @number 5
    @array
      @name let
      @name b
      @number 10
      @array
        @name +
        @name a
        @name b
  > (function (a) {
      return function (b) {
        return a + b
      }(10)
    })(5)

@>>>+o
  @array
    @name let
    @name a
    @number 5
    @array
      @name let
      @name b
      @number 10
      @array
        @name +
        @name a
        @name b
  > (function (a) {
      var b = 10;
      return a + b
    })(5)

@>>>
  @array
    @name var
    @name a
    @number 1
    @name b
    @number 2
  > var a = 1
      , b = 2

@>>>
  @array
    @name fn
    @array
    @array
      @name dict
      @name a
      @number 1
      @string foobar
      @number 2
  > (function () {
      return function (b) {
        b[a] = 1;
        b["foobar"] = 2;
        return b
      }({})
    })

@>>>+o
  @array
    @name fn
    @array
    @array
      @name dict
      @name a
      @number 1
      @string foobar
      @number 2
  > (function () {
      var b = {};
      b[a] = 1;
      b.foobar = 2;
      return b
    })

@>>>
  @array
    @name array
    @number 1
    @number 2
    @number 3
  [1, 2, 3]

@>>>
  @array
    @name mac
    @name foo
    @array
      @name bar
      @name qux
    @array
      @name +
      @name foo
      @name bar
  > NINO.macros["foo"] = function (bar, qux) {
      return foo + bar
    }
