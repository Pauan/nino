var NINO = (function (n) {
  "use strict";

  n.Error = function (x, s) {
    this.message = s
  }

  n.String = function (x) {
    this.value = x
    this.type = t.String
  }

  n.Number = function (x) {
    this.value = x
    if (Math.round(x) === x) {
      this.type = t.Integer
    } else if (x === x) {
      this.type = t.Float
    } else {
      this.type = t.NaN
    }
  }


  var type = {
    check: function (x) {
      return this === x || this.isPrototypeOf(x)
    },
    toString: function () {
      return this.name
    }
  }

  n.Type = function (s, f) {
    var o = Object.create(type)
    if (f) {
      o.check = f
    }
    o.name = s
    return o
  }

  n.SubtypeOf = function (s, x) {
    var o = Object.create(x)
    o.name = s
    return o
  }

  n.Or = function () {
    var a = [].slice.call(arguments)
    return n.Type("($or? " + a.join(" ") + ")", function (x) {
      return a.some(function (y) {
        return y.check(x)
      })
    })
  }

  var t      = n.types = {}
  t.Any      = n.Type("any?")

  t.True     = n.Type("true?")
  t.False    = n.Type("false?")

  t.Null     = n.Type("null?")
  t.Void     = n.Type("void?")

  t.String   = n.Type("str?")

  t.Integer  = n.Type("int?")
  t.Float    = n.Type("float?")
  t.Infinite = n.Type("infinite?") // TODO
  t.NaN      = n.Type("nan?")

  t.Object   = n.Type("dict?")
  t.Array    = n.SubtypeOf("list?", t.Object)

  t.Boolean  = n.Or(t.True, t.False)
  t.Number   = n.Or(t.Integer, t.Float, t.Infinite)


  function any(o, x, y) {
    if (y == null) {
      y = x
    }
    for (var i = 0, iLen = o.args.length; i < iLen; ++i) {
      if (x.check(o.args[i].type)) {
        return y
      }
    }
  }
/*
  function all(o, x) {
    for (var i = 0, iLen = o.args.length; i < iLen; ++i) {
      if (t.Any.check(o.args[i].type) || !x.check(o.args[i].type)) {
        return
      }
    }
    return x
  }*/

  function ret(o) {
    return n.Or.apply(null, o.args.map(function (x) { return x.type }))
  }

  function require(o, x, f) {
    o.args = o.args.map(function (y) {
      y = n.checkType(y)
      if (!t.Any.check(y.type) && !x.check(y.type)) {
        console.error(new n.Error(y, "expected type " + x + " but got type " + y.type))
      }
      return y
    })
    o.type = f(o, x)
    return o
  }

  function numeric(o) {
    return require(o, n.Or(t.Number, t.NaN), function (o, type) {
      return any(o, t.NaN) ||
             any(o, t.Any, type) ||
             ret(o)
    })
  }

  var types = {
    "+": function (o) {
      return require(o, n.Or(t.String, t.Number, t.NaN), function (o, type) {
        return any(o, t.String) ||
               any(o, t.Any, type) ||
               ret(o)
      })
    },
    "-": numeric,
    "*": numeric,
    "/": numeric,
    "cast": function (o) {
      var x = n.checkType(o.args[0])
      x.type = o.args[1]
      return x
    }
  }

  n.checkType = function (o) {
    if (types[o.name] != null) {
      o = types[o.name](o)
    }
    if (o.type == null) {
      o.type = t.Any
    }
    return o
  }

  console.log(n.checkType({ name: "+", args: [new n.String("hiya"), new n.Number(5)] }))
  console.log(n.checkType({ name: "+", args: [{}, new n.Number(5)] }))
  console.log(n.checkType({ name: "+", args: [new n.Number(10), new n.Number(5)] }))
  console.log(n.checkType({ name: "+", args: [new n.Number(10.5), new n.Number(5)] }))
  console.log(n.checkType({ name: "-", args: [new n.String("0.5"), new n.Number(5)] }))
  console.log(n.checkType({ name: "-", args: [{ name: "cast", args: [new n.String("0.5"), t.Integer] }, new n.Number(5)] }))
  console.log(n.checkType({ name: "-", args: [new n.Number(10.5), new n.Number(5)] }))

  return n
})(NINO || {})
