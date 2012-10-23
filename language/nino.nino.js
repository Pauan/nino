var exports, NINO = (function (n) {
  "use strict";

  // TODO: move into generic array utilities
  function pairwise(x, f) {
    var r = []
    for (var i = 0, iLen = x.length - 1; i < iLen; i += 2) {
      r.push(f(x[i], x[i + 1]))
    }
    return r
  }

  var undef = ["void", ["number", "0"]]

  function unary(s) {
    return function (x) {
      return [s, macex(x)]
    }
  }

  function binary(s) {
    return function (x, y) {
      return [s, macex(x), macex(y)]
    }
  }

  function reducer(s, sAlt) {
    return function () {
      return [].map.call(arguments, macex).reduce(function (x, y) {
        return [s, x, y]
      })
    }
  }

  function bin_reducer(s, s2) {
    return function () {
      var r = []
      ;[].map.call(arguments, macex).reduce(function (x, y) {
        r.push([s, x, y])
        return y
      })
      return r.reduce(function (x, y) {
        return [s2, x, y]
      })
    }
  }

  function updater(s) {
    return function (x, y) {
      x = macex(x)
      return ["=", x, [s, x, macex(y)]]
    }
  }

  function isMacro(x) {
    if (typeof x === "function") {
      return x
    } else if ((x = n.macros[x])) {
      return x
    }
  }

  function macex(a) {
    if (Array.isArray(a)) {
      var x = isMacro(a[0])
      if (x) {
        return x.apply(null, a.slice(1))
      } else {
        x = a.map(macex)
        return ["call", x[0], x.slice(1)]
      }
    } else if (a instanceof n.String) {
      return ["string", a.value]
    } else if (a instanceof n.Number) {
      return ["number", a.value]
    } else {
      return ["name", a]
    }
  }

  function transform(x) {
    switch (x[0]) {
    case "array":  return x.slice(1).map(transform)
    case "name":   return x[1]
    case "string": return new n.String(x[1])
    case "number": return new n.Number(x[1])
    }
  }

  n.String = function (x) {
    this.value = x
  }
  n.Number = function (x) {
    this.value = x
  }
/*
  ((get (get chrome "windows") "getAll") (dict "populate" true)
    (fn (array a)
      (readWriteDB o "tabs"
        (fn (array o)
          (do (var doit
                (fn (array)
                  (do (var i 0)
                      (if (is (get w "type") "normal")
                        (each (get w "tabs")
                          (fn (array t)
                            (do (set! (get t "index") (add! i 1))
                                (var s (urlToId (get t "url")))
                                (if (not (get ignoreId s))
                                  ((get o "put") (addChromeTab s t))))))))))
              (if (is (sub! iInit 1) 0)
                (init))
              (set! (get ((get o "openCursor")) "onsuccess")
                (fn (array e)
                  (do (var cursor (get (get e "target") "result"))
                      (if cursor
                        (do! (var t (get cursor "value"))
                             (set! (get tabs (get t "id")) (diskToTab t))
                             ((get cursor "continue")))
                        (doit))))))))))
*/
  // TODO: actual gensyms
  n.uniq = function () {
    return ["name", "b"]
  }

  n.macros = {
    // Transformers
    "fn": function (args, body) {
      return ["function", "", args, [["return", macex(body)]]]
    },
    "if": function anon() {
      var a = arguments
      switch (a.length) {
      case 0:
        return undef
      case 1:
        return macex(a[0])
      case 2:
        return ["if", macex(a[0]),
                 [macex(a[1])],
                 []]
      case 3:
        return ["if", macex(a[0]),
                 [macex(a[1])],
                 [macex(a[2])]]
      default:
        return ["if", macex(a[0]),
                 [macex(a[1])],
                 [anon.apply(null, [].slice.call(arguments, 2))]]
      }
    },
    "get": function (x, y, z) {
      if (z == null) {
        return ["[]", macex(x), macex(y)]
      } else {
        return ["||", ["[]", macex(x), macex(y)], macex(z)]
      }
    },
    "var": function () {
      var a = pairwise(arguments, function (x, y) {
        return [x, macex(y)]
      })
      return ["var", a]
    },
    "dict": function () {
      var u = n.uniq()
      var a = pairwise(arguments, function (x, y) {
        return ["=", ["[]", u, macex(x)], macex(y)]
      })
      a.push(["return", u])
      return ["call", ["function", "", [u], a],
                      [["object", []]]]
    },
    "array": function () {
      return ["array", [].map.call(arguments, macex)]
    },
    "set!": function (x, y) {
      return ["=", macex(x), macex(y)]
    },
    "add!": updater("+"),
    "sub!": updater("-"),
    "+":    reducer("+"),
    "-":    reducer("-"),
    "*":    reducer("*"),
    "/":    reducer("/"),
    "mod":  binary("%"),
    "and":  reducer("&&"),
    "or":   reducer("||"),
    "<":    bin_reducer("<",   "&&"),
    ">":    bin_reducer(">",   "&&"),
    "is":   bin_reducer("===", "&&"),
    "isnt": bin_reducer("!==", "&&"),
    "not":  unary("!"),

    // Macros
    "let": function (v, e, b) {
      return macex([[n.macros["fn"], [v], b], e])
    },
    "do": function () {
      return macex([].reduceRight.call(arguments, function (x, y) {
        return [[n.macros["fn"], [], x], y]
      }))
    },
    "or-is": function (x) {
      var r = [n.macros["or"]]
      ;[].slice.call(arguments, 1).forEach(function (y) {
        r.push([n.macros["is"], x, y])
      })
      return macex(r)
    },
    "mac": function (x, a, b) {
      var f = macex([n.macros["fn"], a, b])
      console.log(f[2], f[3])
      n.macros[x] = new Function(f[2], f[3])
      /*console.log(macex([n.macros["set!"],
                          [n.macros["get"],
                            [n.macros["get"], "NINO", new n.String("macros")],
                            new n.String(x)],
                          ]))*/
      return undef
    }
  }

  n.nino = function (a) {
    a = a.map(transform)
    a = a.map(macex)
    return a
  }

  return n
})(exports || NINO || {})
