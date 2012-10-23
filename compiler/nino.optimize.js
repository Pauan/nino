var exports, NINO = (function (n) {
  "use strict";

  n.translator = {}

  function each(a, f) {
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      f(a[i])
    }
  }

  function map(a, f) {
    var r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      r.push(f(a[i]))
    }
    return r
  }

  function eachpair(x, y, f) {
    for (var i = 0, iLen = Math.min(x.length, y.length); i < iLen; ++i) {
      f(x[i], y[i])
    }
  }

  /*function doublemap(a, f) {
    var r = []
    for (var i = 0, iLen = a.length; i < iLen; i += 2) {
      if ((i + 1) in a) {
        r.push.apply(r, f(a[i], a[i + 1]))
      }
    }
    return r
  }

  function mapreduce(a, f) {
    var r = []
    if (0 in a) {
      var x    = a[0]
        , i    = 1
        , iLen = a.length
      while (i < iLen) {
        x = f.call(r, x, a[i])
        ++i
      }
      r.push(x)
    }
    return r
  }

  function compressVars(a) {
    return mapreduce(a, function (x, y) {
      if (x[0] === "var" && y[0] === "var") {
        return [x[0]].concat(x.slice(1), y.slice(1))
      } else if (x[0] === "function") {
        this.push([x[0], x[1], x[2], compressVars(x[3])])
        return y
      } else {
        this.push(x)
        return y
      }
    })
  }*/

  function immutable(x) {
    return x === "true"  ||
           x === "false" ||
           x === "null"  ||
           (Array.isArray(x) &&
             (x[0] === "number" ||
              x[0] === "string" ||
              x[0] === "name"   ||
              x[0] === "void"))
  }

  function hoistVars(a) {
    var l = []
    function loop(a) {
      var r = []
      each(a, function (x) {
        if (x[0] === "var") {
          each(x.slice(1), function (x) {
            if (1 in x) {
              if (immutable(x[1])) {
                l.push(x)
              } else {
                l.push([x[0]])
                r.push(["=", x[0], x[1]])
              }
            } else {
              l.push([x[0]])
            }
          })
        } else if (x[0] === "function") {
          r.push([x[0], x[1], x[2], hoistVars(x[3])])
        } else if (Array.isArray(x)) {
          r.push(loop(x))
        } else {
          console.log(x)
          r.push(x)
        }
      })
      return r
    }
    a = loop(a)
    return [["var"].concat(l)].concat(a)
  }
/*
  [["var", ["a"]], ["foo"], ["var", ["b"]]]
  [["var", ["a"], ["b"]], ["foo"]]

  [["var", ["a"]], ["foo"], ["var", ["b", ["number", "5"]]]]
  [["var", ["a"], ["b", ["number", "5"]]], ["foo"]]

  [["var", ["a"]], ["foo"], ["var", ["b", ["=", "c", 5]]]]
  [["var", ["a"], ["b"]], ["foo"], ["=", "b", ["=", "c", 5]]]

  [["var", ["a"]], ["foo"], ["var", ["b", ["=", "c", 5]], ["d", ["=", "e", 10]]]]
  [["var", ["a"], ["b"], ["d"]], ["foo"], ["=", "b", ["=", "c", 5]], ["=", "d", ["=", "e", 10]]]

  [["foo"], ["var", ["a"]], ["function", 0, 0, [["bar"], ["var", ["b", 10]]]]]
  [["var", ["a"]], ["foo"], ["function", 0, 0, [["var", ["b"]], ["bar"], ["=", "b", "10"]]]]

  var b = c = 5

  ["call"]
  ["new"]
  ["delete"]
  ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
  ["++", "--"]

  boolean number(+ NaN) string null names
  . [] ! ~ + - typeof void * / % << >> >>> < <= > >= in instanceof == != === !=== & ^ | && || ?: ,

  // Only if assignment occurs between the top and the in
  ["in"]
  ["instanceof"]
  ["==", "!=", "===", "!=="]
  ["<", ">", "<=", ">="]

  ["."]
  ["[]"]
  ["?:"]
  [","]

  ["new"] // Maybe?
*/

  function removeFunctions(a) {
    function statement(a) {
      var r = []
      function expression(a) {
        var r2 = []
        a.forEach(function (x) {
          if (Array.isArray(x)) {
            if (x[0] === "call" && Array.isArray(x[1]) && x[1][0] === "function") {
              var a  = []
                , a2 = []
              eachpair(x[1][2], x[2], function (x, y) {
                console.log(x, y)
                a.push([x])
                a2.push(["=", x, y])
              })
              a2.push.apply(a2, x[1][3])
              r2.push(a2.reduce(function (x, y) {
                return [",", x, y]
              }))
              if (a.length) {
                r.push(["var", a])
              }
              console.log(r, r2)
            } else if (x[0] === "function") {
              r2.push([x[0], x[1], x[2], statement(x[3])])
            } else {
              r2.push(expression(x))
            }
          } else {
            r2.push(x)
          }
        })
        return r2
      }
      a.forEach(function (x) {
        if (Array.isArray(x)) {
          if (x[0] === "call" && Array.isArray(x[1]) && x[1][0] === "function") {
            console.log(x)
            var a = []
            eachpair(x[1][2], x[2], function (x, y) {
              a.push([x, y])
            })
            r.push(["var", a])
          } else {
            r.push(expression(x))
          }
        } else {
          r.push(x)
        }
      })
      return r
    }
    function loop(a) {
      var r = []
      a.forEach(function (x) {
        if (Array.isArray(x)) {
          if (x[0] === "function") {
            r.push([x[0], x[1], x[2], statement(x[3])])
          } else {
            r.push(loop(x))
          }
        } else {
          r.push(x)
        }
      })
      return r
    }
    return loop(a)
  }

  /*

  [["call", ["function", 0, ["a", "b", "c"], []], [1, 2, 3]]]
  [["call", ["function", 0, ["a", "b", "c"], []], [1, 2, 3]]]

  [["call", ["function", 0, ["a", "b", "c"], [
              ["call", ["function", 0, ["a", "b", "c"], []],
                       [1, 2, 3]]]],
            [1, 2, 3]]]

  */

  n.hoistVars = function (x) {
    return x
    return hoistVars(x)
  }

  n.removeFunctions = function (x) {
    return removeFunctions(x)
  }

  n.optimize = function (x) {
    return n.hoistVars(n.removeFunctions(x))
  }

  return n
})(exports || NINO || {})
