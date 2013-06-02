var NINO = (function (n) {
  "use strict"

  n.error = function (x, s) {
    return new Error(s + ": " + n.print(x))
  }

  n.setUnique = function (s, scope) {
    var s2 = s
      , i  = 2
    while (scope[s2]) {
      s2 = s + i
      ++i
    }
    return s2
  }

  n.mangle = function (s) {
    return s.replace(/[^$a-zA-Z0-9]/g, function (s, s1, s2) {
      return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
    })
  }

  n.ops = {}

  n.opArray = function (s) {
    var args = [].slice.call(arguments, 1, -1)
    args = args.concat(arguments[arguments.length - 1])

    if (n.ops[s] == null) {
      throw new Error("unknown operator: " + s)
    }
    var o = Object.create(n.ops[s])
    o.args = args
    return o
  }

  n.op = function (s) {
    return n.opArray(s, [].slice.call(arguments, 1))
  }

  n.fromJSON = function (x) {
    if (typeof x === "number") {
      return n.op("number", x)
    } else if (typeof x === "string") {
      return n.op("string", x)
    } else if (Array.isArray(x)) {
      if (n.ops[x[0]].isLiteral) {
        return n.opArray(x[0], x.slice(1))
      } else {
        return n.opArray(x[0], x.slice(1).map(n.fromJSON))
      }
    } else {
      return x
    }
  }

  n.makeOp = function (s, info) {
    info.op = s
    n.ops[s] = info
  }

  n.minified = false
  n.warnings = true

  n.builtins = (function (n) {
    "Number Math Boolean TypeError String Int16Array Float32Array isFinite Array DataView Float64Array ReferenceError SyntaxError Int32Array Uint16Array clearTimeout decodeURIComponent Uint32Array setTimeout eval console URIError unescape Date escape encodeURI Error Int8Array EvalError RangeError NaN isNaN parseInt undefined Object Uint8ClampedArray parseFloat Uint8Array clearInterval Infinity JSON Function setInterval encodeURIComponent decodeURI ArrayBuffer RegExp".split(" ").forEach(function (x) { n[x] = true })
    n["arguments"] = true
    return n
  })({})

  n.wrap = function (x, y) {
    if (x.op === "wrapper") {
      x.args[0] = y
      return x
    } else {
      return y
    }
  }

  n.unwrap = function (x) {
    if (x.op === "wrapper") {
      return n.unwrap(x.args[0])
    } else {
      return x
    }
  }

  n.print = function (x) {
    if (x.op === "unique") {
      return x.args[0] // TODO
    } else if (x.isLiteral) {
      return x.compile(x)
    } else if (x.op === "wrapper") {
      return n.print(x.args[0])
    } else {
      return "(" + x.op + x.args.map(function (x) {
        return " " + n.print(x)
      }).join("") + ")"
    }
  }

  n.space = function () {
    if (n.minified) {
      return ""
    } else {
      return new Array((indent * 2) + 1).join(" ")
    }
  }

  n.minify = function (s, s2) {
    if (n.minified) {
      return s2 || ""
    } else {
      return s
    }
  }

  n.compile = function (x, scope, type) {
    return withVars(scope, function () {
      return compileFn(blockWith(x, type))
    })
  }

  n.traverse = function (x, scope) {
    var scopes = [{ scope: scope, bound: {} }]

    function find(x) {
      var i = scopes.length
        , index
      while (i--) {
        if (scopes[i].uniques && (index = scopes[i].uniques.indexOf(x)) !== -1) {
          return scopes[i].replace[index]
        }
      }
      return x
    }

    function seen(w) {
      var x = n.unwrap(w)
      if (x.op === "variable") {
        var s = compile(x)
          , i = scopes.length
        while (i--) {
          scopes[i].scope[s] = true
          if (scopes[i].bound[s]) {
            break
          }
        }
      }
    }

    function bind(w) {
      var last = scopes[scopes.length - 1]
      var x = n.unwrap(w)
      if (x.op === "variable") {
        last.bound[compile(x)] = true
        seen(x)
        return n.wrap(w, x)
      } else if (x.op === "unique" && last.uniques) {
        var y = find(x)
        if (y === x) {
          y = n.opArray("unique", x.args)
          last.uniques.push(x)
          last.replace.push(y)
        }
        return n.wrap(w, y)
      } else {
        return n.wrap(w, x)
      }
    }

    function withScope(x, f) {
      scopes.push({ scope: x, bound: {}, uniques: [], replace: [] })
      try {
        return f()
      } finally {
        scopes.pop()
      }
    }

    function traverse1(w) {
      var x = n.unwrap(w)
      if (x.isLiteral) {
        if (x.op === "unique") {
          return n.wrap(w, find(x))
        } else {
          return n.wrap(w, x)
        }
      } else {
        x.args = x.args.map(traverse1)
        return n.wrap(w, x)
      }
    }

    function traverse(w) {
      var x = n.unwrap(w)
      if (x.isLiteral) {
        seen(x)
        return n.wrap(w, x)
      } else if (x.op === "function") {
        x.scope = {}
        withScope(x.scope, function () {
          var args = n.unwrap(x.args[0])
          args.args = args.args.map(bind)
          x.args[1] = traverse1(traverse(x.args[1]))
        })
      } else if (x.op === "function-var") {
        x.args[0] = bind(x.args[0])
        x.scope = {}
        withScope(x.scope, function () {
          var args = n.unwrap(x.args[1])
          args.args = args.args.map(bind)
          x.args[2] = traverse1(traverse(x.args[2]))
        })
      } else if (x.op === "var") {
        x.args = x.args.map(function (w) {
          var x = n.unwrap(w)
          if (x.op === "=") {
            x.args[0] = bind(x.args[0])
            x.args[1] = traverse(x.args[1]) // TODO
            return n.wrap(w, x)
          } else {
            return n.wrap(w, bind(x))
          }
        })
      /* TODO
      } else if (x.op === "catch") {
        // scopes[scopes.length - 1].scope
        x.scope = {}
        withScope(x.scope, function () {
          x.args[0] = bind(x.args[0])
          x.args[1] = traverse(x.args[1])
        })*/
      } else {
        x.args = x.args.map(traverse)
      }
      return n.wrap(w, x)
    }

    return traverse(x)
  }


  var indent   = 0
    , priority = 0
    , scope    = { loop: false, function: false }
    , statements
    , expressions

  var reserved = {}

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
  ;("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with " +
    "class enum export extends import super " +
    "implements interface let package private protected public static yield " +
    "null true false").split(" ").forEach(function (s) {
    reserved[s] = true
  })

  /*
  Object.getOwnPropertyNames(window).forEach(function (s) {
    if (!Object.getOwnPropertyDescriptor(window, s).writable) {
      console.log(s)
    }
  })
  */

  function mangle(s) {
    if (reserved[s]) {
      return "_" + s
    } else {
      return n.mangle(s).replace(/^[0-9]/, function (s) {
        return "_" + s
      })
    }
  }

  /*function unmangle(s) {
    // |([a-z])([A-Z])
    return s.replace(/_([0-9]*)_|^_([a-z0-9])/g, function (_, s, s1) {
      if (s1) {
        return s1
      } else {
        return s === "" ? "_" : String.fromCharCode(s)
      }
    })
  }*/

  function getNextUniq(s) {
    var r = s.split("")
      , i = r.length
    while (i--) {
      if (r[i] === "z") {
        r[i] = "a"
      } else {
        r[i] = String.fromCharCode(r[i].charCodeAt(0) + 1)
        return r.join("")
      }
    }
    return r.join("") + "a"
  }

  function getUniq(scope) {
    var s = "a"
    while (scope[s]) {
      s = getNextUniq(s)
    }
    return s
  }

  function serialize(x) {
    var s = Object.prototype.toString.call(x)
    switch (s) {
    case "[object String]":
    case "[object Number]":
    case "[object Boolean]":
      return "" + x

    case "[object Undefined]":
      return "void 0"

    case "[object Arguments]":
      return serialize([].slice.call(x))

    default:
      if (n.warnings) {
        console.warn("serializing object, identity and prototype will be lost")
      }
      switch (s) {
      case "[object Array]":
        if (n.minified) {
          return "[" + [].map.call(x, serialize).join(",") + "]"
        } else {
          return "[" + [].map.call(x, serialize).join(", ") + "]"
        }

      case "[object Object]":
        if (n.minified) {
          return "(" + JSON.stringify(x) + ")"
        } else {
          return "(" + JSON.stringify(x, null, 2) + ")"
        }

      case "[object Date]":
        return "new Date(" + (+x) + ")"

      case "[object RegExp]":
        return "" + x

      // "[object Function]"
      // "[object Error]"
      // "[object Math]"
      // "[object JSON]"
      default:
        throw new Error("can't serialize " + s)
      }
    }
  }

  // mangle("50fooBar-qux")

  /*n.onScope.push(function (scope) {
    if (scope.vars == null) {
      scope.vars = Object.create(vars)
    }
    if (scope.uniques == null) {
      scope.uniques = Object.create(uniques)
    }

    vars  = Object.create(vars)
    uniques = Object.create(scope)
  })*/

  function jsProp(w) {
    var x = n.unwrap(w)
    if (x.op === "string") {
      var y = x.args[0]
      if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(y)) {
        return y
      }
    }
  }

  function compile(w) {
    var x = n.unwrap(w)
      , s = x.compile(x)
    if (x.isUseless && n.warnings) {
      if (/\n/.test(s)) {
        console.warn("useless expression:\n" + n.space() + s)
      } else {
        console.warn("useless expression: " + s)
      }
    }
    return s
  }

  function compileFn(w) {
    var x = n.unwrap(w)
    if (x.op === "function" || x.op === "object") {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
    }
  }

  function withBraces(x) {
    return (x == null || (x.op === ";" && x.args.length === 0)
             ? "{}"
             : "{" + n.minify("\n") + block(x) + n.minify("\n") + n.space() + "}")
  }

  function compileFunction(scope, name, args, body) {
    return resetPriority(0, function () {
      return withVars(scope, function () {
        // n.changeScope(this.scope)
        return withScope("function", function () {
          args = n.unwrap(args).args.map(compile).join("," + n.minify(" "))
          return "function" + (name ? " " + name : n.minify(" ")) + "(" +
                   args + ")" + n.minify(" ") + withBraces(body)
        })
      })
    })
  }

  function compileBlock(name, test, body) {
    return resetPriority(0, function () {
      return name + n.minify(" ") + test + withBraces(body)
    })
  }

  function compileLoop(x, name, test, body) {
    return withScope("loop", function () {
      if (body == null || (body = n.unwrap(body)).op === ";") {
        n.unwrap(x).noSemicolon = true
        return compileBlock(name, "(" + test + ")" + n.minify(" "), body)
      } else {
        return resetPriority(0, function () {
          return name + n.minify(" ") + "(" + test + ")" + n.minify("\n") + block(body)
        })
      }
    })
  }

  function useless(w) {
    var x = n.unwrap(w)
    if (!isImpure(x) && !x.isUseful) { // TODO
      x.isUseless = true
    }
  }

  /*function contains(w, x) {
    var y = n.unwrap(w)
    if (y === x) {
      return true
    } else if (y.isLiteral) {
      return false
    } else {
      return y.args.some(function (y) {
        return contains(y, x)
      })
    }
  }*/

  function argumentError(x, min, max) {
    var len = x.args.length
    if ((min != null && len < min) || (max != null && len > max)) {
      if (max == null) {
        throw n.error(x, "expected at least " + min + " argument but got " + len)
      } else if (min == null) {
        throw n.error(x, "expected at most " + max + " argument but got " + len)
      }

      if (min === max) {
        throw n.error(x, "expected " + min + " arguments but got " + len)
      } else {
        throw n.error(x, "expected " + min + " to " + max + " arguments but got " + len)
      }
    }
  }

  function makeLiteral(s, min, max, info) {
    if (typeof info === "function") {
      info = { compile: info }
    }
    info.expression = function (x) {
      argumentError(x, min, max)
      return x
    }
    info.isLiteral = true
    n.makeOp(s, info)
  }

  function stmt(s, isBreak, f) {
    n.makeOp(s, {
      isImpure: true,
      isStatement: true,
      isBreak: isBreak,
      statement: function (x) {
        argumentError(x, 0, 1)

        if (x.args.length) {
          spliceBlock(x, 0)
        }

        x.args = x.args.map(expression)

        if (x.args.length > 0 && s === "return") { // TODO ew
          var y = n.unwrap(x.args[0])
          if (y.op === "void") {
            if (isImpure(y.args[0])) {
              statements.push(y.args[0])
            }
            x.args.shift()
          }
        }
                          // TODO ew
        if (!(x.isLast && s === "return" && x.args.length === 0)) {
          statements.push(x)
        }
      },
      expression: function (x) {
        if (n.warnings) {
          console.warn("\"" + s + "\" was used in expression position")
        }
        if (x.isBreak) {
          expressions.forEach(function (y) {
            if (isImpure(y)/* && !contains(y, x)*/) {
              statement(y)
              y.args[0] = n.op("void", n.op("number", 0))
            }
          })
        } else {
          pushExpressions(x, isImpure)
        }
        expressions = []
        statement(x)
        return expression(n.op("void", n.op("number", 0)))
      },
      compile: function (x) {
        if (f != null) {
          f(x)
        }
        if (x.args.length) {
          return s + " " + compile(x.args[0])
        } else {
          return s
        }
      }
    })
  }

  function spliceBlock(w, i) {
    var x = n.unwrap(w)
      , y = n.unwrap(x.args[i])
    if (y.op === ",") {
      while (y.op === ",") {
        y.args.slice(0, -1).forEach(statement)
        y = n.unwrap(y.args[y.args.length - 1])
      }
      x.args[i] = n.wrap(x.args[i], y)
    }
  }

  function makeAssignOp(s, s2) {
    n.makeOp(s, {
      unary: 75,
      binary: 10,
      isImpure: true,
      expression: function (x) {
        argumentError(x, 1, 2)

        switch (x.args.length) {
        case 1:
          x.args = x.args.map(expression)
          return x
        case 2:
          // TODO should this be before or after expression ?
          var y = n.unwrap(x.args[1])
          if (y.op === "number" && y.args[0] === 1) {
            x.args = [x.args[0]]
          }
          x.args = x.args.map(expression)
          return x
        }
      },
      compile: function (x) {
        // TODO code duplication with op
        if (x.args.length === 1) {
          return withPriority(x.unary, function () {
            var right = n.unwrap(x.args[0])
            if (x.unary === right.unary) {
              throw n.error(x, "invalid assignment") // s2 + compile(right)
            }
            return s + compile(right)
          })
        } else {
          return withPriority(x.binary, function () {
            var left  = n.unwrap(x.args[0])
              , right = n.unwrap(x.args[1])
            if (x.binary === left.binary) {
              throw n.error(x, "invalid left hand side for assignment")
                                   /*
                                   compile(left) + ")" +
                                   n.minify(" ") + s2 + n.minify(" ") +
                                   compile(right)
                                   */
            }
            return compileFn(left) +
                   n.minify(" ") + s2 + n.minify(" ") +
                   compile(right)
          })
        }
      }
    })
  }

  function op(s, o) {
    var s2 = o.name || s
    n.makeOp(s, {
      unary: o.unary,
      binary: o.binary,
      isImpure: o.isImpure,
      statement: function (x) {
        if (x.args.length === 1) {
          spliceBlock(x, 0)
        } else {
          if (o.order === "right") {
            spliceBlock(x, 0) // TODO
            spliceBlock(x, 1)
          } else {
            spliceBlock(x, 0)
          }
        }
        statements.push(expression(x))
      },
      expression: function (x) {
        argumentError(x, 1)

        if (o.args == null) {
          if (x.args.length > 2) {
            if (o.pairwise) {
              var len = x.args.length - 1
                , r   = []
              if (isImpure(x.args[0]) && x.args.slice(1).some(isImpure)) {
                var u = n.op("unique")
                statements.push(n.op("var", n.op("=", u, x.args[0])))
                x.args[0] = u
              }
              x.args.reduce(function (x, y, i) {
                if (isImpure(y) && i !== len) {
                  var u = n.op("unique")
                  statements.push(n.op("var", n.op("=", u, y)))
                  r.push(n.op(s, x, u))
                  return u
                } else {
                  r.push(n.op(s, x, y))
                  return y
                }
              })
              return expression(n.opArray(o.pairwise, r))
            } else {
              return expression(x.args.reduce(function (x, y) {
                return n.op(s, x, y)
              }))
            }
          }
        } else {
          argumentError(x, o.args, o.args)
        }

        if (x.args.length === 1 && x.unary == null) {
          return expression(x.args[0])
        } else {
          if (x.args.length === 1) {
            x.args[0] = expression(x.args[0])
          } else {
            x.args[0] = expression(x.args[0])
            x.args[1] = expression(x.args[1])
          }
          return x
        }
      },
      compile: function (x) {
        if (x.args.length === 1) {
          return withPriority(x.unary, function () {
            var right = n.unwrap(x.args[0])

            if (x.unary === right.unary) {
              if (o.wrap && x.op === right.op) {
                return s2 + "(" + compile(right) + ")"
              }
            }

            return s2 + compile(right)
          })
        } else {
          return withPriority(x.binary, function () {
            var left  = n.unwrap(x.args[0])
              , right = n.unwrap(x.args[1])

            if (o.order === "right") {
              if (x.binary === left.binary) {
                throw n.error(x, "invalid left hand side for assignment")
                                     /*
                                     compile(left) + ")" +
                                     n.minify(" ") + s2 + n.minify(" ") +
                                     compile(right)
                                     */
              }
            } else if (x.binary === right.binary) {
              var temp = left
              left     = right
              right    = temp
            }

            return compileFn(left) +
                   n.minify(" ") + s2 + n.minify(" ") +
                   compile(right)
          })
        }
      }
    })
  }

  function optimizeVar(x, a) {
    var r = []
    x.forEach(function (w) {
      var x = n.unwrap(w)
        , y
        , z
      if (x.op === "=" && (y = n.unwrap(x.args[1])).op === "function") {
        if (r.length) {
          a.push(n.opArray("var", r))
          r = []
        }
        z = n.op("function-var", x.args[0], y.args[0], y.args[1])
        z.scope = y.scope
        a.push(n.wrap(w, z))
      } else {
        r.push(n.wrap(w, x))
      }
    })
    if (r.length) {
      a.push(n.opArray("var", r))
    }
  }

  function blockWith(w, type) {
    var old  = statements
      , old2 = expressions
    statements  = []
    expressions = []
    try {
      var x = n.unwrap(w)
      if (type === "statement") {
        statement(x)
      } else if (type === "expression") {
        if (x.op === ",") {
          topStatement(x)
        } else {
          statements.push(expression(x))
        }
      }
      var a = []
        , r = []
        , seen
      statements.forEach(function (w) {
        var x = n.unwrap(w)
        if (x.op === "var") {
          r = r.concat(x.args)
        } else {
          var b = false
          if (r.length) {
            var last = n.unwrap(r[r.length - 1])
            b = (x.op === "=" &&
                 last.isVariable &&
                 last.args[0] === n.unwrap(x.args[0]).args[0])
            if (b) {
              r[r.length - 1] = n.wrap(r[r.length - 1], x)
            }
            optimizeVar(r, a)
            r = []
            if (b && type === "expression") {
              a.push(x.args[0])
            }
          }
          if (!b) {
            if (seen) {
              x.isUseless = true
            } else if (x.isBreak) {
              seen = true
            }
            a.push(n.wrap(w, x))
          }
        }
      })
      if (r.length) {
        optimizeVar(r, a)
      }
      if (type === "statement") {
        a.forEach(useless)
      } else if (type === "expression") {
        var len = a.length - 1
        a.forEach(function (x, i) {
          if (i !== len) {
            useless(x)
          }
        })
      }
      if (a.length === 1) {
        return n.wrap(w, a[0])
      } else {
        return n.wrap(w, n.opArray(";", a))
      }
    } finally {
      statements  = old
      expressions = old2
    }
  }

  function isImpure(w) {
    var x = n.unwrap(w)
    if (x.isImpure) {
      return true
    } else if (x.isLiteral || x.isFunction) {
      return false
    } else {
      return x.args.some(isImpure)
    }
  }

  function isStatement(w) {
    var x = n.unwrap(w)
    if (x.isStatement) {
      return true
    } else if (x.isLiteral || x.isFunction) {
      return false
    } else {
      return x.args.some(isStatement)
    }
  }

  // TODO rename
  function withVars(x, f) {
    var old = n.scope
    n.scope = x
    try {
      return f()
    } finally {
      n.scope = old
    }
  }

  function resetPriority(i, f) {
    var old = priority
    priority = i
    try {
      return f()
    } finally {
      priority = old
    }
  }

  function withPriority(i, f) {
    var old = priority
    return resetPriority(i, function () {
      var r = f()
      if (old > priority) {
        return "(" + r + ")"
      } else {
        return r
      }
    })
  }

/*
  if (var foo = 1) {}

  var foo
  if (foo = 1) {}


  if (try { 1 } finally {}) {}

  var u
  try { u = 1 } finally {}
  if (u) {}


  if (try { 1 } catch (e) { ... }) {}

  var u
  try { u = 1 } catch (e) { u = ... }
  if (u) {}


  if (while () {}) {}

  while () {}
  if (void 0) {}


  if (break) {}
  if (continue) {}
  if (debugger) {}
  if (return) {}
  if (throw 1) {}

  break
  if (void 0) {}
  continue
  if (void 0) {}
  debugger
  if (void 0) {}
  return
  if (void 0) {}
  throw 1
  if (void 0) {}

  foo(1) + return 2

  foo(1)
  return 2

  1 + return 2

  return 2
*/

  function expression(w) {
    var x = n.unwrap(w)
      , y = x.expression(x)
    if (!x.isStatement) {
      y = n.op("wrapper", y)
      expressions.push(y)
    }
    return n.wrap(w, y)
  }

  function statement(w) {
    var x = n.unwrap(w)
    if (x.statement) {
      x.statement(x)
    } else {
      statements.push(expression(w))
    }
  }

  function topStatement(w) {
    var x = n.unwrap(w)
    if (x.topStatement) {
      x.topStatement(x)
    } else {
      statement(w)
    }
  }

  function pushExpressions(x, f) {
    expressions.forEach(function (y) {
      if (/*!contains(y, x) && */f(y)) {
        var u = n.op("unique")
        statement(n.op("var", n.op("=", u, y.args[0])))
        y.args[0] = u
      }
    })
    expressions = []
  }

  function blockStatement(x) {
    return blockWith(x, "statement")
  }

  function functionStatement(x) {
    var y = n.unwrap(x)
    while (y.op === ",") {
      y = n.unwrap(y.args[y.args.length - 1])
    }
    y.isLast = true
    return blockStatement(x)
  }

  function withIndent(i, f) {
    var old = indent
    indent = i
    try {
      return f()
    } finally {
      indent = old
    }
  }

  function withScope(s, f) {
    var old = scope[s]
    scope[s] = true
    try {
      return f()
    } finally {
      scope[s] = old
    }
  }

  function block(x) {
    return withIndent(indent + 1, function () {
      return n.space() + compileFn(x)
    })
  }

  /**
   *  Operators
   */
  makeLiteral("bypass", 1, 1, {
    isImpure: true, // TODO
    isStatement: true, // TODO
    compile: function (x) {
      return serialize(x.args[0])
    }
  })

  makeLiteral("line-comment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return n.minify("//")
      } else {
        return n.minify("// " + x.args[0])
      }
    }
  })

  makeLiteral("block-comment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return "/**/"
      } else {
        return n.minify("/* " + x.args[0].replace(/\*\/|\n/g, function (s) {
          if (s === "\n") {
            return "\n" + n.space() + "   "
          } else {
            return "* /"
          }
        }) + " */")
      }
    }
  })

  makeLiteral("doc-comment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return "/**/"
      } else {
        return "/**\n" + n.space() + " * " + x.args[0].replace(/\*\/|\n/g, function (s) {
          if (s === "\n") {
            return "\n" + n.space() + " * "
          } else {
            return "* /"
          }
        }) + "\n" + n.space() + " */"
      }
    }
  })

  makeLiteral("variable", 1, 1, {
    isVariable: true,
    compile: function (x) {
      return mangle(x.args[0])
    }
  })

  makeLiteral("unique", 0, 1, {
    isVariable: true,
    compile: function (x) {
      if (x.string == null) {
        if (x.args[0] != null && !n.minified) {
          x.string = n.setUnique(mangle(x.args[0]), n.scope)
        } else {
          x.string = getUniq(n.scope)
        }
        n.scope[x.string] = true
      }
      return x.string
    }
  })

  makeLiteral("true", 0, 0, function () {
    return "true"
  })

  makeLiteral("false", 0, 0, function () {
    return "false"
  })

  makeLiteral("null", 0, 0, function () {
    return "null"
  })

  makeLiteral("number", 1, 1, function (x) {
    return x.args[0]
  })

  makeLiteral("string", 0, 1, function (x) {
    if (x.args[0] == null) {
      return "\"\""
    } else {
      return "\"" + x.args[0].replace(/["\\\b\f\n\r\t\v]/g, function (s) {
        if (s === "\"" || s === "\\") {
          return "\\" + s
        } else if (s === "\b") {
          return "\\b"
        } else if (s === "\f") {
          return "\\f"
        } else if (s === "\n") {
          return "\\n"
        } else if (s === "\r") {
          return "\\r"
        } else if (s === "\t") {
          return "\\t"
        } else if (s === "\v") {
          return "\\v"
        }
      }) + "\""
    }
  })

  makeLiteral("regexp", 1, 1, function (x) {
    return "/" + x.args[0].replace(/[\/\\]/g, "\\$&") + "/"
  })

  n.makeOp("wrapper", {})

  op("!",          { unary:  70, args: 1 })
  op("~",          { unary:  70, args: 1 })
  op("typeof",     { unary:  70, args: 1, name: "typeof " })
  op("void",       { unary:  70, args: 1, name: "void " })
  op("delete",     { unary:  70, args: 1, name: "delete ", isImpure: true })
  op("*",          { binary: 65 })
  op("/",          { binary: 65 })
  op("%",          { binary: 65, args: 2 })
  op("+",          { binary: 60, unary: 70, wrap: true })
  op("-",          { binary: 60, unary: 70, wrap: true })
  op("<<",         { binary: 55, args: 2 })
  op(">>",         { binary: 55, args: 2 })
  op(">>>",        { binary: 55, args: 2 })
  op("<",          { binary: 50, pairwise: "&&" })
  op("<=",         { binary: 50, pairwise: "&&" })
  op(">",          { binary: 50, pairwise: "&&" })
  op(">=",         { binary: 50, pairwise: "&&" })
  op("in",         { binary: 50, args: 2, name: " in " })
  op("instanceof", { binary: 50, args: 2, name: " instanceof " })
  op("==",         { binary: 45, pairwise: "&&" })
  op("!=",         { binary: 45, pairwise: "&&" })
  op("===",        { binary: 45, pairwise: "&&" })
  op("!==",        { binary: 45, pairwise: "&&" })
  op("&",          { binary: 40, args: 2 })
  op("^",          { binary: 35, args: 2 })
  op("|",          { binary: 30, args: 2 })
  op("&&",         { binary: 25 })
  op("||",         { binary: 20 })
  op("=",          { binary: 10, args: 2, order: "right", isImpure: true })
  op("*=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("/=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("%=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("<<=",        { binary: 10, args: 2, order: "right", isImpure: true })
  op(">>=",        { binary: 10, args: 2, order: "right", isImpure: true })
  op(">>>=",       { binary: 10, args: 2, order: "right", isImpure: true })
  op("&=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("^=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("|=",         { binary: 10, args: 2, order: "right", isImpure: true })

  makeAssignOp("++", "+=")
  makeAssignOp("--", "-=")

  stmt("debugger", false)
  stmt("throw", true)
  stmt("break", true, function (x) {
    if (!scope.loop) {
      throw n.error(x, "must be inside of a loop")
    }
  })
  stmt("continue", true, function (x) {
    if (!scope.loop) {
      throw n.error(x, "must be inside of a loop")
    }
  })
  stmt("return", true, function (x) {
    if (!scope.function) {
      throw n.error(x, "must be inside of a function")
    }
  })

  n.makeOp("empty", {
    topStatement: function (x) {
      statements.push(expression(x))
    },
    statement: function (x) {
      argumentError(x, 0, 0)
    },
    expression: function (x) {
      argumentError(x, 0, 0)
      return n.op("void", n.op("number", "0"))
    }
  })

  n.makeOp(";", {
    compile: function (x) {
      //argumentError(x, 1)
      var r   = []
        , len = x.args.length - 1
      x.args.forEach(function (w, i) {
        var x = n.unwrap(w)
        r.push(compileFn(x))
        if (i !== len) {
          if (!x.noSemicolon) {
            r.push(";")
          }
          r.push(n.minify("\n") + n.space())
        }
      })
      return r.join("")
    }
  })

  n.makeOp(",", {
    topStatement: function (x) {
      argumentError(x, 1)
      x.args.forEach(topStatement)
    },
    statement: function (x) {
      argumentError(x, 1)
      x.args.forEach(statement)
    },
    expression: function (x) {
      argumentError(x, 1)
      if (x.args.length === 1) {
        return expression(x.args[0])
      } else {
        var len = x.args.length - 1
          , r   = []
        x.args.forEach(function (x, i) {
          var y = expression(x)
          if (i !== len) {
            useless(y)
          }
          if (i === len || n.unwrap(x).op !== "empty") {
            r.push(y)
          }
        })
        x.args = r
        return x
      }
    },
    compile: function (x) {
      return withPriority(5, function () {
        return x.args.map(function (x, i) {
          if (i === 0) {
            return compileFn(x)
          } else {
            return compile(x)
          }
        }).join("," + n.minify(" "))
      })
    }
  })

  n.makeOp(".", {
    expression: function (x) {
      argumentError(x, 2, 2)
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(85, function () {
        return resetPriority(0, function () { // TODO check this, also tests
          var y
          if ((y = jsProp(x.args[1]))) {
            x = n.unwrap(x.args[0])
                                     // TODO not sure how efficient this is...
            if (x.op === "number" && Math.round(x.args[0]) === x.args[0]) {
              x = compile(x) + "."
            } else {
              x = compile(x)
            }
            return x + "." + y
          } else {
            return compile(x.args[0]) + "[" + compile(x.args[1]) + "]"
          }
        })
      })
    }
  })

  n.makeOp("var", {
    isImpure: true,
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1)

      var first = n.unwrap(x.args[0])
      if (first.op === "=") {
        spliceBlock(first, 1)
      }
      /* TODO
      x.args.some(function (x) {
        if (x.op === "=") {
          spliceBlock(x, 1)
          return true
        }
      })*/
      x.args = x.args.map(function (w) {
        var x = n.unwrap(w)
        if (x.op === "=") {
          x.args[0] = expression(x.args[0])
          x.args[1] = expression(x.args[1])
          return n.wrap(w, x)
        } else {
          return expression(w)
        }
      })
      //x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      var seen = []

      x.args.forEach(function (w) {
        var x = n.unwrap(w)
        if (x.op === "=") {
          if (isImpure(x.args[1])) {
            seen.push(isImpure)
          }
          x = x.args[0]
        }
        x = n.unwrap(x)
        seen.push(function (w) {
          var y = n.unwrap(w)
          return x.op === y.op && x.args[0] === y.args[0]
        })
      })

      pushExpressions(x, function (x) {
        return seen.some(function (f) {
          return f(x)
        })
      })

      var last = n.unwrap(x.args[x.args.length - 1])
      if (last.op === "=") {
        last = last.args[0]
      }

      statement(x)
      return expression(last)


      /*var top  = []
        , bot  = []
        , seen = {}
        , len  = x.args.length - 1

      x.args.forEach(function (w, i) {
        var x = n.unwrap(w)
        if (x.op === "=") {
          if (isImpure(x.args[1]) || seen[n.unwrap(x.args[0]).args[0]]) {
            seen[n.unwrap(x.args[0]).args[0]] = true
            top.push(x.args[0])
            bot.push(w)
          } else {
            top.push(w)
            if (i === len) {
              bot.push(x.args[0])
            }
          }
        } else {
          top.push(w)
          if (i === len) {
            bot.push(x)
          }
        }
      })

      statement(n.opArray("var", top))
      return expression(n.opArray(",", bot))*/
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var s = n.minify(x.isInline ? " " : "\n" + n.space() + "    ")
        return "var " + x.args.map(compile).join("," + s)
      })
    }
  })

  n.makeOp("if", {
    statement: function (x) {
      argumentError(x, 1, 3)
      spliceBlock(x, 0)
      switch (x.args.length) {
      case 1:
        statement(x.args[0])
        break
      case 2:
        if (isStatement(x.args[1])) {
          x.isExpression = false
          x.args[0] = expression(x.args[0])
          x.args[1] = blockStatement(x.args[1])
          statements.push(x)
        } else {
          statements.push(expression(x))
        }
        break
      case 3:
        if (isStatement(x.args[1]) || isStatement(x.args[2])) {
          x.isExpression = false
          x.args[0] = expression(x.args[0])
          x.args[1] = blockStatement(x.args[1])
          x.args[2] = blockStatement(x.args[2])
          statements.push(x)
        } else {
          statements.push(expression(x))
        }
      }
    },
    expression: function (x) {
      argumentError(x, 1, 3)
      switch (x.args.length) {
      case 1:
        return expression(x.args[0])
      case 2:
        if (isStatement(x.args[1])) {
          var u = n.op("unique")
          x.args[1] = n.op("=", u, x.args[1])
          statement(n.op("var", u))
          statement(x)
          return expression(u)
        } else {
          return expression(n.op("&&", x.args[0], x.args[1]))
        }
      case 3:
        if (isStatement(x.args[1]) || isStatement(x.args[2])) {
          var u = n.op("unique")
          x.args[1] = n.op("=", u, x.args[1])
          x.args[2] = n.op("=", u, x.args[2])
          statement(n.op("var", u))
          statement(x)
          return expression(u)
        } else {
          x.args[0] = expression(x.args[0])
          x.args[1] = expression(x.args[1])
          x.args[2] = expression(x.args[2])
          x.isExpression = true
          return x
        }
      }
    },
    compile: function (x) {
      if (x.isExpression) {
        return withPriority(15, function () {
          return compileFn(x.args[0]) + n.minify(" ") + "?" + n.minify(" ") +
                 compile(x.args[1])   + n.minify(" ") + ":" + n.minify(" ") +
                 compile(x.args[2])
        })
      } else {
        x.args[1] = n.unwrap(x.args[1])
        var b = (x.args[1].op === ";")
          , s = []
        if (x.args.length > 2) {
          x.args[2] = n.unwrap(x.args[2])
          b = b || x.args[2].op === ";"
        }
        s.push("if", n.minify(" "), "(", compile(x.args[0]), ")", n.minify(" "))
        if (b) {
          //if (x.args.length === 2) {
          x.noSemicolon = true
          //}
          s.push("{", n.minify("\n"))
          s.push(block(x.args[1]))
          s.push(n.minify("\n"), n.space(), "}")
          if (x.args.length > 2) {
            s.push(n.minify(" "))
          }
        } else {
          s.push(n.minify("\n"), block(x.args[1]))
          if (x.args.length > 2) {
            s.push(";", n.minify("\n"))
          }
        }
        if (x.args.length > 2) {
          if (b) {
            //x.noSemicolon = true
            s.push("else", n.minify(" "), "{", n.minify("\n"))
            s.push(block(x.args[2]))
            s.push(n.minify("\n"), n.space(), "}")
          } else {
            s.push(n.space())
            if (x.args[2].op === "if") {
              s.push("else ", compile(x.args[2]))
            } else {
              s.push("else", n.minify("\n", " "), block(x.args[2]))
            }
          }
        }
        return s.join("")
      }
    }
  })

  n.makeOp("call", {
    isImpure: true,
    statement: function (x) {
      spliceBlock(x, 0)
      statements.push(expression(x))
    },
    expression: function (x) {
      argumentError(x, 1)
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(80, function () {
        return compileFn(x.args[0]) + "(" +
               // TODO: don't hardcode 6
               resetPriority(6, function () {
                 return x.args.slice(1).map(compile).join("," + n.minify(" "))
               }) + ")"
      })
    }
  })

  n.makeOp("new", {
    isImpure: true,
    statement: function (x) {
      spliceBlock(x, 0)
      statements.push(expression(x))
    },
    expression: function (x) {
      argumentError(x, 1)
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(85, function () {
        return "new " + compile(x.args[0]) + "(" +
               // TODO: don't hardcode 6
               resetPriority(6, function () {
                 return x.args.slice(1).map(compile).join("," + n.minify(" "))
               }) + ")"
      })
    }
  })

  n.makeOp("array", {
    expression: function (x) {
      argumentError(x, 0)
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        return "[" + x.args.map(compile).join("," + n.minify(" ")) + "]"
      })
    }
  })

  n.makeOp("object", {
    expression: function (x) {
      argumentError(x, 0)
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        var r = []
        withIndent(indent + 1, function () {
          for (var i = 0, iLen = x.args.length - 1; i < iLen; i += 2) {
            r.push(n.minify("\n") + n.space() +
                   (jsProp(x.args[i]) || compile(x.args[i])) + ":" + n.minify(" ") +
                   compile(x.args[i + 1]))
          }
        })
        if (r.length) {
          return "{" + r.join(",") + n.minify("\n") + n.space() + "}"
        } else {
          return "{}"
        }
      })
    }
  })

  n.makeOp("while", {
    isImpure: true, // TODO should this be impure?
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1, 2)
      spliceBlock(x, 0)
      x.args[0] = expression(x.args[0])
      if (x.args.length > 1) {
        x.args[1] = blockStatement(x.args[1])
      }
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure)
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      return compileLoop(x, "while", compile(x.args[0]), x.args[1])
    }
  })

  n.makeOp("for", {
    isImpure: true, // TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 0, 4)
      if (x.args.length > 0) {
        var first = n.unwrap(x.args[0])
        if (first.op === "var") {
          x.args[0] = n.wrap(x.args[0], blockStatement(first))
        } else {
          spliceBlock(x, 0)
          x.args[0] = expression(x.args[0])
        }
      }
      if (x.args.length > 1) {
        x.args[1] = expression(x.args[1])
      }
      if (x.args.length > 2) {
        x.args[2] = expression(x.args[2])
      }
      if (x.args.length > 3) {
        x.args[3] = blockStatement(x.args[3])
      }
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure)
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      var r = []
      if (x.args.length > 0) {
        var first = n.unwrap(x.args[0])
        if (first.op === "var") {
          first.isInline = true
        }
        r.push(compile(first) + ";")
      }
      if (x.args.length > 1) {
        r.push(compile(x.args[1]) + ";")
      }
      if (x.args.length > 2) {
        r.push(compile(x.args[2]) + ";")
      }
      while (r.length < 3) {
        r.push(";")
      }
      return compileLoop(x, "for", r.join(n.minify(" ")), x.args[3])
    }
  })

  n.makeOp("for-in", {
    isImpure: true, // TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 2, 3)
      spliceBlock(x, 0)
      var first = n.unwrap(x.args[0])
      if (first.op !== "var" && !first.isVariable) {
        throw n.error(x.args[0], "must be a variable or (var ...)")
      }
      if (first.op === "var") {
        if (first.args.length === 1 && n.unwrap(first.args[0]).isVariable) {
          x.args[0] = n.wrap(x.args[0], blockStatement(first))
        } else {
          throw n.error(x.args[0], "invalid assignment")
        }
      } else {
        x.args[0] = n.wrap(x.args[0], expression(first))
      }
      spliceBlock(x, 1)
      x.args[1] = expression(x.args[1])
      if (x.args.length > 2) {
        x.args[2] = blockStatement(x.args[2])
      }
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure)
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      return compileLoop(x, "for", compile(x.args[0]) + " in " + compile(x.args[1]), x.args[2])
    }
  })

  n.makeOp("function", {
    isFunction: true,
    expression: function (x) {
      argumentError(x, 1, 2)
      var args = n.unwrap(x.args[0])
      args.args = args.args.map(expression)
      if (x.args.length > 1) {
        x.args[1] = functionStatement(x.args[1])
      }
      return x
    },
    compile: function (x) {
      return compileFunction(x.scope, "", x.args[0], x.args[1])
    }
  })

  n.makeOp("function-var", {
    noSemicolon: true,
    isFunction: true,
    isImpure: true,
    isStatement: true,
    statement: function (x) {
      argumentError(x, 2, 3)
      var args = n.unwrap(x.args[1])
      x.args[0] = expression(x.args[0])
      args.args = args.args.map(expression)
      if (x.args.length > 2) {
        x.args[2] = functionStatement(x.args[2])
      }
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure) // TODO
      statement(x)
      return expression(x.args[0])
    },
    compile: function (x) {
      return compileFunction(x.scope, compile(x.args[0]), x.args[1], x.args[2])
    }
  })

  n.makeOp("try", {
    noSemicolon: true,
    //isImpure: "children", TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1)
      x.args = x.args.map(function (x, i) {
        if (i === 0) {
          return blockStatement(x)
        } else {
          return expression(x)
        }
      })
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure)

      var u = n.op("unique")

      x.args = x.args.map(function (w, i) {
        var x = n.unwrap(w)
        if (i === 0) {
          return n.wrap(w, n.op("=", u, x))
        } else if (x.op === "catch") {
          x.args[1] = n.op("=", u, x.args[1])
          return w
        } else if (x.op === "finally") {
          return w
        }
      })
      statement(n.op("var", u))
      statement(x)

      return expression(u)
    },
    compile: function (x) {
      return compileBlock("try", "", x.args[0]) + x.args.slice(1).map(function (x) {
               return n.minify(" ") + compile(x)
             }).join("")
    }
  })

  n.makeOp("catch", {
    noSemicolon: true,
    expression: function (x) {
      argumentError(x, 1, 2)
      x.args[0] = expression(x.args[0])
      if (x.args.length > 1) {
        x.args[1] = blockStatement(x.args[1])
      }
      return x
    },
    compile: function (x) {
      return compileBlock("catch", "(" + compile(x.args[0]) + ")" + n.minify(" "), x.args[1])
    }
  })

  n.makeOp("finally", {
    noSemicolon: true,
    expression: function (x) {
      argumentError(x, 0, 1)
      if (x.args.length > 0) {
        x.args[0] = blockStatement(x.args[0])
      }
      return x
    },
    compile: function (x) {
      return compileBlock("finally", "", x.args[0])
    }
  })

  /*n.makeOp("switch", {
    noSemicolon: true,
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      var u = n.op("unique")

      x.args = x.args.map(function (x, i) {
        if (i !== 0) {
          if (x.op === "case") {
            x.args[1] = n.op("=", u, x.args[1])
          } else if (x.op === "default") {
            x.args[0] = n.op("=", u, x.args[0])
          }
        }
        return expression(x)
      })
      statement(n.op("var", u))
      statements.push(x)

      return expression(u)
    },
    compile: function (x) {
      return withScope("loop", function () {
        return "switch" + n.minify(" ") + "(" + compile(x.args[0]) + ")" + n.minify(" ") +
                 "{" + n.minify("\n") + x.args.slice(1).map(compile).join(";" + n.minify("\n")) +
                 n.minify("\n") + "}"
      })
    }
  })

  n.makeOp("case", {
    expression: function (x) {
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return "case " + compile(x.args[0]) + ":" + n.minify("\n") + block(x.args[1])
    }
  })

  n.makeOp("fallthru", {
    expression: function (x) {
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return "case " + compile(x.args[0]) + ":" + n.minify("\n") + block(x.args[1])
    }
  })

  n.makeOp("default", {
    expression: function (x) {
      x.args[0] = blockStatement(x.args[0])
      return x
    },
    compile: function (x) {
      return "default:" + n.minify("\n") + block(x.args[0])
    }
  })*/

  return n
})(NINO || {})
