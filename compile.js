var NINO = (function (n) {
  "use strict"

  n.opArray = function (s, args) {
    var o = Object.create(ops[s])
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
    } else if (x.isLiteral) {
      return x
    } else {
      return n.opArray(x[0], x.slice(1).map(n.fromJSON))
    }
  }

  n.compile = function (x, scope, info) {
    if (info == null) {
      info = {}
    }
    if (info.type == null) {
      info.type = "expression"
    }
    if (info.minified == null) {
      info.minified = false
    }
    if (info.warnings == null) {
      info.warnings = true
    }
    minified = info.minified
    warnings = info.warnings
    return withVars(scope, function () {
      return compileFn(blockWith(x, info.type))
    })
  }

  n.traverse = function (x, scope) {
    var scopes = [{ scope: scope, bound: {} }]

    function find(x) {
      var i = scopes.length
        , index
      while (i--) {
        if (scopes[i].boxes && (index = scopes[i].boxes.indexOf(x)) !== -1) {
          return scopes[i].replace[index]
        }
      }
      return x
    }

    function seen(x) {
      if (x.op === "symbol") {
        var s = x.args[0]
          , i = scopes.length
        while (i--) {
          scopes[i].scope[s] = true
          if (scopes[i].bound[s]) {
            break
          }
        }
      }
      return x
    }

    function bind(x) {
      var last = scopes[scopes.length - 1]
      if (x.op === "symbol") {
        last.bound[x.args[0]] = true
        return seen(x)
      } else if (x.op === "box" && last.boxes) {
        var y = find(x)
        if (y === x) {
          y = n.opArray("box", x.args)
          last.boxes.push(x)
          last.replace.push(y)
        }
        return y
      } else {
        return x
      }
    }

    function withScope(x, f) {
      scopes.push({ scope: x, bound: {}, boxes: [], replace: [] })
      try {
        return f()
      } finally {
        scopes.pop()
      }
    }

    function traverse1(x) {
      if (x.isLiteral) {
        if (x.op === "box") {
          return find(x)
        } else {
          return x
        }
      } else {
        x.args = x.args.map(traverse1)
        return x
      }
    }

    function traverse(x) {
      if (x.isLiteral) {
        return seen(x)
      } else if (x.op === "function") {
        x.scope = {}
        withScope(x.scope, function () {
          x.args[0].args = x.args[0].args.map(bind)
          x.args[1]      = traverse1(traverse(x.args[1]))
        })
      } else if (x.op === "function-var") {
        x.args[0] = bind(x.args[0])
        x.scope = {}
        withScope(x.scope, function () {
          x.args[1].args = x.args[1].args.map(bind)
          x.args[2]      = traverse1(traverse(x.args[2]))
        })
      } else if (x.op === "var") {
        x.args = x.args.map(function (x) {
          if (x.op === "=") {
            x.args[0] = bind(x.args[0])
            x.args[1] = traverse(x.args[1]) // TODO
            return x
          } else {
            return bind(x)
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
      return x
    }

    return traverse(x)
  }


  var impure   = false
    , indent   = 0
    , priority = 0
    , ops      = {}
    , scope    = { loop: false, function: false }
    , vars
    , minified
    , warnings
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
      return s.replace(/^([0-9])|([a-z])\-([a-z])|[^$a-zA-Z0-9]/g, function (s, s1, s2, s3) {
        if (s1) {
          return "_" + s1
        } else if (s2) {
          return s2 + s3.toLocaleUpperCase()
        } else {
          return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
        }
      })
    }
  }

  function unmangle(s) {
    // |([a-z])([A-Z])
    return s.replace(/_([0-9]*)_|^_([a-z0-9])/g, function (_, s, s1) {
      if (s1) {
        return s1
      } else {
        return s === "" ? "_" : String.fromCharCode(s)
      }
    })
  }

  function mangleBox(s) {
    // TODO: document isn't writable either, but should probably be handled in a different way
    if (s === "undefined" || s === "NaN" || s === "Infinity" || s === "arguments") {
      return "_" + s
    } else {
      return mangle(s)
    }
  }

  // mangle("50fooBar-qux")

  /*n.onScope.push(function (scope) {
    if (scope.vars == null) {
      scope.vars = Object.create(vars)
    }
    if (scope.boxes == null) {
      scope.boxes = Object.create(boxes)
    }

    vars  = Object.create(vars)
    boxes = Object.create(scope)
  })*/

  function jsProp(x) {
    if (x.op === "string" && /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x.args[0])) {
      return x.args[0]
    }
  }

  function compile(x) {
    var s = x.compile(x)
    if (x.isUseless && warnings) {
      if (/\n/.test(s)) {
        console.warn("useless expression:\n" + space() + s)
      } else {
        console.warn("useless expression: " + s)
      }
    }
    return s
  }

  function compileFn(x) {
    if (x.op === "function" || x.op === "object") {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
    }
  }

  function compileFunction(scope, name, args, body) {
    return resetPriority(0, function () {
      return withVars(scope, function () {
        // n.changeScope(this.scope)
        return withScope("function", function () {
          args = args.args.map(compile).join("," + minify(" "))
          body = block(body)
          return "function" + (name ? " " + name : minify(" ")) + "(" +
                   args + ")" + minify(" ") +
                   "{" + minify("\n") +
                     body + minify("\n") + space() +
                   "}"
        })
      })
    })
  }

  function compileBlock(name, test, body) {
    return resetPriority(0, function () {
      return name + minify(" ") + test + "{" + minify("\n") +
               block(body) + minify("\n") + space() +
               "}"
    })
  }

  function compileLoop(x, name, test, body) {
    return withScope("loop", function () {
      if (body.op === ";") {
        x.noSemicolon = true
        return compileBlock(name, "(" + test + ")" + minify(" "), body)
      } else {
        return resetPriority(0, function () {
          return name + minify(" ") + "(" + test + ")" + minify("\n") + block(body)
        })
      }
    })
  }

  function useless(x) {
    if (!isImpure(x)) {
      x.isUseless = true
    }
    return x
  }

  function contains(y, x) {
    if (y === x) {
      return true
    } else if (y.isLiteral) {
      return false
    } else {
      return y.args.some(function (y) {
        return contains(y, x)
      })
    }
  }

  function stmt(s, isBreak, f) {
    makeOp(s, {
      isImpure: true,
      isBreak: isBreak,
      statement: function (x) {
        x.args = x.args.map(expression)
        statements.push(x)
      },
      expression: function (x) {
        if (warnings) {
          console.warn("\"" + s + "\" was used in expression position")
        }
        expressions.forEach(function (y) {
          if (!contains(y, x) && isImpure(y)) {
            statement(y)
          }
        })
        expressions = []
        statement(x)
        return expression(n.op("void", n.op("number", 0)))
      },
      compile: function (x) {
        var s2
        if (x.args.length) {
          if (x.args.length > 1) {
            throw new Error("\"" + s + "\" expected 0 or 1 arguments but got " + x.args.length)
          }
          s2 = s + " " + compile(x.args[0])
        } else {
          s2 = s
        }
        if (f != null) {
          f(s2)
        }
        return s2
      }
    })
  }

  function spliceBlock(x, i) {
    var y = x.args[i]
    if (y.op === ",") {
      y.args.slice(0, -1).forEach(statement)
      x.args[i] = y.args[y.args.length - 1]
    }
  }

  function op(s, o) {
    var s2 = o.name || s
    makeOp(s, {
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
        if (o.args == null) {
          if (x.args.length > 2) {
            // TODO should store the middle expressions so it doesn't recompute them
            if (o.pairwise) {
              var r = []
              var len = x.args.length - 1
              x.args.reduce(function (x, y, i) {
                if (isImpure(y) && i !== len) {
                  var u = n.op("box")
                  r.push(n.op(s, x, n.op("var", n.op("=", u, y))))
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
        } else if (x.args.length !== o.args) {
          // TODO
          throw new Error("\"" + s + "\" expected " + o.args + " arguments but got " + x.args.length)
        }

        if (x.args.length === 1 && x.unary == null) {
          return expression(x.args[0])
        } else if (x.args.length === 0) {
          throw new Error("\"" + s + "\" expected at least 1 argument")
        }

        if (x.args.length === 1) {
          x.args[0] = expression(x.args[0])
        } else {
          x.args[0] = expression(x.args[0])
          x.args[1] = expression(x.args[1])
        }
        return x
      },
      compile: function (x) {
        if (x.args.length === 1) {
          return withPriority(x.unary, function () {
            var right = x.args[0]

            if (x.unary === right.unary) {
              if (o.error) {
                throw new Error("invalid assignment: " +
                                s2 + compile(right))
              } else if (o.wrap && x.op === right.op) {
                return s2 + "(" + compile(right) + ")"
              }
            }

            return s2 + compile(right)
          })
        } else {
          return withPriority(x.binary, function () {
            var left  = x.args[0]
              , right = x.args[1]

            if (o.order === "right") {
              if (x.binary === left.binary) {
                throw new Error("invalid left hand side for assignment: (" +
                                compile(left) + ")" +
                                minify(" ") + s2 + minify(" ") +
                                compile(right))
              }
            } else if (x.binary === right.binary) {
              var temp = left
              left     = right
              right    = temp
            }

            return compileFn(left) +
                   minify(" ") + s2 + minify(" ") +
                   compile(right)
          })
        }
      }
    })
  }

  function getBox(x, s) {
    var s2 = s
      , i  = 2
    while (vars[s2]) {
      s2 = s + i
      ++i
    }
    vars[s2] = true
    x.string = s2
    return s2
  }

  function getUniq(x) {
    var s, i = 1
    while (true) {
      for (var a = "a".charCodeAt(0), b = "z".charCodeAt(0); a <= b; ++a) {
        s = new Array(i + 1).join(String.fromCharCode(a))
        if (!vars[s]) {
          vars[s] = true
          x.string = s
          return s
        }
      }
      ++i
    }
  }

  function optimizeVar(x, a) {
    var r = []
    x.forEach(function (x) {
      var y, z
      if (x.op === "=" && (y = x.args[1]).op === "function") {
        if (r.length) {
          a.push(n.opArray("var", r))
          r = []
        }
        z = n.op("function-var", x.args[0], y.args[0], y.args[1])
        z.scope = y.scope
        a.push(z)
      } else {
        r.push(x)
      }
    })
    if (r.length) {
      a.push(n.opArray("var", r))
    }
  }

  function blockWith(x, type) {
    var old1 = expressions
      , old2 = statements
    expressions = []
    statements = []
    try {
      if (x.op === "," || type === "statement") {
        statement(x)
      } else if (type === "expression") {
        statements.push(expression(x))
      }
      var a = []
        , r = []
        , seen
      statements.forEach(function (x) {
        if (x.op === "var") {
          r = r.concat(x.args)
        } else {
          var b = false
          if (r.length) {
            var last = r[r.length - 1]
            b = (x.op === "=" &&
                 last.isVariable &&
                 last.args[0] === x.args[0].args[0])
            if (b) {
              r[r.length - 1] = x
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
            a.push(x)
          }
        }
      })
      if (r.length) {
        optimizeVar(r, a)
      }
      if (type === "statement") {
        a = a.map(useless)
      } else if (type === "expression") {
        var len = a.length - 1
        a = a.map(function (x, i) {
          if (i === len) {
            return x
          } else {
            return useless(x)
          }
        })
      }
      if (a.length === 1) {
        return a[0]
      } else {
        return n.opArray(";", a)
      }
    } finally {
      expressions = old1
      statements = old2
    }
  }

  function space() {
    if (minified) {
      return ""
    } else {
      return new Array((indent * 2) + 1).join(" ")
    }
  }

  function minify(s, s2) {
    if (minified) {
      return s2 || ""
    } else {
      return s
    }
  }

  function makeOp(s, info) {
    info.op = s
    ops[s] = info
  }

  function isImpure1(x) {
    if (x.isImpure) {
      return true
    } else if (x.isLiteral) {
      return false
    } else {
      return x.args.some(isImpure1)
    }
  }

  function isImpure(x) {
    if (x.isImpure) {
      if (x.isImpure === "children") {
        return x.args.some(isImpure1)
      } else {
        return true
      }
    } else {
      return false
    }
  }

  function withVars(x, f) {
    var old = vars
    vars = x
    try {
      return f()
    } finally {
      vars = old
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

  function expression(x) {
    if (x.expression) {
      expressions.push(x)
      return x.expression(x)
    } else {
      return x
      //throw new Error("\"" + x.op + "\" is not allowed in expression position")
    }
  }

  function statement(x) {
    if (x.statement) {
      x.statement(x)
    } else {
      statements.push(expression(x))
    }
  }

  function blockStatement(x) {
    return blockWith(x, "statement")
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
      return space() + compileFn(x)
    })
  }

  /**
   *  Operators
   */
  makeOp("symbol", {
    isVariable: true,
    isLiteral: true,
    compile: function (x) {
      return mangle(x.args[0])
    }
  })

  makeOp("box", {
    isVariable: true,
    isLiteral: true,
    compile: function (x) {
      if (x.string) {
        return mangleBox(x.string)
      } else if (x.args[0] && !minified) {
        return mangleBox(getBox(x, x.args[0]))
      } else {
        return getUniq(x)
      }
    }
  })

  makeOp("true", {
    isLiteral: true,
    compile: function () {
      return "true"
    }
  })

  makeOp("false", {
    isLiteral: true,
    compile: function () {
      return "false"
    }
  })

  makeOp("null", {
    isLiteral: true,
    compile: function () {
      return "null"
    }
  })

  makeOp("number", {
    isLiteral: true,
    compile: function (x) {
      return x.args[0]
    }
  })

  makeOp("string", {
    isLiteral: true,
    compile: function (x) {
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

  makeOp("regexp", {
    isLiteral: true,
    compile: function (x) {
      return "/" + x.args[0].replace(/[\/\\]/g, "\\$&") + "/"
    }
  })

  op("++",         { unary:  75, args: 1, isImpure: true, error: true })
  op("--",         { unary:  75, args: 1, isImpure: true, error: true })
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
  op("+=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("-=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("*=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("/=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("%=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("<<=",        { binary: 10, args: 2, order: "right", isImpure: true })
  op(">>=",        { binary: 10, args: 2, order: "right", isImpure: true })
  op(">>>=",       { binary: 10, args: 2, order: "right", isImpure: true })
  op("&=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("^=",         { binary: 10, args: 2, order: "right", isImpure: true })
  op("|=",         { binary: 10, args: 2, order: "right", isImpure: true })

  stmt("debugger", false)
  stmt("throw", true)
  stmt("break", true, function (x) {
    if (!scope.loop) {
      throw new Error("must be inside of a loop: " + x)
    }
  })
  stmt("continue", true, function (x) {
    if (!scope.loop) {
      throw new Error("must be inside of a loop: " + x)
    }
  })
  stmt("return", true, function (x) {
    if (!scope.function) {
      throw new Error("must be inside of a function: " + x)
    }
  })

  makeOp(";", {
    compile: function (x) {
      if (x.args.length === 0) {
        throw new Error("\",\" expected at least 1 argument but got 0")
      } else {
        var r   = []
          , len = x.args.length - 1
        x.args.forEach(function (x, i) {
          r.push(compileFn(x))
          if (i !== len) {
            if (!x.noSemicolon) {
              r.push(";")
            }
            r.push(minify("\n") + space())
          }
        })
        return r.join("")
      }
    }
  })

  makeOp(",", {
    statement: function (x) {
      x.args.forEach(statement)
    },
    expression: function (x) {
      if (x.args.length === 1) {
        return expression(x.args[0])
      } else {
        var len = x.args.length - 1
        x.args = x.args.map(function (x, i) {
          if (i === len) {
            return expression(x)
          } else {
            return useless(expression(x))
          }
        })
        return x
      }
    },
    compile: function (x) {
      if (x.args.length === 0) {
        throw new Error("\",\" expected at least 1 argument but got 0")
      } else {
        return withPriority(5, function () {
          return x.args.map(function (x, i) {
            if (i === 0) {
              return compileFn(x)
            } else {
              return compile(x)
            }
          }).join("," + minify(" "))
        })
      }
    }
  })

  makeOp(".", {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(85, function () {
        var y
        if ((y = jsProp(x.args[1]))) {
          return compile(x.args[0]) + "." + y
        } else {
          return compile(x.args[0]) + "[" + compile(x.args[1]) + "]"
        }
      })
    }
  })

  makeOp("var", {
    isImpure: true,
    statement: function (x) {
      if (x.args[0].op === "=") {
        spliceBlock(x.args[0], 1)
      }
      /* TODO
      x.args.some(function (x) {
        if (x.op === "=") {
          spliceBlock(x, 1)
          return true
        }
      })*/
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      var top  = []
        , bot  = []
        , seen = {}
        , len  = x.args.length - 1

      x.args.forEach(function (x, i) {
        if (x.op === "=") {
          if (isImpure(x.args[1]) || seen[x.args[0].args[0]]) {
            seen[x.args[0].args[0]] = true
            top.push(x.args[0])
            bot.push(x)
          } else {
            top.push(x)
            if (i === len) {
              bot.push(x.args[0])
            }
          }
        } else {
          top.push(x)
          if (i === len) {
            bot.push(x)
          }
        }
      })

      statement(n.opArray("var", top))
      return expression(n.opArray(",", bot))
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var s = minify(x.isInline ? " " : "\n" + space() + "    ")
        return "var " + x.args.map(compile).join("," + s)
      })
    }
  })

  makeOp("if", {
    isImpure: "children",
    statement: function (x) {
      spliceBlock(x, 0)
      if (minified) {
        statements.push(expression(x))
      } else {
        switch (x.args.length) {
        case 0:
          throw new Error("\"if\" expected at least 1 argument but got 0")
        case 1:
          statement(x.args[0])
          break
        case 2:
          x.isExpression = false
          x.args[0] = expression(x.args[0])
          x.args[1] = blockStatement(x.args[1])
          statements.push(x)
          break
        default:
          x.isExpression = false
          x.args[0] = expression(x.args[0])
          x.args[1] = blockStatement(x.args[1])
          if (x.args.length > 3) {
            x.args[2] = blockStatement(n.opArray("if", x.args.slice(2)))
          } else {
            x.args[2] = blockStatement(x.args[2])
          }
          statements.push(x)
        }
      }
    },
    expression: function (x) {
      switch (x.args.length) {
      case 0:
        throw new Error("\"if\" expected at least 1 argument but got 0")
      case 1:
        return expression(x.args[0])
      case 2:
        return expression(n.op("&&", x.args[0], x.args[1]))
      default:
        x.args[0] = expression(x.args[0])
        x.args[1] = expression(x.args[1])
        if (x.args.length > 3) {
          x.args[2] = expression(n.opArray("if", x.args.slice(2)))
        } else {
          x.args[2] = expression(x.args[2])
        }
        x.isExpression = true
        return x
      }
    },
    compile: function (x) {
      if (x.isExpression) {
        return withPriority(15, function () {
          return compileFn(x.args[0]) + minify(" ") + "?" + minify(" ") +
                 compile(x.args[1])   + minify(" ") + ":" + minify(" ") +
                 compile(x.args[2])
        })
      } else {
        var b1 = (x.args[1].op === ";")
          , s  = []
        s.push("if", minify(" "), "(", compile(x.args[0]), ")", minify(" "))
        if (b1) {
          if (x.args.length === 2) {
            x.noSemicolon = true
          }
          s.push("{", minify("\n"))
          s.push(block(x.args[1]))
          s.push(minify("\n"), space(), "}")
          if (x.args.length > 2) {
            s.push(minify(" "))
          }
        } else {
          s.push(minify("\n"), block(x.args[1]))
          if (x.args.length > 2) {
            s.push(";", minify("\n"))
          }
        }
        if (x.args.length > 2) {
          if (x.args[2].op === ";") {
            x.noSemicolon = true
            s.push("else", minify(" "), "{", minify("\n"))
            s.push(block(x.args[2]))
            s.push(minify("\n"), space(), "}")
          } else {
            if (!b1) {
              s.push(space())
            }
            if (x.args[2].op === "if") {
              s.push("else ", compile(x.args[2]))
            } else {
              s.push("else", minify("\n", " "), block(x.args[2]))
            }
          }
        }
        return s.join("")
      }
    }
  })

  makeOp("call", {
    isImpure: true,
    statement: function (x) {
      spliceBlock(x, 0)
      statements.push(expression(x))
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(80, function () {
        return compileFn(x.args[0]) + "(" +
               // TODO: don't hardcode 6
               resetPriority(6, function () {
                 return x.args.slice(1).map(compile).join("," + minify(" "))
               }) + ")"
      })
    }
  })

  makeOp("new", {
    isImpure: true,
    statement: function (x) {
      spliceBlock(x, 0)
      statements.push(expression(x))
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(85, function () {
        return "new " + compile(x.args[0]) + "(" +
               // TODO: don't hardcode 6
               resetPriority(6, function () {
                 return x.args.slice(1).map(compile).join("," + minify(" "))
               }) + ")"
      })
    }
  })

  makeOp("array", {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        return "[" + x.args.map(compile).join("," + minify(" ")) + "]"
      })
    }
  })

  makeOp("object", {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        var r = []
        withIndent(indent + 1, function () {
          for (var i = 0, iLen = x.args.length; i < iLen; i += 2) {
            r.push(minify("\n") + space() +
                   (jsProp(x.args[i]) || compile(x.args[i])) + ":" + minify(" ") +
                   compile(x.args[i + 1]))
          }
        })
        if (r.length) {
          return "{" + r.join(",") + minify("\n") + space() + "}"
        } else {
          return "{}"
        }
      })
    }
  })

  makeOp("while", {
    isImpure: true, // TODO should this be impure?
    statement: function (x) {
      spliceBlock(x, 0)
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      statements.push(x)
    },
    expression: function (x) {
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      return compileLoop(x, "while", compile(x.args[0]), x.args[1])
    }
  })

  makeOp("for", {
    isImpure: true, // TODO
    statement: function (x) {
      if (x.args[0].op === "var") {
        x.args[0] = blockStatement(x.args[0])
      } else {
        spliceBlock(x, 0)
        x.args[0] = expression(x.args[0])
      }
      x.args[1] = expression(x.args[1])
      x.args[2] = expression(x.args[2])
      x.args[3] = blockStatement(x.args[3])
      statements.push(x)
    },
    expression: function (x) {
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      if (x.args[0].op === "var") {
        x.args[0].isInline = true
      }
      return compileLoop(x, "for", compile(x.args[0]) + ";" + minify(" ") +
                                   compile(x.args[1]) + ";" + minify(" ") +
                                   compile(x.args[2]), x.args[3])
    }
  })

  makeOp("for-in", {
    isImpure: true, // TODO
    statement: function (x) {
      spliceBlock(x, 0)
      var first = x.args[0]
      if (first.op !== "var" && !first.isVariable) {
        throw new Error("the first argument for \"for-in\" must be a variable or \"var\" but it was \"" + first.op + "\"")
      }
      if (first.op === "var") {
        if (first.args.length === 1 && first.args[0].isVariable) {
          x.args[0] = blockStatement(first)
        } else {
          throw new Error("invalid assignment for first argument of \"for-in\"")
        }
      } else {
        x.args[0] = expression(first)
      }
      spliceBlock(x, 1)
      x.args[1] = expression(x.args[1])
      x.args[2] = blockStatement(x.args[2])
      statements.push(x)
    },
    expression: function (x) {
      statement(x)
      return expression(n.op("void", n.op("number", 0)))
    },
    compile: function (x) {
      return compileLoop(x, "for", compile(x.args[0]) + " in " + compile(x.args[1]), x.args[2])
    }
  })

  makeOp("function", {
    expression: function (x) {
      x.args[0].args = x.args[0].args.map(expression)
      x.args[1]      = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return compileFunction(x.scope, "", x.args[0], x.args[1])
    }
  })

  makeOp("function-var", {
    noSemicolon: true,
    isImpure: true,
    statement: function (x) {
      x.args[0]      = expression(x.args[0])
      x.args[1].args = x.args[1].args.map(expression)
      x.args[2]      = blockStatement(x.args[2])
      statements.push(x)
    },
    expression: function (x) {
      statement(x)
      return expression(x.args[0])
    },
    compile: function (x) {
      return compileFunction(x.scope, compile(x.args[0]), x.args[1], x.args[2])
    }
  })

  makeOp("try", {
    noSemicolon: true,
    isImpure: "children",
    statement: function (x) {
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
      var u = n.op("box")

      x.args = x.args.map(function (x, i) {
        if (i === 0) {
          return blockStatement(n.op("=", u, x))
        } else if (x.op === "catch") {
          x.args[1] = n.op("=", u, x.args[1])
          return expression(x)
        } else if (x.op === "finally") {
          return expression(x)
        }
      })
      statement(n.op("var", u))
      statements.push(x)

      return expression(u)
    },
    compile: function (x) {
      return compileBlock("try", "", x.args[0]) + x.args.slice(1).map(function (x) {
               return minify(" ") + compile(x)
             }).join("")
    }
  })

  makeOp("catch", {
    expression: function (x) {
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return compileBlock("catch", "(" + compile(x.args[0]) + ")" + minify(" "), x.args[1])
    }
  })

  makeOp("finally", {
    expression: function (x) {
      x.args[0] = blockStatement(x.args[0])
      return x
    },
    compile: function (x) {
      return compileBlock("finally", "", x.args[0])
    }
  })

  makeOp("bypass", {
    isImpure: true, // TODO
    isLiteral: true,
    compile: function (x) {
      return "" + x.args[0]
    }
  })

  /*makeOp("switch", {
    noSemicolon: true,
    isImpure: "children",
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      var u = n.op("box")

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
        return "switch" + minify(" ") + "(" + compile(x.args[0]) + ")" + minify(" ") +
                 "{" + minify("\n") + x.args.slice(1).map(compile).join(";" + minify("\n")) +
                 minify("\n") + "}"
      })
    }
  })

  makeOp("case", {
    expression: function (x) {
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return "case " + compile(x.args[0]) + ":" + minify("\n") + block(x.args[1])
    }
  })

  makeOp("fallthru", {
    expression: function (x) {
      x.args[0] = expression(x.args[0])
      x.args[1] = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return "case " + compile(x.args[0]) + ":" + minify("\n") + block(x.args[1])
    }
  })

  makeOp("default", {
    expression: function (x) {
      x.args[0] = blockStatement(x.args[0])
      return x
    },
    compile: function (x) {
      return "default:" + minify("\n") + block(x.args[0])
    }
  })*/

  return n
})(NINO || {})
