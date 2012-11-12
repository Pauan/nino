var exports, NINO = (function (n) {
  "use strict";

  var o = parser.make({
    rules: {},
    default: function (a, s) {
      return ["name", s]
    }
  })

  function brace(s, sPre) {
    rules[s] = {
      binding: 0
    }
    return {
      binding: 0,
      prefix: function (o) {
        var r = parser.parse(o, rules)
        parser.next(s)
        if (sPre) {
          return
        } else {
          return r
        }
      }
    }
  }

  function binary(i, s, f) {
    return {
      binding: i,
      prefix: f,
      infix: function (o, l) {
        var r = parser.parse(o, rules)
        return [s, l, r]
      }
    }
  }

  var rules = {
    "(" : brace(")"),
    "[" : brace("]", "dict"),
    "{" : brace("}", "array"),

    "." : {
      binding: 0,
      infix: function (o, l) {
        var r = parser.parse(o, rules)
        return ["get", l, new n.String(r.value)]
      }
    },

    "->": {
      binding: 0,
      prefix: function (o) {
        var l = parser.parse(o, rules)
          , r = parser.parse(o, rules)
        return ["fn", l, r]
      }
    },
    "|": {
      binding: 0
    },

    "||": binary(0, "set!"),

    "+" : binary(0, "add", function (o) {
            return ["+", parser.parse(o, rules)]
          }),
    "-" : binary(0, "sub", function (o) {
            return ["-", parser.parse(o, rules)]
          }),
    "*" : binary(0, "mul"),
    "/" : binary(0, "div"),

    "=" : binary(0, "is"),
    "<" : binary(0, "lt"),
    "<=": binary(0, "lt-is"),
    ">" : binary(0, "gt"),
    "=>": binary(0, "gt-is"),
  }

  function symbol(s) {
    return {
      binding: 0
      prefix:
    }
  }

  function number(s) {
    return {
      binding: 0
      prefix:
    }
  }

  function tokenizeSymbol(o, a) {
    var s
    while ((s = o.read()) && /[$_a-zA-Z0-9\-]/.test(s)) {
      a.push(s)
    }
    return symbol(a.join(""))
  }

  function tokenizeNumber(o, a) {
    var s, dotSeen
    while ((s = o.read())) {
      if (s === ".") {
        if (dotSeen) {
          return tokenizeSymbol(o, a)
        } else {
          dotSeen = true
          a.push(s)
        }
      } else if (/[0-9]/.test(s)) {
        a.push(s)
      }
    }
    return number(a.join(""))
  }

  function tokenizeBlockComment(o) {
    while (true) {
      o.read()
      if (o.peek() === "|") {
        if (o.read() === "#") {
          break
        }
      } else if (o.peek() === "#") {
        if (o.read() === "|") {
          tokenizeBlockComment(o)
        }
      }
    }
  }

  function twon(o, s1, s2) {
    var s = o.read()
    if (s === s2) {
      o.read()
      return rules[s1 + s2]
    } else {
      return rules[s1]
    }
  }

  function tokenize(o) {
    var s, r = []
    while (s = o.peek()) {
      if (s === "=") {
        r.push(twon(o, "=", ">"))
      } else if (s === "-") {
        r.push(twon(o, "-", ">"))
      } else if (s === "|") {
        r.push(twon(o, "|", "|"))
      } else if (s === "<") {
        r.push(twon(o, "<", "="))
      } else if (s === " ") {
        o.read()
      } else if (s === "\n") {
        o.read()
      } else if (s === "#") {
        if (o.read() === "|") {
          tokenizeBlockComment(o)
        } else {
          while (o.peek() !== "\n") {
            o.read()
          }
        }
      } else if (rules[s]) {
        o.read()
        r.push(rules[s])
      } else {
        if (/[0-9]/.test(s)) {
          r.push(tokenizeNumber(o, [s]))
        } else if (/[$_a-zA-Z]/.test(s)) {
          r.push(tokenizeSymbol(o, [s]))
        } else {
          throw new Error("invalid character: " + s)
        }
      }
    }
    return r
  }

  n.tokenize = function (s) {
    return tokenize(parser.iterator(s))
    //return parser.iterator(s.split(r).filter(function (s) { return s }))
  }

  n.parse = function (s) {
    return o.parse(n.tokenize(s), 0)
  }

  return n
})(exports || NINO || {})
