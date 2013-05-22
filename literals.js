var NINO = (function (n) {
  "use strict"

  n.ops = {}

  n.opArray = function (s, args) {
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
    } else if (x.op === "wrapper") {
      x.args = x.args.map(n.fromJSON)
      return x
    } else if (x.isLiteral) {
      return x
    } else if (n.ops[x[0]].isLiteral) {
      return n.opArray(x[0], x.slice(1))
    } else {
      return n.opArray(x[0], x.slice(1).map(n.fromJSON))
    }
  }

  n.makeOp = function (s, info) {
    info.op = s
    n.ops[s] = info
  }

  function makeLiteral(s, info) {
    if (typeof info === "function") {
      info = { compile: info }
    }
    info.isLiteral = true
    n.makeOp(s, info)
  }

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

  function mangleBox(s) {
    // TODO: document isn't writable either, but should probably be handled in a different way
    if (s === "undefined" || s === "NaN" || s === "Infinity" || s === "arguments") {
      return "_" + s
    } else {
      return mangle(s)
    }
  }

  function getBox(x, s) {
    var s2 = s
      , i  = 2
    while (n.scope[s2]) {
      s2 = s + i
      ++i
    }
    n.scope[s2] = true
    x.string = s2
    return s2
  }

  function getNextUniq(s, f) {
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

  function getUniq(x) {
    var s = "a"
    while (n.scope[s]) {
      s = getNextUniq(s)
    }
    n.scope[s] = true
    x.string = s
    return s
  }

  makeLiteral("bypass", {
    isImpure: true, // TODO
    isStatement: true, // TODO
    compile: function (x) {
      return "" + x.args[0]
    }
  })

  makeLiteral("line-comment", {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      return n.minify("// " + x.args[0])
    }
  })

  makeLiteral("block-comment", {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      return n.minify("/* " + x.args[0].replace(/\*\/|\n/g, function (s) {
        if (s === "\n") {
          return "\n" + n.space() + "   "
        } else {
          return "* /"
        }
      }) + " */")
    }
  })

  makeLiteral("doc-comment", {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      return "/**\n" + n.space() + " * " + x.args[0].replace(/\*\/|\n/g, function (s) {
        if (s === "\n") {
          return "\n" + n.space() + " * "
        } else {
          return "* /"
        }
      }) + "\n" + n.space() + " */"
    }
  })

  makeLiteral("symbol", {
    isVariable: true,
    compile: function (x) {
      return mangle(x.args[0])
    }
  })

  makeLiteral("unique", {
    isVariable: true,
    compile: function (x) {
      if (x.string) {
        return mangleBox(x.string)
      } else if (x.args[0] && !n.minified) {
        return mangleBox(getBox(x, x.args[0]))
      } else {
        return getUniq(x)
      }
    }
  })

  makeLiteral("true", function () {
    return "true"
  })

  makeLiteral("false", function () {
    return "false"
  })

  makeLiteral("null", function () {
    return "null"
  })

  makeLiteral("number", function (x) {
    return x.args[0]
  })

  makeLiteral("string", function (x) {
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
  })

  makeLiteral("regexp", function (x) {
    return "/" + x.args[0].replace(/[\/\\]/g, "\\$&") + "/"
  })

  return n
})(NINO || {})
