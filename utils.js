var NINO = (function (n) {
  "use strict"

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    if (Object(o) === o) {
      /*n.tokenUpdate(o, function (o) {
        o.type = "error"
      })*/
      var b1 = o.start && o.start.line != null
        , b2 = o.start && o.start.column != null
      if (o.text != null || b1 || b2) {
        var iOffset = 0
        a.push("\n")
        if (o.text != null) {
          a.push("  ", o.text.replace(/^( +)|\n$/g, function (_, s1) {
            if (s1) {
              iOffset = s1.length
            }
            return ""
          }))
        }
        if (b1 || b2) {
          a.push("  (")
          if (b1) {
            a.push("line ", o.start.line)
          }
          if (b1 && b2) {
            a.push(", ")
          }
          if (b2) {
            a.push("column ", o.start.column)
          }
          a.push(")")
        }
        if (o.text != null && b2) {
          // TODO: make it work for multi-line tokens
          a.push("\n ", new Array(o.start.column - iOffset + 1).join(" "),
                        new Array((o.end.column - o.start.column) + 1).join("^"))
        }
      }
      this.text  = o.text
      this.start = o.start
      this.end   = o.end
    }
    this.originalMessage = s
    this.message = a.join("")
  }
  n.Error.prototype = new Error()
  n.Error.prototype.constructor = n.Error
  n.Error.prototype.name = "NINO.Error"

  // Converts any array-like object into an iterator
  n.iterator = function (s) {
    var i = 0
    return {
      peek: function () {
        return s[i]
      },
      read: function () {
        return s[i++]
      },
      has: function () {
        return i < s.length
      }
    }
  }

  function reMatch(r, s) {
    var x = r.exec(s)
    return (x && x[0] !== ""
             ? x[1] + "\n"
             : "")
  }

  // Buffers a string by line and keeps track of line and column information
  // Returns an iterator that moves through the string one character at a time
  // This is useful for error messages
  n.stringBuffer = function (s) {
    var re = /([^\n]*)(?:\n|$)/g
      , o  = { line: 1, column: 1 }
    return {
      text: reMatch(re, s),
      start: o,
      end: o,
      peek: function () {
        return this.text[o.column - 1]
      },
      read: function () {
        var x = this.text[o.column - 1]
        if (o.column >= this.text.length) {
          var y = reMatch(re, s)
          // TODO: a little bit hacky
          if (y === "") {
            ++o.column
            this.read = function () {
              return this.text[o.column - 1]
            }
          } else {
            this.text = y
            o.column = 1
            ++o.line
          }
        } else {
          ++o.column
        }
        return x
      },
      has: function () {
        return o.column <= this.text.length
      }
    }
  }

  n.store = function (o) {
    return { text:  o.text
           , start: { line:   o.start.line
                    , column: o.start.column }
           , end:   { line:   o.end.line
                    , column: o.end.column } }
  }

  n.enrich = function (x, y, z) {
    x.text  = y.text
    x.start = { line:   y.start.line
              , column: y.start.column }
    if (z != null) {
      x.end = { line:   z.end.line
              , column: z.end.column }
    } else {
      x.end = { line:   y.end.line
              , column: y.end.column }
    }
    return x
  }

  return n
})(NINO || {})
