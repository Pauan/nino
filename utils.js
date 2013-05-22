var NINO = (function (n) {
  "use strict"

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    if (Object(o) === o) {
      /*n.tokenUpdate(o, function (o) {
        o.type = "error"
      })*/
      var b1 = o.start && ("line"   in o.start)
        , b2 = o.start && ("column" in o.start)
      if ("text" in o || b1 || b2) {
        var iOffset = 0
        a.push("\n")
        if ("text" in o) {
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
        if ("text" in o && b2) {
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
    return {
      line: 1,
      column: 1,
      text: reMatch(re, s),
      peek: function () {
        return this.text[this.column - 1]
      },
      read: function () {
        var x = this.text[this.column - 1]
        if (this.column >= this.text.length) {
          var y = reMatch(re, s)
          // TODO: a little bit hacky
          if (y === "") {
            ++this.column
            this.read = function () {
              return this.text[this.column - 1]
            }
          } else {
            this.text = y
            this.column = 1
            ++this.line
          }
        } else {
          ++this.column
        }
        return x
      },
      has: function () {
        return this.column <= this.text.length
      }
    }
  }

  //
  n.cloneBuffer = function (x) {
  }

  return n
})(NINO || {})
