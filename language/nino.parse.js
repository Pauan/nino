var exports, NINO = (function (n) {
  "use strict";

  function reJoin() {
    return new RegExp([].map.call(arguments, function (x) {
      return "(?:" + x.source + ")"
    }).join("|"))
  }

  var r = reJoin(/([\(\)\-\:\,])/,
                 /(")((?:\\.|[^\\"])+)(")/,
                 /(r\/)((?:\\.|[^\\\/])+)(\/)([i]?)/,
                 /(?: +(OR) +)|(?: *(\|) *)/,
                 /( ) */,
                 /((?:\\.|[^\(\)\-\:\,\| "])+)/)

  var o = parser.make({
    rules: {},
    default: function (a, s) {
      return ["name", s]
    }
  })

  o.rules["("]

  n.tokenize = function (s) {
    return parser.iterator(s.split(r).filter(function (s) { return s }))
  }

  n.parse = function (s) {
    return o.parse(n.tokenize(s), 0)
  }

  return n
})(exports || NINO || {})
