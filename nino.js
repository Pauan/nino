(function (root, factory) {
  "use strict"

  // Universal Module Definition (UMD) to support AMD, CommonJS/Node.js,
  // and plain browser loading,
  if (typeof define === "function" && define.amd) {
    define(["./lib/esprima/esprima",
            null, //"./lib/escodegen/escodegen",
            null, //"./lib/esmangle/esmangle",
            "exports"], factory)
  } else if (typeof require !== "undefined" && typeof exports !== "undefined") {
    factory(require("./lib/esprima/esprima"),
            null, //require("./lib/escodegen/escodegen"),
            null, //require("./lib/esmangle/esmangle"),
            exports)
  } else {
    if (typeof root.NINO === "undefined") {
      root.NINO = {}
    }
    factory(root.esprima, root.escodegen, root.esmangle, root.NINO)
  }
}(this, function (esprima, escodegen, esmangle, n) {
  "use strict"

  n.error = function (x, s) {
    return new Error(s + ": " + n.print(x))
  }

  n.getUnique = function (s, scope) {
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

  // TODO
  n.fromAST = function (x) {
  }

  n.toAST = function (x) {
  }

  n.fromJSON = function (x) {
    if (typeof x === "number" || typeof x === "string") {
      return n.literal(x)
    } else if (Array.isArray(x)) {
      return n.opArray(x[0], x.slice(1).map(n.fromJSON))
    } else {
      return x
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
    info.op = s
    n[s] = function (x) {
      var y = Object.create(info)
      y.args = [x]
      if (info.init != null) {
        info.init(y)
      }
      return y
    }
  }

  makeLiteral("bypass", 1, 1, {
    isImpure: true, // TODO
    isStatement: true, // TODO
    compile: function (x) {
      return serialize(x.args[0])
    }
  })

  makeLiteral("lineComment", 0, 1, {
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

  makeLiteral("blockComment", 0, 1, {
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

  makeLiteral("docComment", 0, 1, {
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
    init: function (x) {
      x.id = ++uniqueIds
    },
    compile: function (x) {
      throw n.error(x, "cannot compile unique, use NINO.replace")
    }
  })

  makeLiteral("literal", 1, 1, function (x) {
    var y = x.args[0]
    if (typeof y === "string") {
      return "\"" + y.replace(/["\\\b\f\n\r\t\v]/g, function (s) {
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
    } else if ({}.toString.call(y) === "[object RegExp]") {
      return "/" + y.replace(/[\/\\]/g, "\\$&") + "/"
    } else {
      return "" + y
    }
  })

  n.makeOp = function (s, info) {
    info.op = s
    n.ops[s] = info
  }

  n.minified = false
  n.warnings = true

  /*
    Generated with:

      ;(function (self) {
        var seen = {}
        while (self !== null && self !== Object.prototype) {
          Object.getOwnPropertyNames(self).forEach(function (s) {
            seen[s] = true
          })
          self = Object.getPrototypeOf(self)
        }
        return Object.keys(seen).sort(function (x, y) {
          return x.toLocaleLowerCase().localeCompare(y.toLocaleLowerCase())
        }).join(" ")
      })(this)
  */
  n.builtins = (function (n) {
    "Array ArrayBuffer Boolean clearInterval clearTimeout console DataView Date decodeURI decodeURIComponent encodeURI encodeURIComponent Error escape eval EvalError Float32Array Float64Array Function Infinity Int16Array Int32Array Int8Array isFinite isNaN JSON Math NaN Number Object parseFloat parseInt RangeError ReferenceError RegExp setInterval setTimeout String SyntaxError TypeError Uint16Array Uint32Array Uint8Array Uint8ClampedArray undefined unescape URIError".split(" ").forEach(function (x) { n[x] = true })
    //n["arguments"] = true

    // Chrome 27.0.1453.6
    "addEventListener alert applicationCache ArrayBufferView atob Attr Audio AudioProcessingEvent AutocompleteErrorEvent BeforeLoadEvent Blob blur btoa cancelAnimationFrame CanvasGradient CanvasPattern CanvasRenderingContext2D captureEvents CDATASection CharacterData chrome clientInformation ClientRect ClientRectList Clipboard close closed CloseEvent Comment CompositionEvent confirm Counter crypto CSSCharsetRule CSSFontFaceRule CSSHostRule CSSImportRule CSSMediaRule CSSPageRule CSSPrimitiveValue CSSRule CSSRuleList CSSStyleDeclaration CSSStyleRule CSSStyleSheet CSSValue CSSValueList CustomEvent defaultStatus defaultstatus DeviceOrientationEvent devicePixelRatio dispatchEvent document Document DocumentFragment DocumentType DOMException DOMImplementation DOMParser DOMSettableTokenList DOMStringList DOMStringMap DOMTokenList Element Entity EntityReference ErrorEvent Event event EventException EventSource external File FileError FileList FileReader find focus FocusEvent FormData frameElement frames getComputedStyle getMatchedCSSRules getSelection HashChangeEvent history HTMLAllCollection HTMLAnchorElement HTMLAppletElement HTMLAreaElement HTMLAudioElement HTMLBaseElement HTMLBaseFontElement HTMLBodyElement HTMLBRElement HTMLButtonElement HTMLCanvasElement HTMLCollection HTMLContentElement HTMLDataListElement HTMLDirectoryElement HTMLDivElement HTMLDListElement HTMLDocument HTMLElement HTMLEmbedElement HTMLFieldSetElement HTMLFontElement HTMLFormControlsCollection HTMLFormElement HTMLFrameElement HTMLFrameSetElement HTMLHeadElement HTMLHeadingElement HTMLHRElement HTMLHtmlElement HTMLIFrameElement HTMLImageElement HTMLInputElement HTMLKeygenElement HTMLLabelElement HTMLLegendElement HTMLLIElement HTMLLinkElement HTMLMapElement HTMLMarqueeElement HTMLMediaElement HTMLMenuElement HTMLMetaElement HTMLMeterElement HTMLModElement HTMLObjectElement HTMLOListElement HTMLOptGroupElement HTMLOptionElement HTMLOptionsCollection HTMLOutputElement HTMLParagraphElement HTMLParamElement HTMLPreElement HTMLProgressElement HTMLQuoteElement HTMLScriptElement HTMLSelectElement HTMLShadowElement HTMLSourceElement HTMLSpanElement HTMLStyleElement HTMLTableCaptionElement HTMLTableCellElement HTMLTableColElement HTMLTableElement HTMLTableRowElement HTMLTableSectionElement HTMLTemplateElement HTMLTextAreaElement HTMLTitleElement HTMLTrackElement HTMLUListElement HTMLUnknownElement HTMLVideoElement IDBCursor IDBCursorWithValue IDBDatabase IDBFactory IDBIndex IDBKeyRange IDBObjectStore IDBOpenDBRequest IDBRequest IDBTransaction IDBVersionChangeEvent Image ImageData indexedDB innerHeight innerWidth Intl KeyboardEvent length localStorage location locationbar matchMedia MediaController MediaError MediaKeyError MediaKeyEvent MediaList MediaStreamEvent menubar MessageChannel MessageEvent MessagePort MimeType MimeTypeArray MouseEvent moveBy moveTo MutationEvent MutationObserver name NamedNodeMap navigator Node NodeFilter NodeList Notation Notification OfflineAudioCompletionEvent offscreenBuffering onabort onbeforeunload onblur oncanplay oncanplaythrough onchange onclick oncontextmenu ondblclick ondeviceorientation ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus onhashchange oninput oninvalid onkeydown onkeypress onkeyup onload onloadeddata onloadedmetadata onloadstart onmessage onmousedown onmousemove onmouseout onmouseover onmouseup onmousewheel onoffline ononline onpagehide onpageshow onpause onplay onplaying onpopstate onprogress onratechange onreset onresize onscroll onsearch onseeked onseeking onselect onstalled onstorage onsubmit onsuspend ontimeupdate ontransitionend onunload onvolumechange onwaiting onwebkitanimationend onwebkitanimationiteration onwebkitanimationstart onwebkittransitionend open openDatabase opener Option outerHeight outerWidth OverflowEvent PageTransitionEvent pageXOffset pageYOffset parent performance PERSISTENT personalbar Plugin PluginArray PopStateEvent postMessage print ProcessingInstruction ProgressEvent prompt Range RangeException Rect releaseEvents removeEventListener requestAnimationFrame resizeBy resizeTo RGBColor RTCIceCandidate RTCSessionDescription screen screenLeft screenTop screenX screenY scroll scrollbars scrollBy scrollTo scrollX scrollY Selection self sessionStorage SharedWorker showModalDialog SpeechInputEvent SQLException status statusbar stop Storage StorageEvent styleMedia StyleSheet StyleSheetList SVGAElement SVGAltGlyphDefElement SVGAltGlyphElement SVGAltGlyphItemElement SVGAngle SVGAnimateColorElement SVGAnimatedAngle SVGAnimatedBoolean SVGAnimatedEnumeration SVGAnimatedInteger SVGAnimatedLength SVGAnimatedLengthList SVGAnimatedNumber SVGAnimatedNumberList SVGAnimatedPreserveAspectRatio SVGAnimatedRect SVGAnimatedString SVGAnimatedTransformList SVGAnimateElement SVGAnimateMotionElement SVGAnimateTransformElement SVGCircleElement SVGClipPathElement SVGColor SVGComponentTransferFunctionElement SVGCursorElement SVGDefsElement SVGDescElement SVGDocument SVGElement SVGElementInstance SVGElementInstanceList SVGEllipseElement SVGException SVGFEBlendElement SVGFEColorMatrixElement SVGFEComponentTransferElement SVGFECompositeElement SVGFEConvolveMatrixElement SVGFEDiffuseLightingElement SVGFEDisplacementMapElement SVGFEDistantLightElement SVGFEDropShadowElement SVGFEFloodElement SVGFEFuncAElement SVGFEFuncBElement SVGFEFuncGElement SVGFEFuncRElement SVGFEGaussianBlurElement SVGFEImageElement SVGFEMergeElement SVGFEMergeNodeElement SVGFEMorphologyElement SVGFEOffsetElement SVGFEPointLightElement SVGFESpecularLightingElement SVGFESpotLightElement SVGFETileElement SVGFETurbulenceElement SVGFilterElement SVGFontElement SVGFontFaceElement SVGFontFaceFormatElement SVGFontFaceNameElement SVGFontFaceSrcElement SVGFontFaceUriElement SVGForeignObjectElement SVGGElement SVGGlyphElement SVGGlyphRefElement SVGGradientElement SVGHKernElement SVGImageElement SVGLength SVGLengthList SVGLinearGradientElement SVGLineElement SVGMarkerElement SVGMaskElement SVGMatrix SVGMetadataElement SVGMissingGlyphElement SVGMPathElement SVGNumber SVGNumberList SVGPaint SVGPathElement SVGPathSeg SVGPathSegArcAbs SVGPathSegArcRel SVGPathSegClosePath SVGPathSegCurvetoCubicAbs SVGPathSegCurvetoCubicRel SVGPathSegCurvetoCubicSmoothAbs SVGPathSegCurvetoCubicSmoothRel SVGPathSegCurvetoQuadraticAbs SVGPathSegCurvetoQuadraticRel SVGPathSegCurvetoQuadraticSmoothAbs SVGPathSegCurvetoQuadraticSmoothRel SVGPathSegLinetoAbs SVGPathSegLinetoHorizontalAbs SVGPathSegLinetoHorizontalRel SVGPathSegLinetoRel SVGPathSegLinetoVerticalAbs SVGPathSegLinetoVerticalRel SVGPathSegList SVGPathSegMovetoAbs SVGPathSegMovetoRel SVGPatternElement SVGPoint SVGPointList SVGPolygonElement SVGPolylineElement SVGPreserveAspectRatio SVGRadialGradientElement SVGRect SVGRectElement SVGRenderingIntent SVGScriptElement SVGSetElement SVGStopElement SVGStringList SVGStyleElement SVGSVGElement SVGSwitchElement SVGSymbolElement SVGTextContentElement SVGTextElement SVGTextPathElement SVGTextPositioningElement SVGTitleElement SVGTransform SVGTransformList SVGTRefElement SVGTSpanElement SVGUnitTypes SVGUseElement SVGViewElement SVGViewSpec SVGVKernElement SVGZoomAndPan SVGZoomEvent TEMPORARY Text TextEvent TextMetrics TextTrack TextTrackCue TextTrackCueList TextTrackList TimeRanges toolbar top TrackEvent TransitionEvent UIEvent URL v8Intl WebGLActiveInfo WebGLBuffer WebGLContextEvent WebGLFramebuffer WebGLProgram WebGLRenderbuffer WebGLRenderingContext WebGLShader WebGLShaderPrecisionFormat WebGLTexture WebGLUniformLocation WebKitAnimationEvent webkitAudioContext webkitAudioPannerNode webkitCancelAnimationFrame webkitCancelRequestAnimationFrame webkitConvertPointFromNodeToPage webkitConvertPointFromPageToNode WebKitCSSFilterRule WebKitCSSFilterValue WebKitCSSKeyframeRule WebKitCSSKeyframesRule WebKitCSSMatrix WebKitCSSMixFunctionValue WebKitCSSTransformValue webkitIDBCursor webkitIDBDatabase webkitIDBFactory webkitIDBIndex webkitIDBKeyRange webkitIDBObjectStore webkitIDBRequest webkitIDBTransaction webkitIndexedDB WebKitMediaSource webkitMediaStream WebKitMutationObserver webkitNotifications webkitOfflineAudioContext WebKitPoint webkitRequestAnimationFrame webkitRequestFileSystem webkitResolveLocalFileSystemURL webkitRTCPeerConnection WebKitShadowRoot WebKitSourceBuffer WebKitSourceBufferList webkitSpeechGrammar webkitSpeechGrammarList webkitSpeechRecognition webkitSpeechRecognitionError webkitSpeechRecognitionEvent webkitStorageInfo WebKitTransitionEvent webkitURL WebSocket WheelEvent Window window Worker XMLDocument XMLHttpRequest XMLHttpRequestException XMLHttpRequestProgressEvent XMLHttpRequestUpload XMLSerializer XPathEvaluator XPathException XPathResult XSLTProcessor".split(" ").forEach(function (x) { n[x] = true })

    // Node.js 0.8.5
    "Buffer global GLOBAL module process require root".split(" ").forEach(function (x) { n[x] = true })

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
      if (x.args[0] == null) {
        return "<unique>"
      } else {
        return x.args[0] // TODO
      }
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

  n.expression = function (x) {
    return blockWith(x, "expression")
  }

  n.statement = function (x) {
    return blockWith(x, "statement")
  }

  n.compile = function (x) {
    return compileFn(x)
  }

  n.traverse = function (x, scope) {
    var scopes = [{ seen: scope, bound: {} }]

    function seen(x) {
      if (x.op === "variable") {
        var s = compile(x)
          , i = scopes.length
        while (i--) {
          scopes[i].seen[s] = true
          if (scopes[i].bound[s]) {
            break
          }
        }
      }
    }

    function bind(w) {
      var last = scopes[scopes.length - 1]
        , x    = n.unwrap(w)
      if (x.op === "variable") {
        last.bound[compile(x)] = true
        seen(x)
      } else if (x.op === "unique") {
        if (last.uniques != null) {
          last.uniques[x.id] = true
        }
      }
      return w
    }

    function withScope(x, f) {
      scopes.push({ seen:    x.seenVariables
                  , uniques: x.boundUniques
                  , bound:   {} })
      try {
        f()
      } finally {
        scopes.pop()
      }
    }

    function traverse(w) {
      var x = n.unwrap(w)
      if (x.isLiteral) {
        seen(x)
      } else if (x.op === "function") {
        x.seenVariables = {}
        x.boundUniques = {}
        withScope(x, function () {
          seen(n.variable("arguments"))
          var args = n.unwrap(x.args[0])
          args.args = args.args.map(bind)
          if (x.args.length > 1) {
            x.args[1] = traverse(x.args[1])
          }
        })
      } else if (x.op === "function-var") {
        x.args[0] = bind(x.args[0])
        x.seenVariables = {}
        x.boundUniques = {}
        withScope(x, function () {
          seen(n.variable("arguments"))
          var args = n.unwrap(x.args[1])
          args.args = args.args.map(bind)
          if (x.args.length > 2) {
            x.args[2] = traverse(x.args[2])
          }
        })
      } else if (x.op === "var") {
        x.args = x.args.map(function (w) {
          var x = n.unwrap(w)
          if (x.op === "=") {
            x.args[0] = bind(x.args[0])
            x.args[1] = traverse(x.args[1]) // TODO
            return w
          } else {
            return n.wrap(w, bind(x))
          }
        })
      /* TODO
      } else if (x.op === "catch") {
        // scopes[scopes.length - 1].seen
        x.seenVariables = {}
        withScope(x.seenVariables, function () {
          x.args[0] = bind(x.args[0])
          x.args[1] = traverse(x.args[1])
        })*/
      } else {
        x.args = x.args.map(traverse)
      }
      return w
    }

    return traverse(x)
  }

  n.replace = function (x, scope) {
    var replace = {}
      , uniques

    function withScope(x, f) {
      var old  = replace
        , old2 = scope
        , old3 = uniques
      replace = Object.create(replace)
      scope   = x.seenVariables
      uniques = x.boundUniques
      try {
        f()
      } finally {
        replace = old
        scope   = old2
        uniques = old3
      }
    }

    function seen(w) {
      var x = n.unwrap(w)
      if (x.op === "unique") {
        var s = replace[x.id]
        if (s == null) {
          s = replace[x.id] = (x.args[0] != null && !n.minified
                                ? n.getUnique(mangle(x.args[0]), scope)
                                : getUniq(scope))
        }
        scope[s] = true
        return n.wrap(w, n.variable(s)) // TODO clone
      } else {
        return w
      }
    }

    function binder(w) {
      var x = n.unwrap(w)
      if (x.isLiteral) {
        if (x.op === "unique") {
          if (replace[x.id] != null) {
            if (uniques[x.id]) {
              delete scope[replace[x.id]]
              replace[x.id] = null
            } else {
              scope[replace[x.id]] = true
            }
          }
        }
      } else {
        x.args.forEach(binder)
      }
    }

    function traverse(w) {
      var x = n.unwrap(w)
      if (x.isLiteral) {
        return seen(w)
      } else if (x.op === "function") {
        withScope(x, function () {
          var args = n.unwrap(x.args[0])

          args.args.forEach(binder)
          if (x.args.length > 1) {
            binder(x.args[1])
          }

          args.args = args.args.map(seen)
          if (x.args.length > 1) {
            x.args[1] = traverse(x.args[1])
          }
        })
      } else if (x.op === "function-var") {
        x.args[0] = seen(x.args[0]) // TODO test this
        withScope(x, function () {
          var args = n.unwrap(x.args[1])

          args.args.forEach(binder)
          if (x.args.length > 2) {
            binder(x.args[2])
          }

          args.args = args.args.map(seen)
          if (x.args.length > 2) {
            x.args[2] = traverse(x.args[2])
          }
        })
      } else if (x.op === "var") {
        x.args = x.args.map(function (w) {
          var x = n.unwrap(w)
          if (x.op === "=") {
            x.args[0] = seen(x.args[0])
            x.args[1] = traverse(x.args[1]) // TODO
            return w
          } else {
            return n.wrap(w, seen(x))
          }
        })
      } else {
        x.args = x.args.map(traverse)
      }
      return w
    }

    return traverse(x)
  }


  var indent    = 0
    , priority  = 0
    , uniqueIds = 0
    , scope     = { loop: false, function: false }
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
    var x = n.unwrap(w), y
    if (x.op === "literal" && typeof (y = x.args[0]) === "string") {
      if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(y)) {
        return y
      }
    }
  }

  function propToString(w) {
    var x = n.unwrap(w)
    if (x.op === "literal") {
      return x
    } else if (x.op === "variable") {
      return n.wrap(w, n.literal(x.args[0]))
    } else {
      throw n.error("expected literal or variable")
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

  function compileFunction(name, args, body) {
    return resetPriority(0, function () {
      // n.changeScope(this.scope)
      return withScope("function", function () {
        args = n.unwrap(args).args.map(compile).join("," + n.minify(" "))
        return "function" + (name ? " " + name : n.minify(" ")) + "(" +
                 args + ")" + n.minify(" ") + withBraces(body)
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

        var b = (x.isLast && s === "return") // TODO hacky

        if (x.args.length > 0) {
          var y = x.args[0]
          if (y.op === "if" &&
              y.args.length > 1 &&
              (isStatement(y.args[1]) ||
               (y.args.length > 2 && isStatement(y.args[2])))) {
            y.args[1] = NINO.op(s, y.args[1])
            if (y.args.length > 2) {
              y.args[2] = NINO.op(s, y.args[2])
            } else if (!b) {
              y.args[2] = NINO.op(s)
            }
            statement(y)
            return
          } else if (y.op === "try") {
            y.args[0] = NINO.op(s, y.args[0])
            y.args.slice(1).forEach(function (y, i) {
              if (y.op === "catch") {
                if (y.args.length > 1) {
                  y.args[1] = NINO.op(s, y.args[1])
                } else if (!b) {
                  y.args[1] = NINO.op(s)
                }
              }
            })
            statement(y)
            return
          }
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

        if (!(b && x.args.length === 0)) {
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
              y.args[0] = n.op("void", n.literal(0))
            }
          })
        } else {
          pushExpressions(x, isImpure)
        }
        expressions = []
        statement(x)
        return expression(n.op("void", n.literal(0)))
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

  function typer(args, ret) {
    return function () {
      return n.type("function", args.map(function (x) {
        return n.type(x)
      }), n.type(ret))
    }
  }

  function concater(l, r) {
    if (l.length === 0) {
      if (r.length === 1) {
        return r[0]
      } else {
        return n.opArray("call", n.op(".", r[0], n.literal("concat")),
                                 r.slice(1))
      }
    } else {
      l = n.opArray("array", l)
      if (r.length === 0) {
        return l
      } else {
        return n.opArray("call", n.op(".", l, n.literal("concat")), r)
      }
    }
  }

  function makeConcat(a, f) {
    var r1 = []
      , r2 = []
      , seen
    a.forEach(function (w) {
      var x = n.unwrap(w)
      if (x.op === "...") {
        seen = true
        r2.push(expression(x.args[0]))
      } else if (seen) {
        r2.push(expression(n.op("array", w)))
      } else {
        r1.push(expression(w))
      }
    })
    if (seen) {
      return f(concater(r1, r2), seen)
    } else {
      return f(r1, seen)
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
          if (y.op === "literal" && y.args[0] === 1) {
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
                var u = n.unique()
                statements.push(n.op("var", n.op("=", u, x.args[0])))
                x.args[0] = u
              }
              x.args.reduce(function (x, y, i) {
                if (isImpure(y) && i !== len) {
                  var u = n.unique()
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
            if (o.expression != null) {
              return o.expression(x)
            } else {
              x.args[0] = expression(x.args[0])
              x.args[1] = expression(x.args[1])
            }
          }
          return x
        }
      },
      compile: function (x) {
        if (x.args.length === 1) {
          return withPriority(x.unary, function () {
            var right = n.unwrap(x.args[0])

            if (x.unary === right.unary) {
              if (o.wrap && x.op === right.op && right.args.length === 1) {
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
              if (x.binary === left.binary && left.args.length === 2) {
                return "(" + compile(left) + ")" +
                       n.minify(" ") + s2 + n.minify(" ") +
                       "(" + compile(right) + ")"
              } else {
                var temp = left
                left     = right
                right    = temp
                // TODO code duplication
                if (o.wrap && x.op === right.op && right.args.length === 1) {
                  return compileFn(left) +
                         n.minify(" ") + s2 + n.minify(" ") +
                         "(" + compile(right) + ")"
                }
              }
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
        z.seenVariables = y.seenVariables // TODO clone
        z.boundUniques = y.boundUniques // TODO clone
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
        ;(function (w) {
          var x = n.unwrap(w)
          if (x.op === ",") {
            x.args.forEach(function (x) {
              statements.push(x)
            })
          } else {
            statements.push(w)
          }
        })(expression(x))
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

  // TODO better simple detection
  function isSimple(w) {
    var x = n.unwrap(w)
    return x.op === "literal" || x.op === "unique" || x.op === "variable"
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

  function withValue(r, x) {
    if (!isSimple(x)) {
      var u = n.unique()
      r.push(n.op("var", n.op("=", u, x)))
      return u
    } else {
      return x
    }
  }

  function assignToVar(x) {
    if (n.unwrap(x).op === "=") {
      return n.op("var", x)
    } else {
      return x
    }
  }

  function destructureSlice(r, x, value, i, iLen) {
    var a = [n.op(".", n.op(".", n.op("array"),
                                 n.literal("slice")),
                       n.literal("call")),
             value]
    var len = iLen - 1
    if (i !== len) {
      a.push(n.literal(i))
      a.push(n.literal(i - len))
    } else if (i !== 0) {
      a.push(n.literal(i))
    }
    destructure(r, x, n.opArray("call", a))
  }

  function destructureLength(r, x, value, i) {
    destructure(r, x, n.op(".", value,
                                n.op("-", n.op(".", value, n.literal("length")),
                                          n.literal(i))))
  }

  function destructure(r, pattern, value) {
    var x = n.unwrap(pattern)
    if (x.op === "array") {
      value = withValue(r, value)
      var i = 0, iLen = x.args.length
      while (i < iLen) {
        (function (w) {
          var y = n.unwrap(w)
          if (y.op === "...") {
            destructureSlice(r, y.args[0], value, i, iLen)
            ++i
            while (i < iLen) {
              destructureLength(r, x.args[i], value, iLen - i)
              ++i
            }
          } else if (y.op !== "empty") {
            destructure(r, w, n.op(".", value, n.literal(i)))
          }
        })(x.args[i])
        ++i
      }
    } else if (x.op === "object") {
      value = withValue(r, value)
      x.args.forEach(function (w) {
        var x = n.unwrap(w)
        if (x.op === "=") {
          destructure(r, x.args[1], n.op(".", value, propToString(x.args[0])))
        } else {
          destructure(r, x, n.op(".", value, propToString(x)))
        }
      })
    } else if (x.isVariable) {
      r.push(n.op("=", pattern, value))
    } else {
      throw n.error(pattern, "expected array, object, or variable")
    }
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

  function expressionFn(x, min, max) {
    var args = n.unwrap(x.args[min])
      , r    = []
      , a    = []
      , i    = 0
      , iLen = args.args.length

    while (i < iLen) {
      (function (w) {
        var x = n.unwrap(w)
        if (x.isVariable) {
          a.push(expression(w))
        } else {
          var u
          if (x.op === "...") {
            if (i === iLen - 1) {
              u = n.variable("arguments")
            } else {
              u = n.unique()
              r.push(n.op("var", n.op("=", u, n.variable("arguments"))))
            }
            destructureSlice(r, x.args[0], u, i, iLen)
            ++i
            while (i < iLen) {
              x = args.args[i]
              if (n.unwrap(x).op !== "empty") {
                destructureLength(r, x, u, iLen - i)
              }
              ++i
            }
          } else {
            u = n.unique()
            if (x.op !== "empty") {
              destructure(r, w, u)
            }
            a.push(expression(u))
          }
        }
      })(args.args[i])
      ++i
    }
    args.args = a

    if (r.length) {
      r = r.map(assignToVar)
      if (x.args.length > max) {
        r = r.concat([x.args[max]])
      }
      x.args[max] = n.opArray(",", r)
    }
    if (x.args.length > max) {
      x.args[max] = functionStatement(x.args[max])
    }
  }

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

  function pushExpressions(x, f) {
    expressions.forEach(function (y) {
      if (/*!contains(y, x) && */f(y)) {
        var u = n.unique()
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
  n.makeOp("wrapper", {})

  op("!", {
    type: typer(["bool"], "bool"),
    unary: 70,
    args: 1
  })

  op("~", {
    type: typer(["int"], "int"),
    unary: 70,
    args: 1
  })

  op("typeof", {
    type: typer(["any"], "str"),
    unary: 70,
    args: 1,
    name: "typeof "
  })

  op("void", {
    type: typer(["any"], "void"),
    unary: 70,
    args: 1,
    name: "void "
  })

  op("delete", {
    type: typer(["any"], "bool"),
    unary: 70,
    args: 1,
    name: "delete ",
    isImpure: true
  })

  op("*", {
    type: typer(["num", "num"], "num"),
    binary: 65
  })

  op("/", {
    type: typer(["num", "num"], "num"),
    binary: 65
  })

  op("%", {
    type: typer(["num", "num"], "num"),
    binary: 65,
    args: 2
  })

  op("+", {
    type: typer(["strnum", "strnum"], ["|", "num", "str"]),
    binary: 60,
    unary: 70,
    wrap: true
  })

  op("-", {
    type: typer(["num", "num"], "num"),
    binary: 60,
    unary: 70,
    wrap: true
  })

  op("<<", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  op(">>", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  op(">>>", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  op("<", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  op("<=", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  op(">", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  op(">=", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  op("in", {
    type: typer(["str", "object"], "bool"),
    binary: 50,
    args: 2,
    name: " in "
  })

  op("instanceof", {
    type: typer(["object", "function"], "bool"),
    binary: 50,
    args: 2,
    name: " instanceof "
  })

  op("==", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  op("!=", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  op("===", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  op("!==", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  op("&", {
    type: typer(["int", "int"], "int"),
    binary: 40,
    args: 2
  })

  op("^", {
    type: typer(["int", "int"], "int"),
    binary: 35,
    args: 2
  })

  op("|", {
    type: typer(["int", "int"], "int"),
    binary: 30,
    args: 2
  })

  op("&&", {
    type: typer(["bool", "bool"], "bool"),
    binary: 25
  })

  op("||", {
    type: typer(["bool", "bool"], "bool"),
    binary: 20
  })

  op("=", {
    // TODO
    type: function (x) {
      x = x.args[1]
      return x.type(x)
    },
    binary: 10,
    args: 2,
    order: "right",
    isImpure: true,
    expression: function (x) {
      var a = []
        , l = []
        , r = []
      destructure(a, x.args[0], expression(x.args[1]))
      a.forEach(function (x) {
        if (n.unwrap(x).op === "var") {
          l.push(x)
        } else {
          r.push(x)
        }
      })
      return n.opArray(",", l.concat(r))
    }
  })

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

  n.makeOp("...", {
    expression: function (x) {
      argumentError(x, 1, 1)
      throw n.error(x, "invalid use of ...")
    }
  })

  n.makeOp("empty", {
    type: typer([], "void"),
    statement: function (x) {
      argumentError(x, 0, 0)
    },
    expression: function (x) {
      argumentError(x, 0, 0)
      return n.op("void", n.literal(0))
    }
  })

  n.makeOp(";", {
    // TODO
    type: function (x) {
      if (x.args.length) {
        x = x.args[x.args.length - 1]
        return n.type("function", [], x.type(x))
      } else {
        return n.type("void")
      }
    },
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
        x.args.forEach(function anon(w, i) {
          var x = n.unwrap(w)
          if (x.op === ",") {
            x.args.forEach(anon)
          } else {
            var y = expression(w)
            if (i !== len) {
              useless(y)
            }
            if (i === len || x.op !== "empty") {
              r.push(y)
            }
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
                                      // TODO does this work correctly for non-number literals?
            if (x.op === "literal" && Math.round(x.args[0]) === x.args[0]) {
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
      var r = []
      x.args.forEach(function (w) {
        var x = n.unwrap(w)
        if (x.op === "=") {
          //x.args[0] = expression(x.args[0]) // TODO
          //x.args[1] = expression(x.args[1])
          destructure(r, x.args[0], expression(x.args[1]))
          //return n.wrap(w, x) // TODO
        } else {
          r.push(n.op("var", expression(w)))
        }
      })
      r.forEach(function (x) {
        statements.push(assignToVar(x)) // TODO
      })
      //x.args = r.map(assignToVar)
      //x.args = x.args.map(expression)
      //statements.push(x)
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
        last = n.unwrap(last.args[0])
      }

      while (true) {
        if (last.op === "array") {
          last = n.unwrap(last.args[last.args.length - 1])
          if (last.op === "...") {
            last = n.unwrap(last.args[0])
          }
        } else if (last.op === "object") {
          last = n.unwrap(last.args[last.args.length - 1])
          if (last.op === "=") {
            last = n.unwrap(last.args[1])
          }
        } else {
          break
        }
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
          var u = n.unique()
          x.args[1] = n.op("=", u, x.args[1])
          statement(n.op("var", u))
          statement(x)
          return expression(u)
        } else {
          return expression(n.op("&&", x.args[0], x.args[1]))
        }
      case 3:
        if (isStatement(x.args[1]) || isStatement(x.args[2])) {
          var u = n.unique()
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
      var first = expression(x.args[0])
      return makeConcat(x.args.slice(1), function (r, seen) {
        if (seen) {
          x.args = [n.op(".", first, n.literal("apply")),
                    n.literal(null),
                    r]
        } else {
          x.args = [first].concat(r)
        }
        return x
      })
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
      return makeConcat(x.args, function (r, seen) {
        if (seen) {
          return r
        } else {
          x.args = r
          return x
        }
      })
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
          x.args.forEach(function (w) {
            var x = n.unwrap(w)
            if (x.op === "=") {
              r.push(n.minify("\n") + n.space() +
                     (jsProp(x.args[0]) || compile(x.args[0])) + ":" + n.minify(" ") +
                     compile(x.args[1]))
            } else {
              r.push(n.minify("\n") + n.space() +
                     (jsProp(x) || compile(x)) + ":" + n.minify(" ") + compile(x))
            }
          })
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
      return expression(n.op("void", n.literal(0)))
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
      return expression(n.op("void", n.literal(0)))
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
      return expression(n.op("void", n.literal(0)))
    },
    compile: function (x) {
      return compileLoop(x, "for", compile(x.args[0]) + " in " + compile(x.args[1]), x.args[2])
    }
  })

  n.makeOp("function", {
    isFunction: true,
    expression: function (x) {
      argumentError(x, 1, 2)
      expressionFn(x, 0, 1)
      return x
    },
    compile: function (x) {
      return compileFunction("", x.args[0], x.args[1])
    }
  })

  n.makeOp("function-var", {
    noSemicolon: true,
    isFunction: true,
    isImpure: true,
    isStatement: true,
    statement: function (x) {
      argumentError(x, 2, 3)
      x.args[0] = expression(x.args[0])
      expressionFn(x, 1, 2)
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure) // TODO
      statement(x)
      return expression(x.args[0])
    },
    compile: function (x) {
      return compileFunction(compile(x.args[0]), x.args[1], x.args[2])
    }
  })

  n.makeOp("try", {
    noSemicolon: true,
    //isImpure: "children", TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1)
      var seen = {}
      x.args = x.args.map(function (x, i) {
        if (seen[x.op]) {
          throw n.error(x, "cannot have two of the same operator in a try block")
        }
        seen[x.op] = true
        if (x.op === "catch" && seen["finally"]) {
          throw n.error(x, "catch must be before finally")
        }
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

      var u = n.unique()

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
      var u = n.unique()

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

}));
