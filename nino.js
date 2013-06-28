define(["lib/esprima/esprima"], function (esprima) {
  "use strict"

  var opt = {
    minified: false,
    warnings: true,
    mangle: function (s) {
      return s.replace(/[^$a-zA-Z0-9]/g, function (s, s1, s2) {
        return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
      })
    },
    error: function (x, s) {
      return new Error(s + ": " + print(x))
    }
  }

  var ops = {}

  function getUnique(s, scope) {
    var s2 = s
      , i  = 2
    while (scope[s2]) {
      s2 = s + i
      ++i
    }
    return s2
  }

  function opArray(s) {
    var args = [].slice.call(arguments, 1, -1)
    args = args.concat(arguments[arguments.length - 1])

    if (ops[s] == null) {
      throw new Error("unknown operator: " + s)
    }
    var o = Object.create(ops[s])
    o.args = args
    return o
  }

  function op(s) {
    return opArray(s, [].slice.call(arguments, 1))
  }

  // https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
  // TODO clone or whatever
  function fromAST(x) {
    switch (x.type) {
    case "LabeledStatement":
    case "WithStatement":
    case "SwitchStatement":
    case "DoWhileStatement":
    case "ForOfStatement":
    case "LetStatement":
    case "ArrowExpression":
    case "LetExpression":
    case "SwitchCase":
      throw new Error("not supported: " + x.type)
    case "Program":
    case "BlockStatement":
      return opArray(",", x.body.map(fromAST))
    case "EmptyStatement":
      return op(",")
    case "ExpressionStatement":
      return fromAST(x.expression)
    case "IfStatement":
      var a = [fromAST(x.test), fromAST(x.consequent)]
      if (x.alternate !== null) {
        a.push(fromAST(x.alternate))
      }
      return opArray("if", a)
    case "BreakStatement":
      if (x.label !== null) {
        throw new Error("not supported: break [label]")
      }
      return op("break")
    case "ContinueStatement":
      if (x.label !== null) {
        throw new Error("not supported: continue [label]")
      }
      return op("continue")
    case "ReturnStatement":
      return opArray("return", (x.argument === null ? [] : [fromAST(x.argument)]))
    case "ThrowStatement":
      return op("throw", fromAST(x.argument))
    case "TryStatement":
      var a = [fromAST(x.block)]
      if (x.handler !== null) {
        if (x.handler.type !== "CatchClause") {
          throw new Error("not supported: " + x.handler.type)
        }
        if (x.handler.guard !== null) {
          throw new Error("not supported")
        }
        a.push(op("catch", fromAST(x.handler.param), fromAST(x.handler.body)))
      }
      if (x.finalizer !== null) {
        a.push(op("finally", fromAST(x.finalizer)))
      }
      if (x.guardedHandlers.length) {
        throw new Error("not supported")
      }
      return opArray("try", a)
    case "WhileStatement":
      return op("while", fromAST(x.test), fromAST(x.body))
    case "ForStatement":
      return op("for", (x.init === null ? op("empty") : fromAST(x.init)),
                       (x.test === null ? op("empty") : fromAST(x.test)),
                       (x.update === null ? op("empty") : fromAST(x.update)),
                       fromAST(x.body))
    case "ForInStatement":
      if (x.each) {
        throw new Error("not supported")
      }
      return op("for-in", fromAST(x.left), fromAST(x.right), fromAST(x.body))
    case "DebuggerStatement":
      return op("debugger")
    case "FunctionDeclaration":
    case "FunctionExpression":
      var a = x.params.map(fromAST)
      if (x.rest !== null) {
        a.push(op("...", fromAST(x.rest)))
      }
      if (x.defaults.length || x.generator || x.expression) {
        throw new Error("not supported")
      }
      if (x.type === "FunctionDeclaration") {
        return op("function-var", fromAST(x.id), opArray(",", a), fromAST(x.body))
      } else {
        if (x.id !== null) {
          throw new Error("not supported: (function " + x.id + "() { ... })")
        }
        return op("function", opArray(",", a), fromAST(x.body))
      }
    case "VariableDeclaration":
      if (x.kind !== "var") {
        throw new Error("not supported: " + x.kind)
      }
      var a = x.declarations.map(function (x) {
        if (x.type !== "VariableDeclarator") {
          throw new Error("not supported: " + x.type)
        }
        if (x.init === null) {
          return fromAST(x.id)
        } else {
          return op("=", fromAST(x.id), fromAST(x.init))
        }
      })
      return opArray("var", a)
    case "ThisExpression":
      return op("this")
    case "ArrayExpression":
    case "ArrayPattern":
      var a = x.elements.map(function (x) {
        if (x === null) {
          return op("empty")
        } else {
          return fromAST(x)
        }
      })
      return opArray("array", a)
    case "ObjectExpression":
      var a = x.properties.map(function (x) {
        if (x.kind !== "init") {
          throw new Error("not supported: " + x.kind)
        }
        return op("=", fromAST(x.key), fromAST(x.value))
      })
      return opArray("object", a)
    case "ObjectPattern":
      var a = x.properties.map(function (x) {
        return op("=", fromAST(x.key), fromAST(x.value))
      })
      return opArray("object", a)
    case "SequenceExpression":
      return opArray(",", x.expressions.map(fromAST))
    case "UnaryExpression":
    case "UpdateExpression":
      if (!x.prefix) {
        throw new Error("not supported")
      }
      return op(x.operator, fromAST(x.argument))
    case "BinaryExpression":
    case "AssignmentExpression":
    case "LogicalExpression":
      if (x.operator === "+=") {
        return op("++", fromAST(x.left), fromAST(x.right))
      } else if (x.operator === "-=") {
        return op("--", fromAST(x.left), fromAST(x.right))
      } else {
        return op(x.operator, fromAST(x.left), fromAST(x.right))
      }
    case "ConditionalExpression":
      return op("if", fromAST(x.test), fromAST(x.alternate), fromAST(x.consequent))
    case "NewExpression":
    case "CallExpression":
      var a = [fromAST(x.callee)]
      x.arguments.forEach(function (x) {
        if (x === null) {
          // TODO
          throw new Error("not supported")
          //return op("empty")
        } else {
          return fromAST(x)
        }
      })
      return opArray((x.type === "NewExpression" ? "new" : "call"), a)
    case "MemberExpression":
      return op(".", fromAST(x.object), fromAST(x.property))
    case "Identifier":
      return variable(x.name)
    case "Literal":
      return literal(x.value)
    }
  }

  function toAST(x) {
  }

  function fromJSON(x) {
    if (typeof x === "number" || typeof x === "string") {
      return literal(x)
    } else if (Array.isArray(x)) {
      return opArray(x[0], x.slice(1).map(fromJSON))
    } else {
      return x
    }
  }

  function parse(s) {
    return fromAST(esprima.parse(s))
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
    return function (x) {
      var y = Object.create(info)
      y.args = [x]
      if (info.init != null) {
        info.init(y)
      }
      return y
    }
  }

  var bypass = makeLiteral("bypass", 1, 1, {
    isImpure: true, // TODO
    isStatement: true, // TODO
    compile: function (x) {
      return serialize(x.args[0])
    }
  })

  var lineComment = makeLiteral("lineComment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return minify("//")
      } else {
        return minify("// " + x.args[0])
      }
    }
  })

  var blockComment = makeLiteral("blockComment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return "/**/"
      } else {
        return minify("/* " + x.args[0].replace(/\*\/|\n/g, function (s) {
          if (s === "\n") {
            return "\n" + space() + "   "
          } else {
            return "* /"
          }
        }) + " */")
      }
    }
  })

  var docComment = makeLiteral("docComment", 0, 1, {
    isUseful: true,
    noSemicolon: true,
    compile: function (x) {
      if (x.args[0] == null) {
        return "/**/"
      } else {
        return "/**\n" + space() + " * " + x.args[0].replace(/\*\/|\n/g, function (s) {
          if (s === "\n") {
            return "\n" + space() + " * "
          } else {
            return "* /"
          }
        }) + "\n" + space() + " */"
      }
    }
  })

  var variable = makeLiteral("variable", 1, 1, {
    isVariable: true,
    compile: function (x) {
      return mangle(x.args[0])
    }
  })

  var unique = makeLiteral("unique", 0, 1, {
    isVariable: true,
    init: function (x) {
      x.id = ++uniqueIds
    },
    compile: function (x) {
      throw opt.error(x, "cannot compile unique, use nino.replace")
    }
  })

  var literal = makeLiteral("literal", 1, 1, function (x) {
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

  function makeOp(s, info) {
    info.op = s
    ops[s] = info
  }

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
  var builtins = (function (n) {
    "Array ArrayBuffer Boolean clearInterval clearTimeout console DataView Date decodeURI decodeURIComponent encodeURI encodeURIComponent Error escape eval EvalError Float32Array Float64Array Function Infinity Int16Array Int32Array Int8Array isFinite isNaN JSON Math NaN Number Object parseFloat parseInt RangeError ReferenceError RegExp setInterval setTimeout String SyntaxError TypeError Uint16Array Uint32Array Uint8Array Uint8ClampedArray undefined unescape URIError".split(" ").forEach(function (x) { n[x] = true })
    //n["arguments"] = true

    // Chrome 27.0.1453.6
    "addEventListener alert applicationCache ArrayBufferView atob Attr Audio AudioProcessingEvent AutocompleteErrorEvent BeforeLoadEvent Blob blur btoa cancelAnimationFrame CanvasGradient CanvasPattern CanvasRenderingContext2D captureEvents CDATASection CharacterData chrome clientInformation ClientRect ClientRectList Clipboard close closed CloseEvent Comment CompositionEvent confirm Counter crypto CSSCharsetRule CSSFontFaceRule CSSHostRule CSSImportRule CSSMediaRule CSSPageRule CSSPrimitiveValue CSSRule CSSRuleList CSSStyleDeclaration CSSStyleRule CSSStyleSheet CSSValue CSSValueList CustomEvent defaultStatus defaultstatus DeviceOrientationEvent devicePixelRatio dispatchEvent document Document DocumentFragment DocumentType DOMException DOMImplementation DOMParser DOMSettableTokenList DOMStringList DOMStringMap DOMTokenList Element Entity EntityReference ErrorEvent Event event EventException EventSource external File FileError FileList FileReader find focus FocusEvent FormData frameElement frames getComputedStyle getMatchedCSSRules getSelection HashChangeEvent history HTMLAllCollection HTMLAnchorElement HTMLAppletElement HTMLAreaElement HTMLAudioElement HTMLBaseElement HTMLBaseFontElement HTMLBodyElement HTMLBRElement HTMLButtonElement HTMLCanvasElement HTMLCollection HTMLContentElement HTMLDataListElement HTMLDirectoryElement HTMLDivElement HTMLDListElement HTMLDocument HTMLElement HTMLEmbedElement HTMLFieldSetElement HTMLFontElement HTMLFormControlsCollection HTMLFormElement HTMLFrameElement HTMLFrameSetElement HTMLHeadElement HTMLHeadingElement HTMLHRElement HTMLHtmlElement HTMLIFrameElement HTMLImageElement HTMLInputElement HTMLKeygenElement HTMLLabelElement HTMLLegendElement HTMLLIElement HTMLLinkElement HTMLMapElement HTMLMarqueeElement HTMLMediaElement HTMLMenuElement HTMLMetaElement HTMLMeterElement HTMLModElement HTMLObjectElement HTMLOListElement HTMLOptGroupElement HTMLOptionElement HTMLOptionsCollection HTMLOutputElement HTMLParagraphElement HTMLParamElement HTMLPreElement HTMLProgressElement HTMLQuoteElement HTMLScriptElement HTMLSelectElement HTMLShadowElement HTMLSourceElement HTMLSpanElement HTMLStyleElement HTMLTableCaptionElement HTMLTableCellElement HTMLTableColElement HTMLTableElement HTMLTableRowElement HTMLTableSectionElement HTMLTemplateElement HTMLTextAreaElement HTMLTitleElement HTMLTrackElement HTMLUListElement HTMLUnknownElement HTMLVideoElement IDBCursor IDBCursorWithValue IDBDatabase IDBFactory IDBIndex IDBKeyRange IDBObjectStore IDBOpenDBRequest IDBRequest IDBTransaction IDBVersionChangeEvent Image ImageData indexedDB innerHeight innerWidth Intl KeyboardEvent length localStorage location locationbar matchMedia MediaController MediaError MediaKeyError MediaKeyEvent MediaList MediaStreamEvent menubar MessageChannel MessageEvent MessagePort MimeType MimeTypeArray MouseEvent moveBy moveTo MutationEvent MutationObserver name NamedNodeMap navigator Node NodeFilter NodeList Notation Notification OfflineAudioCompletionEvent offscreenBuffering onabort onbeforeunload onblur oncanplay oncanplaythrough onchange onclick oncontextmenu ondblclick ondeviceorientation ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus onhashchange oninput oninvalid onkeydown onkeypress onkeyup onload onloadeddata onloadedmetadata onloadstart onmessage onmousedown onmousemove onmouseout onmouseover onmouseup onmousewheel onoffline ononline onpagehide onpageshow onpause onplay onplaying onpopstate onprogress onratechange onreset onresize onscroll onsearch onseeked onseeking onselect onstalled onstorage onsubmit onsuspend ontimeupdate ontransitionend onunload onvolumechange onwaiting onwebkitanimationend onwebkitanimationiteration onwebkitanimationstart onwebkittransitionend open openDatabase opener Option outerHeight outerWidth OverflowEvent PageTransitionEvent pageXOffset pageYOffset parent performance PERSISTENT personalbar Plugin PluginArray PopStateEvent postMessage print ProcessingInstruction ProgressEvent prompt Range RangeException Rect releaseEvents removeEventListener requestAnimationFrame resizeBy resizeTo RGBColor RTCIceCandidate RTCSessionDescription screen screenLeft screenTop screenX screenY scroll scrollbars scrollBy scrollTo scrollX scrollY Selection self sessionStorage SharedWorker showModalDialog SpeechInputEvent SQLException status statusbar stop Storage StorageEvent styleMedia StyleSheet StyleSheetList SVGAElement SVGAltGlyphDefElement SVGAltGlyphElement SVGAltGlyphItemElement SVGAngle SVGAnimateColorElement SVGAnimatedAngle SVGAnimatedBoolean SVGAnimatedEnumeration SVGAnimatedInteger SVGAnimatedLength SVGAnimatedLengthList SVGAnimatedNumber SVGAnimatedNumberList SVGAnimatedPreserveAspectRatio SVGAnimatedRect SVGAnimatedString SVGAnimatedTransformList SVGAnimateElement SVGAnimateMotionElement SVGAnimateTransformElement SVGCircleElement SVGClipPathElement SVGColor SVGComponentTransferFunctionElement SVGCursorElement SVGDefsElement SVGDescElement SVGDocument SVGElement SVGElementInstance SVGElementInstanceList SVGEllipseElement SVGException SVGFEBlendElement SVGFEColorMatrixElement SVGFEComponentTransferElement SVGFECompositeElement SVGFEConvolveMatrixElement SVGFEDiffuseLightingElement SVGFEDisplacementMapElement SVGFEDistantLightElement SVGFEDropShadowElement SVGFEFloodElement SVGFEFuncAElement SVGFEFuncBElement SVGFEFuncGElement SVGFEFuncRElement SVGFEGaussianBlurElement SVGFEImageElement SVGFEMergeElement SVGFEMergeNodeElement SVGFEMorphologyElement SVGFEOffsetElement SVGFEPointLightElement SVGFESpecularLightingElement SVGFESpotLightElement SVGFETileElement SVGFETurbulenceElement SVGFilterElement SVGFontElement SVGFontFaceElement SVGFontFaceFormatElement SVGFontFaceNameElement SVGFontFaceSrcElement SVGFontFaceUriElement SVGForeignObjectElement SVGGElement SVGGlyphElement SVGGlyphRefElement SVGGradientElement SVGHKernElement SVGImageElement SVGLength SVGLengthList SVGLinearGradientElement SVGLineElement SVGMarkerElement SVGMaskElement SVGMatrix SVGMetadataElement SVGMissingGlyphElement SVGMPathElement SVGNumber SVGNumberList SVGPaint SVGPathElement SVGPathSeg SVGPathSegArcAbs SVGPathSegArcRel SVGPathSegClosePath SVGPathSegCurvetoCubicAbs SVGPathSegCurvetoCubicRel SVGPathSegCurvetoCubicSmoothAbs SVGPathSegCurvetoCubicSmoothRel SVGPathSegCurvetoQuadraticAbs SVGPathSegCurvetoQuadraticRel SVGPathSegCurvetoQuadraticSmoothAbs SVGPathSegCurvetoQuadraticSmoothRel SVGPathSegLinetoAbs SVGPathSegLinetoHorizontalAbs SVGPathSegLinetoHorizontalRel SVGPathSegLinetoRel SVGPathSegLinetoVerticalAbs SVGPathSegLinetoVerticalRel SVGPathSegList SVGPathSegMovetoAbs SVGPathSegMovetoRel SVGPatternElement SVGPoint SVGPointList SVGPolygonElement SVGPolylineElement SVGPreserveAspectRatio SVGRadialGradientElement SVGRect SVGRectElement SVGRenderingIntent SVGScriptElement SVGSetElement SVGStopElement SVGStringList SVGStyleElement SVGSVGElement SVGSwitchElement SVGSymbolElement SVGTextContentElement SVGTextElement SVGTextPathElement SVGTextPositioningElement SVGTitleElement SVGTransform SVGTransformList SVGTRefElement SVGTSpanElement SVGUnitTypes SVGUseElement SVGViewElement SVGViewSpec SVGVKernElement SVGZoomAndPan SVGZoomEvent TEMPORARY Text TextEvent TextMetrics TextTrack TextTrackCue TextTrackCueList TextTrackList TimeRanges toolbar top TrackEvent TransitionEvent UIEvent URL v8Intl WebGLActiveInfo WebGLBuffer WebGLContextEvent WebGLFramebuffer WebGLProgram WebGLRenderbuffer WebGLRenderingContext WebGLShader WebGLShaderPrecisionFormat WebGLTexture WebGLUniformLocation WebKitAnimationEvent webkitAudioContext webkitAudioPannerNode webkitCancelAnimationFrame webkitCancelRequestAnimationFrame webkitConvertPointFromNodeToPage webkitConvertPointFromPageToNode WebKitCSSFilterRule WebKitCSSFilterValue WebKitCSSKeyframeRule WebKitCSSKeyframesRule WebKitCSSMatrix WebKitCSSMixFunctionValue WebKitCSSTransformValue webkitIDBCursor webkitIDBDatabase webkitIDBFactory webkitIDBIndex webkitIDBKeyRange webkitIDBObjectStore webkitIDBRequest webkitIDBTransaction webkitIndexedDB WebKitMediaSource webkitMediaStream WebKitMutationObserver webkitNotifications webkitOfflineAudioContext WebKitPoint webkitRequestAnimationFrame webkitRequestFileSystem webkitResolveLocalFileSystemURL webkitRTCPeerConnection WebKitShadowRoot WebKitSourceBuffer WebKitSourceBufferList webkitSpeechGrammar webkitSpeechGrammarList webkitSpeechRecognition webkitSpeechRecognitionError webkitSpeechRecognitionEvent webkitStorageInfo WebKitTransitionEvent webkitURL WebSocket WheelEvent Window window Worker XMLDocument XMLHttpRequest XMLHttpRequestException XMLHttpRequestProgressEvent XMLHttpRequestUpload XMLSerializer XPathEvaluator XPathException XPathResult XSLTProcessor".split(" ").forEach(function (x) { n[x] = true })

    // Node.js 0.8.5
    "Buffer global GLOBAL module process require root".split(" ").forEach(function (x) { n[x] = true })

    return n
  })({})

  function wrap(x, y) {
    if (x.op === "wrapper") {
      x.args[0] = y
      return x
    } else {
      return y
    }
  }

  function unwrap(x) {
    if (x.op === "wrapper") {
      return unwrap(x.args[0])
    } else {
      return x
    }
  }

  function print(x) {
    if (x.op === "unique") {
      if (x.args[0] == null) {
        return "<unique>"
      } else {
        return x.args[0] // TODO
      }
    } else if (x.isLiteral) {
      return x.compile(x)
    } else if (x.op === "wrapper") {
      return print(x.args[0])
    } else {
      return "(" + x.op + x.args.map(function (x) {
        return " " + print(x)
      }).join("") + ")"
    }
  }

  function space() {
    if (opt.minified) {
      return ""
    } else {
      return new Array((indent * 2) + 1).join(" ")
    }
  }

  function minify(s, s2) {
    if (opt.minified) {
      return s2 || ""
    } else {
      return s
    }
  }

  function expressionTop(x) {
    return blockWith(x, "expression")
  }

  function statementTop(x) {
    return blockWith(x, "statement")
  }

  /*function moduleTop(x) {
    var requires = []
      , exports  = []

    return statementTop(op("call", variable("require"), op("function", op(","), x)))
  }*/

  function compileTop(x) {
    return compileFn(x)
  }

  function traverse(x, scope) {
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
        , x    = unwrap(w)
      if (x.op === "variable") {
        last.bound[compile(x)] = true
        seen(x)
      } else if (x.op === "unique") {
        if (last.uniques != null) {
          last.uniques[x.id] = true
        }
      }
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
      var x = unwrap(w)
      if (x.isLiteral) {
        seen(x)
      } else if (x.op === "function") {
        x.seenVariables = {}
        x.boundUniques = {}
        withScope(x, function () {
          seen(variable("arguments"))
          unwrap(x.args[0]).args.forEach(bind)
          if (x.args.length > 1) {
            traverse(x.args[1])
          }
        })
      } else if (x.op === "function-var") {
        bind(x.args[0])
        x.seenVariables = {}
        x.boundUniques = {}
        withScope(x, function () {
          seen(variable("arguments"))
          unwrap(x.args[1]).args.forEach(bind)
          if (x.args.length > 2) {
            traverse(x.args[2])
          }
        })
      } else if (x.op === "var") {
        x.args.forEach(function (w) {
          var x = unwrap(w)
          if (x.op === "=") {
            bind(x.args[0])
            traverse(x.args[1]) // TODO
          } else {
            bind(x)
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
        x.args.forEach(traverse)
      }
    }

    traverse(x)
    return x
  }

  function replace(x, scope) {
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
      var x = unwrap(w)
      if (x.op === "unique") {
        var s = replace[x.id]
        if (s == null) {
          s = replace[x.id] = (x.args[0] != null && !opt.minified
                                ? getUnique(mangle(x.args[0]), scope)
                                : getUniq(scope))
        }
        scope[s] = true
        return wrap(w, variable(s)) // TODO clone
      } else {
        return w
      }
    }

    function binder(w) {
      var x = unwrap(w)
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
      var x = unwrap(w)
      if (x.isLiteral) {
        return seen(w)
      } else if (x.op === "function") {
        withScope(x, function () {
          var args = unwrap(x.args[0])

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
          var args = unwrap(x.args[1])

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
          var x = unwrap(w)
          if (x.op === "=") {
            x.args[0] = seen(x.args[0])
            x.args[1] = traverse(x.args[1]) // TODO
            return w
          } else {
            return wrap(w, seen(x))
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
      return opt.mangle(s).replace(/^[0-9]/, function (s) {
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
      if (opt.warnings) {
        console.warn("serializing object, identity and prototype will be lost")
      }
      switch (s) {
      case "[object Array]":
        if (opt.minified) {
          return "[" + [].map.call(x, serialize).join(",") + "]"
        } else {
          return "[" + [].map.call(x, serialize).join(", ") + "]"
        }

      case "[object Object]":
        if (opt.minified) {
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

  function jsProp(w) {
    var x = unwrap(w), y
    if (x.op === "literal" && typeof (y = x.args[0]) === "string") {
      if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(y)) {
        return y
      }
    }
  }

  function propToString(w) {
    var x = unwrap(w)
    if (x.op === "literal") {
      return x
    } else if (x.op === "variable") {
      return wrap(w, literal(x.args[0]))
    } else {
      throw opt.error("expected literal or variable")
    }
  }

  function compile(w) {
    var x = unwrap(w)
      , s = x.compile(x)
    if (x.isUseless && opt.warnings) {
      if (/\n/.test(s)) {
        console.warn("useless expression:\n" + space() + s)
      } else {
        console.warn("useless expression: " + s)
      }
    }
    return s
  }

  function compileFn(w) {
    var x = unwrap(w)
    if (x.op === "function" || x.op === "object") {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
    }
  }

  function withBraces(x) {
    return (x == null || (x.op === ";" && x.args.length === 0)
             ? "{}"
             : "{" + minify("\n") + block(x) + minify("\n") + space() + "}")
  }

  function compileFunction(name, args, body) {
    return resetPriority(0, function () {
      return withScope("function", function () {
        args = unwrap(args).args.map(compile).join("," + minify(" "))
        return "function" + (name ? " " + name : minify(" ")) + "(" +
                 args + ")" + minify(" ") + withBraces(body)
      })
    })
  }

  function compileBlock(name, test, body) {
    return resetPriority(0, function () {
      return name + minify(" ") + test + withBraces(body)
    })
  }

  function compileLoop(x, name, test, body) {
    return withScope("loop", function () {
      if (body == null || (body = unwrap(body)).op === ";") {
        unwrap(x).noSemicolon = true
        return compileBlock(name, "(" + test + ")" + minify(" "), body)
      } else {
        return resetPriority(0, function () {
          return name + minify(" ") + "(" + test + ")" + minify("\n") + block(body)
        })
      }
    })
  }

  function useless(w) {
    var x = unwrap(w)
    if (!isImpure(x) && !x.isUseful) { // TODO
      x.isUseless = true
    }
  }

  /*function contains(w, x) {
    var y = unwrap(w)
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
        throw opt.error(x, "expected at least " + min + " argument but got " + len)
      } else if (min == null) {
        throw opt.error(x, "expected at most " + max + " argument but got " + len)
      }

      if (min === max) {
        throw opt.error(x, "expected " + min + " arguments but got " + len)
      } else {
        throw opt.error(x, "expected " + min + " to " + max + " arguments but got " + len)
      }
    }
  }

  function makeOpStatement(s, isBreak, f) {
    makeOp(s, {
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
            y.args[1] = op(s, y.args[1])
            if (y.args.length > 2) {
              y.args[2] = op(s, y.args[2])
            } else if (!b) {
              y.args[2] = op(s)
            }
            statement(y)
            return
          } else if (y.op === "try") {
            y.args[0] = op(s, y.args[0])
            y.args.slice(1).forEach(function (y, i) {
              if (y.op === "catch") {
                if (y.args.length > 1) {
                  y.args[1] = op(s, y.args[1])
                } else if (!b) {
                  y.args[1] = op(s)
                }
              }
            })
            statement(y)
            return
          }
        }

        x.args = x.args.map(expression)

        if (x.args.length > 0 && s === "return") { // TODO ew
          var y = unwrap(x.args[0])
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
        if (opt.warnings) {
          console.warn("\"" + s + "\" was used in expression position")
        }
        if (x.isBreak) {
          expressions.forEach(function (y) {
            if (isImpure(y)/* && !contains(y, x)*/) {
              statement(y)
              y.args[0] = op("void", literal(0))
            }
          })
        } else {
          pushExpressions(x, isImpure)
        }
        expressions = []
        statement(x)
        return expression(op("void", literal(0)))
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
    var x = unwrap(w)
      , y = unwrap(x.args[i])
    if (y.op === ",") {
      while (y.op === ",") {
        y.args.slice(0, -1).forEach(statement)
        y = unwrap(y.args[y.args.length - 1])
      }
      x.args[i] = wrap(x.args[i], y)
    }
  }

  function typer(args, ret) {
    return function () {
      return type("function", args.map(function (x) {
        return type(x)
      }), type(ret))
    }
  }

  function concater(l, r) {
    if (l.length === 0) {
      if (r.length === 1) {
        return r[0]
      } else {
        return opArray("call", op(".", r[0], literal("concat")),
                               r.slice(1))
      }
    } else {
      l = opArray("array", l)
      if (r.length === 0) {
        return l
      } else {
        return opArray("call", op(".", l, literal("concat")), r)
      }
    }
  }

  function makeConcat(a, f) {
    var r1 = []
      , r2 = []
      , seen
    a.forEach(function (w) {
      var x = unwrap(w)
      if (x.op === "...") {
        seen = true
        r2.push(expression(x.args[0]))
      } else if (seen) {
        r2.push(expression(op("array", w)))
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

  function makeOpAssign(s, s2) {
    makeOp(s, {
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
          var y = unwrap(x.args[1])
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
            var right = unwrap(x.args[0])
            if (x.unary === right.unary) {
              throw opt.error(x, "invalid assignment") // s2 + compile(right)
            }
            return s + compile(right)
          })
        } else {
          return withPriority(x.binary, function () {
            var left  = unwrap(x.args[0])
              , right = unwrap(x.args[1])
            if (x.binary === left.binary) {
              throw opt.error(x, "invalid left hand side for assignment")
                                   /*
                                   compile(left) + ")" +
                                   minify(" ") + s2 + minify(" ") +
                                   compile(right)
                                   */
            }
            return compileFn(left) +
                   minify(" ") + s2 + minify(" ") +
                   compile(right)
          })
        }
      }
    })
  }

  function makeOpHelper(s, o) {
    var s2 = o.name || s
    makeOp(s, {
      unary: o.unary,
      binary: o.binary,
      isImpure: o.isImpure,
      destructure: o.destructure,
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
                var u = unique()
                statements.push(op("var", op("=", u, x.args[0])))
                x.args[0] = u
              }
              x.args.reduce(function (x, y, i) {
                if (isImpure(y) && i !== len) {
                  var u = unique()
                  statements.push(op("var", op("=", u, y)))
                  r.push(op(s, x, u))
                  return u
                } else {
                  r.push(op(s, x, y))
                  return y
                }
              })
              return expression(opArray(o.pairwise, r))
            } else {
              return expression(x.args.reduce(function (x, y) {
                return op(s, x, y)
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
            var right = unwrap(x.args[0])

            if (x.unary === right.unary) {
              if (o.wrap && x.op === right.op && right.args.length === 1) {
                return s2 + "(" + compile(right) + ")"
              }
            }

            return s2 + compile(right)
          })
        } else {
          return withPriority(x.binary, function () {
            var left  = unwrap(x.args[0])
              , right = unwrap(x.args[1])

            if (o.order === "right") {
              if (x.binary === left.binary) {
                throw opt.error(x, "invalid left hand side for assignment")
                                     /*
                                     compile(left) + ")" +
                                     minify(" ") + s2 + minify(" ") +
                                     compile(right)
                                     */
              }
            } else if (x.binary === right.binary) {
              if (x.binary === left.binary && left.args.length === 2) {
                return "(" + compile(left) + ")" +
                       minify(" ") + s2 + minify(" ") +
                       "(" + compile(right) + ")"
              } else {
                var temp = left
                left     = right
                right    = temp
                // TODO code duplication
                if (o.wrap && x.op === right.op && right.args.length === 1) {
                  return compileFn(left) +
                         minify(" ") + s2 + minify(" ") +
                         "(" + compile(right) + ")"
                }
              }
            }

            return compileFn(left) +
                   minify(" ") + s2 + minify(" ") +
                   compile(right)
          })
        }
      }
    })
  }

  function optimizeVar(x, a) {
    var r = []
    x.forEach(function (w) {
      var x = unwrap(w)
        , y
        , z
      if (x.op === "=" && (y = unwrap(x.args[1])).op === "function") {
        if (r.length) {
          a.push(opArray("var", r))
          r = []
        }
        z = op("function-var", x.args[0], y.args[0], y.args[1])
        z.seenVariables = y.seenVariables // TODO clone
        z.boundUniques = y.boundUniques // TODO clone
        a.push(wrap(w, z))
      } else {
        r.push(wrap(w, x))
      }
    })
    if (r.length) {
      a.push(opArray("var", r))
    }
  }

  function blockWith(w, type) {
    var old  = statements
      , old2 = expressions
    statements  = []
    expressions = []
    try {
      if (type === "statement") {
        statement(w)
      } else if (type === "expression") {
        ;(function () {
          var x = unwrap(w)
          if (x.op === ",") {
            var len = x.args.length - 1
            x.args.forEach(function (x, i) {
              if (i === len) {
                statements.push(expression(x))
              } else {
                statement(x)
              }
            })
          } else {
            var w2 = expression(w)
            x = unwrap(w2)
            if (x.op === ",") {
              x.args.forEach(function (x) {
                statements.push(x)
              })
            } else {
              statements.push(w2)
            }
          }
        })()
      }
      var a = []
        , r = []
        , seen
      statements.forEach(function (w) {
        var x = unwrap(w)
        if (x.op === "var") {
          r = r.concat(x.args)
        } else {
          var b = false
          if (r.length) {
            var last = unwrap(r[r.length - 1])
            b = (x.op === "=" &&
                 last.isVariable &&
                 last.args[0] === unwrap(x.args[0]).args[0])
            if (b) {
              r[r.length - 1] = wrap(r[r.length - 1], x)
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
            a.push(wrap(w, x))
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
        return wrap(w, a[0])
      } else {
        return wrap(w, opArray(";", a))
      }
    } finally {
      statements  = old
      expressions = old2
    }
  }

  // TODO better simple detection
  function isSimple(w) {
    var x = unwrap(w)
    return x.op === "literal" || x.op === "unique" || x.op === "variable"
  }

  function isImpure(w) {
    var x = unwrap(w)
    if (x.isImpure) {
      return true
    } else if (x.isLiteral || x.isFunction) {
      return false
    } else {
      return x.args.some(isImpure)
    }
  }

  function isStatement(w) {
    var x = unwrap(w)
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

  function withValue(x, f) {
    if (!isSimple(x)) {
      var u = unique()
      return [op("var", op("=", u, x))].concat(f(u))
    } else {
      return f(x)
    }
  }

  function assignToVar(x) {
    return op("var", x)
  }

  function destructureSlice(x, value, i, iLen, f) {
    var a = [op(".", op(".", op("array"), literal("slice")), literal("call")), value]
    var len = iLen - 1
    if (i !== len) {
      a.push(literal(i))
      a.push(literal(i - len))
    } else if (i !== 0) {
      a.push(literal(i))
    }
    return destructure(x, opArray("call", a), f)
  }

  function destructureLength(x, value, i, f) {
    return destructure(x, op(".", value,
                                  op("-", op(".", value, literal("length")),
                                          literal(i))), f)
  }

  function destructure(pattern, value, f) {
    var x = unwrap(pattern)
    if (x.destructure != null) {
      return x.destructure(x, value, f)
    } else if (x.isVariable) {
      return [f(op("=", pattern, value))]
    } else {
      throw opt.error(pattern, "cannot destructure")
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
    var args = unwrap(x.args[min])
      , r    = []
      , a    = []
      , i    = 0
      , iLen = args.args.length

    while (i < iLen) {
      (function (w) {
        var x = unwrap(w)
        if (x.isVariable) {
          a.push(expression(w))
        } else {
          var u
          if (x.op === "...") {
            if (i === iLen - 1) {
              u = variable("arguments")
            } else {
              u = unique()
              r.push(op("var", op("=", u, variable("arguments"))))
            }
            r = r.concat(destructureSlice(x.args[0], u, i, iLen, assignToVar))
            ++i
            while (i < iLen) {
              x = args.args[i]
              if (unwrap(x).op !== "empty") {
                r = r.concat(destructureLength(x, u, iLen - i, assignToVar))
              }
              ++i
            }
          } else {
            u = unique()
            if (x.op !== "empty") {
              r = r.concat(destructure(w, u, assignToVar))
            }
            a.push(expression(u))
          }
        }
      })(args.args[i])
      ++i
    }
    args.args = a

    if (r.length) {
      if (x.args.length > max) {
        r = r.concat([x.args[max]])
      }
      x.args[max] = opArray(",", r)
    }
    if (x.args.length > max) {
      x.args[max] = functionStatement(x.args[max])
    }
  }

  function expression(w) {
    var x = unwrap(w)
      , y = x.expression(x)
    if (!x.isStatement) {
      y = op("wrapper", y)
      expressions.push(y)
    }
    return wrap(w, y)
  }

  function statement(w) {
    var x = unwrap(w)
    if (x.statement) {
      x.statement(x)
    } else {
      statements.push(expression(w))
    }
  }

  function pushExpressions(x, f) {
    expressions.forEach(function (y) {
      if (/*!contains(y, x) && */f(y)) {
        var u = unique()
        statement(op("var", op("=", u, y.args[0])))
        y.args[0] = u
      }
    })
    expressions = []
  }

  function blockStatement(x) {
    return blockWith(x, "statement")
  }

  function functionStatement(x) {
    var y = unwrap(x)
    while (y.op === ",") {
      y = unwrap(y.args[y.args.length - 1])
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
      return space() + compileFn(x)
    })
  }

  /**
   *  Operators
   */
  makeOp("wrapper", {})

  makeOpHelper("!", {
    type: typer(["bool"], "bool"),
    unary: 70,
    args: 1
  })

  makeOpHelper("~", {
    type: typer(["int"], "int"),
    unary: 70,
    args: 1
  })

  makeOpHelper("typeof", {
    type: typer(["any"], "str"),
    unary: 70,
    args: 1,
    name: "typeof "
  })

  makeOpHelper("void", {
    type: typer(["any"], "void"),
    unary: 70,
    args: 1,
    name: "void "
  })

  makeOpHelper("delete", {
    type: typer(["any"], "bool"),
    unary: 70,
    args: 1,
    name: "delete ",
    isImpure: true
  })

  makeOpHelper("*", {
    type: typer(["num", "num"], "num"),
    binary: 65
  })

  makeOpHelper("/", {
    type: typer(["num", "num"], "num"),
    binary: 65
  })

  makeOpHelper("%", {
    type: typer(["num", "num"], "num"),
    binary: 65,
    args: 2
  })

  makeOpHelper("+", {
    type: typer(["strnum", "strnum"], ["|", "num", "str"]),
    binary: 60,
    unary: 70,
    wrap: true
  })

  makeOpHelper("-", {
    type: typer(["num", "num"], "num"),
    binary: 60,
    unary: 70,
    wrap: true
  })

  makeOpHelper("<<", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  makeOpHelper(">>", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  makeOpHelper(">>>", {
    type: typer(["int", "int"], "int"),
    binary: 55,
    args: 2
  })

  makeOpHelper("<", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  makeOpHelper("<=", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  makeOpHelper(">", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  makeOpHelper(">=", {
    type: typer(["strnum", "strnum"], "bool"),
    binary: 50,
    pairwise: "&&"
  })

  makeOpHelper("in", {
    type: typer(["str", "object"], "bool"),
    binary: 50,
    args: 2,
    name: " in "
  })

  makeOpHelper("instanceof", {
    type: typer(["object", "function"], "bool"),
    binary: 50,
    args: 2,
    name: " instanceof "
  })

  makeOpHelper("==", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  makeOpHelper("!=", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  makeOpHelper("===", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  makeOpHelper("!==", {
    type: typer(["any", "any"], "bool"),
    binary: 45,
    pairwise: "&&"
  })

  makeOpHelper("&", {
    type: typer(["int", "int"], "int"),
    binary: 40,
    args: 2
  })

  makeOpHelper("^", {
    type: typer(["int", "int"], "int"),
    binary: 35,
    args: 2
  })

  makeOpHelper("|", {
    type: typer(["int", "int"], "int"),
    binary: 30,
    args: 2
  })

  makeOpHelper("&&", {
    type: typer(["bool", "bool"], "bool"),
    binary: 25
  })

  makeOpHelper("||", {
    type: typer(["bool", "bool"], "bool"),
    binary: 20
  })

  makeOpHelper("=", {
    // TODO
    type: function (x) {
      x = x.args[1]
      return x.type(x)
    },
    binary: 10,
    args: 2,
    order: "right",
    isImpure: true,
    destructure: function (x, value, f) {
      return withValue(value, function (value) {
        return destructure(x.args[0],
                           op("if", op("===", value, op("void", literal(0))),
                                    x.args[1],
                                    value),
                           f)
      })
    },
    expression: function (x) {
      var a = destructure(x.args[0], x.args[1], function (x) { return x })
        , l = []
        , r = []
      a.forEach(function (x) {
        if (unwrap(x).op === "var") {
          l.push(x)
        } else {
          r.push(x)
        }
      })
      return opArray(",", l.concat(r).map(function (w) {
        var x = unwrap(w)
        if (x.op === "=") {
          x.args[1] = expression(x.args[1])
          return w
        } else {
          return expression(w)
        }
      }))
    }
  })

  makeOpHelper("*=",         { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("/=",         { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("%=",         { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("<<=",        { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper(">>=",        { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper(">>>=",       { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("&=",         { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("^=",         { binary: 10, args: 2, order: "right", isImpure: true })
  makeOpHelper("|=",         { binary: 10, args: 2, order: "right", isImpure: true })

  makeOpAssign("++", "+=")
  makeOpAssign("--", "-=")

  makeOpStatement("debugger", false)
  makeOpStatement("throw", true)
  makeOpStatement("break", true, function (x) {
    if (!scope.loop) {
      throw opt.error(x, "must be inside of a loop")
    }
  })
  makeOpStatement("continue", true, function (x) {
    if (!scope.loop) {
      throw opt.error(x, "must be inside of a loop")
    }
  })
  makeOpStatement("return", true, function (x) {
    if (!scope.function) {
      throw opt.error(x, "must be inside of a function")
    }
  })

  makeOp("import", {
    module: function (x, requires, exports) {

    },
    expression: function (x) {
      throw opt.error(x, "only allowed with nino.module")
    }
  })

  makeOp("export", {
    module: function (x, requires, exports) {

    },
    expression: function (x) {
      throw opt.error(x, "only allowed with nino.module")
    }
  })

  makeOp("...", {
    expression: function (x) {
      argumentError(x, 1, 1)
      throw opt.error(x, "invalid use of ...")
    }
  })

  makeOp("this", {
    expression: function (x) {
      argumentError(x, 0, 0)
      return x
    },
    compile: function (x) {
      return "this"
    }
  })

  makeOp("empty", {
    type: typer([], "void"),
    statement: function (x) {
      argumentError(x, 0, 0)
    },
    expression: function (x) {
      argumentError(x, 0, 0)
      return op("void", literal(0))
    }
  })

  makeOp(";", {
    // TODO
    type: function (x) {
      if (x.args.length) {
        x = x.args[x.args.length - 1]
        return type("function", [], x.type(x))
      } else {
        return type("void")
      }
    },
    compile: function (x) {
      //argumentError(x, 1)
      var r   = []
        , len = x.args.length - 1
      x.args.forEach(function (w, i) {
        var x = unwrap(w)
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
  })

  makeOp(",", {
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
          var x = unwrap(w)
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
        }).join("," + minify(" "))
      })
    }
  })

  makeOp(".", {
    destructure: function (x, value) {
      return [op("=", x, value)]
    },
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
            x = unwrap(x.args[0])
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

  makeOp("var", {
    isImpure: true,
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1)

      var first = unwrap(x.args[0])
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
        var x = unwrap(w)
        if (x.op === "=") {
          //x.args[0] = expression(x.args[0]) // TODO
          //x.args[1] = expression(x.args[1])
          r = r.concat(destructure(x.args[0], expression(x.args[1]), assignToVar))
          //return wrap(w, x) // TODO
        } else {
          r.push(op("var", expression(w)))
        }
      })
      r.forEach(function (w) {
        statements.push(w) // TODO
      })
      //x.args = r.map(assignToVar)
      //x.args = x.args.map(expression)
      //statements.push(x)
    },
    expression: function (x) {
      var seen = []

      x.args.forEach(function (w) {
        var x = unwrap(w)
        if (x.op === "=") {
          if (isImpure(x.args[1])) {
            seen.push(isImpure)
          }
          x = x.args[0]
        }
        x = unwrap(x)
        seen.push(function (w) {
          var y = unwrap(w)
          return x.op === y.op && x.args[0] === y.args[0]
        })
      })

      pushExpressions(x, function (x) {
        return seen.some(function (f) {
          return f(x)
        })
      })

      var last = unwrap(x.args[x.args.length - 1])
      if (last.op === "=") {
        last = unwrap(last.args[0])
      }

      // TODO really hacky
      while (true) {
        if (last.op === "array") {
          last = unwrap(last.args[last.args.length - 1])
          if (last.op === "...") {
            last = unwrap(last.args[0])
          }
        } else if (last.op === "object") {
          last = unwrap(last.args[last.args.length - 1])
          if (last.op === "=") {
            last = unwrap(last.args[1])
          }
        } else if (last.op === "=") {
          last = unwrap(last.args[0])
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
        var x = unwrap(w)
        if (x.op === "=") {
          if (isImpure(x.args[1]) || seen[unwrap(x.args[0]).args[0]]) {
            seen[unwrap(x.args[0]).args[0]] = true
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

      statement(opArray("var", top))
      return expression(opArray(",", bot))*/
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var s = minify(x.isInline ? " " : "\n" + space() + "    ")
        return "var " + x.args.map(compile).join("," + s)
      })
    }
  })

  makeOp("if", {
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
          var u = unique()
          x.args[1] = op("=", u, x.args[1])
          statement(op("var", u))
          statement(x)
          return expression(u)
        } else {
          return expression(op("&&", x.args[0], x.args[1]))
        }
      case 3:
        if (isStatement(x.args[1]) || isStatement(x.args[2])) {
          var u = unique()
          x.args[1] = op("=", u, x.args[1])
          x.args[2] = op("=", u, x.args[2])
          statement(op("var", u))
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
          return compileFn(x.args[0]) + minify(" ") + "?" + minify(" ") +
                 compile(x.args[1])   + minify(" ") + ":" + minify(" ") +
                 compile(x.args[2])
        })
      } else {
        x.args[1] = unwrap(x.args[1])
        var b = (x.args[1].op === ";")
          , s = []
        if (x.args.length > 2) {
          x.args[2] = unwrap(x.args[2])
          b = b || x.args[2].op === ";"
        }
        s.push("if", minify(" "), "(", compile(x.args[0]), ")", minify(" "))
        if (b) {
          //if (x.args.length === 2) {
          x.noSemicolon = true
          //}
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
          if (b) {
            //x.noSemicolon = true
            s.push("else", minify(" "), "{", minify("\n"))
            s.push(block(x.args[2]))
            s.push(minify("\n"), space(), "}")
          } else {
            s.push(space())
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
      argumentError(x, 1)
      var first = expression(x.args[0])
      return makeConcat(x.args.slice(1), function (r, seen) {
        if (seen) {
          x.args = [op(".", first, literal("apply")),
                    literal(null),
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
      argumentError(x, 1)
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
    destructure: function (x, value, f) {
      return withValue(value, function (value) {
        var r    = []
          , i    = 0
          , iLen = x.args.length
        while (i < iLen) {
          (function (w) {
            var y = unwrap(w)
            if (y.op === "...") {
              r = r.concat(destructureSlice(y.args[0], value, i, iLen, f))
              ++i
              while (i < iLen) {
                r = r.concat(destructureLength(x.args[i], value, iLen - i, f))
                ++i
              }
            } else if (y.op !== "empty") {
              r = r.concat(destructure(w, op(".", value, literal(i)), f))
            }
          })(x.args[i])
          ++i
        }
        return r
      })
    },
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
        return "[" + x.args.map(compile).join("," + minify(" ")) + "]"
      })
    }
  })

  makeOp("object", {
    destructure: function (x, value, f) {
      return withValue(value, function (value) {
        return x.args.map(function (w) {
          var x = unwrap(w)
          if (x.op === "=") {
            return destructure(x.args[1], op(".", value, propToString(x.args[0])), f)
          } else {
            return destructure(x, op(".", value, propToString(x)), f)
          }
        })
      })
    },
    expression: function (x) {
      argumentError(x, 0)
      x.args = x.args.map(function (x) {
        if (unwrap(x).op === "=") {
          x.args[0] = expression(x.args[0])
          x.args[1] = expression(x.args[1])
          return x
        } else {
          return expression(x)
        }
      })
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        var r = []
        withIndent(indent + 1, function () {
          x.args.forEach(function (w) {
            var x = unwrap(w)
            if (x.op === "=") {
              r.push(minify("\n") + space() +
                     (jsProp(x.args[0]) || compile(x.args[0])) + ":" + minify(" ") +
                     compile(x.args[1]))
            } else {
              r.push(minify("\n") + space() +
                     (jsProp(x) || compile(x)) + ":" + minify(" ") + compile(x))
            }
          })
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
      return expression(op("void", literal(0)))
    },
    compile: function (x) {
      return compileLoop(x, "while", compile(x.args[0]), x.args[1])
    }
  })

  makeOp("for", {
    isImpure: true, // TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 0, 4)
      if (x.args.length > 0) {
        var first = unwrap(x.args[0])
        if (first.op === "var") {
          x.args[0] = wrap(x.args[0], blockStatement(first))
        } else if (first.op !== "empty") {
          spliceBlock(x, 0)
          x.args[0] = expression(x.args[0])
        }
      }
      if (x.args.length > 1) {
        if (unwrap(x.args[1]).op !== "empty") {
          x.args[1] = expression(x.args[1])
        }
      }
      if (x.args.length > 2) {
        if (unwrap(x.args[2]).op !== "empty") {
          x.args[2] = expression(x.args[2])
        }
      }
      if (x.args.length > 3) {
        if (unwrap(x.args[3]).op !== "empty") {
          x.args[3] = blockStatement(x.args[3])
        }
      }
      statements.push(x)
    },
    expression: function (x) {
      pushExpressions(x, isImpure)
      statement(x)
      return expression(op("void", literal(0)))
    },
    compile: function (x) {
      var r = []
      if (x.args.length > 0) {
        var first = unwrap(x.args[0])
        if (first.op === "var") {
          first.isInline = true
        }
        if (first.op !== "empty") {
          r.push(compile(first) + ";")
        }
      }
      if (x.args.length > 1) {
        if (unwrap(x.args[1]).op !== "empty") {
          r.push(compile(x.args[1]) + ";")
        }
      }
      if (x.args.length > 2) {
        if (unwrap(x.args[2]).op !== "empty") {
          r.push(compile(x.args[2]) + ";")
        }
      }
      while (r.length < 3) {
        r.push(";")
      }
      return compileLoop(x, "for", r.join(minify(" ")), x.args[3])
    }
  })

  makeOp("for-in", {
    isImpure: true, // TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 2, 3)
      spliceBlock(x, 0)
      var first = unwrap(x.args[0])
      if (first.op !== "var" && !first.isVariable) {
        throw opt.error(x.args[0], "must be a variable or (var ...)")
      }
      if (first.op === "var") {
        if (first.args.length === 1 && unwrap(first.args[0]).isVariable) {
          x.args[0] = wrap(x.args[0], blockStatement(first))
        } else {
          throw opt.error(x.args[0], "invalid assignment")
        }
      } else {
        x.args[0] = wrap(x.args[0], expression(first))
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
      return expression(op("void", literal(0)))
    },
    compile: function (x) {
      return compileLoop(x, "for", compile(x.args[0]) + " in " + compile(x.args[1]), x.args[2])
    }
  })

  makeOp("function", {
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

  makeOp("function-var", {
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

  makeOp("try", {
    noSemicolon: true,
    //isImpure: "children", TODO
    isStatement: true,
    statement: function (x) {
      argumentError(x, 1)
      var seen = {}
      x.args = x.args.map(function (x, i) {
        if (seen[x.op]) {
          throw opt.error(x, "cannot have two of the same operator in a try block")
        }
        seen[x.op] = true
        if (x.op === "catch" && seen["finally"]) {
          throw opt.error(x, "catch must be before finally")
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

      var u = unique()

      x.args = x.args.map(function (w, i) {
        var x = unwrap(w)
        if (i === 0) {
          return wrap(w, op("=", u, x))
        } else if (x.op === "catch") {
          x.args[1] = op("=", u, x.args[1])
          return w
        } else if (x.op === "finally") {
          return w
        }
      })
      statement(op("var", u))
      statement(x)

      return expression(u)
    },
    compile: function (x) {
      return compileBlock("try", "", x.args[0]) + x.args.slice(1).map(function (x) {
               return minify(" ") + compile(x)
             }).join("")
    }
  })

  makeOp("catch", {
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
      return compileBlock("catch", "(" + compile(x.args[0]) + ")" + minify(" "), x.args[1])
    }
  })

  makeOp("finally", {
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

  /*makeOp("switch", {
    noSemicolon: true,
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      var u = unique()

      x.args = x.args.map(function (x, i) {
        if (i !== 0) {
          if (x.op === "case") {
            x.args[1] = op("=", u, x.args[1])
          } else if (x.op === "default") {
            x.args[0] = op("=", u, x.args[0])
          }
        }
        return expression(x)
      })
      statement(op("var", u))
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

  return {
    opArray: opArray,
    op: op,
    fromJSON: fromJSON,
    parse: parse,

    bypass: bypass,
    lineComment: lineComment,
    blockComment: blockComment,
    docComment: docComment,
    variable: variable,
    unique: unique,
    literal: literal,

    fromAST: fromAST,
    toAST: toAST,

    opt: opt,
    builtins: builtins,
    print: print,

    expression: expressionTop,
    statement: statementTop,
    traverse: traverse,
    replace: replace,
    compile: compileTop,
  }
})
