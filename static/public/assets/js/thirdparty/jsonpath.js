/**
 * Bundled by jsDelivr using Rollup v2.79.1 and Terser v5.19.2.
 * Original file: /npm/jsonpath@1.1.1/jsonpath.js
 *
 * Do NOT use SRI with dynamically generated files! More information: https://www.jsdelivr.com/using-sri-with-dynamic-files
 */
var e = 'undefined' != typeof globalThis ? globalThis : 'undefined' != typeof window ? window : 'undefined' != typeof global ? global : 'undefined' != typeof self ? self : {}
function t(e) {
  throw new Error(
    'Could not dynamically require "' + e + '". Please configure the dynamicRequireTargets or/and ignoreDynamicRequires option of @rollup/plugin-commonjs appropriately for this require call to work.'
  )
}
var n = { exports: {} },
  r = (n.exports = (function e(n, r, i) {
    function a(s, c) {
      if (!r[s]) {
        if (!n[s]) {
          if (!c && t) return t(s)
          if (o) return o(s, !0)
          var u = new Error("Cannot find module '" + s + "'")
          throw ((u.code = 'MODULE_NOT_FOUND'), u)
        }
        var l = (r[s] = { exports: {} })
        n[s][0].call(
          l.exports,
          function (e) {
            var t = n[s][1][e]
            return a(t || e)
          },
          l,
          l.exports,
          e,
          n,
          r,
          i
        )
      }
      return r[s].exports
    }
    for (var o = t, s = 0; s < i.length; s++) a(i[s])
    return a
  })(
    {
      './aesprim': [
        function (e, t, n) {
          var r, i
          ;(r = this),
            (i = function (e) {
              var t, n, r, i, a, o, s, c, u, l, p, f, h, d, y, m, g, v
              function S(e, t) {
                if (!e) throw new Error('ASSERT: ' + t)
              }
              function E(e) {
                return e >= 48 && e <= 57
              }
              function b(e) {
                return '0123456789abcdefABCDEF'.indexOf(e) >= 0
              }
              function x(e) {
                return '01234567'.indexOf(e) >= 0
              }
              function k(e) {
                return (
                  32 === e ||
                  9 === e ||
                  11 === e ||
                  12 === e ||
                  160 === e ||
                  (e >= 5760 && [5760, 6158, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288, 65279].indexOf(e) >= 0)
                )
              }
              function I(e) {
                return 10 === e || 13 === e || 8232 === e || 8233 === e
              }
              function _(e) {
                return 64 == e || 36 === e || 95 === e || (e >= 65 && e <= 90) || (e >= 97 && e <= 122) || 92 === e || (e >= 128 && s.NonAsciiIdentifierStart.test(String.fromCharCode(e)))
              }
              function w(e) {
                return 36 === e || 95 === e || (e >= 65 && e <= 90) || (e >= 97 && e <= 122) || (e >= 48 && e <= 57) || 92 === e || (e >= 128 && s.NonAsciiIdentifierPart.test(String.fromCharCode(e)))
              }
              function C(e) {
                switch (e) {
                  case 'class':
                  case 'enum':
                  case 'export':
                  case 'extends':
                  case 'import':
                  case 'super':
                    return !0
                  default:
                    return !1
                }
              }
              function O(e) {
                switch (e) {
                  case 'implements':
                  case 'interface':
                  case 'package':
                  case 'private':
                  case 'protected':
                  case 'public':
                  case 'static':
                  case 'yield':
                  case 'let':
                    return !0
                  default:
                    return !1
                }
              }
              function N(e) {
                return 'eval' === e || 'arguments' === e
              }
              function T(e) {
                if (l && O(e)) return !0
                switch (e.length) {
                  case 2:
                    return 'if' === e || 'in' === e || 'do' === e
                  case 3:
                    return 'var' === e || 'for' === e || 'new' === e || 'try' === e || 'let' === e
                  case 4:
                    return 'this' === e || 'else' === e || 'case' === e || 'void' === e || 'with' === e || 'enum' === e
                  case 5:
                    return 'while' === e || 'break' === e || 'catch' === e || 'throw' === e || 'const' === e || 'yield' === e || 'class' === e || 'super' === e
                  case 6:
                    return 'return' === e || 'typeof' === e || 'delete' === e || 'switch' === e || 'export' === e || 'import' === e
                  case 7:
                    return 'default' === e || 'finally' === e || 'extends' === e
                  case 8:
                    return 'function' === e || 'continue' === e || 'debugger' === e
                  case 10:
                    return 'instanceof' === e
                  default:
                    return !1
                }
              }
              function A(e, t, n, r, i) {
                var a
                S('number' == typeof n, 'Comment must have valid position'),
                  g.lastCommentStart >= n ||
                    ((g.lastCommentStart = n),
                    (a = { type: e, value: t }),
                    v.range && (a.range = [n, r]),
                    v.loc && (a.loc = i),
                    v.comments.push(a),
                    v.attachComment && (v.leadingComments.push(a), v.trailingComments.push(a)))
              }
              function L(e) {
                var t, n, r, i
                for (t = p - e, n = { start: { line: f, column: p - h - e } }; p < d; )
                  if (((r = u.charCodeAt(p)), ++p, I(r)))
                    return (
                      v.comments && ((i = u.slice(t + e, p - 1)), (n.end = { line: f, column: p - h - 1 }), A('Line', i, t, p - 1, n)), 13 === r && 10 === u.charCodeAt(p) && ++p, ++f, void (h = p)
                    )
                v.comments && ((i = u.slice(t + e, p)), (n.end = { line: f, column: p - h }), A('Line', i, t, p, n))
              }
              function P() {
                var e, t, n, r
                for (v.comments && ((e = p - 2), (t = { start: { line: f, column: p - h - 2 } })); p < d; )
                  if (I((n = u.charCodeAt(p)))) 13 === n && 10 === u.charCodeAt(p + 1) && ++p, ++f, ++p, (h = p), p >= d && ie({}, o.UnexpectedToken, 'ILLEGAL')
                  else if (42 === n) {
                    if (47 === u.charCodeAt(p + 1)) return ++p, ++p, void (v.comments && ((r = u.slice(e + 2, p - 2)), (t.end = { line: f, column: p - h }), A('Block', r, e, p, t)))
                    ++p
                  } else ++p
                ie({}, o.UnexpectedToken, 'ILLEGAL')
              }
              function R() {
                var e, t
                for (t = 0 === p; p < d; )
                  if (k((e = u.charCodeAt(p)))) ++p
                  else if (I(e)) ++p, 13 === e && 10 === u.charCodeAt(p) && ++p, ++f, (h = p), (t = !0)
                  else if (47 === e)
                    if (47 === (e = u.charCodeAt(p + 1))) ++p, ++p, L(2), (t = !0)
                    else {
                      if (42 !== e) break
                      ++p, ++p, P()
                    }
                  else if (t && 45 === e) {
                    if (45 !== u.charCodeAt(p + 1) || 62 !== u.charCodeAt(p + 2)) break
                    ;(p += 3), L(3)
                  } else {
                    if (60 !== e) break
                    if ('!--' !== u.slice(p + 1, p + 4)) break
                    ++p, ++p, ++p, ++p, L(4)
                  }
              }
              function D(e) {
                var t,
                  n,
                  r,
                  i = 0
                for (n = 'u' === e ? 4 : 2, t = 0; t < n; ++t) {
                  if (!(p < d && b(u[p]))) return ''
                  ;(r = u[p++]), (i = 16 * i + '0123456789abcdef'.indexOf(r.toLowerCase()))
                }
                return String.fromCharCode(i)
              }
              function j() {
                var e, t
                for (
                  e = u.charCodeAt(p++),
                    t = String.fromCharCode(e),
                    92 === e &&
                      (117 !== u.charCodeAt(p) && ie({}, o.UnexpectedToken, 'ILLEGAL'), ++p, ((e = D('u')) && '\\' !== e && _(e.charCodeAt(0))) || ie({}, o.UnexpectedToken, 'ILLEGAL'), (t = e));
                  p < d && w((e = u.charCodeAt(p)));

                )
                  ++p,
                    (t += String.fromCharCode(e)),
                    92 === e &&
                      ((t = t.substr(0, t.length - 1)),
                      117 !== u.charCodeAt(p) && ie({}, o.UnexpectedToken, 'ILLEGAL'),
                      ++p,
                      ((e = D('u')) && '\\' !== e && w(e.charCodeAt(0))) || ie({}, o.UnexpectedToken, 'ILLEGAL'),
                      (t += e))
                return t
              }
              function $() {
                var e, t
                for (e = p++; p < d; ) {
                  if (92 === (t = u.charCodeAt(p))) return (p = e), j()
                  if (!w(t)) break
                  ++p
                }
                return u.slice(e, p)
              }
              function U() {
                var e, n
                return (
                  (e = p),
                  {
                    type:
                      1 === (n = 92 === u.charCodeAt(p) ? j() : $()).length
                        ? t.Identifier
                        : T(n)
                        ? t.Keyword
                        : 'null' === n
                        ? t.NullLiteral
                        : 'true' === n || 'false' === n
                        ? t.BooleanLiteral
                        : t.Identifier,
                    value: n,
                    lineNumber: f,
                    lineStart: h,
                    start: e,
                    end: p,
                  }
                )
              }
              function M() {
                var e,
                  n,
                  r,
                  i,
                  a = p,
                  s = u.charCodeAt(p),
                  c = u[p]
                switch (s) {
                  case 46:
                  case 40:
                  case 41:
                  case 59:
                  case 44:
                  case 123:
                  case 125:
                  case 91:
                  case 93:
                  case 58:
                  case 63:
                  case 126:
                    return (
                      ++p,
                      v.tokenize && (40 === s ? (v.openParenToken = v.tokens.length) : 123 === s && (v.openCurlyToken = v.tokens.length)),
                      { type: t.Punctuator, value: String.fromCharCode(s), lineNumber: f, lineStart: h, start: a, end: p }
                    )
                  default:
                    if (61 === (e = u.charCodeAt(p + 1)))
                      switch (s) {
                        case 43:
                        case 45:
                        case 47:
                        case 60:
                        case 62:
                        case 94:
                        case 124:
                        case 37:
                        case 38:
                        case 42:
                          return (p += 2), { type: t.Punctuator, value: String.fromCharCode(s) + String.fromCharCode(e), lineNumber: f, lineStart: h, start: a, end: p }
                        case 33:
                        case 61:
                          return (p += 2), 61 === u.charCodeAt(p) && ++p, { type: t.Punctuator, value: u.slice(a, p), lineNumber: f, lineStart: h, start: a, end: p }
                      }
                }
                return '>>>=' === (i = u.substr(p, 4))
                  ? ((p += 4), { type: t.Punctuator, value: i, lineNumber: f, lineStart: h, start: a, end: p })
                  : '>>>' === (r = i.substr(0, 3)) || '<<=' === r || '>>=' === r
                  ? ((p += 3), { type: t.Punctuator, value: r, lineNumber: f, lineStart: h, start: a, end: p })
                  : (c === (n = r.substr(0, 2))[1] && '+-<>&|'.indexOf(c) >= 0) || '=>' === n
                  ? ((p += 2), { type: t.Punctuator, value: n, lineNumber: f, lineStart: h, start: a, end: p })
                  : '<>=!+-*%&|^/'.indexOf(c) >= 0
                  ? (++p, { type: t.Punctuator, value: c, lineNumber: f, lineStart: h, start: a, end: p })
                  : void ie({}, o.UnexpectedToken, 'ILLEGAL')
              }
              function B(e) {
                for (var n = ''; p < d && b(u[p]); ) n += u[p++]
                return (
                  0 === n.length && ie({}, o.UnexpectedToken, 'ILLEGAL'),
                  _(u.charCodeAt(p)) && ie({}, o.UnexpectedToken, 'ILLEGAL'),
                  { type: t.NumericLiteral, value: parseInt('0x' + n, 16), lineNumber: f, lineStart: h, start: e, end: p }
                )
              }
              function F(e) {
                for (var n = '0' + u[p++]; p < d && x(u[p]); ) n += u[p++]
                return (
                  (_(u.charCodeAt(p)) || E(u.charCodeAt(p))) && ie({}, o.UnexpectedToken, 'ILLEGAL'),
                  { type: t.NumericLiteral, value: parseInt(n, 8), octal: !0, lineNumber: f, lineStart: h, start: e, end: p }
                )
              }
              function z() {
                var e, n, r
                if ((S(E((r = u[p]).charCodeAt(0)) || '.' === r, 'Numeric literal must start with a decimal digit or a decimal point'), (n = p), (e = ''), '.' !== r)) {
                  if (((e = u[p++]), (r = u[p]), '0' === e)) {
                    if ('x' === r || 'X' === r) return ++p, B(n)
                    if (x(r)) return F(n)
                    r && E(r.charCodeAt(0)) && ie({}, o.UnexpectedToken, 'ILLEGAL')
                  }
                  for (; E(u.charCodeAt(p)); ) e += u[p++]
                  r = u[p]
                }
                if ('.' === r) {
                  for (e += u[p++]; E(u.charCodeAt(p)); ) e += u[p++]
                  r = u[p]
                }
                if ('e' === r || 'E' === r)
                  if (((e += u[p++]), ('+' !== (r = u[p]) && '-' !== r) || (e += u[p++]), E(u.charCodeAt(p)))) for (; E(u.charCodeAt(p)); ) e += u[p++]
                  else ie({}, o.UnexpectedToken, 'ILLEGAL')
                return _(u.charCodeAt(p)) && ie({}, o.UnexpectedToken, 'ILLEGAL'), { type: t.NumericLiteral, value: parseFloat(e), lineNumber: f, lineStart: h, start: n, end: p }
              }
              function q() {
                var e,
                  n,
                  r,
                  i,
                  a,
                  s,
                  c,
                  l,
                  y = '',
                  m = !1
                for (c = f, l = h, S("'" === (e = u[p]) || '"' === e, 'String literal must starts with a quote'), n = p, ++p; p < d; ) {
                  if ((r = u[p++]) === e) {
                    e = ''
                    break
                  }
                  if ('\\' === r)
                    if ((r = u[p++]) && I(r.charCodeAt(0))) ++f, '\r' === r && '\n' === u[p] && ++p, (h = p)
                    else
                      switch (r) {
                        case 'u':
                        case 'x':
                          ;(s = p), (a = D(r)) ? (y += a) : ((p = s), (y += r))
                          break
                        case 'n':
                          y += '\n'
                          break
                        case 'r':
                          y += '\r'
                          break
                        case 't':
                          y += '\t'
                          break
                        case 'b':
                          y += '\b'
                          break
                        case 'f':
                          y += '\f'
                          break
                        case 'v':
                          y += '\v'
                          break
                        default:
                          x(r)
                            ? (0 !== (i = '01234567'.indexOf(r)) && (m = !0),
                              p < d && x(u[p]) && ((m = !0), (i = 8 * i + '01234567'.indexOf(u[p++])), '0123'.indexOf(r) >= 0 && p < d && x(u[p]) && (i = 8 * i + '01234567'.indexOf(u[p++]))),
                              (y += String.fromCharCode(i)))
                            : (y += r)
                      }
                  else {
                    if (I(r.charCodeAt(0))) break
                    y += r
                  }
                }
                return (
                  '' !== e && ie({}, o.UnexpectedToken, 'ILLEGAL'), { type: t.StringLiteral, value: y, octal: m, startLineNumber: c, startLineStart: l, lineNumber: f, lineStart: h, start: n, end: p }
                )
              }
              function G(e, t) {
                var n
                try {
                  n = new RegExp(e, t)
                } catch (e) {
                  ie({}, o.InvalidRegExp)
                }
                return n
              }
              function H() {
                var e, t, n, r
                for (S('/' === (e = u[p]), 'Regular expression literal must start with a slash'), t = u[p++], n = !1, r = !1; p < d; )
                  if (((t += e = u[p++]), '\\' === e)) I((e = u[p++]).charCodeAt(0)) && ie({}, o.UnterminatedRegExp), (t += e)
                  else if (I(e.charCodeAt(0))) ie({}, o.UnterminatedRegExp)
                  else if (n) ']' === e && (n = !1)
                  else {
                    if ('/' === e) {
                      r = !0
                      break
                    }
                    '[' === e && (n = !0)
                  }
                return r || ie({}, o.UnterminatedRegExp), { value: t.substr(1, t.length - 2), literal: t }
              }
              function X() {
                var e, t, n, r
                for (t = '', n = ''; p < d && w((e = u[p]).charCodeAt(0)); )
                  if ((++p, '\\' === e && p < d))
                    if ('u' === (e = u[p])) {
                      if (((r = ++p), (e = D('u')))) for (n += e, t += '\\u'; r < p; ++r) t += u[r]
                      else (p = r), (n += 'u'), (t += '\\u')
                      ae({}, o.UnexpectedToken, 'ILLEGAL')
                    } else (t += '\\'), ae({}, o.UnexpectedToken, 'ILLEGAL')
                  else (n += e), (t += e)
                return { value: n, literal: t }
              }
              function W() {
                var e, n, r, i
                return (
                  (m = null),
                  R(),
                  (e = p),
                  (n = H()),
                  (r = X()),
                  (i = G(n.value, r.value)),
                  v.tokenize ? { type: t.RegularExpression, value: i, lineNumber: f, lineStart: h, start: e, end: p } : { literal: n.literal + r.literal, value: i, start: e, end: p }
                )
              }
              function K() {
                var e, t, n, r
                return (
                  R(),
                  (e = p),
                  (t = { start: { line: f, column: p - h } }),
                  (n = W()),
                  (t.end = { line: f, column: p - h }),
                  v.tokenize ||
                    (v.tokens.length > 0 && (r = v.tokens[v.tokens.length - 1]).range[0] === e && 'Punctuator' === r.type && (('/' !== r.value && '/=' !== r.value) || v.tokens.pop()),
                    v.tokens.push({ type: 'RegularExpression', value: n.literal, range: [e, p], loc: t })),
                  n
                )
              }
              function V(e) {
                return e.type === t.Identifier || e.type === t.Keyword || e.type === t.BooleanLiteral || e.type === t.NullLiteral
              }
              function J() {
                var e, t
                if (!(e = v.tokens[v.tokens.length - 1])) return K()
                if ('Punctuator' === e.type) {
                  if (']' === e.value) return M()
                  if (')' === e.value)
                    return !(t = v.tokens[v.openParenToken - 1]) || 'Keyword' !== t.type || ('if' !== t.value && 'while' !== t.value && 'for' !== t.value && 'with' !== t.value) ? M() : K()
                  if ('}' === e.value) {
                    if (v.tokens[v.openCurlyToken - 3] && 'Keyword' === v.tokens[v.openCurlyToken - 3].type) {
                      if (!(t = v.tokens[v.openCurlyToken - 4])) return M()
                    } else {
                      if (!v.tokens[v.openCurlyToken - 4] || 'Keyword' !== v.tokens[v.openCurlyToken - 4].type) return M()
                      if (!(t = v.tokens[v.openCurlyToken - 5])) return K()
                    }
                    return r.indexOf(t.value) >= 0 ? M() : K()
                  }
                  return K()
                }
                return 'Keyword' === e.type ? K() : M()
              }
              function Q() {
                var e
                return (
                  R(),
                  p >= d
                    ? { type: t.EOF, lineNumber: f, lineStart: h, start: p, end: p }
                    : _((e = u.charCodeAt(p)))
                    ? U()
                    : 40 === e || 41 === e || 59 === e
                    ? M()
                    : 39 === e || 34 === e
                    ? q()
                    : 46 === e
                    ? E(u.charCodeAt(p + 1))
                      ? z()
                      : M()
                    : E(e)
                    ? z()
                    : v.tokenize && 47 === e
                    ? J()
                    : M()
                )
              }
              function Z() {
                var e, r, i
                return (
                  R(),
                  (e = { start: { line: f, column: p - h } }),
                  (r = Q()),
                  (e.end = { line: f, column: p - h }),
                  r.type !== t.EOF && ((i = u.slice(r.start, r.end)), v.tokens.push({ type: n[r.type], value: i, range: [r.start, r.end], loc: e })),
                  r
                )
              }
              function Y() {
                var e
                return (p = (e = m).end), (f = e.lineNumber), (h = e.lineStart), (m = void 0 !== v.tokens ? Z() : Q()), (p = e.end), (f = e.lineNumber), (h = e.lineStart), e
              }
              function ee() {
                var e, t, n
                ;(e = p), (t = f), (n = h), (m = void 0 !== v.tokens ? Z() : Q()), (p = e), (f = t), (h = n)
              }
              function te(e, t) {
                ;(this.line = e), (this.column = t)
              }
              function ne(e, t, n, r) {
                ;(this.start = new te(e, t)), (this.end = new te(n, r))
              }
              function re() {
                var e, t, n, r
                return (e = p), (t = f), (n = h), R(), (r = f !== t), (p = e), (f = t), (h = n), r
              }
              function ie(e, t) {
                var n,
                  r = Array.prototype.slice.call(arguments, 2),
                  i = t.replace(/%(\d)/g, function (e, t) {
                    return S(t < r.length, 'Message reference must be in range'), r[t]
                  })
                throw (
                  ('number' == typeof e.lineNumber
                    ? (((n = new Error('Line ' + e.lineNumber + ': ' + i)).index = e.start), (n.lineNumber = e.lineNumber), (n.column = e.start - h + 1))
                    : (((n = new Error('Line ' + f + ': ' + i)).index = p), (n.lineNumber = f), (n.column = p - h + 1)),
                  (n.description = i),
                  n)
                )
              }
              function ae() {
                try {
                  ie.apply(null, arguments)
                } catch (e) {
                  if (!v.errors) throw e
                  v.errors.push(e)
                }
              }
              function oe(e) {
                if (
                  (e.type === t.EOF && ie(e, o.UnexpectedEOS),
                  e.type === t.NumericLiteral && ie(e, o.UnexpectedNumber),
                  e.type === t.StringLiteral && ie(e, o.UnexpectedString),
                  e.type === t.Identifier && ie(e, o.UnexpectedIdentifier),
                  e.type === t.Keyword)
                ) {
                  if (C(e.value)) ie(e, o.UnexpectedReserved)
                  else if (l && O(e.value)) return void ae(e, o.StrictReservedWord)
                  ie(e, o.UnexpectedToken, e.value)
                }
                ie(e, o.UnexpectedToken, e.value)
              }
              function se(e) {
                var n = Y()
                ;(n.type === t.Punctuator && n.value === e) || oe(n)
              }
              function ce(e) {
                var n = Y()
                ;(n.type === t.Keyword && n.value === e) || oe(n)
              }
              function ue(e) {
                return m.type === t.Punctuator && m.value === e
              }
              function le(e) {
                return m.type === t.Keyword && m.value === e
              }
              function pe() {
                var e
                return (
                  m.type === t.Punctuator &&
                  ('=' === (e = m.value) || '*=' === e || '/=' === e || '%=' === e || '+=' === e || '-=' === e || '<<=' === e || '>>=' === e || '>>>=' === e || '&=' === e || '^=' === e || '|=' === e)
                )
              }
              function fe() {
                var e
                59 === u.charCodeAt(p) || ue(';') ? Y() : ((e = f), R(), f === e && (m.type === t.EOF || ue('}') || oe(m)))
              }
              function he(e) {
                return e.type === i.Identifier || e.type === i.MemberExpression
              }
              function de() {
                var e,
                  t = []
                for (e = m, se('['); !ue(']'); ) ue(',') ? (Y(), t.push(null)) : (t.push(Pe()), ue(']') || se(','))
                return Y(), y.markEnd(y.createArrayExpression(t), e)
              }
              function ye(e, t) {
                var n, r, i
                return (n = l), (i = m), (r = ot()), t && l && N(e[0].name) && ae(t, o.StrictParamName), (l = n), y.markEnd(y.createFunctionExpression(null, e, [], r), i)
              }
              function me() {
                var e, n
                return (
                  (n = m),
                  (e = Y()).type === t.StringLiteral || e.type === t.NumericLiteral
                    ? (l && e.octal && ae(e, o.StrictOctalLiteral), y.markEnd(y.createLiteral(e), n))
                    : y.markEnd(y.createIdentifier(e.value), n)
                )
              }
              function ge() {
                var e, n, r, i, a, s
                return (
                  (s = m),
                  (e = m).type === t.Identifier
                    ? ((r = me()),
                      'get' !== e.value || ue(':')
                        ? 'set' !== e.value || ue(':')
                          ? (se(':'), (i = Pe()), y.markEnd(y.createProperty('init', r, i), s))
                          : ((n = me()),
                            se('('),
                            (e = m).type !== t.Identifier ? (se(')'), ae(e, o.UnexpectedToken, e.value), (i = ye([]))) : ((a = [$e()]), se(')'), (i = ye(a, e))),
                            y.markEnd(y.createProperty('set', n, i), s))
                        : ((n = me()), se('('), se(')'), (i = ye([])), y.markEnd(y.createProperty('get', n, i), s)))
                    : e.type !== t.EOF && e.type !== t.Punctuator
                    ? ((n = me()), se(':'), (i = Pe()), y.markEnd(y.createProperty('init', n, i), s))
                    : void oe(e)
                )
              }
              function ve() {
                var e,
                  t,
                  n,
                  r,
                  s,
                  c = [],
                  u = {},
                  p = String
                for (s = m, se('{'); !ue('}'); )
                  (t = (e = ge()).key.type === i.Identifier ? e.key.name : p(e.key.value)),
                    (r = 'init' === e.kind ? a.Data : 'get' === e.kind ? a.Get : a.Set),
                    (n = '$' + t),
                    Object.prototype.hasOwnProperty.call(u, n)
                      ? (u[n] === a.Data
                          ? l && r === a.Data
                            ? ae({}, o.StrictDuplicateProperty)
                            : r !== a.Data && ae({}, o.AccessorDataProperty)
                          : r === a.Data
                          ? ae({}, o.AccessorDataProperty)
                          : u[n] & r && ae({}, o.AccessorGetSet),
                        (u[n] |= r))
                      : (u[n] = r),
                    c.push(e),
                    ue('}') || se(',')
                return se('}'), y.markEnd(y.createObjectExpression(c), s)
              }
              function Se() {
                var e
                return se('('), (e = Re()), se(')'), e
              }
              function Ee() {
                var e, n, r, i
                if (ue('(')) return Se()
                if (ue('[')) return de()
                if (ue('{')) return ve()
                if (((e = m.type), (i = m), e === t.Identifier)) r = y.createIdentifier(Y().value)
                else if (e === t.StringLiteral || e === t.NumericLiteral) l && m.octal && ae(m, o.StrictOctalLiteral), (r = y.createLiteral(Y()))
                else if (e === t.Keyword) {
                  if (le('function')) return ut()
                  le('this') ? (Y(), (r = y.createThisExpression())) : oe(Y())
                } else
                  e === t.BooleanLiteral
                    ? (((n = Y()).value = 'true' === n.value), (r = y.createLiteral(n)))
                    : e === t.NullLiteral
                    ? (((n = Y()).value = null), (r = y.createLiteral(n)))
                    : ue('/') || ue('/=')
                    ? ((r = void 0 !== v.tokens ? y.createLiteral(K()) : y.createLiteral(W())), ee())
                    : oe(Y())
                return y.markEnd(r, i)
              }
              function be() {
                var e = []
                if ((se('('), !ue(')'))) for (; p < d && (e.push(Pe()), !ue(')')); ) se(',')
                return se(')'), e
              }
              function xe() {
                var e, t
                return (t = m), V((e = Y())) || oe(e), y.markEnd(y.createIdentifier(e.value), t)
              }
              function ke() {
                return se('.'), xe()
              }
              function Ie() {
                var e
                return se('['), (e = Re()), se(']'), e
              }
              function _e() {
                var e, t, n
                return (n = m), ce('new'), (e = Ce()), (t = ue('(') ? be() : []), y.markEnd(y.createNewExpression(e, t), n)
              }
              function we() {
                var e, t, n, r, i
                for (i = m, e = g.allowIn, g.allowIn = !0, t = le('new') ? _e() : Ee(), g.allowIn = e; ; ) {
                  if (ue('.')) (r = ke()), (t = y.createMemberExpression('.', t, r))
                  else if (ue('(')) (n = be()), (t = y.createCallExpression(t, n))
                  else {
                    if (!ue('[')) break
                    ;(r = Ie()), (t = y.createMemberExpression('[', t, r))
                  }
                  y.markEnd(t, i)
                }
                return t
              }
              function Ce() {
                var e, t, n, r
                for (r = m, e = g.allowIn, t = le('new') ? _e() : Ee(), g.allowIn = e; ue('.') || ue('['); )
                  ue('[') ? ((n = Ie()), (t = y.createMemberExpression('[', t, n))) : ((n = ke()), (t = y.createMemberExpression('.', t, n))), y.markEnd(t, r)
                return t
              }
              function Oe() {
                var e,
                  n,
                  r = m
                return (
                  (e = we()),
                  m.type === t.Punctuator &&
                    ((!ue('++') && !ue('--')) ||
                      re() ||
                      (l && e.type === i.Identifier && N(e.name) && ae({}, o.StrictLHSPostfix),
                      he(e) || ae({}, o.InvalidLHSInAssignment),
                      (n = Y()),
                      (e = y.markEnd(y.createPostfixExpression(n.value, e), r)))),
                  e
                )
              }
              function Ne() {
                var e, n, r
                return (
                  m.type !== t.Punctuator && m.type !== t.Keyword
                    ? (n = Oe())
                    : ue('++') || ue('--')
                    ? ((r = m),
                      (e = Y()),
                      (n = Ne()),
                      l && n.type === i.Identifier && N(n.name) && ae({}, o.StrictLHSPrefix),
                      he(n) || ae({}, o.InvalidLHSInAssignment),
                      (n = y.createUnaryExpression(e.value, n)),
                      (n = y.markEnd(n, r)))
                    : ue('+') || ue('-') || ue('~') || ue('!')
                    ? ((r = m), (e = Y()), (n = Ne()), (n = y.createUnaryExpression(e.value, n)), (n = y.markEnd(n, r)))
                    : le('delete') || le('void') || le('typeof')
                    ? ((r = m),
                      (e = Y()),
                      (n = Ne()),
                      (n = y.createUnaryExpression(e.value, n)),
                      (n = y.markEnd(n, r)),
                      l && 'delete' === n.operator && n.argument.type === i.Identifier && ae({}, o.StrictDelete))
                    : (n = Oe()),
                  n
                )
              }
              function Te(e, n) {
                var r = 0
                if (e.type !== t.Punctuator && e.type !== t.Keyword) return 0
                switch (e.value) {
                  case '||':
                    r = 1
                    break
                  case '&&':
                    r = 2
                    break
                  case '|':
                    r = 3
                    break
                  case '^':
                    r = 4
                    break
                  case '&':
                    r = 5
                    break
                  case '==':
                  case '!=':
                  case '===':
                  case '!==':
                    r = 6
                    break
                  case '<':
                  case '>':
                  case '<=':
                  case '>=':
                  case 'instanceof':
                    r = 7
                    break
                  case 'in':
                    r = n ? 7 : 0
                    break
                  case '<<':
                  case '>>':
                  case '>>>':
                    r = 8
                    break
                  case '+':
                  case '-':
                    r = 9
                    break
                  case '*':
                  case '/':
                  case '%':
                    r = 11
                }
                return r
              }
              function Ae() {
                var e, t, n, r, i, a, o, s, c, u
                if (((e = m), (c = Ne()), 0 === (i = Te((r = m), g.allowIn)))) return c
                for (r.prec = i, Y(), t = [e, m], a = [c, r, (o = Ne())]; (i = Te(m, g.allowIn)) > 0; ) {
                  for (; a.length > 2 && i <= a[a.length - 2].prec; )
                    (o = a.pop()), (s = a.pop().value), (c = a.pop()), (n = y.createBinaryExpression(s, c, o)), t.pop(), (e = t[t.length - 1]), y.markEnd(n, e), a.push(n)
                  ;((r = Y()).prec = i), a.push(r), t.push(m), (n = Ne()), a.push(n)
                }
                for (n = a[(u = a.length - 1)], t.pop(); u > 1; ) (n = y.createBinaryExpression(a[u - 1].value, a[u - 2], n)), (u -= 2), (e = t.pop()), y.markEnd(n, e)
                return n
              }
              function Le() {
                var e, t, n, r, i
                return (
                  (i = m),
                  (e = Ae()),
                  ue('?') && (Y(), (t = g.allowIn), (g.allowIn = !0), (n = Pe()), (g.allowIn = t), se(':'), (r = Pe()), (e = y.createConditionalExpression(e, n, r)), y.markEnd(e, i)),
                  e
                )
              }
              function Pe() {
                var e, t, n, r, a
                return (
                  (e = m),
                  (a = m),
                  (r = t = Le()),
                  pe() &&
                    (he(t) || ae({}, o.InvalidLHSInAssignment),
                    l && t.type === i.Identifier && N(t.name) && ae(e, o.StrictLHSAssignment),
                    (e = Y()),
                    (n = Pe()),
                    (r = y.markEnd(y.createAssignmentExpression(e.value, t, n), a))),
                  r
                )
              }
              function Re() {
                var e,
                  t = m
                if (((e = Pe()), ue(','))) {
                  for (e = y.createSequenceExpression([e]); p < d && ue(','); ) Y(), e.expressions.push(Pe())
                  y.markEnd(e, t)
                }
                return e
              }
              function De() {
                for (var e, t = []; p < d && !ue('}') && void 0 !== (e = lt()); ) t.push(e)
                return t
              }
              function je() {
                var e, t
                return (t = m), se('{'), (e = De()), se('}'), y.markEnd(y.createBlockStatement(e), t)
              }
              function $e() {
                var e, n
                return (n = m), (e = Y()).type !== t.Identifier && oe(e), y.markEnd(y.createIdentifier(e.value), n)
              }
              function Ue(e) {
                var t,
                  n,
                  r = null
                return (
                  (n = m), (t = $e()), l && N(t.name) && ae({}, o.StrictVarName), 'const' === e ? (se('='), (r = Pe())) : ue('=') && (Y(), (r = Pe())), y.markEnd(y.createVariableDeclarator(t, r), n)
                )
              }
              function Me(e) {
                var t = []
                do {
                  if ((t.push(Ue(e)), !ue(','))) break
                  Y()
                } while (p < d)
                return t
              }
              function Be() {
                var e
                return ce('var'), (e = Me()), fe(), y.createVariableDeclaration(e, 'var')
              }
              function Fe(e) {
                var t, n
                return (n = m), ce(e), (t = Me(e)), fe(), y.markEnd(y.createVariableDeclaration(t, e), n)
              }
              function ze() {
                return se(';'), y.createEmptyStatement()
              }
              function qe() {
                var e = Re()
                return fe(), y.createExpressionStatement(e)
              }
              function Ge() {
                var e, t, n
                return ce('if'), se('('), (e = Re()), se(')'), (t = at()), le('else') ? (Y(), (n = at())) : (n = null), y.createIfStatement(e, t, n)
              }
              function He() {
                var e, t, n
                return ce('do'), (n = g.inIteration), (g.inIteration = !0), (e = at()), (g.inIteration = n), ce('while'), se('('), (t = Re()), se(')'), ue(';') && Y(), y.createDoWhileStatement(e, t)
              }
              function Xe() {
                var e, t, n
                return ce('while'), se('('), (e = Re()), se(')'), (n = g.inIteration), (g.inIteration = !0), (t = at()), (g.inIteration = n), y.createWhileStatement(e, t)
              }
              function We() {
                var e, t, n
                return (n = m), (e = Y()), (t = Me()), y.markEnd(y.createVariableDeclaration(t, e.value), n)
              }
              function Ke() {
                var e, t, n, r, i, a, s
                return (
                  (e = t = n = null),
                  ce('for'),
                  se('('),
                  ue(';')
                    ? Y()
                    : (le('var') || le('let')
                        ? ((g.allowIn = !1), (e = We()), (g.allowIn = !0), 1 === e.declarations.length && le('in') && (Y(), (r = e), (i = Re()), (e = null)))
                        : ((g.allowIn = !1), (e = Re()), (g.allowIn = !0), le('in') && (he(e) || ae({}, o.InvalidLHSInForIn), Y(), (r = e), (i = Re()), (e = null))),
                      void 0 === r && se(';')),
                  void 0 === r && (ue(';') || (t = Re()), se(';'), ue(')') || (n = Re())),
                  se(')'),
                  (s = g.inIteration),
                  (g.inIteration = !0),
                  (a = at()),
                  (g.inIteration = s),
                  void 0 === r ? y.createForStatement(e, t, n, a) : y.createForInStatement(r, i, a)
                )
              }
              function Ve() {
                var e,
                  n = null
                return (
                  ce('continue'),
                  59 === u.charCodeAt(p)
                    ? (Y(), g.inIteration || ie({}, o.IllegalContinue), y.createContinueStatement(null))
                    : re()
                    ? (g.inIteration || ie({}, o.IllegalContinue), y.createContinueStatement(null))
                    : (m.type === t.Identifier && ((e = '$' + (n = $e()).name), Object.prototype.hasOwnProperty.call(g.labelSet, e) || ie({}, o.UnknownLabel, n.name)),
                      fe(),
                      null !== n || g.inIteration || ie({}, o.IllegalContinue),
                      y.createContinueStatement(n))
                )
              }
              function Je() {
                var e,
                  n = null
                return (
                  ce('break'),
                  59 === u.charCodeAt(p)
                    ? (Y(), g.inIteration || g.inSwitch || ie({}, o.IllegalBreak), y.createBreakStatement(null))
                    : re()
                    ? (g.inIteration || g.inSwitch || ie({}, o.IllegalBreak), y.createBreakStatement(null))
                    : (m.type === t.Identifier && ((e = '$' + (n = $e()).name), Object.prototype.hasOwnProperty.call(g.labelSet, e) || ie({}, o.UnknownLabel, n.name)),
                      fe(),
                      null !== n || g.inIteration || g.inSwitch || ie({}, o.IllegalBreak),
                      y.createBreakStatement(n))
                )
              }
              function Qe() {
                var e = null
                return (
                  ce('return'),
                  g.inFunctionBody || ae({}, o.IllegalReturn),
                  32 === u.charCodeAt(p) && _(u.charCodeAt(p + 1))
                    ? ((e = Re()), fe(), y.createReturnStatement(e))
                    : re()
                    ? y.createReturnStatement(null)
                    : (ue(';') || ue('}') || m.type === t.EOF || (e = Re()), fe(), y.createReturnStatement(e))
                )
              }
              function Ze() {
                var e, t
                return l && (R(), ae({}, o.StrictModeWith)), ce('with'), se('('), (e = Re()), se(')'), (t = at()), y.createWithStatement(e, t)
              }
              function Ye() {
                var e,
                  t,
                  n,
                  r = []
                for (n = m, le('default') ? (Y(), (e = null)) : (ce('case'), (e = Re())), se(':'); p < d && !(ue('}') || le('default') || le('case')); ) (t = at()), r.push(t)
                return y.markEnd(y.createSwitchCase(e, r), n)
              }
              function et() {
                var e, t, n, r, i
                if ((ce('switch'), se('('), (e = Re()), se(')'), se('{'), (t = []), ue('}'))) return Y(), y.createSwitchStatement(e, t)
                for (r = g.inSwitch, g.inSwitch = !0, i = !1; p < d && !ue('}'); ) null === (n = Ye()).test && (i && ie({}, o.MultipleDefaultsInSwitch), (i = !0)), t.push(n)
                return (g.inSwitch = r), se('}'), y.createSwitchStatement(e, t)
              }
              function tt() {
                var e
                return ce('throw'), re() && ie({}, o.NewlineAfterThrow), (e = Re()), fe(), y.createThrowStatement(e)
              }
              function nt() {
                var e, t, n
                return (n = m), ce('catch'), se('('), ue(')') && oe(m), (e = $e()), l && N(e.name) && ae({}, o.StrictCatchVariable), se(')'), (t = je()), y.markEnd(y.createCatchClause(e, t), n)
              }
              function rt() {
                var e,
                  t = [],
                  n = null
                return ce('try'), (e = je()), le('catch') && t.push(nt()), le('finally') && (Y(), (n = je())), 0 !== t.length || n || ie({}, o.NoCatchOrFinally), y.createTryStatement(e, [], t, n)
              }
              function it() {
                return ce('debugger'), fe(), y.createDebuggerStatement()
              }
              function at() {
                var e,
                  n,
                  r,
                  a,
                  s = m.type
                if ((s === t.EOF && oe(m), s === t.Punctuator && '{' === m.value)) return je()
                if (((a = m), s === t.Punctuator))
                  switch (m.value) {
                    case ';':
                      return y.markEnd(ze(), a)
                    case '(':
                      return y.markEnd(qe(), a)
                  }
                if (s === t.Keyword)
                  switch (m.value) {
                    case 'break':
                      return y.markEnd(Je(), a)
                    case 'continue':
                      return y.markEnd(Ve(), a)
                    case 'debugger':
                      return y.markEnd(it(), a)
                    case 'do':
                      return y.markEnd(He(), a)
                    case 'for':
                      return y.markEnd(Ke(), a)
                    case 'function':
                      return y.markEnd(ct(), a)
                    case 'if':
                      return y.markEnd(Ge(), a)
                    case 'return':
                      return y.markEnd(Qe(), a)
                    case 'switch':
                      return y.markEnd(et(), a)
                    case 'throw':
                      return y.markEnd(tt(), a)
                    case 'try':
                      return y.markEnd(rt(), a)
                    case 'var':
                      return y.markEnd(Be(), a)
                    case 'while':
                      return y.markEnd(Xe(), a)
                    case 'with':
                      return y.markEnd(Ze(), a)
                  }
                return (e = Re()).type === i.Identifier && ue(':')
                  ? (Y(),
                    (r = '$' + e.name),
                    Object.prototype.hasOwnProperty.call(g.labelSet, r) && ie({}, o.Redeclaration, 'Label', e.name),
                    (g.labelSet[r] = !0),
                    (n = at()),
                    delete g.labelSet[r],
                    y.markEnd(y.createLabeledStatement(e, n), a))
                  : (fe(), y.markEnd(y.createExpressionStatement(e), a))
              }
              function ot() {
                var e,
                  n,
                  r,
                  a,
                  s,
                  c,
                  f,
                  h,
                  v = []
                for (h = m, se('{'); p < d && m.type === t.StringLiteral && ((n = m), (e = lt()), v.push(e), e.expression.type === i.Literal); )
                  'use strict' === u.slice(n.start + 1, n.end - 1) ? ((l = !0), r && ae(r, o.StrictOctalLiteral)) : !r && n.octal && (r = n)
                for (
                  a = g.labelSet, s = g.inIteration, c = g.inSwitch, f = g.inFunctionBody, g.labelSet = {}, g.inIteration = !1, g.inSwitch = !1, g.inFunctionBody = !0;
                  p < d && !ue('}') && void 0 !== (e = lt());

                )
                  v.push(e)
                return se('}'), (g.labelSet = a), (g.inIteration = s), (g.inSwitch = c), (g.inFunctionBody = f), y.markEnd(y.createBlockStatement(v), h)
              }
              function st(e) {
                var t,
                  n,
                  r,
                  i,
                  a,
                  s,
                  c = []
                if ((se('('), !ue(')')))
                  for (
                    i = {};
                    p < d &&
                    ((n = m),
                    (t = $e()),
                    (a = '$' + n.value),
                    l
                      ? (N(n.value) && ((r = n), (s = o.StrictParamName)), Object.prototype.hasOwnProperty.call(i, a) && ((r = n), (s = o.StrictParamDupe)))
                      : e ||
                        (N(n.value)
                          ? ((e = n), (s = o.StrictParamName))
                          : O(n.value)
                          ? ((e = n), (s = o.StrictReservedWord))
                          : Object.prototype.hasOwnProperty.call(i, a) && ((e = n), (s = o.StrictParamDupe))),
                    c.push(t),
                    (i[a] = !0),
                    !ue(')'));

                  )
                    se(',')
                return se(')'), { params: c, stricted: r, firstRestricted: e, message: s }
              }
              function ct() {
                var e,
                  t,
                  n,
                  r,
                  i,
                  a,
                  s,
                  c,
                  u,
                  p = []
                return (
                  (u = m),
                  ce('function'),
                  (n = m),
                  (e = $e()),
                  l ? N(n.value) && ae(n, o.StrictFunctionName) : N(n.value) ? ((a = n), (s = o.StrictFunctionName)) : O(n.value) && ((a = n), (s = o.StrictReservedWord)),
                  (p = (i = st(a)).params),
                  (r = i.stricted),
                  (a = i.firstRestricted),
                  i.message && (s = i.message),
                  (c = l),
                  (t = ot()),
                  l && a && ie(a, s),
                  l && r && ae(r, s),
                  (l = c),
                  y.markEnd(y.createFunctionDeclaration(e, p, [], t), u)
                )
              }
              function ut() {
                var e,
                  t,
                  n,
                  r,
                  i,
                  a,
                  s,
                  c,
                  u = null,
                  p = []
                return (
                  (c = m),
                  ce('function'),
                  ue('(') ||
                    ((e = m), (u = $e()), l ? N(e.value) && ae(e, o.StrictFunctionName) : N(e.value) ? ((n = e), (r = o.StrictFunctionName)) : O(e.value) && ((n = e), (r = o.StrictReservedWord))),
                  (p = (i = st(n)).params),
                  (t = i.stricted),
                  (n = i.firstRestricted),
                  i.message && (r = i.message),
                  (s = l),
                  (a = ot()),
                  l && n && ie(n, r),
                  l && t && ae(t, r),
                  (l = s),
                  y.markEnd(y.createFunctionExpression(u, p, [], a), c)
                )
              }
              function lt() {
                if (m.type === t.Keyword)
                  switch (m.value) {
                    case 'const':
                    case 'let':
                      return Fe(m.value)
                    case 'function':
                      return ct()
                    default:
                      return at()
                  }
                if (m.type !== t.EOF) return at()
              }
              function pt() {
                for (var e, n, r, a = []; p < d && (n = m).type === t.StringLiteral && ((e = lt()), a.push(e), e.expression.type === i.Literal); )
                  'use strict' === u.slice(n.start + 1, n.end - 1) ? ((l = !0), r && ae(r, o.StrictOctalLiteral)) : !r && n.octal && (r = n)
                for (; p < d && void 0 !== (e = lt()); ) a.push(e)
                return a
              }
              function ft() {
                var e, t
                return R(), ee(), (t = m), (l = !1), (e = pt()), y.markEnd(y.createProgram(e), t)
              }
              function ht() {
                var e,
                  t,
                  n,
                  r = []
                for (e = 0; e < v.tokens.length; ++e) (n = { type: (t = v.tokens[e]).type, value: t.value }), v.range && (n.range = t.range), v.loc && (n.loc = t.loc), r.push(n)
                v.tokens = r
              }
              function dt(e, n) {
                var r
                'string' == typeof e || e instanceof String || (e = String(e)),
                  (y = c),
                  (p = 0),
                  (f = (u = e).length > 0 ? 1 : 0),
                  (h = 0),
                  (d = u.length),
                  (m = null),
                  (g = { allowIn: !0, labelSet: {}, inFunctionBody: !1, inIteration: !1, inSwitch: !1, lastCommentStart: -1 }),
                  (v = {}),
                  ((n = n || {}).tokens = !0),
                  (v.tokens = []),
                  (v.tokenize = !0),
                  (v.openParenToken = -1),
                  (v.openCurlyToken = -1),
                  (v.range = 'boolean' == typeof n.range && n.range),
                  (v.loc = 'boolean' == typeof n.loc && n.loc),
                  'boolean' == typeof n.comment && n.comment && (v.comments = []),
                  'boolean' == typeof n.tolerant && n.tolerant && (v.errors = [])
                try {
                  if ((ee(), m.type === t.EOF)) return v.tokens
                  for (Y(); m.type !== t.EOF; )
                    try {
                      Y()
                    } catch (e) {
                      if (v.errors) {
                        v.errors.push(e)
                        break
                      }
                      throw e
                    }
                  ht(), (r = v.tokens), void 0 !== v.comments && (r.comments = v.comments), void 0 !== v.errors && (r.errors = v.errors)
                } catch (e) {
                  throw e
                } finally {
                  v = {}
                }
                return r
              }
              function yt(e, t) {
                var n, r
                ;(r = String),
                  'string' == typeof e || e instanceof String || (e = r(e)),
                  (y = c),
                  (p = 0),
                  (f = (u = e).length > 0 ? 1 : 0),
                  (h = 0),
                  (d = u.length),
                  (m = null),
                  (g = { allowIn: !0, labelSet: {}, inFunctionBody: !1, inIteration: !1, inSwitch: !1, lastCommentStart: -1 }),
                  (v = {}),
                  void 0 !== t &&
                    ((v.range = 'boolean' == typeof t.range && t.range),
                    (v.loc = 'boolean' == typeof t.loc && t.loc),
                    (v.attachComment = 'boolean' == typeof t.attachComment && t.attachComment),
                    v.loc && null !== t.source && void 0 !== t.source && (v.source = r(t.source)),
                    'boolean' == typeof t.tokens && t.tokens && (v.tokens = []),
                    'boolean' == typeof t.comment && t.comment && (v.comments = []),
                    'boolean' == typeof t.tolerant && t.tolerant && (v.errors = []),
                    v.attachComment && ((v.range = !0), (v.comments = []), (v.bottomRightStack = []), (v.trailingComments = []), (v.leadingComments = [])))
                try {
                  ;(n = ft()), void 0 !== v.comments && (n.comments = v.comments), void 0 !== v.tokens && (ht(), (n.tokens = v.tokens)), void 0 !== v.errors && (n.errors = v.errors)
                } catch (e) {
                  throw e
                } finally {
                  v = {}
                }
                return n
              }
              ;((n = {})[(t = { BooleanLiteral: 1, EOF: 2, Identifier: 3, Keyword: 4, NullLiteral: 5, NumericLiteral: 6, Punctuator: 7, StringLiteral: 8, RegularExpression: 9 }).BooleanLiteral] =
                'Boolean'),
                (n[t.EOF] = '<end>'),
                (n[t.Identifier] = 'Identifier'),
                (n[t.Keyword] = 'Keyword'),
                (n[t.NullLiteral] = 'Null'),
                (n[t.NumericLiteral] = 'Numeric'),
                (n[t.Punctuator] = 'Punctuator'),
                (n[t.StringLiteral] = 'String'),
                (n[t.RegularExpression] = 'RegularExpression'),
                (r = [
                  '(',
                  '{',
                  '[',
                  'in',
                  'typeof',
                  'instanceof',
                  'new',
                  'return',
                  'case',
                  'delete',
                  'throw',
                  'void',
                  '=',
                  '+=',
                  '-=',
                  '*=',
                  '/=',
                  '%=',
                  '<<=',
                  '>>=',
                  '>>>=',
                  '&=',
                  '|=',
                  '^=',
                  ',',
                  '+',
                  '-',
                  '*',
                  '/',
                  '%',
                  '++',
                  '--',
                  '<<',
                  '>>',
                  '>>>',
                  '&',
                  '|',
                  '^',
                  '!',
                  '~',
                  '&&',
                  '||',
                  '?',
                  ':',
                  '===',
                  '==',
                  '>=',
                  '<=',
                  '<',
                  '>',
                  '!=',
                  '!==',
                ]),
                (i = {
                  AssignmentExpression: 'AssignmentExpression',
                  ArrayExpression: 'ArrayExpression',
                  BlockStatement: 'BlockStatement',
                  BinaryExpression: 'BinaryExpression',
                  BreakStatement: 'BreakStatement',
                  CallExpression: 'CallExpression',
                  CatchClause: 'CatchClause',
                  ConditionalExpression: 'ConditionalExpression',
                  ContinueStatement: 'ContinueStatement',
                  DoWhileStatement: 'DoWhileStatement',
                  DebuggerStatement: 'DebuggerStatement',
                  EmptyStatement: 'EmptyStatement',
                  ExpressionStatement: 'ExpressionStatement',
                  ForStatement: 'ForStatement',
                  ForInStatement: 'ForInStatement',
                  FunctionDeclaration: 'FunctionDeclaration',
                  FunctionExpression: 'FunctionExpression',
                  Identifier: 'Identifier',
                  IfStatement: 'IfStatement',
                  Literal: 'Literal',
                  LabeledStatement: 'LabeledStatement',
                  LogicalExpression: 'LogicalExpression',
                  MemberExpression: 'MemberExpression',
                  NewExpression: 'NewExpression',
                  ObjectExpression: 'ObjectExpression',
                  Program: 'Program',
                  Property: 'Property',
                  ReturnStatement: 'ReturnStatement',
                  SequenceExpression: 'SequenceExpression',
                  SwitchStatement: 'SwitchStatement',
                  SwitchCase: 'SwitchCase',
                  ThisExpression: 'ThisExpression',
                  ThrowStatement: 'ThrowStatement',
                  TryStatement: 'TryStatement',
                  UnaryExpression: 'UnaryExpression',
                  UpdateExpression: 'UpdateExpression',
                  VariableDeclaration: 'VariableDeclaration',
                  VariableDeclarator: 'VariableDeclarator',
                  WhileStatement: 'WhileStatement',
                  WithStatement: 'WithStatement',
                }),
                (a = { Data: 1, Get: 2, Set: 4 }),
                (o = {
                  UnexpectedToken: 'Unexpected token %0',
                  UnexpectedNumber: 'Unexpected number',
                  UnexpectedString: 'Unexpected string',
                  UnexpectedIdentifier: 'Unexpected identifier',
                  UnexpectedReserved: 'Unexpected reserved word',
                  UnexpectedEOS: 'Unexpected end of input',
                  NewlineAfterThrow: 'Illegal newline after throw',
                  InvalidRegExp: 'Invalid regular expression',
                  UnterminatedRegExp: 'Invalid regular expression: missing /',
                  InvalidLHSInAssignment: 'Invalid left-hand side in assignment',
                  InvalidLHSInForIn: 'Invalid left-hand side in for-in',
                  MultipleDefaultsInSwitch: 'More than one default clause in switch statement',
                  NoCatchOrFinally: 'Missing catch or finally after try',
                  UnknownLabel: "Undefined label '%0'",
                  Redeclaration: "%0 '%1' has already been declared",
                  IllegalContinue: 'Illegal continue statement',
                  IllegalBreak: 'Illegal break statement',
                  IllegalReturn: 'Illegal return statement',
                  StrictModeWith: 'Strict mode code may not include a with statement',
                  StrictCatchVariable: 'Catch variable may not be eval or arguments in strict mode',
                  StrictVarName: 'Variable name may not be eval or arguments in strict mode',
                  StrictParamName: 'Parameter name eval or arguments is not allowed in strict mode',
                  StrictParamDupe: 'Strict mode function may not have duplicate parameter names',
                  StrictFunctionName: 'Function name may not be eval or arguments in strict mode',
                  StrictOctalLiteral: 'Octal literals are not allowed in strict mode.',
                  StrictDelete: 'Delete of an unqualified identifier in strict mode.',
                  StrictDuplicateProperty: 'Duplicate data property in object literal not allowed in strict mode',
                  AccessorDataProperty: 'Object literal may not have data and accessor property with the same name',
                  AccessorGetSet: 'Object literal may not have multiple get/set accessors with the same name',
                  StrictLHSAssignment: 'Assignment to eval or arguments is not allowed in strict mode',
                  StrictLHSPostfix: 'Postfix increment/decrement may not have eval or arguments operand in strict mode',
                  StrictLHSPrefix: 'Prefix increment/decrement may not have eval or arguments operand in strict mode',
                  StrictReservedWord: 'Use of future reserved word in strict mode',
                }),
                (s = {
                  NonAsciiIdentifierStart: new RegExp(
                    '[ªµºÀ-ÖØ-öø-ˁˆ-ˑˠ-ˤˬˮͰ-ʹͶͷͺ-ͽΆΈ-ΊΌΎ-ΡΣ-ϵϷ-ҁҊ-ԧԱ-Ֆՙա-ևא-תװ-ײؠ-يٮٯٱ-ۓەۥۦۮۯۺ-ۼۿܐܒ-ܯݍ-ޥޱߊ-ߪߴߵߺࠀ-ࠕࠚࠤࠨࡀ-ࡘࢠࢢ-ࢬऄ-हऽॐक़-ॡॱ-ॷॹ-ॿঅ-ঌএঐও-নপ-রলশ-হঽৎড়ঢ়য়-ৡৰৱਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹਖ਼-ੜਫ਼ੲ-ੴઅ-ઍએ-ઑઓ-નપ-રલળવ-હઽૐૠૡଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହଽଡ଼ଢ଼ୟ-ୡୱஃஅ-ஊஎ-ஐஒ-கஙசஜஞடணதந-பம-ஹௐఅ-ఌఎ-ఐఒ-నప-ళవ-హఽౘౙౠౡಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹಽೞೠೡೱೲഅ-ഌഎ-ഐഒ-ഺഽൎൠൡൺ-ൿඅ-ඖක-නඳ-රලව-ෆก-ะาำเ-ๆກຂຄງຈຊຍດ-ທນ-ຟມ-ຣລວສຫອ-ະາຳຽເ-ໄໆໜ-ໟༀཀ-ཇཉ-ཬྈ-ྌက-ဪဿၐ-ၕၚ-ၝၡၥၦၮ-ၰၵ-ႁႎႠ-ჅჇჍა-ჺჼ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚᎀ-ᎏᎠ-Ᏼᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᛮ-ᛰᜀ-ᜌᜎ-ᜑᜠ-ᜱᝀ-ᝑᝠ-ᝬᝮ-ᝰក-ឳៗៜᠠ-ᡷᢀ-ᢨᢪᢰ-ᣵᤀ-ᤜᥐ-ᥭᥰ-ᥴᦀ-ᦫᧁ-ᧇᨀ-ᨖᨠ-ᩔᪧᬅ-ᬳᭅ-ᭋᮃ-ᮠᮮᮯᮺ-ᯥᰀ-ᰣᱍ-ᱏᱚ-ᱽᳩ-ᳬᳮ-ᳱᳵᳶᴀ-ᶿḀ-ἕἘ-Ἕἠ-ὅὈ-Ὅὐ-ὗὙὛὝὟ-ώᾀ-ᾴᾶ-ᾼιῂ-ῄῆ-ῌῐ-ΐῖ-Ίῠ-Ῥῲ-ῴῶ-ῼⁱⁿₐ-ₜℂℇℊ-ℓℕℙ-ℝℤΩℨK-ℭℯ-ℹℼ-ℿⅅ-ⅉⅎⅠ-ↈⰀ-Ⱞⰰ-ⱞⱠ-ⳤⳫ-ⳮⳲⳳⴀ-ⴥⴧⴭⴰ-ⵧⵯⶀ-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞⸯ々-〇〡-〩〱-〵〸-〼ぁ-ゖゝ-ゟァ-ヺー-ヿㄅ-ㄭㄱ-ㆎㆠ-ㆺㇰ-ㇿ㐀-䶵一-鿌ꀀ-ꒌꓐ-ꓽꔀ-ꘌꘐ-ꘟꘪꘫꙀ-ꙮꙿ-ꚗꚠ-ꛯꜗ-ꜟꜢ-ꞈꞋ-ꞎꞐ-ꞓꞠ-Ɦꟸ-ꠁꠃ-ꠅꠇ-ꠊꠌ-ꠢꡀ-ꡳꢂ-ꢳꣲ-ꣷꣻꤊ-ꤥꤰ-ꥆꥠ-ꥼꦄ-ꦲꧏꨀ-ꨨꩀ-ꩂꩄ-ꩋꩠ-ꩶꩺꪀ-ꪯꪱꪵꪶꪹ-ꪽꫀꫂꫛ-ꫝꫠ-ꫪꫲ-ꫴꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꯀ-ꯢ가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆﬓ-ﬗיִײַ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷻﹰ-ﹴﹶ-ﻼＡ-Ｚａ-ｚｦ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ]'
                  ),
                  NonAsciiIdentifierPart: new RegExp(
                    '[ªµºÀ-ÖØ-öø-ˁˆ-ˑˠ-ˤˬˮ̀-ʹͶͷͺ-ͽΆΈ-ΊΌΎ-ΡΣ-ϵϷ-ҁ҃-҇Ҋ-ԧԱ-Ֆՙա-և֑-ׇֽֿׁׂׅׄא-תװ-ײؐ-ؚؠ-٩ٮ-ۓە-ۜ۟-۪ۨ-ۼۿܐ-݊ݍ-ޱ߀-ߵߺࠀ-࠭ࡀ-࡛ࢠࢢ-ࢬࣤ-ࣾऀ-ॣ०-९ॱ-ॷॹ-ॿঁ-ঃঅ-ঌএঐও-নপ-রলশ-হ়-ৄেৈো-ৎৗড়ঢ়য়-ৣ০-ৱਁ-ਃਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹ਼ਾ-ੂੇੈੋ-੍ੑਖ਼-ੜਫ਼੦-ੵઁ-ઃઅ-ઍએ-ઑઓ-નપ-રલળવ-હ઼-ૅે-ૉો-્ૐૠ-ૣ૦-૯ଁ-ଃଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହ଼-ୄେୈୋ-୍ୖୗଡ଼ଢ଼ୟ-ୣ୦-୯ୱஂஃஅ-ஊஎ-ஐஒ-கஙசஜஞடணதந-பம-ஹா-ூெ-ைொ-்ௐௗ௦-௯ఁ-ఃఅ-ఌఎ-ఐఒ-నప-ళవ-హఽ-ౄె-ైొ-్ౕౖౘౙౠ-ౣ౦-౯ಂಃಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹ಼-ೄೆ-ೈೊ-್ೕೖೞೠ-ೣ೦-೯ೱೲംഃഅ-ഌഎ-ഐഒ-ഺഽ-ൄെ-ൈൊ-ൎൗൠ-ൣ൦-൯ൺ-ൿංඃඅ-ඖක-නඳ-රලව-ෆ්ා-ුූෘ-ෟෲෳก-ฺเ-๎๐-๙ກຂຄງຈຊຍດ-ທນ-ຟມ-ຣລວສຫອ-ູົ-ຽເ-ໄໆ່-ໍ໐-໙ໜ-ໟༀ༘༙༠-༩༹༵༷༾-ཇཉ-ཬཱ-྄྆-ྗྙ-ྼ࿆က-၉ၐ-ႝႠ-ჅჇჍა-ჺჼ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚ፝-፟ᎀ-ᎏᎠ-Ᏼᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᛮ-ᛰᜀ-ᜌᜎ-᜔ᜠ-᜴ᝀ-ᝓᝠ-ᝬᝮ-ᝰᝲᝳក-៓ៗៜ៝០-៩᠋-᠍᠐-᠙ᠠ-ᡷᢀ-ᢪᢰ-ᣵᤀ-ᤜᤠ-ᤫᤰ-᤻᥆-ᥭᥰ-ᥴᦀ-ᦫᦰ-ᧉ᧐-᧙ᨀ-ᨛᨠ-ᩞ᩠-᩿᩼-᪉᪐-᪙ᪧᬀ-ᭋ᭐-᭙᭫-᭳ᮀ-᯳ᰀ-᰷᱀-᱉ᱍ-ᱽ᳐-᳔᳒-ᳶᴀ-ᷦ᷼-ἕἘ-Ἕἠ-ὅὈ-Ὅὐ-ὗὙὛὝὟ-ώᾀ-ᾴᾶ-ᾼιῂ-ῄῆ-ῌῐ-ΐῖ-Ίῠ-Ῥῲ-ῴῶ-ῼ‌‍‿⁀⁔ⁱⁿₐ-ₜ⃐-⃥⃜⃡-⃰ℂℇℊ-ℓℕℙ-ℝℤΩℨK-ℭℯ-ℹℼ-ℿⅅ-ⅉⅎⅠ-ↈⰀ-Ⱞⰰ-ⱞⱠ-ⳤⳫ-ⳳⴀ-ⴥⴧⴭⴰ-ⵧⵯ⵿-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞⷠ-ⷿⸯ々-〇〡-〯〱-〵〸-〼ぁ-ゖ゙゚ゝ-ゟァ-ヺー-ヿㄅ-ㄭㄱ-ㆎㆠ-ㆺㇰ-ㇿ㐀-䶵一-鿌ꀀ-ꒌꓐ-ꓽꔀ-ꘌꘐ-ꘫꙀ-꙯ꙴ-꙽ꙿ-ꚗꚟ-꛱ꜗ-ꜟꜢ-ꞈꞋ-ꞎꞐ-ꞓꞠ-Ɦꟸ-ꠧꡀ-ꡳꢀ-꣄꣐-꣙꣠-ꣷꣻ꤀-꤭ꤰ-꥓ꥠ-ꥼꦀ-꧀ꧏ-꧙ꨀ-ꨶꩀ-ꩍ꩐-꩙ꩠ-ꩶꩺꩻꪀ-ꫂꫛ-ꫝꫠ-ꫯꫲ-꫶ꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꯀ-ꯪ꯬꯭꯰-꯹가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆﬓ-ﬗיִ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷻ︀-️︠-︦︳︴﹍-﹏ﹰ-ﹴﹶ-ﻼ０-９Ａ-Ｚ＿ａ-ｚｦ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ]'
                  ),
                }),
                (c = {
                  name: 'SyntaxTree',
                  processComment: function (e) {
                    var t, n
                    if (!(e.type === i.Program && e.body.length > 0)) {
                      for (
                        v.trailingComments.length > 0
                          ? v.trailingComments[0].range[0] >= e.range[1]
                            ? ((n = v.trailingComments), (v.trailingComments = []))
                            : (v.trailingComments.length = 0)
                          : v.bottomRightStack.length > 0 &&
                            v.bottomRightStack[v.bottomRightStack.length - 1].trailingComments &&
                            v.bottomRightStack[v.bottomRightStack.length - 1].trailingComments[0].range[0] >= e.range[1] &&
                            ((n = v.bottomRightStack[v.bottomRightStack.length - 1].trailingComments), delete v.bottomRightStack[v.bottomRightStack.length - 1].trailingComments);
                        v.bottomRightStack.length > 0 && v.bottomRightStack[v.bottomRightStack.length - 1].range[0] >= e.range[0];

                      )
                        t = v.bottomRightStack.pop()
                      t
                        ? t.leadingComments && t.leadingComments[t.leadingComments.length - 1].range[1] <= e.range[0] && ((e.leadingComments = t.leadingComments), delete t.leadingComments)
                        : v.leadingComments.length > 0 && v.leadingComments[v.leadingComments.length - 1].range[1] <= e.range[0] && ((e.leadingComments = v.leadingComments), (v.leadingComments = [])),
                        n && (e.trailingComments = n),
                        v.bottomRightStack.push(e)
                    }
                  },
                  markEnd: function (e, t) {
                    return (
                      v.range && (e.range = [t.start, p]),
                      v.loc &&
                        ((e.loc = new ne(void 0 === t.startLineNumber ? t.lineNumber : t.startLineNumber, t.start - (void 0 === t.startLineStart ? t.lineStart : t.startLineStart), f, p - h)),
                        this.postProcess(e)),
                      v.attachComment && this.processComment(e),
                      e
                    )
                  },
                  postProcess: function (e) {
                    return v.source && (e.loc.source = v.source), e
                  },
                  createArrayExpression: function (e) {
                    return { type: i.ArrayExpression, elements: e }
                  },
                  createAssignmentExpression: function (e, t, n) {
                    return { type: i.AssignmentExpression, operator: e, left: t, right: n }
                  },
                  createBinaryExpression: function (e, t, n) {
                    return { type: '||' === e || '&&' === e ? i.LogicalExpression : i.BinaryExpression, operator: e, left: t, right: n }
                  },
                  createBlockStatement: function (e) {
                    return { type: i.BlockStatement, body: e }
                  },
                  createBreakStatement: function (e) {
                    return { type: i.BreakStatement, label: e }
                  },
                  createCallExpression: function (e, t) {
                    return { type: i.CallExpression, callee: e, arguments: t }
                  },
                  createCatchClause: function (e, t) {
                    return { type: i.CatchClause, param: e, body: t }
                  },
                  createConditionalExpression: function (e, t, n) {
                    return { type: i.ConditionalExpression, test: e, consequent: t, alternate: n }
                  },
                  createContinueStatement: function (e) {
                    return { type: i.ContinueStatement, label: e }
                  },
                  createDebuggerStatement: function () {
                    return { type: i.DebuggerStatement }
                  },
                  createDoWhileStatement: function (e, t) {
                    return { type: i.DoWhileStatement, body: e, test: t }
                  },
                  createEmptyStatement: function () {
                    return { type: i.EmptyStatement }
                  },
                  createExpressionStatement: function (e) {
                    return { type: i.ExpressionStatement, expression: e }
                  },
                  createForStatement: function (e, t, n, r) {
                    return { type: i.ForStatement, init: e, test: t, update: n, body: r }
                  },
                  createForInStatement: function (e, t, n) {
                    return { type: i.ForInStatement, left: e, right: t, body: n, each: !1 }
                  },
                  createFunctionDeclaration: function (e, t, n, r) {
                    return { type: i.FunctionDeclaration, id: e, params: t, defaults: n, body: r, rest: null, generator: !1, expression: !1 }
                  },
                  createFunctionExpression: function (e, t, n, r) {
                    return { type: i.FunctionExpression, id: e, params: t, defaults: n, body: r, rest: null, generator: !1, expression: !1 }
                  },
                  createIdentifier: function (e) {
                    return { type: i.Identifier, name: e }
                  },
                  createIfStatement: function (e, t, n) {
                    return { type: i.IfStatement, test: e, consequent: t, alternate: n }
                  },
                  createLabeledStatement: function (e, t) {
                    return { type: i.LabeledStatement, label: e, body: t }
                  },
                  createLiteral: function (e) {
                    return { type: i.Literal, value: e.value, raw: u.slice(e.start, e.end) }
                  },
                  createMemberExpression: function (e, t, n) {
                    return { type: i.MemberExpression, computed: '[' === e, object: t, property: n }
                  },
                  createNewExpression: function (e, t) {
                    return { type: i.NewExpression, callee: e, arguments: t }
                  },
                  createObjectExpression: function (e) {
                    return { type: i.ObjectExpression, properties: e }
                  },
                  createPostfixExpression: function (e, t) {
                    return { type: i.UpdateExpression, operator: e, argument: t, prefix: !1 }
                  },
                  createProgram: function (e) {
                    return { type: i.Program, body: e }
                  },
                  createProperty: function (e, t, n) {
                    return { type: i.Property, key: t, value: n, kind: e }
                  },
                  createReturnStatement: function (e) {
                    return { type: i.ReturnStatement, argument: e }
                  },
                  createSequenceExpression: function (e) {
                    return { type: i.SequenceExpression, expressions: e }
                  },
                  createSwitchCase: function (e, t) {
                    return { type: i.SwitchCase, test: e, consequent: t }
                  },
                  createSwitchStatement: function (e, t) {
                    return { type: i.SwitchStatement, discriminant: e, cases: t }
                  },
                  createThisExpression: function () {
                    return { type: i.ThisExpression }
                  },
                  createThrowStatement: function (e) {
                    return { type: i.ThrowStatement, argument: e }
                  },
                  createTryStatement: function (e, t, n, r) {
                    return { type: i.TryStatement, block: e, guardedHandlers: t, handlers: n, finalizer: r }
                  },
                  createUnaryExpression: function (e, t) {
                    return '++' === e || '--' === e ? { type: i.UpdateExpression, operator: e, argument: t, prefix: !0 } : { type: i.UnaryExpression, operator: e, argument: t, prefix: !0 }
                  },
                  createVariableDeclaration: function (e, t) {
                    return { type: i.VariableDeclaration, declarations: e, kind: t }
                  },
                  createVariableDeclarator: function (e, t) {
                    return { type: i.VariableDeclarator, id: e, init: t }
                  },
                  createWhileStatement: function (e, t) {
                    return { type: i.WhileStatement, test: e, body: t }
                  },
                  createWithStatement: function (e, t) {
                    return { type: i.WithStatement, object: e, body: t }
                  },
                }),
                (e.version = '1.2.2'),
                (e.tokenize = dt),
                (e.parse = yt),
                (e.Syntax = (function () {
                  var e,
                    t = {}
                  for (e in ('function' == typeof Object.create && (t = Object.create(null)), i)) i.hasOwnProperty(e) && (t[e] = i[e])
                  return 'function' == typeof Object.freeze && Object.freeze(t), t
                })())
            }),
            i(void 0 !== n ? n : (r.esprima = {}))
        },
        {},
      ],
      1: [
        function (e, t, n) {
          ;(function (r) {
            var i = (function () {
              var e = {
                  trace: function () {},
                  yy: {},
                  symbols_: {
                    error: 2,
                    JSON_PATH: 3,
                    DOLLAR: 4,
                    PATH_COMPONENTS: 5,
                    LEADING_CHILD_MEMBER_EXPRESSION: 6,
                    PATH_COMPONENT: 7,
                    MEMBER_COMPONENT: 8,
                    SUBSCRIPT_COMPONENT: 9,
                    CHILD_MEMBER_COMPONENT: 10,
                    DESCENDANT_MEMBER_COMPONENT: 11,
                    DOT: 12,
                    MEMBER_EXPRESSION: 13,
                    DOT_DOT: 14,
                    STAR: 15,
                    IDENTIFIER: 16,
                    SCRIPT_EXPRESSION: 17,
                    INTEGER: 18,
                    END: 19,
                    CHILD_SUBSCRIPT_COMPONENT: 20,
                    DESCENDANT_SUBSCRIPT_COMPONENT: 21,
                    '[': 22,
                    SUBSCRIPT: 23,
                    ']': 24,
                    SUBSCRIPT_EXPRESSION: 25,
                    SUBSCRIPT_EXPRESSION_LIST: 26,
                    SUBSCRIPT_EXPRESSION_LISTABLE: 27,
                    ',': 28,
                    STRING_LITERAL: 29,
                    ARRAY_SLICE: 30,
                    FILTER_EXPRESSION: 31,
                    QQ_STRING: 32,
                    Q_STRING: 33,
                    $accept: 0,
                    $end: 1,
                  },
                  terminals_: {
                    2: 'error',
                    4: 'DOLLAR',
                    12: 'DOT',
                    14: 'DOT_DOT',
                    15: 'STAR',
                    16: 'IDENTIFIER',
                    17: 'SCRIPT_EXPRESSION',
                    18: 'INTEGER',
                    19: 'END',
                    22: '[',
                    24: ']',
                    28: ',',
                    30: 'ARRAY_SLICE',
                    31: 'FILTER_EXPRESSION',
                    32: 'QQ_STRING',
                    33: 'Q_STRING',
                  },
                  productions_: [
                    0,
                    [3, 1],
                    [3, 2],
                    [3, 1],
                    [3, 2],
                    [5, 1],
                    [5, 2],
                    [7, 1],
                    [7, 1],
                    [8, 1],
                    [8, 1],
                    [10, 2],
                    [6, 1],
                    [11, 2],
                    [13, 1],
                    [13, 1],
                    [13, 1],
                    [13, 1],
                    [13, 1],
                    [9, 1],
                    [9, 1],
                    [20, 3],
                    [21, 4],
                    [23, 1],
                    [23, 1],
                    [26, 1],
                    [26, 3],
                    [27, 1],
                    [27, 1],
                    [27, 1],
                    [25, 1],
                    [25, 1],
                    [25, 1],
                    [29, 1],
                    [29, 1],
                  ],
                  performAction: function (e, n, r, i, a, o, s) {
                    i.ast || ((i.ast = t), t.initialize())
                    var c = o.length - 1
                    switch (a) {
                      case 1:
                        return i.ast.set({ expression: { type: 'root', value: o[c] } }), i.ast.unshift(), i.ast.yield()
                      case 2:
                        return i.ast.set({ expression: { type: 'root', value: o[c - 1] } }), i.ast.unshift(), i.ast.yield()
                      case 3:
                        return i.ast.unshift(), i.ast.yield()
                      case 4:
                        return i.ast.set({ operation: 'member', scope: 'child', expression: { type: 'identifier', value: o[c - 1] } }), i.ast.unshift(), i.ast.yield()
                      case 5:
                      case 6:
                      case 11:
                      case 13:
                      case 18:
                      case 21:
                      case 22:
                      case 23:
                        break
                      case 7:
                        i.ast.set({ operation: 'member' }), i.ast.push()
                        break
                      case 8:
                        i.ast.set({ operation: 'subscript' }), i.ast.push()
                        break
                      case 9:
                      case 19:
                        i.ast.set({ scope: 'child' })
                        break
                      case 10:
                      case 20:
                        i.ast.set({ scope: 'descendant' })
                        break
                      case 12:
                        i.ast.set({ scope: 'child', operation: 'member' })
                        break
                      case 14:
                        i.ast.set({ expression: { type: 'wildcard', value: o[c] } })
                        break
                      case 15:
                        i.ast.set({ expression: { type: 'identifier', value: o[c] } })
                        break
                      case 16:
                        i.ast.set({ expression: { type: 'script_expression', value: o[c] } })
                        break
                      case 17:
                        i.ast.set({ expression: { type: 'numeric_literal', value: parseInt(o[c]) } })
                        break
                      case 24:
                        o[c].length > 1 ? i.ast.set({ expression: { type: 'union', value: o[c] } }) : (this.$ = o[c])
                        break
                      case 25:
                        this.$ = [o[c]]
                        break
                      case 26:
                        this.$ = o[c - 2].concat(o[c])
                        break
                      case 27:
                        ;(this.$ = { expression: { type: 'numeric_literal', value: parseInt(o[c]) } }), i.ast.set(this.$)
                        break
                      case 28:
                        ;(this.$ = { expression: { type: 'string_literal', value: o[c] } }), i.ast.set(this.$)
                        break
                      case 29:
                        ;(this.$ = { expression: { type: 'slice', value: o[c] } }), i.ast.set(this.$)
                        break
                      case 30:
                        ;(this.$ = { expression: { type: 'wildcard', value: o[c] } }), i.ast.set(this.$)
                        break
                      case 31:
                        ;(this.$ = { expression: { type: 'script_expression', value: o[c] } }), i.ast.set(this.$)
                        break
                      case 32:
                        ;(this.$ = { expression: { type: 'filter_expression', value: o[c] } }), i.ast.set(this.$)
                        break
                      case 33:
                      case 34:
                        this.$ = o[c]
                    }
                  },
                  table: [
                    { 3: 1, 4: [1, 2], 6: 3, 13: 4, 15: [1, 5], 16: [1, 6], 17: [1, 7], 18: [1, 8], 19: [1, 9] },
                    { 1: [3] },
                    { 1: [2, 1], 5: 10, 7: 11, 8: 12, 9: 13, 10: 14, 11: 15, 12: [1, 18], 14: [1, 19], 20: 16, 21: 17, 22: [1, 20] },
                    { 1: [2, 3], 5: 21, 7: 11, 8: 12, 9: 13, 10: 14, 11: 15, 12: [1, 18], 14: [1, 19], 20: 16, 21: 17, 22: [1, 20] },
                    { 1: [2, 12], 12: [2, 12], 14: [2, 12], 22: [2, 12] },
                    { 1: [2, 14], 12: [2, 14], 14: [2, 14], 22: [2, 14] },
                    { 1: [2, 15], 12: [2, 15], 14: [2, 15], 22: [2, 15] },
                    { 1: [2, 16], 12: [2, 16], 14: [2, 16], 22: [2, 16] },
                    { 1: [2, 17], 12: [2, 17], 14: [2, 17], 22: [2, 17] },
                    { 1: [2, 18], 12: [2, 18], 14: [2, 18], 22: [2, 18] },
                    { 1: [2, 2], 7: 22, 8: 12, 9: 13, 10: 14, 11: 15, 12: [1, 18], 14: [1, 19], 20: 16, 21: 17, 22: [1, 20] },
                    { 1: [2, 5], 12: [2, 5], 14: [2, 5], 22: [2, 5] },
                    { 1: [2, 7], 12: [2, 7], 14: [2, 7], 22: [2, 7] },
                    { 1: [2, 8], 12: [2, 8], 14: [2, 8], 22: [2, 8] },
                    { 1: [2, 9], 12: [2, 9], 14: [2, 9], 22: [2, 9] },
                    { 1: [2, 10], 12: [2, 10], 14: [2, 10], 22: [2, 10] },
                    { 1: [2, 19], 12: [2, 19], 14: [2, 19], 22: [2, 19] },
                    { 1: [2, 20], 12: [2, 20], 14: [2, 20], 22: [2, 20] },
                    { 13: 23, 15: [1, 5], 16: [1, 6], 17: [1, 7], 18: [1, 8], 19: [1, 9] },
                    { 13: 24, 15: [1, 5], 16: [1, 6], 17: [1, 7], 18: [1, 8], 19: [1, 9], 22: [1, 25] },
                    { 15: [1, 29], 17: [1, 30], 18: [1, 33], 23: 26, 25: 27, 26: 28, 27: 32, 29: 34, 30: [1, 35], 31: [1, 31], 32: [1, 36], 33: [1, 37] },
                    { 1: [2, 4], 7: 22, 8: 12, 9: 13, 10: 14, 11: 15, 12: [1, 18], 14: [1, 19], 20: 16, 21: 17, 22: [1, 20] },
                    { 1: [2, 6], 12: [2, 6], 14: [2, 6], 22: [2, 6] },
                    { 1: [2, 11], 12: [2, 11], 14: [2, 11], 22: [2, 11] },
                    { 1: [2, 13], 12: [2, 13], 14: [2, 13], 22: [2, 13] },
                    { 15: [1, 29], 17: [1, 30], 18: [1, 33], 23: 38, 25: 27, 26: 28, 27: 32, 29: 34, 30: [1, 35], 31: [1, 31], 32: [1, 36], 33: [1, 37] },
                    { 24: [1, 39] },
                    { 24: [2, 23] },
                    { 24: [2, 24], 28: [1, 40] },
                    { 24: [2, 30] },
                    { 24: [2, 31] },
                    { 24: [2, 32] },
                    { 24: [2, 25], 28: [2, 25] },
                    { 24: [2, 27], 28: [2, 27] },
                    { 24: [2, 28], 28: [2, 28] },
                    { 24: [2, 29], 28: [2, 29] },
                    { 24: [2, 33], 28: [2, 33] },
                    { 24: [2, 34], 28: [2, 34] },
                    { 24: [1, 41] },
                    { 1: [2, 21], 12: [2, 21], 14: [2, 21], 22: [2, 21] },
                    { 18: [1, 33], 27: 42, 29: 34, 30: [1, 35], 32: [1, 36], 33: [1, 37] },
                    { 1: [2, 22], 12: [2, 22], 14: [2, 22], 22: [2, 22] },
                    { 24: [2, 26], 28: [2, 26] },
                  ],
                  defaultActions: { 27: [2, 23], 29: [2, 30], 30: [2, 31], 31: [2, 32] },
                  parseError: function (e, t) {
                    if (!t.recoverable) throw new Error(e)
                    this.trace(e)
                  },
                  parse: function (e) {
                    var t = this,
                      n = [0],
                      r = [null],
                      i = [],
                      a = this.table,
                      o = '',
                      s = 0,
                      c = 0,
                      u = 2,
                      l = 1,
                      p = i.slice.call(arguments, 1)
                    this.lexer.setInput(e), (this.lexer.yy = this.yy), (this.yy.lexer = this.lexer), (this.yy.parser = this), void 0 === this.lexer.yylloc && (this.lexer.yylloc = {})
                    var f = this.lexer.yylloc
                    i.push(f)
                    var h = this.lexer.options && this.lexer.options.ranges
                    function d() {
                      var e
                      return 'number' != typeof (e = t.lexer.lex() || l) && (e = t.symbols_[e] || e), e
                    }
                    'function' == typeof this.yy.parseError ? (this.parseError = this.yy.parseError) : (this.parseError = Object.getPrototypeOf(this).parseError)
                    for (var y, m, g, v, S, E, b, x, k = {}; ; ) {
                      if (((m = n[n.length - 1]), this.defaultActions[m] ? (g = this.defaultActions[m]) : (null == y && (y = d()), (g = a[m] && a[m][y])), void 0 === g || !g.length || !g[0])) {
                        var I = ''
                        for (S in ((x = []), a[m])) this.terminals_[S] && S > u && x.push("'" + this.terminals_[S] + "'")
                        ;(I = this.lexer.showPosition
                          ? 'Parse error on line ' + (s + 1) + ':\n' + this.lexer.showPosition() + '\nExpecting ' + x.join(', ') + ", got '" + (this.terminals_[y] || y) + "'"
                          : 'Parse error on line ' + (s + 1) + ': Unexpected ' + (y == l ? 'end of input' : "'" + (this.terminals_[y] || y) + "'")),
                          this.parseError(I, { text: this.lexer.match, token: this.terminals_[y] || y, line: this.lexer.yylineno, loc: f, expected: x })
                      }
                      if (g[0] instanceof Array && g.length > 1) throw new Error('Parse Error: multiple actions possible at state: ' + m + ', token: ' + y)
                      switch (g[0]) {
                        case 1:
                          n.push(y),
                            r.push(this.lexer.yytext),
                            i.push(this.lexer.yylloc),
                            n.push(g[1]),
                            (y = null),
                            (c = this.lexer.yyleng),
                            (o = this.lexer.yytext),
                            (s = this.lexer.yylineno),
                            (f = this.lexer.yylloc)
                          break
                        case 2:
                          if (
                            ((E = this.productions_[g[1]][1]),
                            (k.$ = r[r.length - E]),
                            (k._$ = {
                              first_line: i[i.length - (E || 1)].first_line,
                              last_line: i[i.length - 1].last_line,
                              first_column: i[i.length - (E || 1)].first_column,
                              last_column: i[i.length - 1].last_column,
                            }),
                            h && (k._$.range = [i[i.length - (E || 1)].range[0], i[i.length - 1].range[1]]),
                            void 0 !== (v = this.performAction.apply(k, [o, c, s, this.yy, g[1], r, i].concat(p))))
                          )
                            return v
                          E && ((n = n.slice(0, -1 * E * 2)), (r = r.slice(0, -1 * E)), (i = i.slice(0, -1 * E))),
                            n.push(this.productions_[g[1]][0]),
                            r.push(k.$),
                            i.push(k._$),
                            (b = a[n[n.length - 2]][n[n.length - 1]]),
                            n.push(b)
                          break
                        case 3:
                          return !0
                      }
                    }
                    return !0
                  },
                },
                t = {
                  initialize: function () {
                    ;(this._nodes = []), (this._node = {}), (this._stash = [])
                  },
                  set: function (e) {
                    for (var t in e) this._node[t] = e[t]
                    return this._node
                  },
                  node: function (e) {
                    return arguments.length && (this._node = e), this._node
                  },
                  push: function () {
                    this._nodes.push(this._node), (this._node = {})
                  },
                  unshift: function () {
                    this._nodes.unshift(this._node), (this._node = {})
                  },
                  yield: function () {
                    var e = this._nodes
                    return this.initialize(), e
                  },
                },
                n = {
                  EOF: 1,
                  parseError: function (e, t) {
                    if (!this.yy.parser) throw new Error(e)
                    this.yy.parser.parseError(e, t)
                  },
                  setInput: function (e) {
                    return (
                      (this._input = e),
                      (this._more = this._backtrack = this.done = !1),
                      (this.yylineno = this.yyleng = 0),
                      (this.yytext = this.matched = this.match = ''),
                      (this.conditionStack = ['INITIAL']),
                      (this.yylloc = { first_line: 1, first_column: 0, last_line: 1, last_column: 0 }),
                      this.options.ranges && (this.yylloc.range = [0, 0]),
                      (this.offset = 0),
                      this
                    )
                  },
                  input: function () {
                    var e = this._input[0]
                    return (
                      (this.yytext += e),
                      this.yyleng++,
                      this.offset++,
                      (this.match += e),
                      (this.matched += e),
                      e.match(/(?:\r\n?|\n).*/g) ? (this.yylineno++, this.yylloc.last_line++) : this.yylloc.last_column++,
                      this.options.ranges && this.yylloc.range[1]++,
                      (this._input = this._input.slice(1)),
                      e
                    )
                  },
                  unput: function (e) {
                    var t = e.length,
                      n = e.split(/(?:\r\n?|\n)/g)
                    ;(this._input = e + this._input), (this.yytext = this.yytext.substr(0, this.yytext.length - t - 1)), (this.offset -= t)
                    var r = this.match.split(/(?:\r\n?|\n)/g)
                    ;(this.match = this.match.substr(0, this.match.length - 1)), (this.matched = this.matched.substr(0, this.matched.length - 1)), n.length - 1 && (this.yylineno -= n.length - 1)
                    var i = this.yylloc.range
                    return (
                      (this.yylloc = {
                        first_line: this.yylloc.first_line,
                        last_line: this.yylineno + 1,
                        first_column: this.yylloc.first_column,
                        last_column: n ? (n.length === r.length ? this.yylloc.first_column : 0) + r[r.length - n.length].length - n[0].length : this.yylloc.first_column - t,
                      }),
                      this.options.ranges && (this.yylloc.range = [i[0], i[0] + this.yyleng - t]),
                      (this.yyleng = this.yytext.length),
                      this
                    )
                  },
                  more: function () {
                    return (this._more = !0), this
                  },
                  reject: function () {
                    return this.options.backtrack_lexer
                      ? ((this._backtrack = !0), this)
                      : this.parseError(
                          'Lexical error on line ' +
                            (this.yylineno + 1) +
                            '. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n' +
                            this.showPosition(),
                          { text: '', token: null, line: this.yylineno }
                        )
                  },
                  less: function (e) {
                    this.unput(this.match.slice(e))
                  },
                  pastInput: function () {
                    var e = this.matched.substr(0, this.matched.length - this.match.length)
                    return (e.length > 20 ? '...' : '') + e.substr(-20).replace(/\n/g, '')
                  },
                  upcomingInput: function () {
                    var e = this.match
                    return e.length < 20 && (e += this._input.substr(0, 20 - e.length)), (e.substr(0, 20) + (e.length > 20 ? '...' : '')).replace(/\n/g, '')
                  },
                  showPosition: function () {
                    var e = this.pastInput(),
                      t = new Array(e.length + 1).join('-')
                    return e + this.upcomingInput() + '\n' + t + '^'
                  },
                  test_match: function (e, t) {
                    var n, r, i
                    if (
                      (this.options.backtrack_lexer &&
                        ((i = {
                          yylineno: this.yylineno,
                          yylloc: { first_line: this.yylloc.first_line, last_line: this.last_line, first_column: this.yylloc.first_column, last_column: this.yylloc.last_column },
                          yytext: this.yytext,
                          match: this.match,
                          matches: this.matches,
                          matched: this.matched,
                          yyleng: this.yyleng,
                          offset: this.offset,
                          _more: this._more,
                          _input: this._input,
                          yy: this.yy,
                          conditionStack: this.conditionStack.slice(0),
                          done: this.done,
                        }),
                        this.options.ranges && (i.yylloc.range = this.yylloc.range.slice(0))),
                      (r = e[0].match(/(?:\r\n?|\n).*/g)) && (this.yylineno += r.length),
                      (this.yylloc = {
                        first_line: this.yylloc.last_line,
                        last_line: this.yylineno + 1,
                        first_column: this.yylloc.last_column,
                        last_column: r ? r[r.length - 1].length - r[r.length - 1].match(/\r?\n?/)[0].length : this.yylloc.last_column + e[0].length,
                      }),
                      (this.yytext += e[0]),
                      (this.match += e[0]),
                      (this.matches = e),
                      (this.yyleng = this.yytext.length),
                      this.options.ranges && (this.yylloc.range = [this.offset, (this.offset += this.yyleng)]),
                      (this._more = !1),
                      (this._backtrack = !1),
                      (this._input = this._input.slice(e[0].length)),
                      (this.matched += e[0]),
                      (n = this.performAction.call(this, this.yy, this, t, this.conditionStack[this.conditionStack.length - 1])),
                      this.done && this._input && (this.done = !1),
                      n)
                    )
                      return n
                    if (this._backtrack) {
                      for (var a in i) this[a] = i[a]
                      return !1
                    }
                    return !1
                  },
                  next: function () {
                    if (this.done) return this.EOF
                    var e, t, n, r
                    this._input || (this.done = !0), this._more || ((this.yytext = ''), (this.match = ''))
                    for (var i = this._currentRules(), a = 0; a < i.length; a++)
                      if ((n = this._input.match(this.rules[i[a]])) && (!t || n[0].length > t[0].length)) {
                        if (((t = n), (r = a), this.options.backtrack_lexer)) {
                          if (!1 !== (e = this.test_match(n, i[a]))) return e
                          if (this._backtrack) {
                            t = !1
                            continue
                          }
                          return !1
                        }
                        if (!this.options.flex) break
                      }
                    return t
                      ? !1 !== (e = this.test_match(t, i[r])) && e
                      : '' === this._input
                      ? this.EOF
                      : this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. Unrecognized text.\n' + this.showPosition(), { text: '', token: null, line: this.yylineno })
                  },
                  lex: function () {
                    var e = this.next()
                    return e || this.lex()
                  },
                  begin: function (e) {
                    this.conditionStack.push(e)
                  },
                  popState: function () {
                    return this.conditionStack.length - 1 > 0 ? this.conditionStack.pop() : this.conditionStack[0]
                  },
                  _currentRules: function () {
                    return this.conditionStack.length && this.conditionStack[this.conditionStack.length - 1]
                      ? this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules
                      : this.conditions.INITIAL.rules
                  },
                  topState: function (e) {
                    return (e = this.conditionStack.length - 1 - Math.abs(e || 0)) >= 0 ? this.conditionStack[e] : 'INITIAL'
                  },
                  pushState: function (e) {
                    this.begin(e)
                  },
                  stateStackSize: function () {
                    return this.conditionStack.length
                  },
                  options: {},
                  performAction: function (e, t, n, r) {
                    switch (n) {
                      case 0:
                        return 4
                      case 1:
                        return 14
                      case 2:
                        return 12
                      case 3:
                        return 15
                      case 4:
                        return 16
                      case 5:
                        return 22
                      case 6:
                        return 24
                      case 7:
                        return 28
                      case 8:
                        return 30
                      case 9:
                        return 18
                      case 10:
                        return (t.yytext = t.yytext.substr(1, t.yyleng - 2)), 32
                      case 11:
                        return (t.yytext = t.yytext.substr(1, t.yyleng - 2)), 33
                      case 12:
                        return 17
                      case 13:
                        return 31
                    }
                  },
                  rules: [
                    /^(?:\$)/,
                    /^(?:\.\.)/,
                    /^(?:\.)/,
                    /^(?:\*)/,
                    /^(?:[a-zA-Z_]+[a-zA-Z0-9_]*)/,
                    /^(?:\[)/,
                    /^(?:\])/,
                    /^(?:,)/,
                    /^(?:((-?(?:0|[1-9][0-9]*)))?\:((-?(?:0|[1-9][0-9]*)))?(\:((-?(?:0|[1-9][0-9]*)))?)?)/,
                    /^(?:(-?(?:0|[1-9][0-9]*)))/,
                    /^(?:"(?:\\["bfnrt/\\]|\\u[a-fA-F0-9]{4}|[^"\\])*")/,
                    /^(?:'(?:\\['bfnrt/\\]|\\u[a-fA-F0-9]{4}|[^'\\])*')/,
                    /^(?:\(.+?\)(?=\]))/,
                    /^(?:\?\(.+?\)(?=\]))/,
                  ],
                  conditions: { INITIAL: { rules: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], inclusive: !0 } },
                }
              function r() {
                this.yy = {}
              }
              return (e.lexer = n), (r.prototype = e), (e.Parser = r), new r()
            })()
            void 0 !== e &&
              void 0 !== n &&
              ((n.parser = i),
              (n.Parser = i.Parser),
              (n.parse = function () {
                return i.parse.apply(i, arguments)
              }),
              (n.main = function (t) {
                t[1] || (console.log('Usage: ' + t[0] + ' FILE'), r.exit(1))
                var i = e('fs').readFileSync(e('path').normalize(t[1]), 'utf8')
                return n.parser.parse(i)
              }),
              void 0 !== t && e.main === t && n.main(r.argv.slice(1)))
          }).call(this, e('_process'))
        },
        { _process: 14, fs: 12, path: 13 },
      ],
      2: [
        function (e, t, n) {
          t.exports = {
            identifier: '[a-zA-Z_]+[a-zA-Z0-9_]*',
            integer: '-?(?:0|[1-9][0-9]*)',
            qq_string: '"(?:\\\\["bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^"\\\\])*"',
            q_string: "'(?:\\\\['bfnrt/\\\\]|\\\\u[a-fA-F0-9]{4}|[^'\\\\])*'",
          }
        },
        {},
      ],
      3: [
        function (e, t, n) {
          var r = e('./dict'),
            i = e('fs'),
            a = {
              lex: {
                macros: { esc: '\\\\', int: r.integer },
                rules: [
                  ['\\$', "return 'DOLLAR'"],
                  ['\\.\\.', "return 'DOT_DOT'"],
                  ['\\.', "return 'DOT'"],
                  ['\\*', "return 'STAR'"],
                  [r.identifier, "return 'IDENTIFIER'"],
                  ['\\[', "return '['"],
                  ['\\]', "return ']'"],
                  [',', "return ','"],
                  ['({int})?\\:({int})?(\\:({int})?)?', "return 'ARRAY_SLICE'"],
                  ['{int}', "return 'INTEGER'"],
                  [r.qq_string, "yytext = yytext.substr(1,yyleng-2); return 'QQ_STRING';"],
                  [r.q_string, "yytext = yytext.substr(1,yyleng-2); return 'Q_STRING';"],
                  ['\\(.+?\\)(?=\\])', "return 'SCRIPT_EXPRESSION'"],
                  ['\\?\\(.+?\\)(?=\\])', "return 'FILTER_EXPRESSION'"],
                ],
              },
              start: 'JSON_PATH',
              bnf: {
                JSON_PATH: [
                  ['DOLLAR', 'yy.ast.set({ expression: { type: "root", value: $1 } }); yy.ast.unshift(); return yy.ast.yield()'],
                  ['DOLLAR PATH_COMPONENTS', 'yy.ast.set({ expression: { type: "root", value: $1 } }); yy.ast.unshift(); return yy.ast.yield()'],
                  ['LEADING_CHILD_MEMBER_EXPRESSION', 'yy.ast.unshift(); return yy.ast.yield()'],
                  [
                    'LEADING_CHILD_MEMBER_EXPRESSION PATH_COMPONENTS',
                    'yy.ast.set({ operation: "member", scope: "child", expression: { type: "identifier", value: $1 }}); yy.ast.unshift(); return yy.ast.yield()',
                  ],
                ],
                PATH_COMPONENTS: [
                  ['PATH_COMPONENT', ''],
                  ['PATH_COMPONENTS PATH_COMPONENT', ''],
                ],
                PATH_COMPONENT: [
                  ['MEMBER_COMPONENT', 'yy.ast.set({ operation: "member" }); yy.ast.push()'],
                  ['SUBSCRIPT_COMPONENT', 'yy.ast.set({ operation: "subscript" }); yy.ast.push() '],
                ],
                MEMBER_COMPONENT: [
                  ['CHILD_MEMBER_COMPONENT', 'yy.ast.set({ scope: "child" })'],
                  ['DESCENDANT_MEMBER_COMPONENT', 'yy.ast.set({ scope: "descendant" })'],
                ],
                CHILD_MEMBER_COMPONENT: [['DOT MEMBER_EXPRESSION', '']],
                LEADING_CHILD_MEMBER_EXPRESSION: [['MEMBER_EXPRESSION', 'yy.ast.set({ scope: "child", operation: "member" })']],
                DESCENDANT_MEMBER_COMPONENT: [['DOT_DOT MEMBER_EXPRESSION', '']],
                MEMBER_EXPRESSION: [
                  ['STAR', 'yy.ast.set({ expression: { type: "wildcard", value: $1 } })'],
                  ['IDENTIFIER', 'yy.ast.set({ expression: { type: "identifier", value: $1 } })'],
                  ['SCRIPT_EXPRESSION', 'yy.ast.set({ expression: { type: "script_expression", value: $1 } })'],
                  ['INTEGER', 'yy.ast.set({ expression: { type: "numeric_literal", value: parseInt($1) } })'],
                  ['END', ''],
                ],
                SUBSCRIPT_COMPONENT: [
                  ['CHILD_SUBSCRIPT_COMPONENT', 'yy.ast.set({ scope: "child" })'],
                  ['DESCENDANT_SUBSCRIPT_COMPONENT', 'yy.ast.set({ scope: "descendant" })'],
                ],
                CHILD_SUBSCRIPT_COMPONENT: [['[ SUBSCRIPT ]', '']],
                DESCENDANT_SUBSCRIPT_COMPONENT: [['DOT_DOT [ SUBSCRIPT ]', '']],
                SUBSCRIPT: [
                  ['SUBSCRIPT_EXPRESSION', ''],
                  ['SUBSCRIPT_EXPRESSION_LIST', '$1.length > 1? yy.ast.set({ expression: { type: "union", value: $1 } }) : $$ = $1'],
                ],
                SUBSCRIPT_EXPRESSION_LIST: [
                  ['SUBSCRIPT_EXPRESSION_LISTABLE', '$$ = [$1]'],
                  ['SUBSCRIPT_EXPRESSION_LIST , SUBSCRIPT_EXPRESSION_LISTABLE', '$$ = $1.concat($3)'],
                ],
                SUBSCRIPT_EXPRESSION_LISTABLE: [
                  ['INTEGER', '$$ = { expression: { type: "numeric_literal", value: parseInt($1) } }; yy.ast.set($$)'],
                  ['STRING_LITERAL', '$$ = { expression: { type: "string_literal", value: $1 } }; yy.ast.set($$)'],
                  ['ARRAY_SLICE', '$$ = { expression: { type: "slice", value: $1 } }; yy.ast.set($$)'],
                ],
                SUBSCRIPT_EXPRESSION: [
                  ['STAR', '$$ = { expression: { type: "wildcard", value: $1 } }; yy.ast.set($$)'],
                  ['SCRIPT_EXPRESSION', '$$ = { expression: { type: "script_expression", value: $1 } }; yy.ast.set($$)'],
                  ['FILTER_EXPRESSION', '$$ = { expression: { type: "filter_expression", value: $1 } }; yy.ast.set($$)'],
                ],
                STRING_LITERAL: [
                  ['QQ_STRING', '$$ = $1'],
                  ['Q_STRING', '$$ = $1'],
                ],
              },
            }
          i.readFileSync && ((a.moduleInclude = i.readFileSync(e.resolve('../include/module.js'))), (a.actionInclude = i.readFileSync(e.resolve('../include/action.js')))), (t.exports = a)
        },
        { './dict': 2, fs: 12 },
      ],
      4: [
        function (e, t, n) {
          var r = e('./aesprim'),
            i = e('./slice'),
            a = e('static-eval'),
            o = e('underscore').uniq,
            s = function () {
              return this.initialize.apply(this, arguments)
            }
          function c(t, n, i) {
            var a = e('./index'),
              o = d(r.parse(n).body[0].expression, { '@': t.value }),
              s = i.replace(/\{\{\s*value\s*\}\}/g, o),
              c = a.nodes(t.value, s)
            return (
              c.forEach(function (e) {
                e.path = t.path.concat(e.path.slice(1))
              }),
              c
            )
          }
          function u(e) {
            return Array.isArray(e)
          }
          function l(e) {
            return e && !(e instanceof Array) && e instanceof Object
          }
          function p(e) {
            return function (t, n, r, i) {
              var a = t.value,
                o = t.path,
                s = [],
                c = function (t, a) {
                  u(t)
                    ? (t.forEach(function (e, t) {
                        s.length >= i || (r(t, e, n) && s.push({ path: a.concat(t), value: e }))
                      }),
                      t.forEach(function (t, n) {
                        s.length >= i || (e && c(t, a.concat(n)))
                      }))
                    : l(t) &&
                      (this.keys(t).forEach(function (e) {
                        s.length >= i || (r(e, t[e], n) && s.push({ path: a.concat(e), value: t[e] }))
                      }),
                      this.keys(t).forEach(function (n) {
                        s.length >= i || (e && c(t[n], a.concat(n)))
                      }))
                }.bind(this)
              return c(a, o), s
            }
          }
          function f(e) {
            return function (t, n, r) {
              return this.descend(n, t.expression.value, e, r)
            }
          }
          function h(e) {
            return function (t, n, r) {
              return this.traverse(n, t.expression.value, e, r)
            }
          }
          function d() {
            try {
              return a.apply(this, arguments)
            } catch (e) {}
          }
          function y(e) {
            return (
              (e = e.filter(function (e) {
                return e
              })),
              o(e, function (e) {
                return e.path
                  .map(function (e) {
                    return String(e).replace('-', '--')
                  })
                  .join('-')
              })
            )
          }
          function m(e) {
            var t = String(e)
            return t.match(/^-?[0-9]+$/) ? parseInt(t) : null
          }
          ;(s.prototype.initialize = function () {
            ;(this.traverse = p(!0)), (this.descend = p())
          }),
            (s.prototype.keys = Object.keys),
            (s.prototype.resolve = function (e) {
              var t = [e.operation, e.scope, e.expression.type].join('-'),
                n = this._fns[t]
              if (!n) throw new Error("couldn't resolve key: " + t)
              return n.bind(this)
            }),
            (s.prototype.register = function (e, t) {
              if (!t instanceof Function) throw new Error('handler must be a function')
              this._fns[e] = t
            }),
            (s.prototype._fns = {
              'member-child-identifier': function (e, t) {
                var n = e.expression.value,
                  r = t.value
                if (r instanceof Object && n in r) return [{ value: r[n], path: t.path.concat(n) }]
              },
              'member-descendant-identifier': h(function (e, t, n) {
                return e == n
              }),
              'subscript-child-numeric_literal': f(function (e, t, n) {
                return e === n
              }),
              'member-child-numeric_literal': f(function (e, t, n) {
                return String(e) === String(n)
              }),
              'subscript-descendant-numeric_literal': h(function (e, t, n) {
                return e === n
              }),
              'member-child-wildcard': f(function () {
                return !0
              }),
              'member-descendant-wildcard': h(function () {
                return !0
              }),
              'subscript-descendant-wildcard': h(function () {
                return !0
              }),
              'subscript-child-wildcard': f(function () {
                return !0
              }),
              'subscript-child-slice': function (e, t) {
                if (u(t.value)) {
                  var n = e.expression.value.split(':').map(m),
                    r = t.value.map(function (e, n) {
                      return { value: e, path: t.path.concat(n) }
                    })
                  return i.apply(null, [r].concat(n))
                }
              },
              'subscript-child-union': function (e, t) {
                var n = []
                return (
                  e.expression.value.forEach(function (e) {
                    var r = { operation: 'subscript', scope: 'child', expression: e.expression },
                      i = this.resolve(r)(r, t)
                    i && (n = n.concat(i))
                  }, this),
                  y(n)
                )
              },
              'subscript-descendant-union': function (t, n, r) {
                var i = e('..'),
                  a = this,
                  o = []
                return (
                  i
                    .nodes(n, '$..*')
                    .slice(1)
                    .forEach(function (e) {
                      o.length >= r ||
                        t.expression.value.forEach(function (t) {
                          var n = { operation: 'subscript', scope: 'child', expression: t.expression },
                            r = a.resolve(n)(n, e)
                          o = o.concat(r)
                        })
                    }),
                  y(o)
                )
              },
              'subscript-child-filter_expression': function (e, t, n) {
                var i = e.expression.value.slice(2, -1),
                  a = r.parse(i).body[0].expression,
                  o = function (e, t) {
                    return d(a, { '@': t })
                  }
                return this.descend(t, null, o, n)
              },
              'subscript-descendant-filter_expression': function (e, t, n) {
                var i = e.expression.value.slice(2, -1),
                  a = r.parse(i).body[0].expression,
                  o = function (e, t) {
                    return d(a, { '@': t })
                  }
                return this.traverse(t, null, o, n)
              },
              'subscript-child-script_expression': function (e, t) {
                return c(t, e.expression.value.slice(1, -1), '$[{{value}}]')
              },
              'member-child-script_expression': function (e, t) {
                return c(t, e.expression.value.slice(1, -1), '$.{{value}}')
              },
              'member-descendant-script_expression': function (e, t) {
                return c(t, e.expression.value.slice(1, -1), '$..value')
              },
            }),
            (s.prototype._fns['subscript-child-string_literal'] = s.prototype._fns['member-child-identifier']),
            (s.prototype._fns['member-descendant-numeric_literal'] = s.prototype._fns['subscript-descendant-string_literal'] = s.prototype._fns['member-descendant-identifier']),
            (t.exports = s)
        },
        { '..': 'jsonpath', './aesprim': './aesprim', './index': 5, './slice': 7, 'static-eval': 15, underscore: 12 },
      ],
      5: [
        function (e, t, n) {
          var r = e('assert'),
            i = e('./dict'),
            a = e('./parser'),
            o = e('./handlers'),
            s = function () {
              this.initialize.apply(this, arguments)
            }
          function c(e) {
            return '[object String]' == Object.prototype.toString.call(e)
          }
          ;(s.prototype.initialize = function () {
            ;(this.parser = new a()), (this.handlers = new o())
          }),
            (s.prototype.parse = function (e) {
              return r.ok(c(e), 'we need a path'), this.parser.parse(e)
            }),
            (s.prototype.parent = function (e, t) {
              r.ok(e instanceof Object, 'obj needs to be an object'), r.ok(t, 'we need a path')
              var n = this.nodes(e, t)[0]
              return n.path.pop(), this.value(e, n.path)
            }),
            (s.prototype.apply = function (e, t, n) {
              r.ok(e instanceof Object, 'obj needs to be an object'), r.ok(t, 'we need a path'), r.equal(typeof n, 'function', 'fn needs to be function')
              var i = this.nodes(e, t).sort(function (e, t) {
                return t.path.length - e.path.length
              })
              return (
                i.forEach(function (t) {
                  var r = t.path.pop(),
                    i = this.value(e, this.stringify(t.path)),
                    a = (t.value = n.call(e, i[r]))
                  i[r] = a
                }, this),
                i
              )
            }),
            (s.prototype.value = function (e, t, n) {
              if ((r.ok(e instanceof Object, 'obj needs to be an object'), r.ok(t, 'we need a path'), arguments.length >= 3)) {
                var i = this.nodes(e, t).shift()
                if (!i) return this._vivify(e, t, n)
                var a = i.path.slice(-1).shift()
                this.parent(e, this.stringify(i.path))[a] = n
              }
              return this.query(e, this.stringify(t), 1).shift()
            }),
            (s.prototype._vivify = function (e, t, n) {
              var i = this
              r.ok(e instanceof Object, 'obj needs to be an object'), r.ok(t, 'we need a path')
              var a = this.parser.parse(t).map(function (e) {
                  return e.expression.value
                }),
                o = function (t, n) {
                  var r = t.pop(),
                    a = i.value(e, t)
                  a || (o(t.concat(), 'string' == typeof r ? {} : []), (a = i.value(e, t))), (a[r] = n)
                }
              return o(a, n), this.query(e, t)[0]
            }),
            (s.prototype.query = function (e, t, n) {
              return (
                r.ok(e instanceof Object, 'obj needs to be an object'),
                r.ok(c(t), 'we need a path'),
                this.nodes(e, t, n).map(function (e) {
                  return e.value
                })
              )
            }),
            (s.prototype.paths = function (e, t, n) {
              return (
                r.ok(e instanceof Object, 'obj needs to be an object'),
                r.ok(t, 'we need a path'),
                this.nodes(e, t, n).map(function (e) {
                  return e.path
                })
              )
            }),
            (s.prototype.nodes = function (e, t, n) {
              if ((r.ok(e instanceof Object, 'obj needs to be an object'), r.ok(t, 'we need a path'), 0 === n)) return []
              var i = this.parser.parse(t),
                a = this.handlers,
                o = [{ path: ['$'], value: e }],
                s = []
              return (
                i.length && 'root' == i[0].expression.type && i.shift(),
                i.length
                  ? (i.forEach(function (e, t) {
                      if (!(s.length >= n)) {
                        var r = a.resolve(e),
                          c = []
                        o.forEach(function (a) {
                          if (!(s.length >= n)) {
                            var o = r(e, a, n)
                            t == i.length - 1 ? (s = s.concat(o || [])) : (c = c.concat(o || []))
                          }
                        }),
                          (o = c)
                      }
                    }),
                    n ? s.slice(0, n) : s)
                  : o
              )
            }),
            (s.prototype.stringify = function (e) {
              r.ok(e, 'we need a path')
              var t = '$',
                n = { 'descendant-member': '..{{value}}', 'child-member': '.{{value}}', 'descendant-subscript': '..[{{value}}]', 'child-subscript': '[{{value}}]' }
              return (
                (e = this._normalize(e)).forEach(function (e) {
                  if ('root' != e.expression.type) {
                    var r,
                      i = [e.scope, e.operation].join('-'),
                      a = n[i]
                    if (((r = 'string_literal' == e.expression.type ? JSON.stringify(e.expression.value) : e.expression.value), !a)) throw new Error("couldn't find template " + i)
                    t += a.replace(/{{value}}/, r)
                  }
                }),
                t
              )
            }),
            (s.prototype._normalize = function (e) {
              if ((r.ok(e, 'we need a path'), 'string' == typeof e)) return this.parser.parse(e)
              if (Array.isArray(e) && 'string' == typeof e[0]) {
                var t = [{ expression: { type: 'root', value: '$' } }]
                return (
                  e.forEach(function (e, n) {
                    if ('$' != e || 0 !== n)
                      if ('string' == typeof e && e.match('^' + i.identifier + '$')) t.push({ operation: 'member', scope: 'child', expression: { value: e, type: 'identifier' } })
                      else {
                        var r = 'number' == typeof e ? 'numeric_literal' : 'string_literal'
                        t.push({ operation: 'subscript', scope: 'child', expression: { value: e, type: r } })
                      }
                  }),
                  t
                )
              }
              if (Array.isArray(e) && 'object' == typeof e[0]) return e
              throw new Error("couldn't understand path " + e)
            }),
            (s.Handlers = o),
            (s.Parser = a)
          var u = new s()
          ;(u.JSONPath = s), (t.exports = u)
        },
        { './dict': 2, './handlers': 4, './parser': 6, assert: 8 },
      ],
      6: [
        function (e, t, n) {
          var r = e('./grammar'),
            i = e('../generated/parser'),
            a = function () {
              var e = new i.Parser(),
                t = e.parseError
              return (
                (e.yy.parseError = function () {
                  e.yy.ast && e.yy.ast.initialize(), t.apply(e, arguments)
                }),
                e
              )
            }
          ;(a.grammar = r), (t.exports = a)
        },
        { '../generated/parser': 1, './grammar': 3 },
      ],
      7: [
        function (e, t, n) {
          function r(e) {
            return String(e).match(/^[0-9]+$/) ? parseInt(e) : Number.isFinite(e) ? parseInt(e, 10) : 0
          }
          t.exports = function (e, t, n, i) {
            if ('string' == typeof t) throw new Error('start cannot be a string')
            if ('string' == typeof n) throw new Error('end cannot be a string')
            if ('string' == typeof i) throw new Error('step cannot be a string')
            var a = e.length
            if (0 === i) throw new Error('step cannot be zero')
            if (
              ((i = i ? r(i) : 1),
              (n = n < 0 ? a + n : n),
              (t = r(0 === (t = t < 0 ? a + t : t) ? 0 : t || (i > 0 ? 0 : a - 1))),
              (n = r(0 === n ? 0 : n || (i > 0 ? a : -1))),
              (t = i > 0 ? Math.max(0, t) : Math.min(a, t)),
              (n = i > 0 ? Math.min(n, a) : Math.max(-1, n)),
              i > 0 && n <= t)
            )
              return []
            if (i < 0 && t <= n) return []
            for (var o = [], s = t; s != n && !((i < 0 && s <= n) || (i > 0 && s >= n)); s += i) o.push(e[s])
            return o
          }
        },
        {},
      ],
      8: [
        function (e, t, n) {
          var r = e('util/'),
            i = Array.prototype.slice,
            a = Object.prototype.hasOwnProperty,
            o = (t.exports = p)
          function s(e, t) {
            return r.isUndefined(t) ? '' + t : (r.isNumber(t) && !isFinite(t)) || r.isFunction(t) || r.isRegExp(t) ? t.toString() : t
          }
          function c(e, t) {
            return r.isString(e) ? (e.length < t ? e : e.slice(0, t)) : e
          }
          function u(e) {
            return c(JSON.stringify(e.actual, s), 128) + ' ' + e.operator + ' ' + c(JSON.stringify(e.expected, s), 128)
          }
          function l(e, t, n, r, i) {
            throw new o.AssertionError({ message: n, actual: e, expected: t, operator: r, stackStartFunction: i })
          }
          function p(e, t) {
            e || l(e, !0, t, '==', o.ok)
          }
          function f(e, t) {
            if (e === t) return !0
            if (r.isBuffer(e) && r.isBuffer(t)) {
              if (e.length != t.length) return !1
              for (var n = 0; n < e.length; n++) if (e[n] !== t[n]) return !1
              return !0
            }
            return r.isDate(e) && r.isDate(t)
              ? e.getTime() === t.getTime()
              : r.isRegExp(e) && r.isRegExp(t)
              ? e.source === t.source && e.global === t.global && e.multiline === t.multiline && e.lastIndex === t.lastIndex && e.ignoreCase === t.ignoreCase
              : r.isObject(e) || r.isObject(t)
              ? d(e, t)
              : e == t
          }
          function h(e) {
            return '[object Arguments]' == Object.prototype.toString.call(e)
          }
          function d(e, t) {
            if (r.isNullOrUndefined(e) || r.isNullOrUndefined(t)) return !1
            if (e.prototype !== t.prototype) return !1
            if (r.isPrimitive(e) || r.isPrimitive(t)) return e === t
            var n = h(e),
              a = h(t)
            if ((n && !a) || (!n && a)) return !1
            if (n) return f((e = i.call(e)), (t = i.call(t)))
            var o,
              s,
              c = g(e),
              u = g(t)
            if (c.length != u.length) return !1
            for (c.sort(), u.sort(), s = c.length - 1; s >= 0; s--) if (c[s] != u[s]) return !1
            for (s = c.length - 1; s >= 0; s--) if (!f(e[(o = c[s])], t[o])) return !1
            return !0
          }
          function y(e, t) {
            return !(!e || !t) && ('[object RegExp]' == Object.prototype.toString.call(t) ? t.test(e) : e instanceof t || !0 === t.call({}, e))
          }
          function m(e, t, n, i) {
            var a
            r.isString(n) && ((i = n), (n = null))
            try {
              t()
            } catch (e) {
              a = e
            }
            if (
              ((i = (n && n.name ? ' (' + n.name + ').' : '.') + (i ? ' ' + i : '.')),
              e && !a && l(a, n, 'Missing expected exception' + i),
              !e && y(a, n) && l(a, n, 'Got unwanted exception' + i),
              (e && a && n && !y(a, n)) || (!e && a))
            )
              throw a
          }
          ;(o.AssertionError = function (e) {
            ;(this.name = 'AssertionError'),
              (this.actual = e.actual),
              (this.expected = e.expected),
              (this.operator = e.operator),
              e.message ? ((this.message = e.message), (this.generatedMessage = !1)) : ((this.message = u(this)), (this.generatedMessage = !0))
            var t = e.stackStartFunction || l
            if (Error.captureStackTrace) Error.captureStackTrace(this, t)
            else {
              var n = new Error()
              if (n.stack) {
                var r = n.stack,
                  i = t.name,
                  a = r.indexOf('\n' + i)
                if (a >= 0) {
                  var o = r.indexOf('\n', a + 1)
                  r = r.substring(o + 1)
                }
                this.stack = r
              }
            }
          }),
            r.inherits(o.AssertionError, Error),
            (o.fail = l),
            (o.ok = p),
            (o.equal = function (e, t, n) {
              e != t && l(e, t, n, '==', o.equal)
            }),
            (o.notEqual = function (e, t, n) {
              e == t && l(e, t, n, '!=', o.notEqual)
            }),
            (o.deepEqual = function (e, t, n) {
              f(e, t) || l(e, t, n, 'deepEqual', o.deepEqual)
            }),
            (o.notDeepEqual = function (e, t, n) {
              f(e, t) && l(e, t, n, 'notDeepEqual', o.notDeepEqual)
            }),
            (o.strictEqual = function (e, t, n) {
              e !== t && l(e, t, n, '===', o.strictEqual)
            }),
            (o.notStrictEqual = function (e, t, n) {
              e === t && l(e, t, n, '!==', o.notStrictEqual)
            }),
            (o.throws = function (e, t, n) {
              m.apply(this, [!0].concat(i.call(arguments)))
            }),
            (o.doesNotThrow = function (e, t) {
              m.apply(this, [!1].concat(i.call(arguments)))
            }),
            (o.ifError = function (e) {
              if (e) throw e
            })
          var g =
            Object.keys ||
            function (e) {
              var t = []
              for (var n in e) a.call(e, n) && t.push(n)
              return t
            }
        },
        { 'util/': 11 },
      ],
      9: [
        function (e, t, n) {
          'function' == typeof Object.create
            ? (t.exports = function (e, t) {
                ;(e.super_ = t), (e.prototype = Object.create(t.prototype, { constructor: { value: e, enumerable: !1, writable: !0, configurable: !0 } }))
              })
            : (t.exports = function (e, t) {
                e.super_ = t
                var n = function () {}
                ;(n.prototype = t.prototype), (e.prototype = new n()), (e.prototype.constructor = e)
              })
        },
        {},
      ],
      10: [
        function (e, t, n) {
          t.exports = function (e) {
            return e && 'object' == typeof e && 'function' == typeof e.copy && 'function' == typeof e.fill && 'function' == typeof e.readUInt8
          }
        },
        {},
      ],
      11: [
        function (t, n, r) {
          ;(function (e, n) {
            var i = /%[sdj%]/g
            ;(r.format = function (e) {
              if (!x(e)) {
                for (var t = [], n = 0; n < arguments.length; n++) t.push(s(arguments[n]))
                return t.join(' ')
              }
              n = 1
              for (
                var r = arguments,
                  a = r.length,
                  o = String(e).replace(i, function (e) {
                    if ('%%' === e) return '%'
                    if (n >= a) return e
                    switch (e) {
                      case '%s':
                        return String(r[n++])
                      case '%d':
                        return Number(r[n++])
                      case '%j':
                        try {
                          return JSON.stringify(r[n++])
                        } catch (e) {
                          return '[Circular]'
                        }
                      default:
                        return e
                    }
                  }),
                  c = r[n];
                n < a;
                c = r[++n]
              )
                S(c) || !w(c) ? (o += ' ' + c) : (o += ' ' + s(c))
              return o
            }),
              (r.deprecate = function (t, i) {
                if (I(n.process))
                  return function () {
                    return r.deprecate(t, i).apply(this, arguments)
                  }
                if (!0 === e.noDeprecation) return t
                var a = !1
                function o() {
                  if (!a) {
                    if (e.throwDeprecation) throw new Error(i)
                    e.traceDeprecation ? console.trace(i) : console.error(i), (a = !0)
                  }
                  return t.apply(this, arguments)
                }
                return o
              })
            var a,
              o = {}
            function s(e, t) {
              var n = { seen: [], stylize: u }
              return (
                arguments.length >= 3 && (n.depth = arguments[2]),
                arguments.length >= 4 && (n.colors = arguments[3]),
                v(t) ? (n.showHidden = t) : t && r._extend(n, t),
                I(n.showHidden) && (n.showHidden = !1),
                I(n.depth) && (n.depth = 2),
                I(n.colors) && (n.colors = !1),
                I(n.customInspect) && (n.customInspect = !0),
                n.colors && (n.stylize = c),
                p(n, e, n.depth)
              )
            }
            function c(e, t) {
              var n = s.styles[t]
              return n ? '[' + s.colors[n][0] + 'm' + e + '[' + s.colors[n][1] + 'm' : e
            }
            function u(e, t) {
              return e
            }
            function l(e) {
              var t = {}
              return (
                e.forEach(function (e, n) {
                  t[e] = !0
                }),
                t
              )
            }
            function p(e, t, n) {
              if (e.customInspect && t && N(t.inspect) && t.inspect !== r.inspect && (!t.constructor || t.constructor.prototype !== t)) {
                var i = t.inspect(n, e)
                return x(i) || (i = p(e, i, n)), i
              }
              var a = f(e, t)
              if (a) return a
              var o = Object.keys(t),
                s = l(o)
              if ((e.showHidden && (o = Object.getOwnPropertyNames(t)), O(t) && (o.indexOf('message') >= 0 || o.indexOf('description') >= 0))) return h(t)
              if (0 === o.length) {
                if (N(t)) {
                  var c = t.name ? ': ' + t.name : ''
                  return e.stylize('[Function' + c + ']', 'special')
                }
                if (_(t)) return e.stylize(RegExp.prototype.toString.call(t), 'regexp')
                if (C(t)) return e.stylize(Date.prototype.toString.call(t), 'date')
                if (O(t)) return h(t)
              }
              var u,
                v = '',
                S = !1,
                E = ['{', '}']
              return (
                g(t) && ((S = !0), (E = ['[', ']'])),
                N(t) && (v = ' [Function' + (t.name ? ': ' + t.name : '') + ']'),
                _(t) && (v = ' ' + RegExp.prototype.toString.call(t)),
                C(t) && (v = ' ' + Date.prototype.toUTCString.call(t)),
                O(t) && (v = ' ' + h(t)),
                0 !== o.length || (S && 0 != t.length)
                  ? n < 0
                    ? _(t)
                      ? e.stylize(RegExp.prototype.toString.call(t), 'regexp')
                      : e.stylize('[Object]', 'special')
                    : (e.seen.push(t),
                      (u = S
                        ? d(e, t, n, s, o)
                        : o.map(function (r) {
                            return y(e, t, n, s, r, S)
                          })),
                      e.seen.pop(),
                      m(u, v, E))
                  : E[0] + v + E[1]
              )
            }
            function f(e, t) {
              if (I(t)) return e.stylize('undefined', 'undefined')
              if (x(t)) {
                var n = "'" + JSON.stringify(t).replace(/^"|"$/g, '').replace(/'/g, "\\'").replace(/\\"/g, '"') + "'"
                return e.stylize(n, 'string')
              }
              return b(t) ? e.stylize('' + t, 'number') : v(t) ? e.stylize('' + t, 'boolean') : S(t) ? e.stylize('null', 'null') : void 0
            }
            function h(e) {
              return '[' + Error.prototype.toString.call(e) + ']'
            }
            function d(e, t, n, r, i) {
              for (var a = [], o = 0, s = t.length; o < s; ++o) D(t, String(o)) ? a.push(y(e, t, n, r, String(o), !0)) : a.push('')
              return (
                i.forEach(function (i) {
                  i.match(/^\d+$/) || a.push(y(e, t, n, r, i, !0))
                }),
                a
              )
            }
            function y(e, t, n, r, i, a) {
              var o, s, c
              if (
                ((c = Object.getOwnPropertyDescriptor(t, i) || { value: t[i] }).get
                  ? (s = c.set ? e.stylize('[Getter/Setter]', 'special') : e.stylize('[Getter]', 'special'))
                  : c.set && (s = e.stylize('[Setter]', 'special')),
                D(r, i) || (o = '[' + i + ']'),
                s ||
                  (e.seen.indexOf(c.value) < 0
                    ? (s = S(n) ? p(e, c.value, null) : p(e, c.value, n - 1)).indexOf('\n') > -1 &&
                      (s = a
                        ? s
                            .split('\n')
                            .map(function (e) {
                              return '  ' + e
                            })
                            .join('\n')
                            .substr(2)
                        : '\n' +
                          s
                            .split('\n')
                            .map(function (e) {
                              return '   ' + e
                            })
                            .join('\n'))
                    : (s = e.stylize('[Circular]', 'special'))),
                I(o))
              ) {
                if (a && i.match(/^\d+$/)) return s
                ;(o = JSON.stringify('' + i)).match(/^"([a-zA-Z_][a-zA-Z_0-9]*)"$/)
                  ? ((o = o.substr(1, o.length - 2)), (o = e.stylize(o, 'name')))
                  : ((o = o
                      .replace(/'/g, "\\'")
                      .replace(/\\"/g, '"')
                      .replace(/(^"|"$)/g, "'")),
                    (o = e.stylize(o, 'string')))
              }
              return o + ': ' + s
            }
            function m(e, t, n) {
              return e.reduce(function (e, t) {
                return t.indexOf('\n'), e + t.replace(/\u001b\[\d\d?m/g, '').length + 1
              }, 0) > 60
                ? n[0] + ('' === t ? '' : t + '\n ') + ' ' + e.join(',\n  ') + ' ' + n[1]
                : n[0] + t + ' ' + e.join(', ') + ' ' + n[1]
            }
            function g(e) {
              return Array.isArray(e)
            }
            function v(e) {
              return 'boolean' == typeof e
            }
            function S(e) {
              return null === e
            }
            function E(e) {
              return null == e
            }
            function b(e) {
              return 'number' == typeof e
            }
            function x(e) {
              return 'string' == typeof e
            }
            function k(e) {
              return 'symbol' == typeof e
            }
            function I(e) {
              return void 0 === e
            }
            function _(e) {
              return w(e) && '[object RegExp]' === A(e)
            }
            function w(e) {
              return 'object' == typeof e && null !== e
            }
            function C(e) {
              return w(e) && '[object Date]' === A(e)
            }
            function O(e) {
              return w(e) && ('[object Error]' === A(e) || e instanceof Error)
            }
            function N(e) {
              return 'function' == typeof e
            }
            function T(e) {
              return null === e || 'boolean' == typeof e || 'number' == typeof e || 'string' == typeof e || 'symbol' == typeof e || void 0 === e
            }
            function A(e) {
              return Object.prototype.toString.call(e)
            }
            function L(e) {
              return e < 10 ? '0' + e.toString(10) : e.toString(10)
            }
            ;(r.debuglog = function (t) {
              if ((I(a) && (a = e.env.NODE_DEBUG || ''), (t = t.toUpperCase()), !o[t]))
                if (new RegExp('\\b' + t + '\\b', 'i').test(a)) {
                  var n = e.pid
                  o[t] = function () {
                    var e = r.format.apply(r, arguments)
                    console.error('%s %d: %s', t, n, e)
                  }
                } else o[t] = function () {}
              return o[t]
            }),
              (r.inspect = s),
              (s.colors = {
                bold: [1, 22],
                italic: [3, 23],
                underline: [4, 24],
                inverse: [7, 27],
                white: [37, 39],
                grey: [90, 39],
                black: [30, 39],
                blue: [34, 39],
                cyan: [36, 39],
                green: [32, 39],
                magenta: [35, 39],
                red: [31, 39],
                yellow: [33, 39],
              }),
              (s.styles = { special: 'cyan', number: 'yellow', boolean: 'yellow', undefined: 'grey', null: 'bold', string: 'green', date: 'magenta', regexp: 'red' }),
              (r.isArray = g),
              (r.isBoolean = v),
              (r.isNull = S),
              (r.isNullOrUndefined = E),
              (r.isNumber = b),
              (r.isString = x),
              (r.isSymbol = k),
              (r.isUndefined = I),
              (r.isRegExp = _),
              (r.isObject = w),
              (r.isDate = C),
              (r.isError = O),
              (r.isFunction = N),
              (r.isPrimitive = T),
              (r.isBuffer = t('./support/isBuffer'))
            var P = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
            function R() {
              var e = new Date(),
                t = [L(e.getHours()), L(e.getMinutes()), L(e.getSeconds())].join(':')
              return [e.getDate(), P[e.getMonth()], t].join(' ')
            }
            function D(e, t) {
              return Object.prototype.hasOwnProperty.call(e, t)
            }
            ;(r.log = function () {
              console.log('%s - %s', R(), r.format.apply(r, arguments))
            }),
              (r.inherits = t('inherits')),
              (r._extend = function (e, t) {
                if (!t || !w(t)) return e
                for (var n = Object.keys(t), r = n.length; r--; ) e[n[r]] = t[n[r]]
                return e
              })
          }).call(this, t('_process'), void 0 !== e ? e : 'undefined' != typeof self ? self : 'undefined' != typeof window ? window : {})
        },
        { './support/isBuffer': 10, _process: 14, inherits: 9 },
      ],
      12: [function (e, t, n) {}, {}],
      13: [
        function (e, t, n) {
          ;(function (e) {
            function t(e, t) {
              for (var n = 0, r = e.length - 1; r >= 0; r--) {
                var i = e[r]
                '.' === i ? e.splice(r, 1) : '..' === i ? (e.splice(r, 1), n++) : n && (e.splice(r, 1), n--)
              }
              if (t) for (; n--; n) e.unshift('..')
              return e
            }
            function r(e) {
              'string' != typeof e && (e += '')
              var t,
                n = 0,
                r = -1,
                i = !0
              for (t = e.length - 1; t >= 0; --t)
                if (47 === e.charCodeAt(t)) {
                  if (!i) {
                    n = t + 1
                    break
                  }
                } else -1 === r && ((i = !1), (r = t + 1))
              return -1 === r ? '' : e.slice(n, r)
            }
            function i(e, t) {
              if (e.filter) return e.filter(t)
              for (var n = [], r = 0; r < e.length; r++) t(e[r], r, e) && n.push(e[r])
              return n
            }
            ;(n.resolve = function () {
              for (var n = '', r = !1, a = arguments.length - 1; a >= -1 && !r; a--) {
                var o = a >= 0 ? arguments[a] : e.cwd()
                if ('string' != typeof o) throw new TypeError('Arguments to path.resolve must be strings')
                o && ((n = o + '/' + n), (r = '/' === o.charAt(0)))
              }
              return (
                (r ? '/' : '') +
                  (n = t(
                    i(n.split('/'), function (e) {
                      return !!e
                    }),
                    !r
                  ).join('/')) || '.'
              )
            }),
              (n.normalize = function (e) {
                var r = n.isAbsolute(e),
                  o = '/' === a(e, -1)
                return (
                  (e = t(
                    i(e.split('/'), function (e) {
                      return !!e
                    }),
                    !r
                  ).join('/')) ||
                    r ||
                    (e = '.'),
                  e && o && (e += '/'),
                  (r ? '/' : '') + e
                )
              }),
              (n.isAbsolute = function (e) {
                return '/' === e.charAt(0)
              }),
              (n.join = function () {
                var e = Array.prototype.slice.call(arguments, 0)
                return n.normalize(
                  i(e, function (e, t) {
                    if ('string' != typeof e) throw new TypeError('Arguments to path.join must be strings')
                    return e
                  }).join('/')
                )
              }),
              (n.relative = function (e, t) {
                function r(e) {
                  for (var t = 0; t < e.length && '' === e[t]; t++);
                  for (var n = e.length - 1; n >= 0 && '' === e[n]; n--);
                  return t > n ? [] : e.slice(t, n - t + 1)
                }
                ;(e = n.resolve(e).substr(1)), (t = n.resolve(t).substr(1))
                for (var i = r(e.split('/')), a = r(t.split('/')), o = Math.min(i.length, a.length), s = o, c = 0; c < o; c++)
                  if (i[c] !== a[c]) {
                    s = c
                    break
                  }
                var u = []
                for (c = s; c < i.length; c++) u.push('..')
                return (u = u.concat(a.slice(s))).join('/')
              }),
              (n.sep = '/'),
              (n.delimiter = ':'),
              (n.dirname = function (e) {
                if (('string' != typeof e && (e += ''), 0 === e.length)) return '.'
                for (var t = e.charCodeAt(0), n = 47 === t, r = -1, i = !0, a = e.length - 1; a >= 1; --a)
                  if (47 === (t = e.charCodeAt(a))) {
                    if (!i) {
                      r = a
                      break
                    }
                  } else i = !1
                return -1 === r ? (n ? '/' : '.') : n && 1 === r ? '/' : e.slice(0, r)
              }),
              (n.basename = function (e, t) {
                var n = r(e)
                return t && n.substr(-1 * t.length) === t && (n = n.substr(0, n.length - t.length)), n
              }),
              (n.extname = function (e) {
                'string' != typeof e && (e += '')
                for (var t = -1, n = 0, r = -1, i = !0, a = 0, o = e.length - 1; o >= 0; --o) {
                  var s = e.charCodeAt(o)
                  if (47 !== s) -1 === r && ((i = !1), (r = o + 1)), 46 === s ? (-1 === t ? (t = o) : 1 !== a && (a = 1)) : -1 !== t && (a = -1)
                  else if (!i) {
                    n = o + 1
                    break
                  }
                }
                return -1 === t || -1 === r || 0 === a || (1 === a && t === r - 1 && t === n + 1) ? '' : e.slice(t, r)
              })
            var a =
              'b' === 'ab'.substr(-1)
                ? function (e, t, n) {
                    return e.substr(t, n)
                  }
                : function (e, t, n) {
                    return t < 0 && (t = e.length + t), e.substr(t, n)
                  }
          }).call(this, e('_process'))
        },
        { _process: 14 },
      ],
      14: [
        function (e, t, n) {
          var r,
            i,
            a = (t.exports = {})
          function o() {
            throw new Error('setTimeout has not been defined')
          }
          function s() {
            throw new Error('clearTimeout has not been defined')
          }
          function c(e) {
            if (r === setTimeout) return setTimeout(e, 0)
            if ((r === o || !r) && setTimeout) return (r = setTimeout), setTimeout(e, 0)
            try {
              return r(e, 0)
            } catch (t) {
              try {
                return r.call(null, e, 0)
              } catch (t) {
                return r.call(this, e, 0)
              }
            }
          }
          function u(e) {
            if (i === clearTimeout) return clearTimeout(e)
            if ((i === s || !i) && clearTimeout) return (i = clearTimeout), clearTimeout(e)
            try {
              return i(e)
            } catch (t) {
              try {
                return i.call(null, e)
              } catch (t) {
                return i.call(this, e)
              }
            }
          }
          !(function () {
            try {
              r = 'function' == typeof setTimeout ? setTimeout : o
            } catch (e) {
              r = o
            }
            try {
              i = 'function' == typeof clearTimeout ? clearTimeout : s
            } catch (e) {
              i = s
            }
          })()
          var l,
            p = [],
            f = !1,
            h = -1
          function d() {
            f && l && ((f = !1), l.length ? (p = l.concat(p)) : (h = -1), p.length && y())
          }
          function y() {
            if (!f) {
              var e = c(d)
              f = !0
              for (var t = p.length; t; ) {
                for (l = p, p = []; ++h < t; ) l && l[h].run()
                ;(h = -1), (t = p.length)
              }
              ;(l = null), (f = !1), u(e)
            }
          }
          function m(e, t) {
            ;(this.fun = e), (this.array = t)
          }
          function g() {}
          ;(a.nextTick = function (e) {
            var t = new Array(arguments.length - 1)
            if (arguments.length > 1) for (var n = 1; n < arguments.length; n++) t[n - 1] = arguments[n]
            p.push(new m(e, t)), 1 !== p.length || f || c(y)
          }),
            (m.prototype.run = function () {
              this.fun.apply(null, this.array)
            }),
            (a.title = 'browser'),
            (a.browser = !0),
            (a.env = {}),
            (a.argv = []),
            (a.version = ''),
            (a.versions = {}),
            (a.on = g),
            (a.addListener = g),
            (a.once = g),
            (a.off = g),
            (a.removeListener = g),
            (a.removeAllListeners = g),
            (a.emit = g),
            (a.prependListener = g),
            (a.prependOnceListener = g),
            (a.listeners = function (e) {
              return []
            }),
            (a.binding = function (e) {
              throw new Error('process.binding is not supported')
            }),
            (a.cwd = function () {
              return '/'
            }),
            (a.chdir = function (e) {
              throw new Error('process.chdir is not supported')
            }),
            (a.umask = function () {
              return 0
            })
        },
        {},
      ],
      15: [
        function (e, t, n) {
          var r = e('escodegen').generate
          t.exports = function (e, t) {
            t || (t = {})
            var n = {},
              i = (function e(i, a) {
                if ('Literal' === i.type) return i.value
                if ('UnaryExpression' === i.type) {
                  var o = e(i.argument)
                  return '+' === i.operator ? +o : '-' === i.operator ? -o : '~' === i.operator ? ~o : '!' === i.operator ? !o : n
                }
                if ('ArrayExpression' === i.type) {
                  for (var s = [], c = 0, u = i.elements.length; c < u; c++) {
                    if ((g = e(i.elements[c])) === n) return n
                    s.push(g)
                  }
                  return s
                }
                if ('ObjectExpression' === i.type) {
                  var l = {}
                  for (c = 0; c < i.properties.length; c++) {
                    var p = null === (v = i.properties[c]).value ? v.value : e(v.value)
                    if (p === n) return n
                    l[v.key.value || v.key.name] = p
                  }
                  return l
                }
                if ('BinaryExpression' === i.type || 'LogicalExpression' === i.type) {
                  if ((u = e(i.left)) === n) return n
                  var f = e(i.right)
                  if (f === n) return n
                  var h = i.operator
                  return '==' === h
                    ? u == f
                    : '===' === h
                    ? u === f
                    : '!=' === h
                    ? u != f
                    : '!==' === h
                    ? u !== f
                    : '+' === h
                    ? u + f
                    : '-' === h
                    ? u - f
                    : '*' === h
                    ? u * f
                    : '/' === h
                    ? u / f
                    : '%' === h
                    ? u % f
                    : '<' === h
                    ? u < f
                    : '<=' === h
                    ? u <= f
                    : '>' === h
                    ? u > f
                    : '>=' === h
                    ? u >= f
                    : '|' === h
                    ? u | f
                    : '&' === h
                    ? u & f
                    : '^' === h
                    ? u ^ f
                    : '&&' === h
                    ? u && f
                    : '||' === h
                    ? u || f
                    : n
                }
                if ('Identifier' === i.type) return {}.hasOwnProperty.call(t, i.name) ? t[i.name] : n
                if ('ThisExpression' === i.type) return {}.hasOwnProperty.call(t, 'this') ? t.this : n
                if ('CallExpression' === i.type) {
                  var d = e(i.callee)
                  if (d === n) return n
                  if ('function' != typeof d) return n
                  var y = i.callee.object ? e(i.callee.object) : n
                  y === n && (y = null)
                  var m = []
                  for (c = 0, u = i.arguments.length; c < u; c++) {
                    var g
                    if ((g = e(i.arguments[c])) === n) return n
                    m.push(g)
                  }
                  return d.apply(y, m)
                }
                var v
                if ('MemberExpression' === i.type)
                  return (l = e(i.object)) === n || 'function' == typeof l ? n : 'Identifier' === i.property.type ? l[i.property.name] : (v = e(i.property)) === n ? n : l[v]
                if ('ConditionalExpression' === i.type) return (o = e(i.test)) === n ? n : e(o ? i.consequent : i.alternate)
                if ('ExpressionStatement' === i.type) return (o = e(i.expression)) === n ? n : o
                if ('ReturnStatement' === i.type) return e(i.argument)
                if ('FunctionExpression' === i.type) {
                  var S = i.body.body,
                    E = {}
                  for (
                    Object.keys(t).forEach(function (e) {
                      E[e] = t[e]
                    }),
                      c = 0;
                    c < i.params.length;
                    c++
                  ) {
                    var b = i.params[c]
                    if ('Identifier' != b.type) return n
                    t[b.name] = null
                  }
                  for (var c in S) if (e(S[c]) === n) return n
                  t = E
                  var x = Object.keys(t),
                    k = x.map(function (e) {
                      return t[e]
                    })
                  return Function(x.join(', '), 'return ' + r(i)).apply(null, k)
                }
                if ('TemplateLiteral' === i.type) {
                  var I = ''
                  for (c = 0; c < i.expressions.length; c++) (I += e(i.quasis[c])), (I += e(i.expressions[c]))
                  return (I += e(i.quasis[c]))
                }
                if ('TaggedTemplateExpression' === i.type) {
                  var _ = e(i.tag),
                    w = i.quasi,
                    C = w.quasis.map(e),
                    O = w.expressions.map(e)
                  return _.apply(null, [C].concat(O))
                }
                return 'TemplateElement' === i.type ? i.value.cooked : n
              })(e)
            return i === n ? void 0 : i
          }
        },
        { escodegen: 12 },
      ],
      jsonpath: [
        function (e, t, n) {
          t.exports = e('./lib/index')
        },
        { './lib/index': 5 },
      ],
    },
    {},
    ['jsonpath']
  )('jsonpath'))
export { r as default }
//# sourceMappingURL=/sm/a6e81c405ff5ad398cf956383c74d38d5fb4d41c4376b81508140a1f30fe30fe.map
