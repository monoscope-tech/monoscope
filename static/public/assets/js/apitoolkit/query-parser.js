// const { Parjs } = require("parjs");
// const P = Parjs;

// Helper
const ws = whitespace.many(),
  ignoreWs = p => ws.then(p).then(ws)

// Parsers
const identifier = ignoreWs(regex(/[a-zA-Z_][a-zA-Z0-9_]*/)).map(r => r[0])
const pathPart = regex(/[a-zA-Z0-9_\-]+(\[\]|\[[0-9-]+\])?/)
const path = pathPart.then(str('.').then(pathPart).many()).map(([f, r]) => [f, ...r.map(p => p[1])].join('.'))
const number = ignoreWs(regex(/-?\d+/)).map(n => parseInt(n[0], 10))
const stringLit = ignoreWs(regex(/"(?:\\["\\]|[^\n"\\])*"/)).map(s => s[0].slice(1, -1))
const operator = ignoreWs(
  alt(
    str('=').map(() => '='),
    str('ANY'),
    str('EXISTS'),
  ),
)
const value = stringLit.or(number)
const arrayValues = ignoreWs(value.then(str(',').then(value).many()).map(([f, r]) => [f, ...r.map(v => v[1])]))

// Logical Operators
const [and, or, not, lp, rp, lb, rb] = ['AND', 'OR', 'NOT', '(', ')', '[', ']'].map(op => ignoreWs(str(op)))

// Expressions
const expression = seq(path, operator, alt(arrayValues.between(lb, rb), value, succeed(undefined))).map(([p, op, v]) =>
  op === 'EXISTS' ? [p, 'EXISTS'] : [p, op, v],
)

// Conditions
const condition = lazy(() =>
  alt(
    expression,
    not.then(condition).map(c => ({ NOT: c })),
    conditionPart,
  ),
)
const andCondition = seq(condition, and, condition).map(([l, , r]) => ({ AND: [l, r] }))
const orCondition = seq(condition, or, condition).map(([l, , r]) => ({ OR: [l, r] }))
const groupedCondition = lp.then(condition).then(rp)
const conditionPart = lazy(() => alt(andCondition, orCondition, groupedCondition))

// Main Parser
const queryParser = condition

// Parsing Function
function parseQuery(query) {
  const result = queryParser.parse(query.trim())
  return result.kind === 'OK' ? result.value : (console.error('Parsing failed:', result.reason), null)
}

// Example Usage
const query = `
  request_body.v1.v2 = "abc" AND (
    request_body.v3.v4 = 123 OR
    request_body.v5[].v6 = ANY[1,2,3] OR
    request_body[1].v7 OR
    NOT request_body[-1].v8
  )
`
