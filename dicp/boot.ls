{ prim, closure } = require \./repr

default-env = ->

  '+': prim (a, b) -> a + b
  '-': prim (a, b) -> a - b
  '*': prim (a, b) -> a * b
  '/': prim (a, b) -> a / b
  '<': prim (a, b) -> a < b
  '>': prim (a, b) -> a > b
  '=': prim (a, b) -> a === b

  'zero?': prim (x) -> x === 0
  'add1' : prim (x) -> x + 1
  'sub1' : prim (x) -> x - 1

  'p': prim (...xs) -> console.log "[ * ] ", ...xs

  'void': prim -> void

  'array'  : prim (...xs) -> xs
  'array+?': prim (x) -> typeof! x is \Array and x.length > 0
  'null?'  : prim (x) -> typeof! x is \Array and x.length is 0
  'cons'   : prim (x, xs) -> [x].concat xs
  'head'   : prim (x) -> x[0]
  'car'    : prim (x) -> x[0]
  'tail'   : prim (x) -> x[1 to]
  'cdr'    : prim (x) -> x[1 to]
  'cadr'   : prim (x) -> x[1]
  'caddr'  : prim (x) -> x[2]

  'dict' : prim (...xs) -> list-to-obj xs

  'not'  : prim (x) -> !x

  'id'   : closure [\x] [\x]

default-macro-env = ->

  'begin': prim (expr) ->
    case expr.length <= 2 => expr[1]
    case _                => [[\fun [] ...expr[1 to]]]

  'let'  : prim (expr) ->
    case typeof! expr[1] is \String =>
      [ _, name, binds, ...body ] = expr
      [ \letrec [[name, [\fun (binds |> map (.0)), ...body]]]
        [ name, ...(binds |> map (.1)) ]]
    case _ =>
      [ _, binds, ...body ] = expr
      [ [\fun, (map (.0), binds), ...body ]
        ...(map (.1), binds) ]

  'letrec' : prim (expr) ->
    [ _, binds, ...body ] = expr
    [ \let (binds |> map ([n, _]) -> [n, [\void]]),
      ...(binds |> map ([n, expr]) -> [\set, n, expr]),
      ...body ]

  'cond' : prim ([_, clause, ...clauses]) ->
    case not clause =>
    case clause.0 is \else => [\begin ...clause[1 to ]]
    case clause.1 is \=>   =>
      [\let [[\xxx clause.0]]
        [\if \xxx [clause.2, \xxx] [\cond ...clauses]]]
    case _ => [\if clause.0, [\begin ...clause[1 to ]], [\cond ...clauses]]

module.exports = { default-env, default-macro-env }
