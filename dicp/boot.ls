{ prim, closure } = require \./repr

# The initial environment, populated by various handy procedures, mostly
# primitives.
#
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

  'gensym' : prim gensym

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

  'now'  : prim -> new Date!get-time!

# The initial collection of macros. Macros are required to be procedure-like
# objects from the perspective of the hosted interpreter; due to sheer lazyness
# on my part, they are expressed mostly as prims, bits of code for the _hosting_
# interpreter, and not as closures, bits of code for the _hosted_ one.  Although
# either would do.
#
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
      name = gensym!
      [\let [[name, clause.0]]
        [\if name, [clause.2, name] [\cond ...clauses]]]
    case _ => [\if clause.0, [\begin ...clause[1 to ]], [\cond ...clauses]]

  'time' : prim ([_, ...exprs]) ->
    [ t0, res, t1 ] = [ gensym!, gensym!, gensym! ]
    [\let [[t0,  [\now]]
           [res, [\begin ...exprs]]
           [t1,  [\now]]]
      [\p [\quote "real time:"] [\- t1, t0] [\quote "ms"]], res]

gensym = do -> n = 0 ; -> "gen-#{n++}"

module.exports = { default-env, default-macro-env }
