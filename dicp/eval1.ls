# 
# Interpreter The First,
#
# In which we encode programs in simplified javascript as primitive javascript
# structures, and obtain a working metacircular interpreter. The code is
# unreadable, however, since we use dictionaries to represent nodes.
#

{ prim, closure, assign } = require \./repr
{ default-env }           = require \./boot

eval1 = (expr, env = default-env!) ->
  switch typeof! expr
    case \String =>
      if env[expr]? then env[expr] else throw "undefined: #expr"
    case \Object =>
      switch expr.type
        case \if  =>
          if eval1 expr.p, env
            eval1 expr.c, env
          else eval1 expr.a, env
        case \app => apply1 do
          (eval1 expr.op, env)
          map (eval1 _, env), expr.args
        case \function => closure expr.args, expr.body, env
        case \quote    => expr.stuff
        case \var      => env[expr.id] = eval1 expr.val, env
        case \set      => assign env, expr.id, eval1 expr.val, env
    case \Array => map (eval _, env) expr.xs
    case _ => expr

apply1 = (fun, args) ->
  case fun instanceof prim    =>
    fun.fun.apply null, args
  case fun instanceof closure =>
    env = fun.env with list-to-obj (zip fun.args, args)
    fun.body |> map -> eval1 it, env
             |> last
  case _ => throw "not fun: #fun"


###
module.exports = eval: (expr, { env } = {}) -> eval1 expr, env

require \tester .conforms-to eval1, \programs1
