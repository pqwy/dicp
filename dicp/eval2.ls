# 
# Interpreter The Second,
#
# In which merely switch the encoding to arrays, and obtain something eerily
# similar to lisp.
#

{ prim, closure, assign } = require \./repr
{ default-env }           = require \./boot

eval2 = (expr, env = default-env!) ->
  switch typeof! expr
    case \String =>
      if env[expr]? then env[expr] else throw "undefined: #expr"
    case \Array  =>
      switch expr[0]
        case \if =>
          if eval2 expr[1], env
            eval2 expr[2], env
          else eval2 expr[3], env
        case \fun   => closure expr[1], expr[2 to], env
        case \quote => expr[1]
        case \var   => env[expr[1]] = eval2 expr[2], env
        case \set   => assign env, expr[1], eval2 expr[2], env
        case _      => apply1 do
          eval2 expr[0], env
          map (eval2 _, env), expr[1 to]
    case \Object => {[k, eval v, env] for k, v of expr}
    case _ => expr

apply1 = (fun, args) ->
  case fun instanceof prim =>
    fun.fun.apply null, args
  case fun instanceof closure =>
    env = fun.env with list-to-obj (zip fun.args, args)
    fun.body |> map -> eval2 it, env
             |> last
  case _ => throw "not fun: #fun"

###
module.exports = eval: (expr, { env } = {}) -> eval2 expr, env

require \tester .conforms-to eval2, \programs2
