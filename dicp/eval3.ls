#
# Interpreter The Third,
#
# In which we add meta-code for the first time and suddenly enjoy a much richer
# language.
#

{ prim, closure, assign }          = require \./repr
{ default-env, default-macro-env } = require \./boot

eval3 = (expr, env = default-env!) ->
  switch typeof! expr
    case \String =>
      if env[expr]? then env[expr] else throw "undefined: #expr"
    case \Array  =>
      switch expr[0]
        case \if =>
          if eval3 expr[1], env
            eval3 expr[2], env
          else eval3 expr[3], env
        case \fun   => closure expr[1], expr[2 to], env
        case \quote => expr[1]
        case \var   => env[expr[1]] = eval3 expr[2], env
        case \set   => assign env, expr[1], eval3 expr[2], env
        case _      => apply1 do
          eval3 expr[0], env
          map (eval3 _, env), expr[1 to]
    case \Object => {[k, eval v, env] for k, v of expr}
    case _ => expr

apply1 = (fun, args) ->
  case fun instanceof prim =>
    fun.fun.apply null, args
  case fun instanceof closure =>
    env = fun.env with list-to-obj (zip fun.args, args)
    fun.body |> map -> eval3 it, env
             |> last
  case _ => throw "not fun: #fun"

expand = (expr, env = default-env!, menv = default-macro-env!) ->
  switch typeof! expr
    case \Array =>
      switch expr[0]
        case \fun =>
          [ env1, menv1 ] = [ env with {}, menv with {} ]
          [\fun, expr[1], ...(map (expand _, env1, menv1), expr[2 to] ) ]
        case \add-expander => menv[expr[1]] = eval3 expr[2], env ; void
        case \quote        => expr
        case _ =>
          if (expander = menv[expr[0]])?
            expand (apply1 expander, [expr]), env, menv
          else map (expand _, env, menv), expr
    case \Object => {[k, expand v, env, menv] for k, v of expr}
    case _       => expr

full-eval = (expr) -> eval3 expand expr

###
module.exports = eval: eval3, expand: expand

require \tester .conforms-to full-eval, \programs2, \programs3


