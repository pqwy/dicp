#
# Interpreter The Fourth,
#
# In which we recover constant-space tail-calls by using lazy evaluation.
#

{ prim, closure, assign }          = require \./repr
{ default-env, default-macro-env } = require \./boot

class delay then (@thunk) ~>

force = (x) ->
  while x instanceof delay then x = x.thunk!
  x

eval4 = (expr, env = default-env!) ->
  switch typeof! expr
    case \String =>
      if env[expr]? then env[expr] else throw "undefined: #expr"
    case \Array  =>
      switch expr[0]
        case \if =>
          if force (eval4 expr[1], env)
            eval4 expr[2], env
          else eval4 expr[3], env
        case \fun   => closure expr[1], expr[2 to], env
        case \quote => expr[1]
        case \var   => env[expr[1]] = force (eval4 expr[2], env)
        case \set   => assign env, expr[1], force (eval4 expr[2], env)
        case _      => delay ->
          apply2 do
            force (eval4 expr[0], env)
            expr[1 to ] |> map -> force (eval4 it, env)
    case \Object => {[k, force (eval4 v, env)] for k, v of expr}
    case _ => expr

apply2 = (fun, args) ->
  case fun instanceof prim =>
    fun.fun.apply null, args
  case fun instanceof closure =>
    env = fun.env with list-to-obj (zip fun.args, args)
    [ ...exprs, expr ] = fun.body
    exprs |> each -> force (eval4 it, env)
    eval4 expr, env
  case _ => throw "not fun: #fun"

expand = (expr, env = default-env!, menv = default-macro-env!) ->
  switch typeof! expr
    case \Array =>
      switch expr[0]
        case \fun =>
          [ env1, menv1 ] = [ env with {}, menv with {} ]
          [\fun, expr[1], ...(map (expand _, env1, menv1), expr[2 to] ) ]
        case \add-expander => menv[expr[1]] = force (eval4 expr[2], env) ; void
        case \quote        => expr
        case _ =>
          if (expander = menv[expr[0]])?
            expand (force apply2 expander, [expr]), env, menv
          else map (expand _, env, menv), expr
    case \Object => {[k, expand v, env, menv] for k, v of expr}
    case _       => expr

full-eval = (expr) -> force eval4 expand expr

###
module.exports = eval: full-eval, expand: expand

require \tester .conforms-to full-eval, \programs2, \programs3, \programs4
