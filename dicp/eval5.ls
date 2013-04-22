#
# Interpreter The Fifth,
#
# In which we build on the tail-calls to CPS the interpreter and enjoy some
# delimited continuations.
#

{ prim, closure, assign }          = require \./repr
{ default-env, default-macro-env } = require \./boot

class delay then (@thunk) ~>

force = (x) ->
  while x instanceof delay then x = x.thunk!
  x

k-map = (k-fun, k, xs) ->
  ys       = if typeof! xs is \Array then [] else {}
  n        = length xs
  keyspace = keys xs
  go = (i) ->
    if i is n then k ys else
      y <- k-fun xs[keyspace[i]]
      ys[keyspace[i]] = y ; go i + 1
  go 0

eval-exprs = (exprs, env, k) ->
  k-map ((e, k) -> eval5 e, env, k), k, exprs

eval5 = (expr, env = default-env!, k = id) ->
  delay ->
    switch typeof! expr
      case \String =>
        if env[expr]? then k env[expr] else throw "undefined: #expr"
      case \Array  =>
        switch expr[0]
          case \if =>
            x <- eval5 expr[1], env
            if x then eval5 expr[2], env, k
            else eval5 expr[3], env, k
          case \fun   => k closure expr[1], expr[2 to], env
          case \quote => k expr[1]
          case \var   =>
            eval5 expr[2], env, (x) -> env[expr[1]] = x ; k!
          case \set   =>
            eval5 expr[2], env, (x) -> assign env, expr[1], x ; k!
          case \shift =>
            f <- eval5 expr[1], env
            apply3 f, [new prim k], id
          case \reset => k force (eval5 expr[1], env, id)
          case _      =>
            f    <- eval5 expr[0], env
            args <- eval-exprs expr[1 to ], env
            apply3 f, args, k
      case \Object => eval-exprs expr, env, k
      case _ => k expr

apply3 = (fun, args, k = id) ->
  case fun instanceof prim =>
    k fun.fun.apply null, args
  case fun instanceof closure =>
    env = fun.env with list-to-obj (zip fun.args, args)
    xs <- eval-exprs fun.body, env
    delay -> k last xs
  case _ => throw "not fun: #fun"

expand = (expr, env = default-env!, menv = default-macro-env!) ->
  switch typeof! expr
    case \Array =>
      switch expr[0]
        case \fun =>
          [ env1, menv1 ] = [ env with {}, menv with {} ]
          [\fun, expr[1], ...(map (expand _, env1, menv1), expr[2 to] ) ]
        case \add-expander => menv[expr[1]] = force (eval5 expr[2], env) ; void
        case \quote        => expr
        case _ =>
          if (expander = menv[expr[0]])?
            expand (force apply3 expander, [expr]), env, menv
          else map (expand _, env, menv), expr
    case \Object => {[k, expand v, env, menv] for k, v of expr}
    case _       => expr

full-eval = (expr) -> force eval5 expand expr

###
module.exports = eval: full-eval, expand: expand

require \tester .conforms-to full-eval, \programs2, \programs3, \programs4, \programs5
