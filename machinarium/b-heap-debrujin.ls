
class closure then (@body, @env) ->

cons = (car, cdr) -> { car, cdr }

halt     = \halt        # (halt)
constant = \constant    # (constant obj next)
refer-g  = \refer-g     # (refer-g var next)
refer    = \refer       # (refer rib elt next)
close    = \close       # (close vars body next)
test     = \test        # (test then else)
assign   = \assign      # (assign rib elt next)
conti    = \conti       # (conti next)
nuate    = \nuate       # (nuate s var)
frame    = \frame       # (frame ret next) ???
ret      = \ret         # (ret)
argument = \argument    # (argument next)
apply    = \apply       # (apply)

machina = (code, env = noenv!) ->

  [ a, x, e, r, s ] = [ void, code, null, null, null ]

  loop
    console.log "machina: x:", x #,  "e:", e
    switch x.0
      case halt  =>
        return a
      case constant =>
        [_, a, x] = x
      case refer =>
        [ _, rib, elt, x ] = x
        a = (lookup-rib e, rib)[elt]
      case refer-g =>
        [_, name, x] = x
        a = lookup-global env, name
      case close =>
        [_, body, x] = x
        a = new closure body, e
      case test =>
        x = if a is false then x.2 else x.1
      case assign =>
        [_, rib, elt, x] = x
        (lookup-rib e, rib)[elt] = a
        a = void
      case conti =>
        [_, x] = x
        a = new closure [ nuate, s, 0, 0 ], null
      case nuate =>
        [_, s, rib, elt] = x
        [a, x] = [(lookup-rib e, rib)[elt], [ ret ]]
      case frame =>
        [_, save, x] = x
        [r, s] = [null, (cons {e, r, x: save}, s)]
      case ret =>
        {car: {e, r, x}, cdr: s} = s
      case argument =>
        [[_, x], r] = [x, (cons a, r)]
      case apply =>
        switch
          case (a instanceof closure) =>
            { env: local-env, body: x } = a
            e = extend r, local-env
          case _ => error "not fun: #{a}"
      case _ => error "illegal instruction #{x.0}"

error = (msg) -> throw new Error msg

extend = (rib, env) ->
  arr = []
  while rib isnt null
    arr.push rib.car ; rib := rib.cdr
  cons arr, env

lookup-rib = (env, rib) ->
  while rib-- then env = env.cdr
  env.car

lookup-global = (env, id) ->
  case env.has-own-property id => env[id]
  case _ => throw "undefined global: #{id}"

compile-lookup = (env, id) ->
  for rib, i in env
    for name, j in rib when name is id
      return [i, j]
  return void

compile = (x, env = noenv!, next = [ halt ]) ->

  mk-frame = (save, next) ->
    if save.0 is ret then next else [ frame, save, next ]

#    console.log "compile:", x
  switch typeof! x
    case \String =>
      if ( loc = compile-lookup env, x )
        [ refer, loc.0, loc.1, next ]
      else [ refer-g, x, next ]
    case \Array  =>
      switch x.0
        case \quote  => [ constant, x.1, next ]
        case \lambda =>
          env1 = [ x.1, ...env ]
          body = x[2 to ].reduce-right do
            (next, expr) -> compile expr, env1, next
            [ ret ]
          [ close, body, next ]
        case \if =>
          compile x.1, env, [ test, (compile x.2, env, next), (compile x.3, env, next) ]
        case \set! =>
          if ( loc = compile-lookup env, x.1 )
            compile x.2, env, [ assign, loc.0, loc.1, next ]
          else throw "compile: no such local: #{x.1}"
        case \call/cc =>
          mk-frame next,
            [ conti, [ argument, (compile x.1, env, [ apply ]) ] ]
        case _ =>
          mk-frame next,
            x[1 to ].reduce do
              (next, expr) -> compile expr, env, [ argument, next ]
              compile x.0, env, [ apply ]
    case _ => [ constant, x, next ]

noenv = -> new ->

evaluate = (expr, env) ->
  machina (compile expr), env

module.exports = { machina, compile }


peek = do ->
  require! \util
  (...tags, x) ->
    console.log ...tags, util.inspect x, { +colors, depth: null }

x-x = (code, env) ->
  asm = compile code
  res = machina asm
  peek "compiled:\n", asm
  peek "eval:\n", res

tests =

  app:
    [[\lambda [\app \x]
      [\set! \x
        [\app
          [\lambda [\x] \x]
          [\quote \victory!]]]
      [\if false
        [\set! \x 133]
        [\set! \x \x]]
      [\app
        [\lambda [\k] \k]
        \x]]
     [\lambda [\f \a] [\f \a]]
     999]

  deep-app:
    [[\lambda [\x]
      [[\lambda []
        [[\lambda [\y]
           [\set! \x \y]]
         1]]]
      \x]
     666]

  multiarg:
    [[\lambda [\x]
       [[\lambda [\a \b]
          [\set! \x \a]
          [\set! \x \b]]
        1 2]
        \x]
     [\quote \kanarinac]]

  cont :
    [[\lambda [\x]
      [\call/cc
        [\lambda [\cc]
          [\cc [\quote \escapeth!]]
          [\set! \x 666]]]
      \x]
     [\quote \iz-good]]


for progn, prog of tests
  console.log "\n\n   [ #progn ]\n"
  peek prog
  x-x prog

