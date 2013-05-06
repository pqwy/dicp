
class closure then (@vars, @body, @env) ->

cons = (car, cdr) -> { car, cdr }

halt     = \halt        # (halt)
constant = \constant    # (constant obj next)
refer    = \refer       # (refer var next)
close    = \close       # (close vars body next)
test     = \test        # (test then else)
assign   = \assign      # (assign var next)
conti    = \conti       # (conti next)
nuate    = \nuate       # (nuate s var)
frame    = \frame       # (frame ret next) ???
ret      = \ret         # (ret)
argument = \argument    # (argument next)
apply    = \apply       # (apply)

machina = (code, env = noenv!) ->

  [ a, x, e, r, s ] = [ void, code, env, null, null ]

  loop
    console.log "machina: x:", x #, "e:", e
    switch x.0
      case halt  =>
        return a
      case constant =>
        [_, a, x] = x
      case refer =>
        [_, name, x] = x
        a = (lookup e, name)[name]
      case close =>
        [_, vars, body, x] = x
        a = new closure vars, body, e
      case test =>
        x = if a is false then x.2 else x.1
      case assign =>
        [_, name, x] = x
        [a, (lookup e, name)[name]] = [void, a]
      case conti =>
        [_, x] = x
        a = new closure [\v], [ nuate, s, \v ], {}
      case nuate =>
        [_, s, name] = x
        [a, x] = [(lookup e, name)[name], [ ret ]]
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
            { vars, env, body: x } = a
            e = Object.create a.env
            for name in a.vars
              if r is null then error "arity"
              {car: e[name], cdr: r} = r
            unless r is null then "arity"
          case _ => error "not fun: #{a}"
      case _ => error "illegal instruction #{x.0}"

error = (msg) -> throw new Error msg

lookup = (env, id) ->
  while not env.has-own-property id
    if env is {}.__proto__ then error "not defined: #{id}"
    env = env.__proto__
  env

compile = (x, next = [ halt ]) ->

  mk-frame = (save, next) ->
    if save.0 is ret then next else [ frame, save, next ]

#    console.log "compile:", x
  switch typeof! x
    case \String => [ refer, x, next ]
    case \Array  =>
      switch x.0
        case \quote  => [ constant, x.1, next ]
        case \lambda =>
          body = x[2 to ].reduce-right do
            (next, expr) -> compile expr, next
            [ ret ]
          [ close, x.1, body, next ]
        case \if =>
          compile x.1, [ test, (compile x.2, next), (compile x.3, next) ]
        case \set! =>
          compile x.2, [ assign, x.1, next ]
        case \call/cc =>
          mk-frame next,
            [ conti, [ argument, (compile x.1, [ apply ]) ] ]
        case _ =>
          mk-frame next,
            x[1 to ].reduce do
              (next, expr) -> compile expr, [ argument, next ]
              compile x.0, [ apply ]
    case _ => [ constant, x, next ]

noenv = -> new ->

evaluate = (expr) -> machina compile expr, [ halt ]

module.exports = { machina, compile }


peek = do ->
  require! \util
  (...tags, x) ->
    console.log ...tags, util.inspect x, { +colors, depth: null }

x-x = (code, env) ->
  asm = compile code
  peek "compiled:", asm
  peek "eval:", machina asm, env
  peek "env:", env

tests =
  apply:
    [[\lambda [\app]
      [\set! \x
        [\app
          [\lambda [\x] \x]
          [\quote \victory!]]]
      [\if false
        [\set! \x 133]
        [\set! \x \x]]
      [\app
        [\lambda [\k] \k]
        [\quote \desu]]]
     [\lambda [\f \a] [\f \a]]]

  multiarg:
    [[\lambda [\a \b]
      [\set! \x \a]
      [\set! \x \b]
      \x]
     1 2]

  cont :
    [\call/cc
      [\lambda [\cc]
        [\cc [\quote \escapeth]]
        [\set! \x 666]]]


x-x do
  tests.apply
#  peek compile do
  {x: 19}

