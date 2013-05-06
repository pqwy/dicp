
require! \util

halt         = \halt
refer-local  = \refer-local
refer-free   = \refer-free
refer-global = \refer-global
indirect     = \indirect
constant     = \constant
close        = \close
box          = \box
test         = \test
assign-local = \assign-local
assign-free  = \assign-free
conti        = \conti
nuate        = \nuate
frame        = \frame
argument     = \argument
shift        = \shift
apply        = \apply
apply-native = \apply-native
ret          = \ret


class closure then (@body, @env, @arity) ->

class cell then (@val) ->

pop-n = (s, n) ->
  for x from 0 til n then s.pop!

noenv = -> new ->

machina = (code, env = noenv!) ->

  [ a, x, f, c, s ] = [ null, code, 0, null, [] ]

  loop

#      console
#        ..log ''
#        ..log " * a:", a
#        ..log " * s:", s
#        ..log " * f:", f
#        ..log " * c:", c
#        ..log "** x:", x

    switch x.0
      case halt =>
        return a
      case refer-local =>
        [ _, n, x ] = x
        a = s[ f - n ]
      case refer-free =>
        [ _, n, x ] = x
        a = c.env[n]
      case refer-global =>
        [ _, name, x ] = x
        if env.has-own-property name
          a = env[name]
        else throw "undefined: #name"
      case indirect =>
        [ _, x ] = x
        a = a.val
      case constant =>
        [ _, a, x ] = x
      case close =>
        [ _, n, args, body, x ] = x
        a = new closure body, (pop-n s, n), args
      case box =>
        [ _, n, x ] = x
        k = s.length - 1 - n
        s[k] = new cell s[k]
      case test =>
        [ _, cons, alt ] = x
        x = if a != false then cons else alt
      case assign-local =>
        [ _, n, x ] = x
        s[f - n].val = a
        a = void
      case assign-free =>
        [ _, n, x ] = x
        c.env[n].val = a
        a = void
      case conti =>
        [ _, x ] = x
        a = new closure [ refer-local, 0, [ nuate, s[ to ], [ ret, 0 ] ] ], [], 1
      case nuate =>
        [ _, stack, x ] = x
        s = stack[ to ]
      case frame =>
        [ _, save, x ] = x
        s.push c, f, save
      case argument =>
        [ _, x ] = x
        s.push a
      case shift =>
        [ _, n, m, x ] = x
        s.splice (s.length - n - m), m
      case apply =>
        if a instanceof closure
          [ _, n ] = x
          if a.arity is n
            [ x, f, c ] = [ a.body, (s.length - 1), a ]
          else throw "arity mismatch: given #{n} expected: #{a.arity}; #{util.inspect a}"
        else throw "not fun: #{util.inspect a}"
      case apply-native =>
        if a instanceof Function
          [ _, n, x ] = x
          a = a.apply null, (pop-n s, n)
        else throw "not js fun: #a"
      case ret =>
        [ _, n ] = x
        s.splice (s.length - n)
        x := s.pop!
        f := s.pop!
        c := s.pop!
      case _ => throw "unknown instruction: #x"

free-set = (x, bound = {}) ->
  switch typeof! x
    case \String =>
      if x of bound then {} else { +"#x" }
    case \Array =>
      switch x.0
        case \quote   => {}
        case \lambda  =>
          free-set x[2 to ], (bound `merge` {[v, true] for v in x.1})
        case \if      =>
          (free-set x.1, bound) `merge` (free-set x.2, bound) `merge` (free-set x.3, bound)
        case \set!    =>
          free-set x[1 to ], bound
        case \call/cc =>
          free-set x.1, bound
        case \call/native =>
          free-set x[1 to ], bound
        case _ =>
          x |> foldl ((a, x) -> (free-set x, bound) `merge` a), {}
    case _ => {}

mutable-set = (x, vars = {}) ->
  switch typeof! x
    case \Array  =>
      switch x.0
        case \quote   => {}
        case \lambda  =>
          {[v, true] for v, _ of mutable-set x[2 to ], vars when v not in x.1}
        case \if      =>
          (mutable-set x.1, vars) `merge` (mutable-set x.2, vars) `merge` (mutable-set x.3, vars)
        case \set!    =>
          (mutable-set x.2, vars) `merge` (if x.1 of vars then { +"#{x.1}" } else {})
        case \call/cc =>
          mutable-set x.1, vars
        case \call/native =>
          mutable-set x[1 to ], vars
        case _ =>
          x |> foldl ((a, x) -> (mutable-set x, vars) `merge` a), {}
    case _ => {}

merge = (...dicts) ->
  o = {}
  for d in dicts
    for k, v of d then o[k] = v
  o

collect-free = (vars, e, next) ->
  vars.reduce do
    (next, v) -> static-refer v, e, [ \argument, next ]
    next

static-refer = (x, e, next) ->
  case x in e[]local => [ \refer-local, e.local.index-of(x), next ]
  case x in e[]free  => [ \refer-free , e.free .index-of(x), next ]
  case _ => [ \refer-global, x, next]

static-assign = (id, e, next) ->
  case id in e[]local => [ \assign-local, e.local.index-of(id), next ]
  case id in e[]free  => [ \assign-free , e.free .index-of(id), next ]
  case _ => throw "set!: not a local variable: #id"

env-contains = (x, e) -> x in e[]local or x in e[]free

make-boxes = (sets, vars, next) ->
  [[v, n] for v, n in vars when v of sets]
    .reduce-right ((next, [v, n]) -> [ box, n, next ] ), next

compile = (x, e = {}, s = {}, next = [ halt ]) ->
  switch typeof! x
    case \String =>
      static-refer x, e,
        if s[x] then [ \indirect, next ] else next
    case \Array =>
      switch x.0
        case \quote =>
          [ \constant, x.1, next ]
        case \lambda =>

          varset  = {[v, true] for v in x.1}
          freeset = {[v, true] for v of (free-set x[2 to ], varset)
                               when env-contains v, e}
          mutset  = mutable-set x[2 to ], varset
          freevec = [v for v, _ of freeset]

          new-e = local: x.1, free: freevec
          new-s = mutset `merge` {[v, true] for v, _ of freeset when v of s}

          collect-free freevec, e, [
            close
            (length freeset)
            (length varset)
            make-boxes do
              mutset, x.1
              x[2 to ].reduce-right do
                (next, x) -> compile x, new-e, new-s, next
                [ ret, x.1.length ]
            next ]
        case \if =>
          compile x.1, e, s, [
            test
            compile x.2, e, s, next
            compile x.3, e, s, next
          ]
        case \set! =>
          compile x.2, e, s, (static-assign x.1, e, next)
        case \call/cc =>
          call = [ conti, [ argument, compile x.1, e, s,
                    if next.0 isnt ret then [ apply, 1 ]
                    else [ shift, 1, next.1, [ apply, 1 ] ] ] ]
          if next.0 is ret then call else [ frame, next, call ]
        case \call/native =>
          x[2 to ].reduce do
            * (next, expr) -> compile expr, e, s, [ argument, next ]
            * compile x.1, e, s, [ apply-native, (x.length - 2), next ]
        case _ =>
          args    = x.length - 1
          callseq = x[1 to ].reduce do
            * (next, expr) -> compile expr, e, s, [ argument, next ]
            * compile x.0, e, s,
                if next.0 isnt ret then [ apply, args ]
                else [ shift, (x.length - 1), next.1, [ apply, args ] ]
          if next.0 is ret then callseq
          else [ frame, next, callseq ]
    case _ => [ \constant, x, next ]


module.exports = { mutable-set, free-set, static-refer, collect-free, make-boxes, compile }

peek = (x) ->
  require! \util
  console.log util.inspect x, {+colors, depth: null}

#  peek compile [\lambda [\a \b] [\lambda [\c] \a]]
#  peek compile [\lambda [\a \b] [\lambda [\c] [\set! \c 19]]]
#  peek compile [\lambda [\a \b] [\lambda [\c] [\set! \a 19] \a \b]]

#  peek compile [[\lambda [\a \b] [\lambda [\c \d] \a]] 1 2]
#  peek compile [\mrr 1 2]
#  peek compile [\lambda [\a \b] [\lambda [\c \d] \a \b]]
#  peek compile [[\lambda [\a] [\if false [\set! \a 1] \a] \a] 99]

class cons then (@car, @cdr) ->

e1 =
  \p     : (...xs) -> console.log "MACHINA > ", ...xs
  \null? : -> it is null
  \null  : null
  \cons  : (a, b) -> new cons a, b
  \car   : -> it.car
  \cdr   : -> it.cdr
  \+     : (a, b) -> a + b
  \-     : (a, b) -> a - b
  \add1  : -> it + 1
  \sub1  : -> it - 1
  \*     : (a, b) -> a * b
  \/     : (a, b) -> a - b

e2 = do ->

  fn = (n, f) ->
    new closure [ \constant, f, [ \apply-native, n, [ \ret 0 ] ] ], [], n

  \p     : fn 1, -> console.log "MACHINA > ", it
  \id    : fn 1, -> it
#    \id    : new closure [refer-local, 0, [ ret, 1 ] ], [], 1
  \null? : fn 1, -> it is null
  \null  : null
  \cons  : fn 2, (a, b) -> new cons a, b
  \car   : fn 1, -> it.car
  \cdr   : fn 1, -> it.cdr
  \+     : fn 2, (a, b) -> a + b
  \-     : fn 2, (a, b) -> a - b
  \add1  : fn 1, -> it + 1
  \sub1  : fn 1, -> it - 1
  \*     : fn 2, (a, b) -> a * b
  \/     : fn 2, (a, b) -> a - b
  \positive? : fn 1, -> it >= 0
  \zero? : fn 1, -> it is 0
  \<     : fn 2, (a, b) -> a < b
  \>     : fn 2, (a, b) -> a > b

#  peek compile do
#      [\call/native \p [\simo 1] [\tamo 2]]

#        [[\lambda [\a \b]
#          [\set! \a 11]
#          \a]
#  #          [\call/js \p [\quote \pre] \a \b [\quote \post]]]
#          20 30]

time = (f) ->
  t0 = new Date!get-time!
  r  = f!
  t1 = new Date!get-time!
  console.log "[time] #{t1 - t0} ms"
  r

derp = (x, e = e2) ->
  asm = time -> compile x
  res = time -> machina asm, e
  peek asm
  peek res

#  #  peek compile [\lambda [] [\call/native \p [\quote \desu] 2]]
#  derp [[\lambda [\a]
#          [\call/native \cons
#            \a
#            [\call/native \cons
#              [[\lambda [] [\set! \a 7] \a]]
#              [\call/native \cons
#                \a
#                \null]]]]
#          99]

#  derp [[\lambda [\f]
#          [\call/native \cons \f [\f 99]]]
#        [\lambda [\a]
#          [\lambda [\b]
#            [\set! \a [\call/native \+ \a \b]]
#            \a]]]

#  derp do
#    [[\lambda [\f]
#      [\set! \f
#        [\lambda [\n \a]
#          [\if [\> \n 30000] \a
#            [\f [\add1 \n] [\+ \n \a]]]]]
#      [\f 0 0]]
#     void]

#  derp do
#    [[[\lambda [\a \b]
#        [\lambda []
#          [\- \a \b]]]
#      2 1]]

#  derp do
#    [[\lambda []
#      [\call/cc
#        [\lambda [\cc]
#          [\p 111]
#          [\cc 19]
#          [\p 222]
#          333 ]]]]

#  derp do
#    [[\lambda [\a \b]
#      [\+ \a [\+ \b \a]]]
#      1 2]

#  derp do
#    [[\lambda [\a \c]
#      [\set! \a
#        [\+ \a
#          [\call/cc
#            [\lambda [\cc] [\set! \c \cc] 0]]]]
#      [\if [\< \a 5] [\c 1] \a]]
#     0 \null]



#  derp do
#    [[\lambda [\x]
#      [[\lambda [\fun]
#         [\fun 1] [\fun 3]]
#       [\lambda [\y] [\set! \x [\+ \x \y]]]]
#       \x]
#      13]

#  console.log ">>",
#    machina do
#      compile do
#        [[\lambda [\a \b]
#          [\set! \a 11]
#          [\call/js \p [\quote \pre] \a \b [\quote \post]]]
#        20 30]



#        [[\lambda [\acc] [\acc 2] [\acc 3]]
#         [[\lambda [\a] [\lambda [\x] [\set! \a \x] \a]] 1]]
#        [[\lambda [\a]
#          [\if \a [\set! \a 1] \a]
#          \a]
#         99]
#      e1

