# 
# A JSON repl. Best run via rlwrap.
#

require! [ \util, \boot ]

repl = (evaluator) ->

  buffer = []

  [ env, meta-env, macro-env ]  =
    [ boot.default-env!, boot.default-env!, boot.default-macro-env! ]

  process.stdin.on \data, !(buf) ->
    buf.to-string \utf8 .split '\n' |> each (line) ->
      try
        console.log do
          util.inspect do
            evaluator do
              JSON.parse [...buffer, line].join ' '
              { env, meta-env, macro-env }
            { +colors, depth: null }
        buffer := []
      catch e =>
        if e.type is 'unexpected_eos'
          buffer.push line unless line.match /^\s*$/
        else
          buffer := []
          console.error 'error:', e
    prompt!

  prompt = -> process.stdout.write do
    if buffer.length then "...    > "
    else                  "[DICP] > "

  prompt!

mod = require \eval5
repl mod.eval

