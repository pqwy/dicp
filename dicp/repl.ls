# 
# A JSON repl. Best run via rlwrap.
#

require! \util

repl = (evaluator) ->

  [ buffer, cont ] = [ "", false ]

  [ env, meta-env, macro-env ]  = do ->
    require! \boot
    [ boot.default-env!, boot.default-env!, boot.default-macro-env! ]

  process.stdin.on \data, !(buf) ->
    str = buf.to-string \utf8
    switch m = str.match /\n/
      case null => buffer := buffer + str
      case _    =>
        input   = buffer + str.substring 0, m.index
        buffer := str.substring (m.index + 1)
        cont   := false
        try
          prog   = JSON.parse input
          res    = evaluator prog, { env, meta-env, macro-env }
          console.log util.inspect res, {+colors, depth: null}
        catch e =>
          if e.type is 'unexpected_eos'
            cont   := true
            buffer := input + buffer
          else
            console.error "error:", e
        write-prompt!

  write-prompt = ->
    process.stdout.write do
      if cont then "...    > " else "[DICP] > "

  write-prompt!


mod = require \eval5
repl mod.eval

