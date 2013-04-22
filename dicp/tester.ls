
require! \util
require! \assert .deep-equal

eval-display-progs = !(evaluate, progs) ->
  for progn, prog of progs
    console.log "++ #progn :"
    console.log util.inspect prog, {+colors, depth: null}
    res = evaluate prog
    console.log " =", util.inspect res, {+colors, depth: null}
    console.log!

eval-progs = !(evaluate, progs) ->
  for progn, prog of progs
    try
      result = evaluate prog
      result `deep-equal` prog.result if prog.result?
    catch e =>
      console.error "\nerror when evaluating #progn:"
      throw e

conforms-to = !(evaluate, ...modules) ->
  process.stdout.write "checking... "
  for module in modules
    eval-progs evaluate, require module
  console.log "win."

res = (result, prog) --> prog.result = result ; prog

trace = do ->
  d = 0
  (n, fun) ->
    (...xs) ->
      pad = ' ' * d * 2
      console.log "#pad #n +", ...xs
      r = try ( d++ ; fun ...xs ) finally d--
      console.log "#pad #n -", r
      r

module.exports = { eval-progs, eval-display-progs, conforms-to, res, trace }
