#
# This is where the common representations live.
#

# Prim and Closure are "foreign" and "native" procesures, respectively.
# They are consumed by apply and merely need to be distinct from other data, so
# we don't confuse a random dict for a procedure.
#
class prim
  (@fun) ~>
  inspect: -> "#<primitive procedure>"

class closure
  (@args, @body, @env = {}) ~>
  inspect: -> "#<procedure>"

# Assign a value in the environment *on the same level* (of the prototype chain)
# where it's defined.
#
assign = (env, k, v) ->
  case env.has-own-property k => env[k] = v
  case env.__proto__ is {}.__proto__ =>
    throw "set: #k not defined"
  case _ => assign env.__proto__, k, v

module.exports = { prim, closure, assign }
