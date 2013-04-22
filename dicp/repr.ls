
class prim    then (@fun) ~>
class closure then (@args, @body, @env = {}) ~>

assign = (env, k, v) ->
  case env.has-own-property k => env[k] = v
  case env.__proto__ is {}.__proto__ =>
    throw "set: #k not defined"
  case _ => assign env.__proto__, k, v

module.exports = { prim, closure, assign }
