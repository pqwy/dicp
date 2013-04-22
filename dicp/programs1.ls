
module.exports

# 1 + 2
  ..add =
    type: \app
    op  : '+'
    args: [ 1, 2 ]

# 1 + (2 * 3)
  ..add-mul =
    type: \app
    op  : '+'
    args:
      * 1
      * type: \app
        op  : '*'
        args: [2,3]

# ((a) -> a + 1) 2
  ..apply-fun =
    type: \app
    op  :
      type: \function
      args: [\x]
      body: [ type: \app, op: \+, args: [\x, 1] ]
    args: [ 2 ]

# -> var f = ((x) -> x + 1) ; f 2
  ..assign =
    type: \app
    op  :
      type: \function
      args: []
      body:
        * type: \var
          id  : \f
          val :
            type: \function
            args: [\x]
            body: [
              type: \app
              op  : \+
              args: [\x, 1]
            ]
        * type: \app
          op  : \f
          args: [2]
    args: []


