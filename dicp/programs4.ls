require! \tester .res

module.exports

  ..deep-tails =

    [\let \loop [[\acc 0] [\x 10000]]
      [\if [\zero? \x] \acc
        [\loop [\+ \x \acc] [\sub1 \x]]]]

    |> res 50005000

  ..recursive-tails =

    [\letrec
      [[\f1 [\fun [\acc \x]
              [\f2 [\+ \acc \x] \x]]]
       [\f2 [\fun [\acc \x]
              [\if [\zero? \x] \acc
                [\f1 \acc [\- \x 1]]]]]]
      [\f1 0 10000]]

    |> res 50005000
