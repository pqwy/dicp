require! \tester .res

module.exports

  ..add       = [\+ 1 2] |> res 3

  ..add-mul   = [\+ 1 [\* 2 3]] |> res 7

  ..apply-fun = [[\fun [\x] [\+ \x 1]], 2] |> res 3

  ..assign =

    [[\fun []
      [\var \f [\fun [\x] [\+ \x 1]]]
      [\f 2]]]

    |> res 3

  ..recursion-y =

    [[\fun [\Y]
      [[\Y [\fun [\fib]
            [\fun [\n]
              [\if [\< \n 2] \n
                [\+ [\fib [\- \n 1]]
                    [\fib [\- \n 2]]]]]]]
      15]]
    [\fun [\f]
      [[\fun [\u]
        [\f [\fun [\x] [[\u \u] \x]]]]
      [\fun [\u]
        [\f [\fun [\x] [[\u \u] \x]]]]]]]

    |> res 610

  ..recursion-assign =

    [[\fun []
      [\var \fib
        [\fun [\n]
          [\if [\< \n 2] \n
            [\+ [\fib [\- \n 1]]
                [\fib [\- \n 2]]]]]]
      [\fib 15]]]

    |> res 610

  ..shared-env =

    [[\fun []
      [\var \getter [\void]]
      [\var \setter [\void]]
      [[\fun [\x]
        [\set \getter [\fun [] \x]]
        [\set \setter [\fun [\y] [\set \x \y]]]]
       1]
      [\array [\getter]
              [[\fun [] [\setter 2]
                        [\getter]]]]]]
    |> res [1 2]

