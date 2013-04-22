require! \tester .res

module.exports

  ..begin-assign =

    [\begin
      [\var \f [\fun [\x] [\+ \x 1]]]
      [\f 2]]

    |> res 3

  ..let-y =

    [\let
      [[\Y [\fun [\f]
            [[\fun [\u]
              [\f [\fun [\x] [[\u \u] \x]]]]
            [\fun [\u]
              [\f [\fun [\x] [[\u \u] \x]]]]]]]]
      [[\Y [\fun [\fib]
            [\fun [\n]
              [\if [\< \n 2] \n
                [\+ [\fib [\- \n 1]]
                    [\fib [\- \n 2]]]]]]]
      10]]

    |> res 55

  ..begin-rec-assign =

    [\begin
      [\var \fib
        [\fun [\n]
          [\if [\< \n 2] \n
            [\+ [\fib [\- \n 1]]
                [\fib [\- \n 2]]]]]]
      [\fib 10]]

    |> res 55

  ..let-nesting =

    [\let [[\a 1] [\b 3]]
      [\array
        [\+ \a \b]
        [\let [[\a 18]]
          [\set \a [\+ 2 \a]] \a]
        [\array \a \b]]]

    |> res [4 20 [1 3]]

  ..macro-registration =

    [\begin
      [\add-expander \xxx
        [\fun [\syn]
          [\cons [\quote \array] [\tail \syn]]]]
      [\xxx [\quote \a] 1]]
    |> res [\a 1]

  ..cond =

    [\array
      [\cond [false 1]
             [\else 2]
             [\else 3]]
      [\cond [[\= 1 [\+ 0 1]] 1]
             [false 666]
             [false 667]
             [\else 2]]
      [\cond [false 3 2]
             [true  1 0]]]

    |> res [2 1 0]

  ..letrec =

    [\letrec [[\odd
                [\fun [\x]
                  [\cond [[\= \x 1] true]
                         [[\= \x 0] false]
                         [\else [\even [\- \x 1]]]]]]
              [\even
                [\fun [\x]
                  [\cond [[\= \x 1] false]
                         [[\= \x 0] true]
                         [\else [\odd [\- \x 1]]]]]]]
      [\array [\odd 10] [\odd 11]]]

    |> res [false, true]

  ..named-let =

    [\let \loop [[\x 0]]
      [\if [\= \x 5] [\quote []]
        [\cons \x [\loop [\add1 \x]]]]]

    |> res [0 1 2 3 4]

