require! \tester .res

module.exports

  ..reset-nothing = [\+ 1 [\reset 1]] |> res 2

  ..cont-abort =

    [\+ 1
      [\reset
        [\* 666
          [\shift [\fun [\k] 1]]]]]

    |> res 2

  ..cont-compose =

    [\+ 1
      [\reset
        [\* 2
          [\shift
            [\fun [\k]
              [\k [\k [\k 1]]]]]]]]

    |> res 9

  ..cont-multishot =

    [\let [[\m [\reset [\* 3 [\shift \id]]]]]
      [\array [\m 1] [\m 2] [\m 3]]]

    |> res [3 6 9]

  ..traverse-fringe =

    [\begin

      [\var \scan
        [\fun [\x]
          [\cond [[\array+? \x]
                  [\scan [\head \x]] [\scan [\tail \x]]]
                 [[\null? \x] false]
                 [\else
                  [\shift
                    [\fun [\k]
                      [\array \x [\fun [] [\reset [\k]]]]]]]]]]

      [\var \tree->list
        [\fun [\x]
          [\let \loop [[\res [\reset [\scan \x]]]]
            [\if [\array+? \res]
              [\cons [\car \res] [\loop [[\cadr \res]]]]
              [\quote []]]]]]

      [\tree->list [\quote [\a \b [[\c] \d] [[[\e]]] \f]]]]

    |> res [\a \b \c \d \e \f]

  ..dictionary-as-program =

    desu: [\+ 1 2]
    bbq : [\quote \omg]

    |> res desu: 3, bbq: \omg

