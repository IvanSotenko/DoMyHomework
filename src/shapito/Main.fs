namespace shapito

module Main =

    open MyList
    open GeneralTree

    [<EntryPoint>]
    let main argv =

        let test = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))

        printf "%A" (move2 test)

        0
