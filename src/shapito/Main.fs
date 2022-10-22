namespace DoMyHomework

module Main =

    open MyList
    open GeneralTree
    open FsCheck

    [<EntryPoint>]
    let main argv =

        let lst =
            let rec lst' s =
                match s with
                | 0 -> gen { return Empty }
                | n when n > 0 ->
                    let sublst = lst' (n - 1)
                    Gen.map2 (fun x y -> Cons (x, y)) Arb.generate<int> sublst
                | _ -> invalidArg "s" "Only positive arguments are allowed"
            Gen.sized lst'

        printf $"{lst}"
        0

        // let testTree1 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))
        //
        // let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 9, Cons (Leaf 3, Cons (Leaf 0, Cons (Leaf -4, Empty))))), Cons (Leaf 3, Empty)))
        //
        // let testTree3 = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
