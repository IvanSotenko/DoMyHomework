namespace DoMyHomework

module Main =

    open MyList
    open GeneralTree
    open FsCheck

    [<EntryPoint>]
    let main argv =

        let testTree2 = Node (1, Cons (Node (2, Cons (Node (9, Empty), Cons (Node (3, Empty), Cons (Node (0, Empty), Cons (Node (-4, Empty), Empty))))), Cons (Node (3, Empty), Empty)))
        let testTree4 = Node (3, Cons (Node (5, Empty), Cons (Node (2, Empty), Empty)))

        printfn "%A" (toList testTree2)
        printfn $"{countDistinct testTree2}"

        let a = Cons(3, Cons (3, Cons (6, Empty)))
        printfn "%A" (length a)

        0

        // let testTree1 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))
        //
        // let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 9, Cons (Leaf 3, Cons (Leaf 0, Cons (Leaf -4, Empty))))), Cons (Leaf 3, Empty)))
        //
        // let testTree3 = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
