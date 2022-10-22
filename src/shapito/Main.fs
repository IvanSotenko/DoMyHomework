namespace DoMyHomework

module Main =

    open MyList
    open GeneralTree
    open FsCheck

    [<EntryPoint>]
    let main argv =

        let a = Set.ofList [2; 4; 5]
        let b = Set.ofList [2; 4; 5]
        printfn $"{a = b}"

        0

        // let testTree1 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))
        //
        // let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 9, Cons (Leaf 3, Cons (Leaf 0, Cons (Leaf -4, Empty))))), Cons (Leaf 3, Empty)))
        //
        // let testTree3 = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))
