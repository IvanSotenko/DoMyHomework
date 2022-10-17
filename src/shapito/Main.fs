namespace shapito

module Main =

    open MyList
    open GeneralTree

    [<EntryPoint>]
    let main argv =

        // let testTree1 = Node (Some 1, Cons (Node (Some 2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))

        let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))
        printf "%A" (treeList3 testTree2)

        0
