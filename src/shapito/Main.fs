namespace shapito

module Main =

    open MyList
    open GeneralTree

    [<EntryPoint>]
    let main argv =

        let testTree = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))

        printf "%A" (treeList2 testTree)

        0
