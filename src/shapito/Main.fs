namespace shapito

module Main =

    open MyList
    open GeneralTree

    [<EntryPoint>]
    let main argv =

        let testTree1 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))

        let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 9, Cons (Leaf 3, Cons (Leaf 0, Cons (Leaf -4, Empty))))), Cons (Leaf 3, Empty)))

        let testTree3 = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))

        printfn "%A" (treeToList testTree2)
        printfn "%A" (countDistinct testTree2)

        let da = dict<string, int>["Canada", 10; "Germany", 20]
        let db = dict<string, int>["Canada", 10; "Germany", 20]

        printfn "%A" (da = db)
        0
