namespace DoMyHomework

module Main =

    open MyList
    open GeneralTree
    open FsCheck

    [<EntryPoint>]
    let main argv =

        // let testTree1 = Node (1, Cons (Node (2, Cons (Leaf 4, Cons (Leaf 5, Empty))), Cons (Leaf 3, Empty)))
        //
        // let testTree2 = Node (1, Cons (Node (2, Cons (Leaf 9, Cons (Leaf 3, Cons (Leaf 0, Cons (Leaf -4, Empty))))), Cons (Leaf 3, Empty)))
        //
        // let testTree3 = Node ("a", Cons (Node ("b", Cons (Leaf "d", Cons (Leaf "e", Empty))), Cons (Leaf "c", Empty)))

        let rec unsafelist() = Gen.oneof [ gen { return Empty }
                                           Gen.map2 (fun x y -> Cons (x, y)) Arb.generate<int> (unsafelist())]
        // let rec unsafeTree() =
        //     Gen.oneof [ Gen.map Leaf Arb.generate<int>
        //                 Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]


        (*
        let lst =
            let rec lst' s =
                match s with
                | 0 -> gen { return Empty }
                | n when n > 0 ->
                    let sublst = lst' (n - 1)
                    Gen.oneof [ gen { return Empty }
                                Gen.map2 (fun x y -> Cons (x, y)) Arb.generate<int> sublst]
                | _ -> invalidArg "s" "Only positive arguments are allowed"
            Gen.sized lst'

        let a = lst
        printf $"{a}"
        *)

        let b = Cons (3, Cons (6, Cons (6, Cons (4, Empty))))
        let c = SetOfMyList b

        0
