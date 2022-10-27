namespace DoMyHomework

module Main =
    open MyList
    open GeneralTree

    [<EntryPoint>]
    let main argv =
        let input =
            Node(
                1,
                Cons(
                    Node(
                        2,
                        Cons(
                            Node(1, Empty),
                            Cons(Node(3, Empty), Cons(Node(0, Empty), Cons(Node(-4, Empty), Empty)))
                        )
                    ),
                    Cons(Node(3, Empty), Empty)
                )
            )

        printfn "%A" (collectInTree concat oneElementList Empty input)
        printfn "%A" (collectInTree Set.union Set.empty.Add Set.empty input)

        0
